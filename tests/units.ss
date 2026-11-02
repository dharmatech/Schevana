;;; tests/units.ss — the drawing layer piece by piece: the measuring
;;; context, pixmaps, Life's generations and grid, and invalidation; the
;;; edit kit (editing, rows and their lazy layout), the widget kit's dates
;;; and layout, Tasks' own procedures, the window manager's surfaces
;;; and strips, and styles: derived ones, parts by name, a window's own.
;;; Shadow tiles too.   ; shadow
;;;
;;;   scheme --script tests/units.ss [output-dir]
;;;
;;; Headless. output-dir only receives a scratch theme file.

(define *schevana-root*
  (let ([dir (path-parent (path-parent (car (command-line))))])
    (if (string=? dir "") "." dir)))

(load (string-append *schevana-root* "/load.ss"))

(define out-dir
  (if (pair? (command-line-arguments)) (car (command-line-arguments)) "."))

;; Never touch the user's real theme file.
(set! *theme-file* (string-append out-dir "/theme.ss"))
(when (file-exists? *theme-file*) (delete-file *theme-file*))

(define failures 0)

(define-syntax check
  (syntax-rules ()
    [(_ name expr)
     (let ([ok (guard (c [#t (printf "  error: ~a\n" (condition->string c))
                             #f])
                 expr)])
       (unless ok (set! failures (+ failures 1)))
       (printf "~a ~a\n" (if ok "ok  " "FAIL") name))]))

(define (steps n) (do ([i 0 (+ i 1)]) ((= i n)) (schevana-step! 0)))

(define (raises? thunk) (guard (c [#t #t]) (thunk) #f))

;;; ------------------------------------------------------------------ images

;; A fresh pw × ph image drawn by thunk with *cr* on it, scaled by s.
(define (image pw ph s thunk)
  (let* ([surface (cairo-image-surface-create CAIRO_FORMAT_ARGB32 pw ph)]
         [cr (cairo-create surface)])
    (cairo-scale cr s s)
    (fluid-let ([*cr* cr]) (thunk))
    (cairo-destroy cr)
    (cairo-surface-flush surface)
    surface))

(define (pixel-at surface x y)
  (foreign-ref 'unsigned-32 (cairo-image-surface-get-data surface)
               (+ (* y (cairo-image-surface-get-stride surface)) (* 4 x))))

;; The largest channel difference between two images of the same size,
;; outside the mask rects ((x0 y0 x1 y1) ...).
(define (max-diff a b . mask)
  (unless (and (= (cairo-image-surface-get-width a)
                  (cairo-image-surface-get-width b))
               (= (cairo-image-surface-get-height a)
                  (cairo-image-surface-get-height b)))
    (errorf 'max-diff "images differ in size"))
  (cairo-surface-flush a)
  (cairo-surface-flush b)
  (let ([pa (cairo-image-surface-get-data a)]
        [pb (cairo-image-surface-get-data b)]
        [sa (cairo-image-surface-get-stride a)]
        [sb (cairo-image-surface-get-stride b)]
        [w (cairo-image-surface-get-width a)]
        [h (cairo-image-surface-get-height a)])
    (define (masked? x y)
      (exists (lambda (r)
                (and (<= (car r) x) (< x (caddr r))
                     (<= (cadr r) y) (< y (cadddr r))))
              mask))
    (let row ([y 0] [worst 0])
      (cond
        [(= y h) worst]
        [(zero? (sdl-memcmp (+ pa (* y sa)) (+ pb (* y sb)) (* 4 w)))
         (row (+ y 1) worst)]
        [else
         (let col ([x 0] [worst worst])
           (if (= x w)
               (row (+ y 1) worst)
               (let ([p (foreign-ref 'unsigned-32 pa (+ (* y sa) (* 4 x)))]
                     [q (foreign-ref 'unsigned-32 pb (+ (* y sb) (* 4 x)))])
                 (col (+ x 1)
                      (if (or (fx= p q) (masked? x y))
                          worst
                          (fxmax worst (pixel-diff p q)))))))]))))

;; The largest difference of the four channels of two pixels.
(define (pixel-diff p q)
  (let loop ([k 0] [d 0])
    (if (fx= k 32)
        d
        (loop (fx+ k 8)
              (fxmax d (fxabs (fx- (fxand (fxsrl p k) 255)
                                   (fxand (fxsrl q k) 255))))))))

(define (report-worst worst where)
  (printf "  worst difference ~a~a\n"
          worst (if where (format " at ~s" where) "")))

(define (same-bytes? a b)
  (and (= (cairo-image-surface-get-width a) (cairo-image-surface-get-width b))
       (= (cairo-image-surface-get-height a) (cairo-image-surface-get-height b))
       (zero? (max-diff a b))))

;; The canvas's pixels, copied.
(define (canvas-bytes)
  (cairo-surface-flush *canvas*)
  (let* ([n (* (cairo-image-surface-get-stride *canvas*) (canvas-pixel-height))]
         [bv (make-immobile-bytevector n)])
    (sdl-memcpy (object->reference-address bv)
                (cairo-image-surface-get-data *canvas*)
                n)
    bv))

(define (cairo-matrix-xx cr)
  (cairo-get-matrix cr *scratch*)
  (foreign-ref 'double *scratch* 0))

;;; ------------------------------------------------------------------ boot

(schevana-boot! 'headless 1280 800)

;;; ------------------------------------------------------------------
;;; measuring context

;; What text measures with *cr* on the canvas itself, as before.
(define (measured-on-canvas thunk)
  (let ([cr (cairo-create *canvas*)])
    (cairo-scale cr *scale* *scale*)
    (let ([v (fluid-let ([*cr* cr]) (thunk))])
      (cairo-destroy cr)
      v)))

(define (measures-like-canvas?)
  (let ([measure (lambda ()
                   (list (text-width "Schevana λ" 'ui)
                         (text-width "(define x 1)" 'mono)
                         (font-metrics (make-font* "sans" 13))
                         (font-metrics (make-font* "monospace" 11 'bold))))])
    (equal? (measure) (measured-on-canvas measure))))

(check "before the first frame *cr* is the measuring context"
       (and (zero? *frame-ms*) (not (eqv? *measure-cr* 0))
            (eqv? *cr* *measure-cr*) (= (cairo-matrix-xx *cr*) *scale*)))
(check
 "text-width and font-metrics work before the first frame, as on the canvas"
 (and (> (text-width "Schevana" 'ui) 0) (> (font-line-height 'ui) 0)
      (measures-like-canvas?)))

(steps 2)

(define (stray-drawing!)
  (fill-rect 0 0 600 400 'red)
  (draw-text 20 20 "stray" 'text 'big)
  (draw-shadow 100 100 300 200 9 14)   ; shadow
  (draw-pixmap (make-pixmap 2 2 #xff0000) 10 10 50 50))

(check "stray drawing between frames leaves the canvas untouched"
       (let ([before (canvas-bytes)])
         (stray-drawing!)
         (bytevector=? before (canvas-bytes))))

(set-theme! 'scale 200)
(steps 1)
(check "after a scale change *cr* measures at the new scale"
       (and (= *scale* 2.0) (eqv? *cr* *measure-cr*)
            (= (cairo-matrix-xx *cr*) 2.0)))
(check "text-width and font-metrics work after a scale change, as on the canvas"
       (and (> (text-width "Schevana" 'ui) 0) (measures-like-canvas?)))
(check "stray drawing at 200% leaves the canvas untouched"
       (let ([before (canvas-bytes)])
         (stray-drawing!)
         (bytevector=? before (canvas-bytes))))
(set-theme! 'scale 0)
(steps 1)

(check "a canvas context left in *cr* (code from before a reload) is replaced"
       (begin
         (set! *cr* (cairo-create *canvas*))
         (canvas-create! 1280 800 *scale*)
         (and (eqv? *cr* *measure-cr*) (measures-like-canvas?))))
(steps 1)

;; Measuring never depends on *cr*. (Once the fonts were made again, the
;; first measurement of 'mono could happen in Settings' Genera preview,
;; drawn at half scale, and the REPL's characters overlapped from then on.)
;; mono's char width and text-widths in 'mono and Genera's label font when
;; first measured inside (measure-in measure), on a fresh context scaled
;; like the canvas.
(define (first-measured-in measure-in)
  (fonts-rescaled!)
  (let* ([img (cairo-image-surface-create CAIRO_FORMAT_ARGB32 400 300)]
         [cr (cairo-create img)])
    (cairo-scale cr *scale* *scale*)
    (let ([v (fluid-let ([*cr* cr])
               (measure-in
                (lambda ()
                  (list (font-char-width 'mono)
                        (text-width "(set-theme! 'accent #xff9e64)" 'mono)
                        (text-width "Dynamic Lisp Listener" 'ui-italic)))))])
      (cairo-destroy cr)
      (cairo-surface-destroy img)
      v)))

(check "the first measurement of a font, in a half-scale style preview, is \
        the canvas's (100, 125, 150, 200%)"
       (let ([results
              (map (lambda (s)
                     (canvas-create! 1280 800 s)
                     (let ([normal (first-measured-in
                                    (lambda (measure) (measure)))]
                           [preview (first-measured-in
                                     (lambda (measure)
                                       ;; its label is in 'ui-italic
                                       (draw-style-preview
                                        'genera 0 0 200 150 0.5)
                                       (call-with-scale 0.5 measure)))])
                       (list s normal preview)))
                   '(1 1.25 1.5 2))])
         (canvas-create! 1280 800 *host-scale*)
         (fonts-rescaled!)
         (printf "  ~s\n" results)
         (for-all (lambda (r) (equal? (cadr r) (caddr r))) results)))
(steps 1)

;;; ------------------------------------------------------------------
;;; host window

;; (w h scale ratio) -> window units: logical size × scale / pixel ratio.
(check
 "the host window opens at the logical size times the scale, in window units"
 (equal? (map (lambda (args)
                (call-with-values (lambda () (apply host-window-size args))
                                  list))
              '((1280 800 1 1) (1280 800 2 1) (1280 800 2 2) (1280 800 3/2 2)
                (1280 800 1 2) (1024 640 5/4 1) (1281 801 7/4 2)))
         '((1280 800) (2560 1600) (1280 800) (960 600) (640 400) (1280 800)
           (1121 701))))

;; shadow{

;;; ------------------------------------------------------------------
;;; shadow tiles

;; How many 'tiles images were drawn without looking up a tile.
(define untiled 0)

;; An image holding everything the shadow of the box can touch, over a
;; gradient that goes from opaque to translucent, with after-shadow drawn
;; on top. how is 'tiles (draw-shadow), 'stack (draw-shadow-directly,
;; layer by layer over the gradient) or 'stack-alone (the stack drawn on its
;; own, then composited once, as tiles are).
(define (shadow-image how s x y w h r size after-shadow)
  (let*-values ([(bx by bw bh) (shadow-bounds x y w h size)]
                [(pw ph) (values (+ (exact (ceiling (* s (+ bx bw)))) 3)
                                 (+ (exact (ceiling (* s (+ by bh)))) 3))])
    (let* ([alone (and (eq? how 'stack-alone)
                       (image pw ph s
                              (lambda ()
                                (draw-shadow-directly x y w h r size))))]
           [img (image pw ph s
                       (lambda ()
                         (fill-gradient 0 0 (+ bx bw 3) (+ by bh 3) #x3060a0
                                        (alpha #xe0c040 0.4) 'horizontal)
                         (case how
                           [(tiles)
                            (let ([lookups *shadow-tile-clock*])
                              (draw-shadow x y w h r size)
                              (when (= lookups *shadow-tile-clock*)
                                (set! untiled (+ untiled 1))))]
                           [(stack) (draw-shadow-directly x y w h r size)]
                           [else (cairo-save *cr*)
                                 (cairo-identity-matrix *cr*)
                                 (cairo-set-source-surface *cr* alone 0 0)
                                 (cairo-paint *cr*)
                                 (cairo-restore *cr*)])
                         (after-shadow)))])
      (when alone (cairo-surface-destroy alone))
      img)))

(define (shadow-diff how-a how-b s x y w h r size after-shadow)
  (let* ([a (shadow-image how-a s x y w h r size after-shadow)]
         [b (shadow-image how-b s x y w h r size after-shadow)]
         [d (max-diff a b)])
    (cairo-surface-destroy a)
    (cairo-surface-destroy b)
    d))

(define shadow-radii '(0 6 8 9))
(define shadow-sizes '(7 10 12 14 32))

;; Runs (test s x y w h r size) over radii, sizes, both boxes and the given
;; scales and sub-pixel phases (fractions of a logical pixel); returns the
;; worst result and the case that gave it.
(define (over-shadow-cases scales phases test)
  (let ([worst 0] [where #f])
    (for-each
     (lambda (s)
       (canvas-create! 1280 800 s)
       (for-each
        (lambda (r)
          (for-each
           (lambda (size)
             (for-each
              (lambda (wh)
                (for-each
                 (lambda (ph)
                   (let* ([x (+ size 2 (car ph))] [y (+ size 2 (cadr ph))]
                          [d (test s x y (car wh) (cadr wh) r size)])
                     (when (> d worst)
                       (set! worst d)
                       (set! where
                             (list 'scale s 'r r 'size size 'box wh 'at x y)))))
                 phases))
              (list (list (+ r r 5) (+ r r 5)) '(300 200))))
           shadow-sizes))
        shadow-radii))
     scales)
    (canvas-create! 1280 800 *host-scale*)
    (values worst where)))

;; Tiles composite the finished shadow once, so the reference is the stack
;; drawn on its own and then composited: any difference is geometry.
(check "shadow tiles match the stacked shadow (within 2 levels)"
       (let-values ([(worst where)
                     (over-shadow-cases
                      '(1 1.25 1.5 2) '((0 0) (0.25 0.6) (0.5 0.15) (0.77 0.9))
                      (lambda (s x y w h r size)
                        (shadow-diff 'tiles 'stack-alone
                                     s x y w h r size void)))])
         (report-worst worst where)
         (and (<= worst 2) (= untiled 0))))

(check "translucent boxes keep the shadow under them (within 2 levels)"
       (let-values ([(worst where)
                     (over-shadow-cases
                      '(1 1.25 1.5 2) '((0 0) (0.25 0.6) (0.5 0.15) (0.77 0.9))
                      (lambda (s x y w h r size)
                        (shadow-diff 'tiles 'stack-alone s x y w h r size
                                     (lambda ()
                                       (fill-round-rect
                                        x y w h r (alpha #xf0f0f0 0.35))))))])
         (report-worst worst where)
         (<= worst 2)))

;; Drawn layer by layer over a background, each faint layer of the stack
;; rounds to 8 bits by itself: up to half a level per layer, one layer per
;; 2 pixels of size.
(check "the stack drawn over a background differs only by its own rounding"
       (let ([raw 0])
         (let-values ([(excess where)
                       (over-shadow-cases
                        '(1 2) '((0 0) (0.25 0.6))
                        (lambda (s x y w h r size)
                          (let ([d (shadow-diff 'tiles 'stack
                                                s x y w h r size void)])
                            (set! raw (max raw d))
                            (- d (ceiling (/ size 4))))))])
           (printf "  worst difference ~a; worst beyond size/4 levels ~a~a\n"
                   raw excess (if where (format " at ~s" where) ""))
           (<= excess 2))))

(define (the-only-tile)
  (let ([vals (hashtable-values *shadow-tiles*)])
    (and (= 1 (vector-length vals)) (car (vector-ref vals 0)))))

(define (tile-surfaces t)
  (list (shadow-tile-template t) (shadow-tile-top t) (shadow-tile-bottom t)
        (shadow-tile-left t) (shadow-tile-right t) (shadow-tile-center t)))

(check "tiles do not depend on who built them"
       (begin
         (canvas-create! 1280 800 1.5)
         (flush-shadow-tiles!)
         (let* ([one (image 900 700 1.5
                            (lambda () (draw-shadow 40.3 30.7 200 120 8 14)))]
                [t1 (the-only-tile)])
           ;; same phases (whole multiples of 2 logical pixels at 150%),
           ;; another box size
           (let ([kept (map (lambda (s)
                              (image (cairo-image-surface-get-width s)
                                     (cairo-image-surface-get-height s) 1
                                     (lambda ()
                                       (cairo-set-source-surface *cr* s 0 0)
                                       (cairo-paint *cr*))))
                            (tile-surfaces t1))])
             (flush-shadow-tiles!)
             (let* ([two (image 900 700 1.5
                                (lambda ()
                                  (draw-shadow 140.3 230.7 300 180 8 14)))]
                    [t2 (the-only-tile)])
               (let ([ok (and t1 t2 (not (eq? t1 t2))
                              (for-all same-bytes? kept (tile-surfaces t2))
                              (for-all (lambda (f) (= (f t1) (f t2)))
                                       (list
                                        shadow-tile-tL0 shadow-tile-tcL
                                        shadow-tile-tcR shadow-tile-tR1
                                        shadow-tile-tT0 shadow-tile-tcT
                                        shadow-tile-tcB shadow-tile-tB1)))])
                 (for-each cairo-surface-destroy (cons* one two kept))
                 (canvas-create! 1280 800 *host-scale*)
                 ok))))))

(check "a shadow painted in parts equals one painted whole"
       (let* ([whole (image 400 300 1.25
                            (lambda () (draw-shadow 30.4 20.2 250 150 9 14)))]
              [parts
               (image 400 300 1.25
                      (lambda ()
                        (for-each
                         (lambda (r)
                           (call-with-clip (car r) (cadr r) (caddr r) (cadddr r)
                             (lambda () (draw-shadow 30.4 20.2 250 150 9 14))))
                         '((0 0 120 240) (120 0 200 100) (120 100 200 140)))))]
              [ok (same-bytes? whole parts)])
         (cairo-surface-destroy whole)
         (cairo-surface-destroy parts)
         ok))

(check "non-uniform transforms fall back to the stacked shadow, bit for bit"
       (let* ([draw (lambda (shadow)
                      (image 500 400 1
                             (lambda ()
                               (fill-gradient 0 0 500 400 #x3060a0 #xe0c040
                                              'horizontal)
                               (cairo-scale *cr* 0.5 1)
                               (shadow 60.3 40.6 300 200 8 14))))]
              [a (draw draw-shadow)] [b (draw draw-shadow-directly)]
              [ok (same-bytes? a b)])
         (cairo-surface-destroy a)
         (cairo-surface-destroy b)
         ok))

(check "uniform scales inside a frame (style previews) use tiles at that scale"
       (begin
         (flush-shadow-tiles!)
         (let ([a (image 400 300 1
                         (lambda ()
                           (call-with-scale 0.5
                             (lambda () (draw-shadow 60 40 300 200 8 14)))))])
           (cairo-surface-destroy a)
           (let ([t (the-only-tile)]
                 [keys (vector->list (hashtable-keys *shadow-tiles*))])
             (and t (= 1 (length keys)) (= 0.5 (list-ref (car keys) 4)))))))

(define (owned-tile-surfaces)
  (let-values ([(ks vs) (hashtable-entries *gfx-owned*)])
    (length (filter (lambda (tag) (eq? tag 'shadow-tile)) (vector->list vs)))))

(check "the tile cache stays within its budget"
       (begin
         (canvas-create! 1280 800 2)
         (flush-shadow-tiles!)
         (let ([s (image 1200 900 2 void)] [cr #f] [ok #t])
           (set! cr (cairo-create s))
           (cairo-scale cr 2 2)
           (fluid-let ([*cr* cr])
             (do ([i 0 (+ i 1)]) ((= i 300))
               ;; 300 different keys: phases (and so tiles) differ in x and y
               (draw-shadow (+ 40 (/ i 300)) (+ 40 (/ i 97)) 300 200 9 32)
               (unless (<= *shadow-tile-bytes* shadow-tile-budget)
                 (set! ok #f))))
           (cairo-destroy cr)
           (cairo-surface-destroy s)
           (let* ([n (hashtable-size *shadow-tiles*)]
                  [ok
                   (and ok (< 0 n 300)
                        (= (* 6 n) (owned-tile-surfaces))
                        (= *shadow-tile-bytes*
                           (apply +
                                  (map (lambda (e) (shadow-tile-bytes (car e)))
                                       (vector->list
                                        (hashtable-values *shadow-tiles*))))))])
             (printf "  ~a of 300 tiles kept, ~,1f MB\n"
                     n (/ *shadow-tile-bytes* 1048576.0))
             (canvas-create! 1280 800 *host-scale*)
             ok))))

;; A tree without shadows, reloaded live, resets without flushing the
;; tiles first, and leaves draw-shadow bound for whoever still calls it.
(define (one-tile-then-unflushed-reset)
  (flush-shadow-tiles!)
  (let* ([one (image 400 300 1 (lambda () (draw-shadow 30 20 250 150 9 14)))]
         [t (the-only-tile)])
    (fluid-let ([flush-shadow-tiles! void]) (free-owned-surfaces!))
    (values one t)))

(check "tiles freed by a reset that does not flush them are not drawn from"
       (let*-values ([(one t) (one-tile-then-unflushed-reset)]
                     [(two) (image 400 300 1
                                   (lambda ()
                                     (draw-shadow 30 20 250 150 9 14)))]
                     [(t2) (the-only-tile)])
         (let ([ok (and t t2 (not (eq? t t2))
                        (= *shadow-tile-bytes* (shadow-tile-bytes t2))
                        (same-bytes? one two))])
           (cairo-surface-destroy one)
           (cairo-surface-destroy two)
           ok)))

(check "nor freed again by a flush, when a new owned surface has their address"
       (let*-values ([(one t) (one-tile-then-unflushed-reset)]
                     [(a) (shadow-tile-template t)])
         (hashtable-set! *gfx-owned* a 'test)
         (flush-shadow-tiles!)
         (let ([ok (eq? 'test (hashtable-ref *gfx-owned* a #f))])
           (hashtable-delete! *gfx-owned* a)
           (cairo-surface-destroy one)
           ok)))

(check "flushing frees every tile surface"
       (begin (flush-shadow-tiles!)
              (and (= 0 (owned-tile-surfaces)) (= 0 *shadow-tile-bytes*)
                   (= 0 (hashtable-size *shadow-tiles*)))))
;; shadow}

;;; ------------------------------------------------------------------ pixmaps

(check "color->pixel"
       (and (= (color->pixel #x123456) #xff123456)
            (let ([p (color->pixel (alpha #xffffff 0.5))])
              (and (= (fxsrl p 24) #x80) (= (fxand (fxsrl p 16) 255) #x80)
                   (= (fxand (fxsrl p 8) 255) #x80) (= (fxand p 255) #x80)))
            (= (color->pixel (alpha #x000000 0)) 0)
            (let-values ([(r g b a) (color-rgba 'accent)])
              (= (color->pixel 'accent)
                 (fxior #xff000000 (fxsll (exact (round (* 255 r))) 16)
                        (fxsll (exact (round (* 255 g))) 8)
                        (exact (round (* 255 b))))))))

(check "pixmap set and ref round-trip"
       (let ([pm (make-pixmap 3 2)])
         (pixmap-set! pm 2 1 #x123456)
         (pixmap-set-pixel! pm 0 1 #x80402010)
         (and (= (pixmap-w pm) 3) (= (pixmap-h pm) 2)
              (= (pixmap-ref pm 2 1) #xff123456)
              (= (pixmap-ref pm 0 1) #x80402010)
              (= (pixmap-ref pm 0 0) 0)
              (= (bytevector-u32-native-ref (pixmap-bytes pm) (* 4 (+ 2 3)))
                 #xff123456)
              (raises? (lambda () (pixmap-ref pm 3 0)))
              (raises? (lambda () (pixmap-set! pm 0 -1 'red))))))

(check "pixmaps reject sizes that are not positive whole numbers"
       (and (raises? (lambda () (make-pixmap 0 4)))
            (raises? (lambda () (make-pixmap 2.5 4)))))

(check "make-pixmap and pixmap-fill! fill every pixel"
       (let ([pm (make-pixmap 7 5 #x00ff00)])
         (define (all? px)
           (for-all (lambda (i)
                      (= (bytevector-u32-native-ref (pixmap-bytes pm) (* 4 i))
                         px))
                    (iota 35)))
         (and (all? #xff00ff00)
              (begin (pixmap-fill! pm (alpha #xffffff 0.5))
                     (all? #x80808080)))))

(check "pixmap-load! copies raw pixels and rejects a wrong length"
       (let ([pm (make-pixmap 2 2)] [bv (make-bytevector 16 0)])
         (bytevector-u32-native-set! bv 12 #xff010203)
         (pixmap-load! pm bv)
         (bytevector-u32-native-set! bv 12 0)
         (and (= (pixmap-ref pm 1 1) #xff010203)
              (not (eq? (pixmap-bytes pm) bv))
              (raises? (lambda () (pixmap-load! pm (make-bytevector 12 0)))))))

(check "bytevector->pixmap wraps without copying"
       (let ([bv (make-bytevector 16 0)])
         (and (eq? (pixmap-bytes (bytevector->pixmap bv 2 2)) bv)
              (raises? (lambda () (bytevector->pixmap bv 3 2))))))

(check "pixmap-swap-bytes! installs a buffer and returns the old one"
       (let* ([pm (make-pixmap 2 2 #x0000ff)]
              [old (pixmap-bytes pm)]
              [new (make-pixmap-bytevector pm)])
         (bytevector-u32-native-set! new 0 #xffabcdef)
         (and (= (bytevector-length new) 16)
              (eq? (pixmap-swap-bytes! pm new) old)
              (eq? (pixmap-bytes pm) new)
              (= (pixmap-ref pm 0 0) #xffabcdef)
              (raises? (lambda ()
                         (pixmap-swap-bytes! pm (make-bytevector 8 0)))))))

;; Each device pixel of the drawn rect shows its pixmap pixel, and nothing
;; outside the rect changes.
(define (draws-nearest? s pm)
  (canvas-create! 1280 800 s)
  (let* ([before (image *canvas-pw* *canvas-ph* s
                        (lambda ()
                          (fill-gradient 0 0 300 300 #x204060 #x806040)))]
         [after (image *canvas-pw* *canvas-ph* s
                       (lambda ()
                         (fill-gradient 0 0 300 300 #x204060 #x806040)
                         (draw-pixmap pm 100 100 20 20)))]
         [x0 (exact (* s 100))] [x1 (exact (* s 120))]
         [ok (let loop ([y 0])
               (or (= y (exact (* s 300)))
                   (and (let col ([x 0])
                          (or (= x (exact (* s 300)))
                              (and (if (and (<= x0 x) (< x x1)
                                            (<= x0 y) (< y x1))
                                       (= (pixel-at after x y)
                                          (pixmap-ref
                                           pm
                                           (quotient (- x x0) (* s 10))
                                           (quotient (- y x0) (* s 10))))
                                       (= (pixel-at after x y)
                                          (pixel-at before x y)))
                                   (col (+ x 1)))))
                        (loop (+ y 1)))))])
    (cairo-surface-destroy before)
    (cairo-surface-destroy after)
    (canvas-create! 1280 800 *host-scale*)
    ok))

(define (quadrants bv)
  (let ([pm (bytevector->pixmap bv 2 2)])
    (for-each (lambda (i px) (pixmap-set-pixel! pm (mod i 2) (quotient i 2) px))
              '(0 1 2 3) '(#xffff0000 #xff00ff00 #xff0000ff #xff123456))
    pm))

(check "draw-pixmap is nearest-neighbor at 100%"
       (draws-nearest? 1 (quadrants (make-immobile-bytevector 16 0))))
(check "draw-pixmap is nearest-neighbor at 200%"
       (draws-nearest? 2 (quadrants (make-immobile-bytevector 16 0))))
(check "draw-pixmap works with an ordinary (movable) bytevector"
       (draws-nearest? 2 (quadrants (make-bytevector 16 0))))

;;; ------------------------------------------------------------------ Life

;; The next generation on a torus, the obvious way: the reference.
(define (life-reference grid cols rows)
  (let ([next (make-bytevector (* cols rows) 0)])
    (define (at x y)
      (bytevector-u8-ref grid (+ (mod x cols) (* (mod y rows) cols))))
    (do ([y 0 (+ y 1)]) ((= y rows) next)
      (do ([x 0 (+ x 1)]) ((= x cols))
        (let ([n (apply + (map (lambda (d) (at (+ x (car d)) (+ y (cdr d))))
                               '((-1 . -1) (0 . -1) (1 . -1)
                                 (-1 . 0) (1 . 0)
                                 (-1 . 1) (0 . 1) (1 . 1))))])
          (bytevector-u8-set! next (+ x (* y cols))
                              (if (or (= n 3) (and (= n 2) (= 1 (at x y))))
                                  1
                                  0)))))))

;; A grid from lines of text, # for a live cell.
(define (life-grid . lines)
  (let* ([cols (string-length (car lines))]
         [g (make-bytevector (* cols (length lines)) 0)])
    (for-each (lambda (line y)
                (for-each (lambda (x)
                            (when (char=? #\# (string-ref line x))
                              (bytevector-u8-set! g (+ x (* y cols)) 1)))
                          (iota cols)))
              lines (iota (length lines)))
    g))

(define (random-life-grid cols rows)
  (let ([g (make-bytevector (* cols rows) 0)])
    (do ([i 0 (+ i 1)]) ((= i (* cols rows)) g)
      (bytevector-u8-set! g i (if (< (random 1.0) 0.3) 1 0)))))

;; The grid moved by (dx, dy) on the torus.
(define (life-shift grid cols rows dx dy)
  (let ([g (make-bytevector (* cols rows) 0)])
    (do ([i 0 (+ i 1)]) ((= i (* cols rows)) g)
      (bytevector-u8-set! g
                          (+ (mod (+ (mod i cols) dx) cols)
                             (* cols (mod (+ (quotient i cols) dy) rows)))
                          (bytevector-u8-ref grid i)))))

;; k generations by the kernel, checking every generation against the
;; reference and every pixel against its cell.
(define (life-kernel-ok? grid cols rows k)
  (let ([row-px (life-row-pixels rows)]
        [dead (color->pixel 'bg-0)]
        [pix (make-bytevector (* 4 cols rows) 0)])
    (let loop ([i 0] [g grid])
      (or (= i k)
          (let ([next (make-bytevector (* cols rows) 0)])
            (life-step-rows! g next pix row-px dead cols rows 0 rows)
            (and (equal? next (life-reference g cols rows))
                 (for-all (lambda (c)
                            (= (bytevector-u32-native-ref pix (* 4 c))
                               (if (= 1 (bytevector-u8-ref next c))
                                   (fxvector-ref row-px (quotient c cols))
                                   dead)))
                          (iota (* cols rows)))
                 (loop (+ i 1) next)))))))

;; k generations by the kernel alone.
(define (life-kernel-run grid cols rows k)
  (let ([row-px (life-row-pixels rows)]
        [pix (make-bytevector (* 4 cols rows) 0)])
    (let loop ([i 0] [g grid])
      (if (= i k)
          g
          (let ([next (make-bytevector (* cols rows) 0)])
            (life-step-rows! g next pix row-px 0 cols rows 0 rows)
            (loop (+ i 1) next))))))

(let ([block (life-grid "......" ".##..." ".##..." "......" "......")]
      [blinker (life-grid "....." "....." ".###." "....." ".....")]
      [glider (life-grid ".#......" "..#....." "###....." "........"
                         "........" "........" "........" "........")])
  (check "Life's kernel: a block stays, a blinker turns"
         (and (equal? (life-kernel-run block 6 5 1) block)
              (equal? (life-kernel-run blinker 5 5 1)
                      (life-grid "....." "..#.." "..#.." "..#.." "....."))
              (equal? (life-kernel-run blinker 5 5 2) blinker)))
  (check "Life's kernel: a glider moves one cell down and right in 4 \
          generations, and around an 8 × 8 torus in 32"
         (and (equal? (life-kernel-run glider 8 8 4)
                      (life-shift glider 8 8 1 1))
              (equal? (life-kernel-run glider 8 8 12)
                      (life-shift glider 8 8 3 3))
              (equal? (life-kernel-run glider 8 8 32) glider))))

(random-seed 1717)
(check "Life's kernel equals the reference on random grids over 20 \
        generations, its pixels its cells (72 × 44, 37 × 23, 5 × 3, 3 × 3, \
        1 × 5, 4 × 1)"
       (for-all (lambda (size)
                  (life-kernel-ok? (random-life-grid (car size) (cadr size))
                                   (car size) (cadr size) 20))
                '((72 44) (37 23) (5 3) (3 3) (1 5) (4 1))))

(check "Life's kernel on rows [y0, y1) writes those rows of the generation \
        and nothing else"
       (let* ([g (random-life-grid 72 44)]
              [whole (life-reference g 72 44)]
              [part (make-bytevector (* 72 44) 9)]
              [pix (make-bytevector (* 4 72 44) 0)])
         (life-step-rows! g part pix (life-row-pixels 44) 0 72 44 10 27)
         (for-all (lambda (i)
                    (= (bytevector-u8-ref part i)
                       (if (<= (* 10 72) i (- (* 27 72) 1))
                           (bytevector-u8-ref whole i)
                           9)))
                  (iota (* 72 44)))))

(define (bytes-total) (+ (bytes-allocated) (bytes-deallocated)))

(let* ([g (random-life-grid 72 44)]
       [next (make-bytevector (* 72 44) 0)]
       [pix (make-bytevector (* 4 72 44) 0)]
       [row-px (life-row-pixels 44)]
       [dead (color->pixel 'bg-0)])
  (life-step-rows! g next pix row-px dead 72 44 0 44)
  (let ([b0 (bytes-total)])
    (do ([i 0 (fx+ i 1)]) ((fx= i 20))
      (life-step-rows! g next pix row-px dead 72 44 0 44))
    (let ([b (- (bytes-total) b0)])
      ;; an 8-argument + made each generation allocate about 300 KB
      (check (format "Life's kernel allocates nothing (~a bytes in 20 \
                      generations at 72 × 44)" b)
             (< b 1024)))))

;; Life's draw handler into a pw × ph image at scale s, and what it gave
;; draw-life-cells: (pixels cols rows cell ox oy).
(define (life-drawn win s)
  (let-values ([(cw ch) (window-content-size win)])
    (let* ([seen #f]
           [a (fluid-let ([draw-life-cells
                           (let ([real draw-life-cells])
                             (lambda args (set! seen args) (apply real args)))])
                (image (exact (ceiling (* s cw))) (exact (ceiling (* s ch))) s
                       (lambda ()
                         ((hashtable-ref (window-handlers win) 'draw #f)
                          win cw ch))))])
      (values a seen))))

;; The live cells a window shows: those whose pixel is not bg-0's.
(define (life-shown win)
  (let-values ([(a seen) (life-drawn win 1)])
    (cairo-surface-destroy a)
    (apply (lambda (pixels cols rows cell ox oy)
             (let ([g (make-bytevector (* cols rows) 0)]
                   [dead (color->pixel 'bg-0)])
               (do ([i 0 (+ i 1)]) ((= i (* cols rows)) g)
                 (unless (= dead
                            (pixmap-ref pixels (mod i cols) (quotient i cols)))
                   (bytevector-u8-set! g i 1)))))
           seen)))

(let ([win (life)])
  (window-call win 'key #\space '())   ; paused
  (let ([shown (life-shown win)])
    (window-call win 'key #\n '())
    (check "Life's n key shows the next generation"
           (and (> (apply + (bytevector->u8-list shown)) 100)
                (equal? (life-shown win) (life-reference shown 72 44)))))
  (window-call win 'key #\n '())
  (let ([b0 (bytes-total)])
    (do ([i 0 (+ i 1)]) ((= i 20)) (window-call win 'key #\n '()))
    (let ([kb (/ (- (bytes-total) b0) 20 1024.0)])
      ;; the handler's call and (invalidate! win) take about 2.4 KB of it
      (check (format "a step of Life (its n key) allocates under 8 KB: \
                      ~,1f KB (about 300 KB before its kernel)" kb)
             (< kb 8))))
  (close-window! win))

;; Life's draw handler into an image, next to the cells drawn the old way:
;; bg-0 everywhere, each live cell a rectangle. Compared above the status
;; bar.
(define (life-matches-rects? s content-w content-h)
  (canvas-create! 1280 800 s)
  (let ([win (life)])
    (resize-window! win content-w content-h)
    (let-values ([(cw ch) (window-content-size win)]
                 [(a seen) (life-drawn win s)])
      (let* ([pw (cairo-image-surface-get-width a)]
             [ph (cairo-image-surface-get-height a)]
             [dead (color->pixel 'bg-0)] [live 0]
             [b (apply (lambda (pixels cols rows cell ox oy)
                         (image pw ph s
                                (lambda ()
                                  (fill-rect 0 0 cw ch 'bg-0)
                                  (do ([y 0 (+ y 1)]) ((= y rows))
                                    (do ([x 0 (+ x 1)]) ((= x cols))
                                      (unless (= dead (pixmap-ref pixels x y))
                                        (set! live (+ live 1))
                                        (fill-rect
                                         (+ ox (* x cell) 1) (+ oy (* y cell) 1)
                                         (- cell 1) (- cell 1)
                                         (mix 'green 'cyan (/ y rows)))))))))
                       seen)]
             [grid-bottom (exact (floor (* s (- ch (life-bar-height)))))]
             [ok (and (> live 100)
                      (= 0 (max-diff a b (list 0 grid-bottom pw ph))))])
        (cairo-surface-destroy a)
        (cairo-surface-destroy b)
        (close-window! win)
        (canvas-create! 1280 800 *host-scale*)
        ok))))

(check "Life's pixmap drawing equals per-cell rectangles at 100%"
       (and (life-matches-rects? 1 720 466) (life-matches-rects? 1 803 515)))
(check "Life's pixmap drawing equals per-cell rectangles at 200%"
       (and (life-matches-rects? 2 720 466) (life-matches-rects? 2 803 515)))

;;; ------------------------------------------------------------------
;;; invalidation

(let ([w (open-window "Dirty" 'size '(200 . 100))])
  (define (asks? thunk) (set! *dirty* #f) (thunk) *dirty*)
  (check "(invalidate!), (invalidate! #f), (invalidate! win), \
          (invalidate! win x y w h) and (request-frame!) ask for a frame"
         (for-all asks? (list (lambda () (invalidate!))
                              (lambda () (invalidate! #f))
                              (lambda () (invalidate! w))
                              (lambda () (invalidate! w 1 2 3 4))
                              request-frame!)))
  (let-values ([(ex ey ew eh) (window-extent w)])
    (define (asked thunk) (set! *dirty* #f) (thunk) *dirty*)
    (check "(invalidate! win) and (invalidate! win x y w h) ask for its \
            extent; areas asked for make their bounding box, and the \
            whole screen holds them all"
           (and (equal? (asked (lambda () (invalidate! w)))
                        (list ex ey (+ ex ew) (+ ey eh)))
                (equal? (asked (lambda () (invalidate! w -5 10 30 500)))
                        (list ex ey (+ ex ew) (+ ey eh)))
                (equal? (asked (lambda ()
                                 (request-frame! 10 20 30 40)
                                 (request-frame! 100 5 10 10)))
                        '(10 5 110 60))
                (eq? (asked (lambda () (request-frame!) (invalidate! w)))
                     #t))))
  (let ([plain *styles*]
        [bleed-of (lambda (w)   ; how far w's extent reaches past its frame
                    (let-values ([(x y ew eh) (window-extent w)])
                      (- (window-x w) x)))])
    (define-window-style wide "Wide" 'bleed (lambda (win) 5))
    (override! 'shadow 0)   ; shadow
    (window-set! w 'window-style 'wide)
    (let ([wide (bleed-of w)])
      (window-set! w 'window-style 'mac)
      (override! 'shadow -6)   ; shadow
      (let ([narrow (bleed-of w)])
        (override! 'shadow)   ; shadow
        (set! *styles* plain)
        (check (format "a style's 'bleed widens its windows' extent (~a); \
                        nothing makes it smaller than the frame (~a)"
                       wide narrow)
               (and (= wide 5) (= narrow 0))))))
  (window-call w 'key #\a '())   ; no key handler: nothing to see
  (window-on! w 'key (lambda (win key mods) (void)))
  (check "input to a handler asks for a frame"
         (asks? (lambda () (window-call w 'key #\a '()))))
  (window-on! w 'key (lambda (win key mods) (current-window)))
  (check "handlers run with (current-window) set"
         (and (eq? w (window-call w 'key #\a '())) (not (current-window))))
  (close-window! w))

;;; ------------------------------------------------------------------
;;; long text: cut, colored and searched in time at any length

;; Whether thunk ends within the fuel an app's handler gets.
(define (within-fuel? thunk)
  ((make-engine thunk) handler-fuel (lambda (left . r) #t) (lambda (e) #f)))

;; fit-text as plainly as it can be said: a character shorter at a time.
(define (fit-plainly str max-w font)
  (if (<= (text-width str font) max-w)
      str
      (let loop ([n (- (string-length str) 1)])
        (let ([s (string-append (substring str 0 (max n 0)) "…")])
          (if (or (<= n 0) (<= (text-width s font) max-w)) s (loop (- n 1)))))))

(check "fit-text cuts where cutting a character at a time does: window, \
        task and made-up titles, other scripts, in five fonts and many \
        widths"
       (let ([strings
              (append
               (list "" "a" "…" "REPL" "REPL — running · Ctrl+C to interrupt"
                     "Life" "Sketch" "Settings" "Tasks" "Clock" "Monitor"
                     "Überschrift: Größenänderung — ½ × 東京 λ"
                     "Ελληνικά και Русский текст с ударени́ем"
                     "emoji 😀 and a tab\tand spaces    end"
                     (make-string 40 #\λ) (make-string 60 #\i)
                     (make-string 26 #\W))
               (begin (tasks-reset!) (map task-title *tasks*))
               (let ([abc "aW i.λ東Ü\x301;😀-"])   ; made up, the same each run
                 (map (lambda (i)
                        (list->string
                         (map (lambda (j)
                                (string-ref abc (mod (* (+ i 3) (+ j 7) 37)
                                                     (string-length abc))))
                              (iota (mod (* i 17) 90)))))
                      (iota 30))))])
         (for-all (lambda (font)
                    (for-all (lambda (w)
                               (for-all (lambda (s)
                                          (equal? (fit-text s w font)
                                                  (fit-plainly s w font)))
                                        strings))
                             '(-1 0 6 13 25 40 64 100 150 220 320)))
                  '(ui title ui-small mono-small heading))))

(check "fit-text measures log n times: 100,000 characters in at most 18"
       (let* ([n 0] [orig text-width]
              [s (dynamic-wind
                   (lambda ()
                     (set! text-width
                           (lambda args (set! n (+ n 1)) (apply orig args))))
                   (lambda () (fit-text (make-string 100000 #\W) 300 'ui))
                   (lambda () (set! text-width orig)))]
              [k (- (string-length s) 1)])
         (and (<= 1 n 18)
              (<= (text-width s 'ui) 300)
              (> (text-width (string-append (make-string (+ k 1) #\W) "…")
                             'ui)
                 300))))

;; The characters text-width is given while thunk runs.
(define (characters-measured thunk)
  (let ([n 0] [plain text-width])
    (dynamic-wind
      (lambda ()
        (set! text-width
              (case-lambda
                [(s) (set! n (+ n (string-length s))) (plain s)]
                [(s f) (set! n (+ n (string-length s))) (plain s f)])))
      thunk
      (lambda () (set! text-width plain)))
    n))

;; words, then spaces, to n characters: "words words wo".
(define (words-of n)
  (substring (apply string-append (make-list (+ 1 (quotient n 6)) "words "))
             0 n))

(check "fit-text looks at no more characters than its width could show: as \
        many measured to fit 100,000 characters into 300 pixels as to fit \
        2,000, with words or without"
       (for-all (lambda (make)
                  (= (characters-measured
                      (lambda () (fit-text (make 2000) 300 'ui)))
                     (characters-measured
                      (lambda () (fit-text (make 100000) 300 'ui)))))
                (list words-of (lambda (n) (make-string n #\W)))))

(check "text-within: a string cut to one character more than its width in \
        pixels, the string itself when it is no longer"
       (let ([s "a short one"])
         (and (equal? (text-within (make-string 500 #\i) 40.7)
                      (make-string 41 #\i))
              (eq? (text-within s 40) s) (eq? (text-within s 10) s)
              (equal? (text-within s 3) "a sh")
              (equal? (text-within s -5) "a"))))

(check "Reticle's spaced title: cut with … to its room, and an empty one \
        stays empty however little room is left"
       (and (equal? (reticle-label "Tasks" 1000) "TASKS")
            (string-suffix? "…" (reticle-label (make-string 40 #\m) 100))
            (for-all (lambda (room) (equal? (reticle-label "" room) ""))
                     '(-40 -3 -1 0 50))))

(check "app-color: the color the whole hash gives, and a name of 100,000 \
        characters within a handler's fuel"
       (and (for-all
             (lambda (name)
               (eq? (app-color name)
                    (list-ref app-colors
                              (mod (fold-left (lambda (h c)
                                                (+ (* h 31) (char->integer c)))
                                              7
                                              (string->list
                                               (symbol->string name)))
                                   (length app-colors)))))
             '(repl life settings tasks clock |Pavel Uvarov| perf λ ||))
            (let ([long (string->symbol (make-string 100000 #\a))])
              (within-fuel? (lambda () (app-color long))))))

(check "string-contains-ci?: case ignored, the empty string found, and a \
        word missing from 100,000 characters looked for within a handler's \
        fuel"
       (and (string-contains-ci? "Hello World" "O w")
            (string-contains-ci? "ΛΑΜΒΔΑ λ" "λαμ")
            (string-contains-ci? "abc" "") (string-contains-ci? "" "")
            (not (string-contains-ci? "abc" "abcd"))
            (not (string-contains-ci? "" "a"))
            (string-contains-ci? "xxabcab" "cab")
            (let ([s (apply string-append (make-list 12500 "abcdefg "))]
                  [w (make-string 1000 #\z)])
              (within-fuel? (lambda () (string-contains-ci? s w))))))

;;; ------------------------------------------------------------------
;;; the edit kit, the widget kit and Tasks, pure
;;; Text editing, rows, dates, flow and table keys, and Tasks' own
;;; procedures, with no window: text measures 7 px a character.

(define (m7 s) (* 7 (string-length s)))
(define (lay-of text width . measure)
  (edit-layout-of #f text width #f (if (pair? measure) (car measure) m7) #f))
(define (ed text caret . anchor)
  (make-edit text caret (if (pair? anchor) (car anchor) caret) #f #f))
(define (state e) (list (edit-text e) (edit-caret e) (edit-anchor e)))
(define (key e k . mods)
  (edit-key e k mods (lay-of (edit-text e) 1000) 1))
(define fbq "foo-bar  baz.qux")

(check "insert at a caret, over a selection, at both ends, into empty text"
       (and (equal? (state (edit-insert (ed "abc" 1) "X")) '("aXbc" 2 2))
            (equal? (state (edit-insert (ed "abcd" 1 3) "X")) '("aXd" 2 2))
            (equal? (state (edit-insert (ed "abc" 0) "X")) '("Xabc" 1 1))
            (equal? (state (edit-insert (ed "abc" 3) "X")) '("abcX" 4 4))
            (equal? (state (edit-insert (ed "" 0) "X")) '("X" 1 1))
            (equal? (state (edit-insert (ed "abc" 1 2) "")) '("ac" 1 1))))

(check "with a most, typing stops at it, a paste is cut to fit, and a \
        selection makes room; no most is no limit"
       (and (equal? (state (edit-insert (ed "abc" 3) "X" 3)) '("abc" 3 3))
            (equal? (state (edit-insert (ed "abc" 1) "XYZ" 5)) '("aXYbc" 3 3))
            (equal? (state (edit-insert (ed "abcd" 1 3) "XYZ" 4))
                    '("aXYd" 3 3))
            (equal? (state (edit-insert (ed "abc" 3) "XY" #f))
                    '("abcXY" 5 5))))

(check "what changes nothing keeps the very string: an empty insert, an \
        erase of nothing, a move, a text clean already"
       (let ([t "abc"])
         (and (eq? t (edit-text (edit-insert (ed t 1) "")))
              (eq? t (edit-text (key (ed t 0) 'backspace)))
              (eq? t (edit-text (key (ed t 1) 'right)))
              (eq? t (edit-text (edit-insert (ed t 3) "X" 3)))
              (eq? t (edit-clean t #t)))))

(check "an edit says what it changed, the text before and the range \
        replaced; a caret move keeps that word, and so does an edit \
        that changes nothing"
       (let* ([t "abcdef"] [e1 (edit-insert (ed t 2 4) "XYZ")]
              [e2 (key e1 'left)] [e3 (edit-insert e2 "")]
              [e4 (key (ed t 3) 'backspace)])
         (and (equal? (edit-change e1) (list t 2 4))
              (eq? (car (edit-change e1)) t)
              (eq? (edit-change e2) (edit-change e1))
              (eq? (edit-change e3) (edit-change e1))
              (equal? (edit-change e4) (list t 2 3))
              (not (edit-change (ed t 3))))))

(check "Backspace at the start and Delete at the end change nothing; \
        they erase a character, a selection, or a word with Ctrl"
       (and (equal? (state (key (ed "abc" 0) 'backspace)) '("abc" 0 0))
            (equal? (state (key (ed "abc" 3) 'delete)) '("abc" 3 3))
            (equal? (state (key (ed "abc" 2) 'backspace)) '("ac" 1 1))
            (equal? (state (key (ed "abc" 1) 'delete)) '("ac" 1 1))
            (equal? (state (key (ed "abcd" 3 1) 'backspace)) '("ad" 1 1))
            (equal? (state (key (ed fbq 16) 'backspace 'ctrl))
                    '("foo-bar  baz." 13 13))
            (equal? (state (key (ed fbq 9) 'backspace 'ctrl)) '("baz.qux" 0 0))
            (equal? (state (key (ed fbq 0) 'delete 'ctrl)) '("  baz.qux" 0 0))))

(check "Left and Right collapse a selection to its side; Shift extends it \
        and shrinks it across the anchor; Ctrl moves by words; Home, End"
       (and (equal? (state (key (ed "abcd" 3 1) 'left)) '("abcd" 1 1))
            (equal? (state (key (ed "abcd" 1 3) 'right)) '("abcd" 3 3))
            (equal? (state (key (ed "abcd" 2) 'right 'shift)) '("abcd" 3 2))
            (equal? (state (key (key (key (ed "abcd" 2) 'right 'shift)
                                     'left 'shift)
                                'left 'shift))
                    '("abcd" 1 2))
            (equal? (state (key (ed fbq 0) 'right 'ctrl)) (list fbq 7 7))
            (equal? (state (key (ed fbq 7) 'right 'ctrl)) (list fbq 12 12))
            (equal? (state (key (ed fbq 13) 'left 'ctrl)) (list fbq 9 9))
            (equal? (state (key (ed fbq 12) 'left 'ctrl 'shift))
                    (list fbq 9 12))
            (equal? (state (key (ed "abc" 1) 'home)) '("abc" 0 0))
            (equal? (state (key (ed "abc" 1) 'end 'shift)) '("abc" 3 1))))

(define (bounds f text i) (call-with-values (lambda () (f text i)) list))
(check "a double click's word: at its start, end and inside, on spaces, \
        across - and _, on a word's last character; a triple click's line"
       (and (equal? (bounds edit-word-bounds "foo bar" 0) '(0 3))
            (equal? (bounds edit-word-bounds "foo bar" 7) '(4 7))
            (equal? (bounds edit-word-bounds "foo bar" 1) '(0 3))
            (equal? (bounds edit-word-bounds "foo bar" 3) '(0 3))
            (equal? (bounds edit-word-bounds "a   b" 2) '(1 4))
            (equal? (bounds edit-word-bounds "foo-bar_baz qux" 5) '(0 11))
            (equal? (bounds edit-word-bounds "a.b" 1) '(0 1))
            (equal? (bounds edit-word-bounds "a.b" 2) '(2 3))
            (equal? (bounds edit-line-bounds "ab\ncd\nef" 4) '(3 5))))

(check "Ctrl+A selects everything; λ is one character"
       (let ([e (key (ed "λx y" 1) #\a 'ctrl)])
         (and (equal? (state e) '("λx y" 4 0))
              (equal? (edit-selection e) "λx y")
              (equal? (state (key (ed "λx" 0) 'right)) '("λx" 1 1)))))

(check "edit-key leaves Return, Escape, Tab, typed keys and Ctrl+C"
       (for-all (lambda (k) (not (apply key (ed "abc" 1) k)))
                '((return) (escape) (tab) (#\a) (#\c ctrl))))

(define (wrap text width . measure)
  (vector->list (edit-wrap text width (if (pair? measure) (car measure) m7))))
(check "wrapping: a row, newlines, empty text, a trailing newline; a soft \
        break keeps its space; a word wider than the width, and a width \
        under one character, break between characters; a last space \
        hangs"
       (and (equal? (wrap "hello" 100) '((0 . 5)))
            (equal? (wrap "ab\ncd" 100) '((0 . 2) (3 . 5)))
            (equal? (wrap "" 100) '((0 . 0)))
            (equal? (wrap "ab\n" 100) '((0 . 2) (3 . 3)))
            (equal? (wrap "aaa bbb ccc" 56) '((0 . 8) (8 . 11)))
            (equal? (wrap "aaa bbb ccc" 42) '((0 . 4) (4 . 8) (8 . 11)))
            (equal? (wrap "abcdefgh" 21) '((0 . 3) (3 . 6) (6 . 8)))
            (equal? (wrap "abc" 3) '((0 . 1) (1 . 2) (2 . 3)))
            (equal? (wrap "hello " 35) '((0 . 6)))))

;; Widths that differ: a space and narrow letters 3, wide ones 11, the
;; rest 7.
(define (mp s)
  (let loop ([i 0] [w 0])
    (if (= i (string-length s))
        w
        (loop (+ i 1) (+ w (case (string-ref s i)
                             [(#\space #\i #\l) 3]
                             [(#\m #\w) 11]
                             [else 7]))))))

;; The wrapping rule as plainly as it can be said: each row measures the
;; rest of its paragraph, then every space, then every prefix.
(define (wrap-plainly text width . measure)
  (let* ([n (string-length text)]
         [m (if (pair? measure) (car measure) m7)]
         [fits? (lambda (s e) (<= (m (substring text s e)) width))]
         [end (lambda (p)
                (let loop ([q p])
                  (if (or (= q n) (char=? (string-ref text q) #\newline))
                      q
                      (loop (+ q 1)))))])
    (let para ([p 0] [rows '()])
      (let ([q (end p)])
        (define (next rows) (if (< q n) (para (+ q 1) rows) (reverse rows)))
        (let row ([s p] [rows rows])
          (cond
            [(and (= s q) (> s p)) (next rows)]
            [(fits? s q) (next (cons (cons s q) rows))]
            [else
             (let* ([ks (filter (lambda (k) (and (char=? (string-ref text k)
                                                         #\space)
                                                 (fits? s k)))
                                (map (lambda (i) (+ s i)) (iota (- q s))))]
                    [e (if (pair? ks)
                           (+ 1 (apply max ks))
                           (let loop ([j (+ s 1)])
                             (if (and (< j q) (fits? s (+ j 1)))
                                 (loop (+ j 1))
                                 j)))])
               (row e (cons (cons s e) rows)))]))))))

(define (next-random x) (mod (+ (* x 1103515245) 12345) 2147483648))

;; A made-up text of len characters from seed, of the letters in abc.
(define (made-up len seed abc)
  (let fill ([k 0] [x seed] [acc '()])
    (if (= k len)
        (values (list->string acc) x)
        (fill (+ k 1) (next-random x)
              (cons (string-ref abc (mod (quotient x 9) (string-length abc)))
                    acc)))))

;; The rows as a typist breaks them, a character at a time, sharing no
;; code or search with the kit: a row takes characters while what it
;; holds fits, minding the last space before which it fit; at one that
;; does not fit, it breaks after that space (which hangs past the width),
;; else before the character, keeping one at least. A newline ends a row.
(define (wrap-by-character text width m)
  (let ([n (string-length text)])
    (define (end? j) (or (= j n) (char=? (string-ref text j) #\newline)))
    (define (fits? s e) (<= (m (substring text s e)) width))
    (let row ([s 0] [p 0] [rows '()])   ; a row from s, its paragraph's p
      (if (and (> s p) (end? s))   ; a break ended the paragraph
          (if (= s n) (reverse rows) (row (+ s 1) (+ s 1) rows))
          (let walk ([j s] [space #f])   ; [s, j) fits
            (if (end? j)
                (let ([rows (cons (cons s j) rows)])
                  (if (= j n) (reverse rows) (row (+ j 1) (+ j 1) rows)))
                (let ([space (if (char=? (string-ref text j) #\space)
                                 j
                                 space)])
                  (if (fits? s (+ j 1))
                      (walk (+ j 1) space)
                      (let ([e (if space (+ space 1) (max j (+ s 1)))])
                        (row e p (cons (cons s e) rows)))))))))))

(check "a character at a time breaks the made-up texts where the plain \
        rule does"
       (let loop ([i 0] [seed 5])
         (or (= i 500)
             (let-values ([(text x) (made-up (mod (quotient seed 7) 41) seed
                                             "aimw  \n")])
               (let ([width (mod x 78)] [m (if (even? i) m7 mp)])
                 (and (equal? (wrap-by-character text width m)
                              (wrap-plainly text width m))
                      (loop (+ i 1) (next-random x))))))))

(check "wrapping breaks every text where the plain rule does: 2000 made-up \
        texts of letters, spaces and newlines, at widths from 0 to 11 \
        characters, measured alike and unlike"
       (let loop ([i 0] [seed 7])
         (or (= i 2000)
             (let-values ([(text x) (made-up (mod (quotient seed 7) 41) seed
                                             "aimw  \n")])
               (let ([width (mod x 78)] [m (if (even? i) m7 mp)])
                 (and (equal? (wrap text width m) (wrap-plainly text width m))
                      (loop (+ i 1) (next-random x))))))))

;; A layout's rows, computed to the end.
(define (rows-of lay)
  (edit-layout-extend! lay (string-length (edit-layout-text lay)))
  (map (lambda (r) (call-with-values (lambda () (edit-row lay r)) cons))
       (iota (edit-layout-count lay))))

(check "the lazy layout follows edits: after each of 3000 random \
        insertions, deletions and replacements, told where or not, the \
        rows it keeps, takes back and computes, partly or to the end, are \
        those a character at a time finds"
       (let loop ([i 0] [seed 11] [text ""] [lay #f] [width 40] [m m7])
         (cond
           [(= i 3000) #t]
           [(zero? (mod i 100))   ; a new width and measure, a new text
            (let-values ([(t x) (made-up 300 seed "aaimw  \n")])
              (loop (+ i 1) (next-random x) t #f (+ 10 (mod x 90))
                    (if (even? (quotient i 100)) m7 mp)))]
           [else
            (let*-values ([(n) (string-length text)]
                          [(lo) (mod seed (+ n 1))]
                          [(hi) (if (zero? (mod i 3))   ; an insertion
                                    lo
                                    (min n (+ lo (mod (quotient seed 5)
                                                      (if (zero? (mod i 11))
                                                          90
                                                          9)))))]
                          [(ins x) (made-up (if (= (mod i 3) 1)
                                                0
                                                (mod (quotient seed 3) 7))
                                            (next-random seed) "aimw \n")]
                          [(new) (string-append (substring text 0 lo) ins
                                                (substring text hi n))]
                          ;; told where, as by an edit, or compared
                          [(lay) (edit-layout-of lay new width #f m
                                                 (and (even? (quotient i 2))
                                                      (list text lo hi)))])
              ;; as a draw would: rows as far as some index, sometimes all
              (if (zero? (mod x 5))
                  (edit-layout-extend! lay (string-length new))
                  (edit-row-at lay (mod x (+ 1 (string-length new)))))
              (and (or (not (zero? (mod i 7)))
                       (equal? (rows-of (lay-of new width m)) (rows-of lay)))
                   (equal? (wrap-by-character new width m) (rows-of lay))
                   (loop (+ i 1) (next-random x) new lay width m)))])))

(check "a layout computes rows only as far as they are asked for, and \
        remembers them; its total is a count once they reach the end, \
        an estimate before"
       (let* ([n 0] [count (lambda (s) (set! n (+ n 1)) (m7 s))]
              [text (apply string-append (make-list 2000 "words of a row "))]
              [lay (lay-of text 280 count)])
         (edit-layout-extend! lay 14)
         (let ([first (edit-layout-count lay)]
               [estimate (edit-layout-total lay)])
           (edit-row-at lay 3000)
           (let ([again n])
             (edit-row-at lay 3000)
             (and (= first 15) (= again n) (not (edit-layout-done? lay))
                  (edit-layout-scan! lay 1000) (edit-layout-done? lay)
                  (= (edit-layout-total lay) (edit-layout-count lay)
                     (length (wrap text 280)))
                  (< (abs (- estimate (edit-layout-count lay)))
                     (* 1/10 (edit-layout-count lay))))))))

;; The worst cases' texts at m7 and 280 px (40 characters a row): one
;; paragraph of words; no spaces; every row exactly full, so that a
;; character typed at its start moves a word down each row (a cascade).
(define (worst-case which n)
  (let ([o (open-output-string)])
    (let loop ([i 0])
      (when (< i n)
        (case which
          [(words) (put-string o (vector-ref '#("the " "quick " "brown "
                                                 "fox " "jumps " "a ")
                                             (mod i 6)))]
          [(none) (put-string o "0123456789abcdef")]
          [(full) (put-string o "aaaaaaaaa bbbbbbbbb ccccccccc dddddddddd ")])
        (loop (+ i 1))))
    (substring (get-output-string o) 0 n)))

;; What a keystroke measures, as a text area 14 rows high does it: the
;; layout follows the new text, the rows down to the caret's view are
;; computed (the view follows the caret), and the caret's x. Before it,
;; the view showed the caret at k: rows to there were computed then.
(define (keystroke-measures text k op)
  (let* ([n 0] [count (lambda (s) (set! n (+ n 1)) (m7 s))]
         [lay (lay-of text 280 count)])
    (define (view! lay caret)
      (edit-layout-extend! lay (+ (edit-row-at lay caret) 15))
      (edit-caret-x lay caret))
    (view! lay k)
    (set! n 0)
    (let* ([e (op (ed text k) lay)]
           [lay (edit-layout-of lay (edit-text e) 280 #f count
                                (edit-change e))])
      (view! lay (edit-caret e))
      n)))

(check "a keystroke in each worst case measures in proportion to the rows \
        in view, at most 4 a row, at 16,384 characters as at 65,536: \
        typing at the start, middle and end, Backspace, Delete, a paste of \
        a few words, everything selected and deleted, the wheel"
       (let ([ops
              (list (cons 0 (lambda (e lay) (edit-insert e "x")))
                    (cons 1/2 (lambda (e lay) (edit-insert e "x")))
                    (cons 1 (lambda (e lay) (edit-insert e "x")))
                    (cons 1/2 (lambda (e lay)
                                (edit-key e 'backspace '() lay 14)))
                    (cons 1/2 (lambda (e lay) (edit-key e 'delete '() lay 14)))
                    (cons 1/2 (lambda (e lay) (edit-insert e "pasted words ")))
                    (cons 1/2 (lambda (e lay)
                                (edit-key (edit-all (edit-text e)) 'backspace
                                          '() lay 14)))
                    (cons 0 (lambda (e lay)   ; three rows down, no change
                              (edit-move e (edit-row-last lay 3) #f))))])
         (for-all
          (lambda (which)
            (for-all
             (lambda (op)
               (let ([at (lambda (n)
                           (let ([text (worst-case which n)])
                             (keystroke-measures
                              text (exact (floor (* (car op) n))) (cdr op))))])
                 (let ([small (at 16384)] [big (at 65536)])
                   (or (and (<= small (* 4 17)) (<= big (* 4 17)))
                       (begin (printf "  ~a: ~a then ~a measures\n" which
                                      small big)
                              #f)))))
             ops))
          '(words none full))))

(check "Ctrl+End computes the rows to the end once, and a second one \
        measures none of them again"
       (let* ([n 0] [count (lambda (s) (set! n (+ n 1)) (m7 s))]
              [text (worst-case 'words 65536)]
              [lay (lay-of text 280 count)])
         (edit-layout-extend! lay 15)
         (let* ([e (edit-key (ed text 0) 'end '(ctrl) lay 14)]
                [r (edit-row-at lay (edit-caret e))] [first n])
           (edit-row-at lay (edit-caret e))
           (edit-key (ed text 0) 'end '(ctrl) lay 14)
           (and (= (edit-caret e) 65536) (edit-layout-done? lay)
                (> first 1000) (= n first)))))

;; Short lines, as in a log or base64: each a paragraph of its own.
(define (short-lines n)
  (substring (apply string-append
                    (map (lambda (i) (format "line ~a of short lines\n" i))
                         (iota (quotient n 20))))
             0 n))

(check "an edit takes back the rows past it: typed at the start or the \
        middle of 65,536 characters of words, or of short lines, or a \
        line deleted there, the rows to the end come back with a few \
        measured, and they are a whole wrap's"
       (for-all
        (lambda (text op)
          (let* ([n 0] [count (lambda (s) (set! n (+ n 1)) (m7 s))]
                 [lay (lay-of text 280 count)])
            (edit-layout-extend! lay (string-length text))
            (set! n 0)
            (let* ([e (op text)]
                   [lay (edit-layout-of lay (edit-text e) 280 #f count
                                        (edit-change e))])
              (edit-layout-extend! lay (string-length (edit-text e)))
              (or (and (<= n 20) (edit-layout-done? lay)
                       (equal? (rows-of lay)
                               (rows-of (lay-of (edit-text e) 280))))
                  (begin (printf "  ~a measures, ~a\n" n
                                 (substring (edit-text e) 0 50))
                         #f)))))
        (list (worst-case 'words 65536) (worst-case 'words 65536)
              (short-lines 65536) (short-lines 65536) (short-lines 65536))
        (list (lambda (t) (edit-insert (ed t 0) "x"))
              (lambda (t) (edit-insert (ed t 32768) "x"))
              (lambda (t) (edit-insert (ed t 3) "x"))
              (lambda (t) (edit-erase (ed t 30000) 30001))
              (lambda (t) (edit-erase (ed t 40 63) 0)))))

;; What an engine counts of a thunk: a tick for each procedure call or
;; turn of a loop, so a character looked at in a loop is one; a copy
;; (string-copy!, fxvector-copy) is one call.
(define (ticks thunk)
  ((make-engine thunk) 1000000000 (lambda (left . r) (- 1000000000 left))
   (lambda (e) +inf.0)))

(check "a keystroke's text work, in an engine's ticks, is about 500 a row \
        in view (8,500 for 17 rows) at 65,535 characters as at 16,383, in \
        each worst case, where a look at every character would be 16,383 \
        at least: the edit (as a description's box makes it), Tasks taking \
        the text from the box, the rows that follow it to the caret's \
        view, and the editor's line and column"
       (let ([ops (list (cons 0 (lambda (e lay) (edit-insert e "x" 65536)))
                        (cons 1/2 (lambda (e lay) (edit-insert e "x" 65536)))
                        (cons 1 (lambda (e lay) (edit-insert e "x" 65536)))
                        (cons 1/2 (lambda (e lay)
                                    (edit-insert e "\n" 65536)))
                        (cons 1/2 (lambda (e lay)
                                    (edit-key e 'backspace '() lay 14)))
                        (cons 1 (lambda (e lay)
                                  (edit-key e 'backspace '() lay 14)))
                        (cons 1/2 (lambda (e lay)
                                    (edit-key e 'delete '() lay 14))))])
         (define (work text k op)   ; the ticks of op at k in text
           (let* ([lay (lay-of text 280)]
                  [view! (lambda (lay caret)
                           (edit-layout-extend! lay (+ (edit-row-at lay caret)
                                                       15))
                           (edit-caret-x lay caret))]
                  [editor (make-tasks-editor #f #f #f text text #f #f)]
                  [e (ed text k)])
             (view! lay k)
             (tasks-editor-line-column editor e)
             (ticks (lambda ()
                      (let* ([e (op e lay)]
                             [d (tasks-normalize 'task-set! 'description
                                                 (edit-text e))]
                             [lay (edit-layout-of lay d 280 #f m7
                                                  (edit-change e))])
                        (view! lay (edit-caret e))
                        (tasks-editor-line-column editor e))))))
         (for-all
          (lambda (which)
            (for-all
             (lambda (op)
               (let ([at (lambda (n)
                           (let ([text (worst-case which n)])
                             (work text (exact (floor (* (car op) n)))
                                   (cdr op))))])
                 (let ([small (at 16383)] [big (at 65535)])
                   (or (< (max small big) (* 500 17))
                       (begin (printf "  ~a: ~a then ~a ticks\n" which
                                      small big)
                              #f)))))
             ops))
          '(words none full))))

(check "the editor's line and column, counted on from the last count, are \
        a plain count's through 3000 random edits, moves and clicks, \
        counted after each or after two"
       (let ([editor (make-tasks-editor #f #f #f "" "" #f #f)])
         (define (plainly t c)
           (let loop ([i 0] [line 1] [column 1])
             (cond [(= i c) (list line column)]
                   [(char=? (string-ref t i) #\newline)
                    (loop (+ i 1) (+ line 1) 1)]
                   [else (loop (+ i 1) line (+ column 1))])))
         (define (next e seed)   ; an edit, a move or a click
           (let*-values ([(t) (edit-text e)] [(n) (string-length t)]
                         [(ins x) (made-up (mod seed 5) seed "ab\n\n")])
             (values (case (mod (quotient seed 7) 6)
                       [(0 1) (edit-insert e ins)]
                       [(2) (edit-erase e (- (edit-caret e) 1))]
                       [(3) (edit-erase e (+ (edit-caret e) 1))]
                       [(4) (edit-move e (mod x (+ n 1)) (odd? x))]
                       [else (edit-at t (mod x (+ n 1)))])
                     (next-random x))))
         (let loop ([i 0] [seed 3] [e (ed "" 0)])
           (or (= i 3000)
               (let-values ([(line column)
                             (tasks-editor-line-column editor e)])
                 (and (equal? (list line column)
                              (plainly (edit-text e) (edit-caret e)))
                      (let*-values ([(e seed) (next e seed)]
                                    [(e seed) (if (zero? (mod i 5))
                                                  (next e seed)
                                                  (values e seed))])
                        (loop (+ i 1) seed e))))))))

(check "a search looks through a task's texts once for each change of \
        them: matched again, a 65,536-character description is not \
        searched, and changed, it is"
       (let ([t (make-task 0 "Title" (worst-case 'words 65536) 'todo
                           'feature 'normal #f '() #f '() '() 0 0)]
             [words (tasks-words "zebra")])
         (define (match) (tasks-match? t words #f #f #f #f #t))
         (let* ([first (ticks match)] [again (ticks match)])
           (task-description-set! t (string-append (task-description t)
                                                   " zebra"))
           (and (> first 65536) (< again 100) (match)))))

(check "wrapping measures a row about twice, not the paragraph's rest: a \
        6000-character paragraph and a 2000-character word measure under \
        5 characters per character"
       (let* ([n 0]
              [count (lambda (s) (set! n (+ n (string-length s))) (m7 s))]
              [measured (lambda (text)
                          (set! n 0)
                          (edit-wrap text 280 count)
                          (/ n (string-length text)))])
         (and (< (measured (apply string-append (make-list 1200 "word ")))
                 5)
              (< (measured (make-string 2000 #\W)) 5))))

(check "wrapping scans a row, not the rest of a long word: 100,000 \
        characters wrap within a handler's fuel, as one word or as words"
       (and (within-fuel? (lambda () (edit-wrap (make-string 100000 #\W) 280
                                                m7)))
            (within-fuel?
             (lambda ()
               (edit-wrap (apply string-append (make-list 20000 "word ")) 280
                          m7)))))

;; The index nearest x as plainly as it can be said: a character at a time.
(define (index-plainly lay r x)
  (let-values ([(s e) (edit-row lay r)])
    (let ([text (edit-layout-text lay)] [last (edit-row-last lay r)])
      (let loop ([j s])
        (if (or (>= j last)
                (< x (/ (+ (m7 (substring text s j))
                           (m7 (substring text s (+ j 1))))
                        2)))
            j
            (loop (+ j 1)))))))

(check "a click lands where a character at a time finds it, in log n \
        measures: 34 at most in a row of 100,000 characters"
       (let* ([n 0] [count (lambda (s) (set! n (+ n 1)) (m7 s))]
              [long (make-string 100000 #\W)]
              [at (edit-index-at (lay-of long #f count) 0 350003)])
         (and (= at 50000) (<= n 34)
              (for-all
               (lambda (text)
                 (let ([lay (lay-of text 30)])
                   (edit-layout-extend! lay (string-length text))
                   (for-all
                    (lambda (r)
                      (for-all (lambda (x)
                                 (= (edit-index-at lay r x)
                                    (index-plainly lay r x)))
                               (map (lambda (i) (- (/ i 2) 5)) (iota 90))))
                    (iota (edit-layout-count lay)))))
               '("" "a" "abc def" "aaa bbb ccc ddd" "ab\ncd\n"
                 "abcdefghijk")))))

(check "a wrapped label breaks a word wider than it, and its lines end \
        without the space a soft break leaves"
       (let-values ([(w h) (ui-label-size '(wrap) (make-string 200 #\W) 100)])
         (and (> h (* 5 (font-line-height 'ui)))
              (for-all (lambda (l) (<= (text-width l 'ui) 100))
                       (ui-label-lines (make-string 200 #\W) 100 'ui))
              (equal? (ui-label-lines "No task matches this, with Kind: Bug."
                                      120 'ui)
                      (wrap-text "No task matches this, with Kind: Bug."
                                 120 'ui)))))

(let ([soft (lay-of "aaa bbb ccc" 42)] [hard (lay-of "ab\ncd" 100)]
      [three (lay-of "abcdef\nab\nabcdef" 100)])
  (define (vert e lay dir) (state (edit-vertical e lay dir #f)))
  (check "a caret at a soft break shows on the next row, at a hard break on \
          its own; a click lands nearest a character's middle"
         (and (= 1 (edit-row-at soft 4)) (= 0 (edit-row-at soft 3))
              (= 0 (edit-row-at hard 2)) (= 1 (edit-row-at hard 3))
              (= 1 (edit-index-at (lay-of "abcd" 100) 0 10))
              (= 2 (edit-index-at (lay-of "abcd" 100) 0 11))
              (= 0 (edit-index-at (lay-of "abcd" 100) 0 3))))
  (check "Up and Down keep the goal across a short row; Up from the first \
          row goes to 0, Down from the last to the end; End stops before a \
          soft row's space"
         (let* ([e (ed "abcdef\nab\nabcdef" 5)]
                [d1 (edit-vertical e three 1 #f)]
                [d2 (edit-vertical d1 three 1 #f)])
           (and (= 9 (edit-caret d1)) (= 15 (edit-caret d2))
                (equal? (vert e three -1) '("abcdef\nab\nabcdef" 0 0))
                (equal? (vert d2 three 1) '("abcdef\nab\nabcdef" 16 16))
                (= 7 (edit-caret (edit-key (ed "aaa bbb ccc" 4) 'end '()
                                           soft 1)))))))

(check "Page Down and Up move a page of rows keeping the goal; past the \
        first or last row a page goes to it, and from it to the text's \
        start or end"
       (let* ([text (apply string-append
                           (map (lambda (i) (format "line ~2,'0d\n" i))
                                (iota 30)))]
              [lay (lay-of text 1000)]
              [at (lambda (line column) (+ (* 8 line) column))]
              [page (lambda (c k) (edit-caret (edit-key (ed text c) k '() lay
                                                        10)))])
         (and (= (page (at 2 3) 'page-down) (at 12 3))
              (= (page (at 12 3) 'page-up) (at 2 3))
              (= (page (at 5 3) 'page-up) (at 0 3))
              (= (page (at 0 3) 'page-up) 0)
              (= (page (at 25 3) 'page-down) (at 30 0))
              (= (page (at 30 0) 'page-down) (string-length text)))))

(check "edit-clean drops control characters, but a tab becomes a \
        space; without newlines they do too"
       (and (equal? (edit-clean "a\tb\r\nc\x1;" #t) "a b\nc")
            (equal? (edit-clean "a\tb\r\nc" #f) "a b c")
            (equal? (edit-clean "a\x7f;b" #t) "ab")))

(check "ui-flow places, wraps and centers"
       (and (equal? (call-with-values
                      (lambda () (ui-flow 0 0 100 5 '((10 . 10) (20 . 20)
                                                       (30 . 10))))
                      list)
                    '(((0 . 5) (15 . 0) (40 . 5)) 20))
            (equal? (call-with-values
                      (lambda () (ui-flow 0 0 40 5 '((10 . 10) (20 . 20)
                                                      (30 . 10))))
                      list)
                    '(((0 . 5) (15 . 0) (0 . 25)) 35))))

(check "ui-table-step at both ends, by pages, from no row, with no rows"
       (equal? (map (lambda (a) (apply ui-table-step a))
                    '((0 5 2 up) (4 5 2 down) (#f 5 2 down) (#f 5 2 up)
                      (1 5 2 page-down) (4 5 2 page-down) (3 5 2 page-up)
                      (2 5 2 home) (2 5 2 end) (#f 0 2 down) (2 5 2 left)))
               '(0 4 0 4 3 4 1 0 4 #f #f)))

(check "dates: day numbers round-trip from 1970 to 2100; 2026-10-31 is a \
        Saturday; February's lengths; months added clamp the day"
       (and (let loop ([n 0])
              (or (> n (ui-date->day '(2100 12 31)))
                  (and (= n (ui-date->day (ui-day->date n))) (loop (+ n 1)))))
            (equal? (ui-day->date 0) '(1970 1 1))
            (= 5 (ui-week-day (ui-date->day '(2026 10 31))))
            (equal? (map (lambda (y) (ui-days-in-month y 2))
                         '(2024 2025 2100 2000))
                    '(29 28 28 29))
            (equal? (map (lambda (d k)
                           (ui-day->date (ui-add-months (ui-date->day d) k)))
                         '((2026 1 31) (2026 12 15) (2026 1 31)) '(1 1 -2))
                    '((2026 2 28) (2027 1 15) (2025 11 30)))))

(define (mk id title status kind priority who tags due . description)
  (make-task id title (if (pair? description) (car description) "") status
             kind priority who tags due '() '() 0 id))
(define t1 (mk 1 "Fix the menu" 'todo 'bug 'high "Pavel" '("ui") '(2026 11 2)))
(define t2 (mk 2 "Style system" 'doing 'feature 'urgent #f '("style" "kit") #f
               "parts and derived styles"))
(define t3 (mk 3 "Threads spec" 'todo 'chore 'low "Claude" '() '(2026 10 30)))
(define t4 (mk 4 "Old one" 'done 'feature 'normal "Pavel" '("ui") #f))
(define ts (list t1 t2 t3 t4))
(define (ids l) (map task-id l))

(check "tasks-sorted by every key both ways, stable, the empty last"
       (equal? (map (lambda (k)
                      (list (ids (tasks-sorted ts k 'up))
                            (ids (tasks-sorted ts k 'down))))
                    '(id title status kind priority assignee due updated))
               '(((1 2 3 4) (4 3 2 1)) ((1 4 2 3) (3 2 4 1))
                 ((1 3 2 4) (4 2 1 3)) ((1 2 4 3) (3 2 4 1))
                 ((2 1 4 3) (3 4 1 2)) ((3 1 4 2) (1 4 3 2))
                 ((3 1 2 4) (1 3 2 4)) ((1 2 3 4) (4 3 2 1)))))

(check "tasks-words: lowercase, each once, at most 16, each cut to 64 \
        characters"
       (and (equal? (tasks-words " B a  b C ") '("b" "a" "c"))
            (equal? (tasks-words (apply string-append
                                        (map (lambda (i) (format "w~a " i))
                                             (iota 40))))
                    (map (lambda (i) (format "w~a" i)) (iota 16)))
            (equal? (tasks-words (string-append (make-string 100 #\A) " "
                                                (make-string 70 #\a)))
                    (list (make-string 64 #\a)))))

(check "tasks-match?: near misses of 64 and 1,001 characters looked for in \
        a 100,000-character title within a handler's fuel"
       (let ([t (mk 5 (make-string 100000 #\a) 'todo 'bug 'low #f '() #f)])
         (for-all (lambda (n)
                    (let ([words (tasks-words
                                  (string-append (make-string (- n 1) #\a)
                                                 "b"))])
                      (within-fuel?
                       (lambda () (tasks-match? t words #f #f #f #f #t)))))
                  '(64 1001))))

(define (matching words . filters)
  (ids (filter (lambda (t) (apply tasks-match? t (tasks-words words) filters))
               ts)))
(check "tasks-match?: by id, title, description, tag and assignee; each \
        filter; words all match; done tasks hidden"
       (and (equal? (matching "task-1" #f #f #f #f #t) '(1))
            (equal? (matching "4" #f #f #f #f #t) '(4))
            (equal? (matching "MENU" #f #f #f #f #t) '(1))
            (equal? (matching "derived" #f #f #f #f #t) '(2))
            (equal? (matching "kit" #f #f #f #f #t) '(2))
            (equal? (matching "claude" #f #f #f #f #t) '(3))
            (equal? (matching "" 'bug #f #f #f #t) '(1))
            (equal? (matching "" #f 'low #f #f #t) '(3))
            (equal? (matching "" #f #f "pavel" #f #t) '(1 4))
            (equal? (matching "" #f #f 'none #f #t) '(2))
            (equal? (matching "" #f #f #f "ui" #t) '(1 4))
            (equal? (matching "fix menu" #f #f #f #f #t) '(1))
            (equal? (matching "fix style" #f #f #f #f #t) '())
            (equal? (matching "" #f #f #f #f #f) '(1 2 3))))

(check "tasks-move: across columns, up and down a column, to the end, into \
        an empty column"
       (and (equal? (ids (tasks-move ts t1 'doing #f)) '(2 1 3 4))
            (equal? (ids (tasks-move ts t3 'todo t1)) '(3 1 2 4))
            (equal? (ids (tasks-move ts t1 'todo #f)) '(2 3 1 4))
            (equal? (ids (tasks-move ts t1 'done #f)) '(2 3 4 1))
            (equal? (ids (tasks-move ts t2 'review #f)) '(1 3 4 2))))

(check "tasks-slot: above the first card, between cards, past the last, \
        with none"
       (and (equal? (map (lambda (py) (tasks-slot '(8 60 120) '(40 40 40) py))
                         '(10 70 500))
                    '(0 1 3))
            (= 0 (tasks-slot '() '() 100))))

(check "an assignee is one name whatever its case and the spaces around \
        it: the filter lists it once and finds every spelling"
       (let ([t5 (mk 5 "Typed" 'todo 'bug 'low "pavel " '() #f)])
         (and (equal? (tasks-names (list t1 t5 t3)) '("Claude" "Pavel"))
              (equal? (tasks-names (list t5 t1)) '("pavel"))
              (tasks-match? t5 '() #f #f "Pavel" #f #t)
              (tasks-match? t1 '() #f #f " pavel" #f #t))))

(check "a card's tags: as many chips as fit beside \"+k\" for the rest, the \
        first cut when even it does not fit, never past the card's edge"
       (let ([fits?
              (lambda (w tags)
                (let-values ([(ws k) (tasks-card-tags
                                      w (mk 6 "x" 'todo 'bug 'low #f tags #f))])
                  (and (pair? ws) (= (+ (length ws) k) (length tags))
                       (<= (+ (apply + ws) (* 4 (- (length ws) 1))
                              (if (> k 0)
                                  (+ 4 (text-width (format "+~a" k)
                                                   'ui-small))
                                  0))
                           (- w 20))
                       (list (length ws) k))))])
         (and (equal? (fits? 200 '("ui" "kit")) '(2 0))
              (equal? (fits? 145 '("abcdefghijkl" "abcdefgh" "b")) '(1 2))
              (equal? (fits? 200 '("abcdefghijklmnopqrstuvwx" "b")) '(1 1))
              (equal? (fits? 100 '("abcdefghijklmnopqrstuvwx")) '(1 0)))))

(check "tasks-neighbor, tasks-tag, tasks-initials"
       (and (eq? (tasks-neighbor ts t2) t3) (eq? (tasks-neighbor ts t4) t3)
            (not (tasks-neighbor (list t1) t1))
            (equal? (map tasks-tag '("  My Tag " "a,b" "" "   "))
                    '("my-tag" "ab" #f #f))
            (= 24 (string-length (tasks-tag (make-string 30 #\a))))
            ;; a space at the cut would end it in "-"
            (equal? (tasks-tag "dÜnï Cödé Long Tag Name Here")
                    "dünï-cödé-long-tag-name")
            (equal? (tasks-tag "ends-in-") "ends-in-")
            (equal? (map tasks-initials '("Pavel Uvarov" "Claude"
                                          "ada lovelace byron"))
                    '("PU" "C" "AB"))))

(check "a task's tags: made by tasks-tag, each once, in the order given, \
        at most 64"
       (let ([names (lambda (n) (map (lambda (i) (format "t~a" i)) (iota n)))])
         (and (equal? (tasks-normalize 'set-task! 'tags
                                       '(" UI" "perf" "ui" "" "," "Perf "))
                      '("ui" "perf"))
              (equal? (tasks-normalize 'set-task! 'tags (names 100))
                      (names 64)))))

(check "tasks-due-label and tasks-overdue?"
       (let ([today '(2026 10 31)])
         (and (equal? (map (lambda (d) (tasks-due-label d today))
                           '((2026 10 31) (2026 11 1) (2026 10 30)
                             (2026 11 15) (2027 1 2)))
                      '("Today" "Tomorrow" "Yesterday" "Nov 15" "Jan 2, 2027"))
              (tasks-overdue? t3 today) (not (tasks-overdue? t1 today))
              (not (tasks-overdue? (mk 5 "x" 'done 'bug 'low #f '()
                                       '(2026 1 1))
                                   today)))))

(check "ids are never reused, and the REPL's procedures name what is valid"
       (let* ([a (add-task! "first")] [_ (delete-task! a)]
              [b (add-task! "second")])
         (delete-task! b)
         (and (> (task-id b) (task-id a))
              (equal? (guard (c [#t (condition->string c)])
                        (set-task! (car *tasks*) 'priority 'hgih))
                      "Exception in set-task!: priority must be one of \
                       urgent high normal low, not hgih"))))

(check "the REPL's edges: a blank name finds no task; an assignee filter \
        that is not a name says so; a description is cleaned as typing \
        cleans it; an assignee is trimmed, but the details' field keeps a \
        space typed after a name; a blank one is nobody; a name is at most \
        64 characters"
       (let ([t (add-task! "Edges" 'description "a\r\nb\tc"
                           'assignee "  Ada  ")]
             [v (tasks-view-new (make-ui))])
         (let ([ok (and (not (find-task "")) (not (find-task "  "))
                        (equal? (guard (c [#t (condition->string c)])
                                  (tasks-list 'assignee 'pavel))
                                "Exception in tasks-list: assignee must be \
                                 a name or none, not pavel")
                        (list? (tasks-list 'assignee 'none))
                        (equal? (task-description t) "a\nb c")
                        (equal? (task-assignee t) "Ada")
                        (equal? (task-assignee (set-task! t 'assignee " B "))
                                "B")
                        (begin (task-set! v t 'assignee " Ada ")
                               (equal? (task-assignee t) "Ada "))
                        (not (task-assignee (set-task! t 'assignee "  ")))
                        (let ([long (make-string 99 #\a)])
                          (= 64 (string-length
                                 (task-assignee
                                  (set-task! t 'assignee long))))))])
           (delete-task! t)
           ok)))

(check "a description holds at most 65,536 characters: what the REPL \
        gives is cut to them"
       (let ([t (add-task! "Long" 'description (make-string 70000 #\a))])
         (let ([n (string-length (task-description t))])
           (set-task! t 'description (make-string 65536 #\b))
           (let ([ok (and (= n 65536)
                          (= 65536 (string-length (task-description t))))])
             (delete-task! t)
             ok))))

(check "the seed's long descriptions: the README as it is, the kit's \
        source cut to the most, the backlog as one paragraph and a base64 \
        blob with no spaces, the same at every seeding; with the files \
        gone, the paragraph"
       (let* ([text (lambda (what) (tasks-seed-text what))]
              [readme (call-with-input-file
                        (string-append *schevana-root* "/README.md")
                        get-string-all)]
              [has? (lambda (s c)
                      (exists (lambda (i) (char=? (string-ref s i) c))
                              (iota (string-length s))))]
              [blob (text 'blob)] [paragraph (text 'changelog)]
              [gone (fluid-let ([*schevana-root* "/no/such/tree"])
                      (text 'readme))])
         (and (equal? (text 'readme) readme)
              (= 65536 (string-length (text 'kit-source)))
              (= 65536 (string-length paragraph) (string-length blob))
              (not (has? paragraph #\newline)) (not (has? blob #\space))
              (string-prefix? "Ozs7IGtpdHMvdWkuc3M" blob)   ; ";;; kits/ui.ss"
              (equal? blob (text 'blob)) (equal? gone paragraph)
              (for-all (lambda (id)
                         (<= (string-length (task-description (find-task id)))
                             tasks-description-max))
                       '(27 28 29 30)))))

(check "a tooltip and a dialog show at most six lines of a long text, the \
        last cut with …"
       (let ([long (apply string-append (make-list 300 "words "))])
         (let-values ([(lines cut?) (ui-wrap-lines long 100 'ui 3)]
                      [(all whole?) (ui-wrap-lines "a b" 100 'ui 3)]
                      [(tw th) (ui-tooltip-size long)]
                      [(dw dh) (ui-dialog-size "Delete?" long '("OK"))]
                      [(dw1 dh1) (ui-dialog-size "Delete?" "short" '("OK"))])
           (and (= 3 (length lines)) cut?
                (string=? "…" (substring (caddr lines)
                                         (- (string-length (caddr lines)) 1)
                                         (string-length (caddr lines))))
                (equal? all '("a b")) (not whole?)
                (= th (+ 10 (* 6 (font-line-height 'ui-small))))
                (< (abs (- dh dh1 (* 5 (font-line-height 'ui)))) 1)))))

(check "blocked-by: ids, each once, of tasks named any way; whom a task \
        blocks worked out; a cycle refused, the error naming it, and the \
        task unchanged; a deleted task blocks nothing"
       (let* ([a (add-task! "Dep A")]
              [b (add-task! "Dep B" 'blocked-by (list a))]
              [c (add-task! "Dep C" 'blocked-by (list (task-id b) (task-label a)
                                                      (task-id b)))]
              [refused (lambda (x ids)
                         (guard (e [#t (condition->string e)])
                           (set-task! x 'blocked-by ids)))]
              [cycle (lambda ts
                       (format "Exception in set-task!: ~a would wait for \
                                itself: ~a (each waits for the next)"
                               (task-label (car ts))
                               (string-join (map task-label ts) " → ")))])
         (let ([ok (and (equal? (task-blocked-by c) (map task-id (list b a)))
                        (equal? (task-blockers c) (list b a))
                        (equal? (task-blocks a) (list b c))
                        (null? (task-blocks c))
                        (equal? (refused a (list c)) (cycle a c b a))
                        (equal? (refused a (list a)) (cycle a a))
                        (null? (task-blocked-by a))
                        (raises? (lambda () (set-task! a 'blocked-by '(0))))
                        (raises? (lambda () (set-task! a 'blocked-by 3)))
                        (begin (delete-task! b)
                               (and (equal? (task-blocked-by c)
                                            (list (task-id a)))
                                    (equal? (task-blocks a) (list c)))))])
           (for-each delete-task! (list a c))
           ok)))

;; Made-up tasks: id, then the ids it waits for.
(define (linked . specs)
  (map (lambda (s)
         (let ([t (mk (car s) (format "T~a" (car s)) 'todo 'bug 'low #f '()
                      #f)])
           (task-blocked-by-set! t (cdr s))
           t))
       specs))

(check "tasks-forest: the roots are the tasks nothing waits for, then any \
        cycle left over; a blocker shared by several shows its own under \
        the first, depth first; tasks-cycle finds the path back"
       (let ([ts (linked '(1 2 3) '(2) '(3 5) '(4 3) '(5) '(6) '(7 8) '(8 7))])
         (let-values ([(roots homes order) (tasks-forest ts)])
           (let ([by-id (tasks-id-table ts)])
             (and (equal? roots '(1 4 6 7))
                  (equal? order '(1 2 3 5 4 6 7 8))
                  (equal? (map (lambda (id) (hashtable-ref homes id 'none))
                               '(1 2 3 5 4 6 7 8))
                          '(#f 1 1 3 #f #f #f 7))
                  (equal? (ids (tasks-cycle by-id (list-ref ts 2) '(4)))
                          '(3 4 3))
                  (equal? (ids (tasks-cycle by-id (list-ref ts 4) '(1)))
                          '(5 1 3 5))
                  (not (tasks-cycle by-id (car ts) '(6)))
                  (equal? (sort < (vector->list
                                   (hashtable-keys
                                    (tasks-waiters (tasks-inverse ts)
                                                   (list-ref ts 4)))))
                          '(1 3 4))
                  (equal? (ids (hashtable-ref (tasks-inverse ts) 3 '()))
                          '(1 4)))))))

(check "the seed's links: no cycle; (tasks-tree) nests what each task waits \
        for, a shared blocker under its first waiter and its id under the \
        others; Add offers what would make no cycle, nor is done"
       (let ([tree (tasks-tree)] [choices (map task-id (tasks-blocker-choices
                                                       (find-task 14)))])
         (let-values ([(roots homes order)
                       (tasks-forest (tasks-board-order *tasks*))])
           (and (= (length order) (length *tasks*))
                (for-all (lambda (id) (null? (task-blocks id))) roots)
                (equal? (assv 11 tree) '(11 (9)))
                (equal? (assv 13 tree) '(13 (31) (10)))
                (equal? (assv 17 tree) '(17 31))
                (equal? (assv 22 tree) '(22 (14 (33 9))))
                (equal? (assv 15 tree) '(15 (32)))
                (not (find (lambda (id) (memv id '(14 22 33 1 32))) choices))
                (memv 9 choices) (memv 16 choices) #t))))

(check "what is worked out from which tasks there are, their order, \
        statuses and links is kept while only a title or a description \
        changes, and worked out again when a link or a status does"
       (let* ([forest (tasks-whole-forest)] [order (tasks-ordered)]
              [kept? (begin (set-task! 13 'title "Kept" 'description "x")
                            (and (eq? forest (tasks-whole-forest))
                                 (eq? order (tasks-ordered))))]
              [linked (begin (set-task! 16 'blocked-by '(12))
                             (tasks-whole-forest))]
              [blocks (task-blocks 12)])
         (set-task! 16 'blocked-by '())
         (let* ([moved (begin (set-task! 16 'status 'review) (tasks-ordered))]
                [ok (and kept? (not (eq? linked forest))
                         (memq (find-task 16) blocks) (not (eq? moved order))
                         (eq? (find-task 16)
                              (car (last-pair (tasks-in 'review moved)))))])
           (tasks-reset!)
           ok)))

(check "Add's menu: the choices whose id or title holds what was typed, \
        the one it names by id first, as many as there is room for, the \
        last saying how many more"
       (let* ([cs (map (lambda (i)
                         (mk (+ 1 i) (format "Chain ~a" (- 30 i)) 'todo
                             'feature 'normal #f '() #f))
                       (iota 30))]
              [picked #f]
              [labels (lambda (f room)
                        (map car (tasks-blocker-entries
                                  cs f room (lambda (c) (set! picked c)))))])
         (and (equal? (labels "" 4)
                      '("TASK-1  Chain 30" "TASK-2  Chain 29" "TASK-3  Chain 28"
                        "… 27 more: type to narrow"))
              (equal? (labels "chain 2" 3)
                      '("TASK-2  Chain 29" "TASK-3  Chain 28"
                        "… 9 more: type to narrow"))
              (equal? (labels "task-12" 30) '("TASK-12  Chain 19"))
              (equal? (labels "20" 3)
                      '("TASK-20  Chain 11" "TASK-11  Chain 20"))
              (null? (labels "zz" 5))
              (let ([pick (cdar (tasks-blocker-entries
                                 cs "TASK-7" 5 (lambda (c) (set! picked c))))])
                (pick)
                (eq? picked (list-ref cs 6))))))

;; What a draw by thunk shows as text: ((x . string) ...), in order.
(define (texts-drawn thunk)
  (let ([drawn '()] [plain draw-text])
    (dynamic-wind
      (lambda ()
        (set! draw-text (lambda (x y s . more)
                          (set! drawn (cons (cons x s) drawn))
                          (apply plain x y s more))))
      (lambda () (image 400 40 1 thunk))
      (lambda () (set! draw-text plain)))
    (reverse drawn)))
(define (strings-drawn thunk) (map cdr (texts-drawn thunk)))

(check "four-digit ids keep clear of the titles in the Tree, three digits \
        wide at least"
       (let ([next *tasks-next-id*] [t (find-task 31)])
         (define (room)   ; from the id's start to the title's
           (let ([texts (texts-drawn
                         (lambda () (tasks-draw-node 0 0 400 30 '() t #f)))])
             (- (car (cadr texts)) (car (car texts)))))
         (let ([seed (room)])
           (set! *tasks-next-id* 1234)
           (let* ([new (add-task! "Four digits")] [wide (room)])
             (delete-task! new)
             (set! *tasks-next-id* next)
             (and (= seed (+ 8 (text-width "TASK-000" 'mono-small)))
                  (>= wide (+ 8 (text-width "TASK-1234" 'mono-small)))
                  (= (room) seed))))))

(check "a later row's title keeps at least half its room: the note where \
        its own row is gets shorter, then goes, as the cell narrows"
       (let* ([t (find-task 31)] [title (tasks-shown-title t)]
              [long (text-width "↑ under TASK-13" 'ui-small)]
              [short (text-width "↑ TASK-13" 'ui-small)]
              [w (lambda (room)   ; the cell's width for that room
                   (+ room (* 2 (tasks-cell-inset)) 8
                      (text-width "TASK-100" 'mono-small)))]   ; the seed's
              [shown (lambda (room)   ; => (title-shown note ...)
                       (cdr (strings-drawn
                             (lambda ()
                               (tasks-draw-node 0 0 (w room) 30 '(reference)
                                                t "TASK-13")))))]
              [rooms (list 1000 (+ long short 16) (+ short 8))]
              [all (map shown rooms)])
         (and (equal? (map cdr all)
                      '(("↑ under TASK-13") ("↑ TASK-13") ()))
              (> (text-width title 'ui) (/ (+ long short 16) 2))
              (for-all (lambda (s room)   ; less at most what a cut takes
                         (>= (+ (text-width (car s) 'ui) (text-width "W…" 'ui))
                             (min (text-width title 'ui) (/ room 2))))
                       all rooms))))

(check "a record of task's from before a reload that changed its fields is \
        carried over by name, a new field empty"
       (let ([rtd (make-record-type-descriptor
                   'task #f 'units-old-task #f #f
                   '#((immutable id) (immutable title) (immutable checklist)
                      (immutable created)))])
         (let ([t (tasks-carried ((record-constructor
                                   (make-record-constructor-descriptor
                                    rtd #f #f))
                                  7 "Kept" '(("a" . #t)) 99))])
           (and (task? t) (= 7 (task-id t)) (equal? "Kept" (task-title t))
                (equal? '(("a" . #t)) (task-checklist t))
                (null? (task-blocked-by t)) (= 99 (task-created t))
                (eq? 'todo (task-status t)) (eq? t (tasks-carried t))))))

;; A tree of symbols: what each has under it.
(define tree-kids '((a b c) (b d) (c) (d)))
(define (tree-rows expanded?)
  (map (lambda (r) (list (ui-tree-row-node r) (ui-tree-row-depth r)
                         (ui-tree-row-parent r) (ui-tree-row-kids? r)
                         (ui-tree-row-expanded? r)))
       (vector->list (ui-tree-rows '(a) (lambda (n) (cdr (assq n tree-kids)))
                                   expanded?))))

(check "a tree's rows: the expanded nodes' children, depth first, and only \
        theirs asked for; a key's step: Up, Down, End and Page Down as a \
        table's, Right expands or goes to the first child, Left collapses or \
        goes to the parent, Space toggles, Return opens"
       (let* ([asked '()]
              [rows (ui-tree-rows '(a)
                                  (lambda (n)
                                    (set! asked (cons n asked))
                                    (cdr (assq n tree-kids)))
                                  (lambda (n) (not (eq? n 'b))))]
              [all (ui-tree-rows '(a) (lambda (n) (cdr (assq n tree-kids)))
                                 (lambda (n) #t))]
              [step (lambda (i key) (ui-tree-step all i 2 key))])
         (and (equal? (tree-rows (lambda (n) #t))
                      '((a 0 #f #t #t) (b 1 0 #t #t) (d 2 1 #f #f)
                        (c 1 0 #f #f)))
              (equal? (tree-rows (lambda (n) #f)) '((a 0 #f #t #f)))
              (= 3 (vector-length rows)) (not (memq 'd asked))
              (equal? (map (lambda (a) (apply step a))
                           '((0 down) (0 up) (#f down) (1 end) (0 page-down)
                             (0 right) (1 right) (2 right) (0 left) (2 left)
                             (3 left) (1 #\space) (2 #\space) (2 return)
                             (#f right) (#f left)))
                      '((select . 1) (select . 0) (select . 0) (select . 3)
                        (select . 2) (select . 1) (select . 2) #f
                        (collapse . 0) (select . 1) (select . 0)
                        (collapse . 1) #f (open . 2) #f #f))
              (equal? (ui-tree-step rows 1 2 'right) '(expand . 1))
              (equal? (ui-tree-step rows 1 2 #\space) '(expand . 1))
              (= 1 (ui-tree-index rows 'b)) (not (ui-tree-index rows 'd)))))

(check "a row under another at any depth, and not under its siblings"
       (let ([all (ui-tree-rows '(a) (lambda (n) (cdr (assq n tree-kids)))
                                (lambda (n) #t))])   ; a b d c
         (and (ui-tree-under? all 2 0) (ui-tree-under? all 2 1)
              (ui-tree-under? all 3 0) (not (ui-tree-under? all 3 1))
              (not (ui-tree-under? all 1 2)) (not (ui-tree-under? all 0 0))
              (not (ui-tree-under? all #f 0)))))

(check "a dimmed tree row shades its indentation and twisty as the cells \
        shade themselves, and nothing of its cell; selected, it is not \
        dimmed"
       (let* ([cx (cadr (assq 'cell (ui-tree-areas 0 0 200 30 1)))]
              [row (lambda (flags)
                     (image 300 30 1
                            (lambda ()
                              (fill-rect 0 0 300 30 'bg-1)
                              (ui-draw-tree-row 0 0 300 30 flags 200 1))))]
              [cell (lambda (flags)
                      (image 300 30 1
                             (lambda ()
                               (fill-rect 0 0 300 30 'bg-1)
                               (ui-draw-dim 0 0 cx 30 flags))))]
              [plain (row '(kids expanded))] [dim (row '(kids expanded dim))]
              [picked (row '(kids expanded selected))]
              [dim-picked (row '(kids expanded dim selected))]
              [shade (cell '(dim))])
         (and (> (max-diff plain dim (list cx 0 300 30)) 0)
              (= 0 (max-diff plain dim (list 0 0 cx 30)))
              (= 0 (max-diff picked dim-picked))
              (= (pixel-at dim 2 2) (pixel-at shade 2 2))
              (not (= (pixel-at dim 2 2) (pixel-at plain 2 2))))))

(check "a deep row's indentation stops at half its column, so its cell \
        keeps room"
       (let* ([at (lambda (depth k)
                    (cdr (assq k (ui-tree-areas 0 0 200 20 depth))))]
              [twisty (at 1 'twisty)] [cell (at 1 'cell)] [deep (at 500 'cell)])
         (and (< (car twisty) (car cell))
              (= (+ (car cell) (caddr cell)) 200)
              (>= (caddr deep) 80)
              (equal? deep (at 1000 'cell)))))

(let ([at (lambda (key)
            (cdr (assq key (ui-calendar-areas 0 0 230 0))))])
  (set-theme! 'ui-font-size 24)
  (let-values ([(cw ch) (ui-calendar-size)])
    (let ([prev (at 'prev)] [week (at 'week)] [today (at 'today)])
      (set-theme! 'ui-font-size 13)
      (check "the calendar at 24 px fonts: the weekdays below its arrows, \
              Today inside it"
             (and (>= (cadr week) (+ (cadr prev) (cadddr prev)))
                  (<= (+ (cadr today) (cadddr today)) ch))))))

(let ([v (tasks-view-new (make-ui))])
  (define (mode w) (call-with-values (lambda () (tasks-layout v w 700 44))
                     (lambda (m view details) m)))
  (let ([at-13 (list (mode 1205) (mode 760))])
    (set-theme! 'ui-font-size 24)
    (let ([at-24 (list (mode 1205) (mode 2400))])
      (set-theme! 'ui-font-size 13)
      (check "tasks-layout: the details beside the view when they fit, at 13 \
              and 24 px fonts"
             (equal? (list at-13 at-24) '((wide narrow) (narrow wide)))))))

;; A keystroke in 13's description, Tasks maximized, with a long title on
;; TASK-10, which 13 and others wait for (cards, rows and tooltips fit its
;; title): the characters it measures do not grow with that title.
(let* ([w (tasks)] [v (window-ref w 'state)] [ui (window-ref w 'ui)])
  (define (measured mode title)
    (set-task! 10 'title title)
    (set-task! 13 'description "")
    (tasks-view-mode-set! v mode)
    (tasks-select! v (find-task 13))
    (ui-focus! ui 'description 0)
    (invalidate! w)
    (steps 3)
    (characters-measured (lambda () (inject! '(text "a")) (steps 1))))
  (maximize-window! w)
  (steps 2)
  (let ([counts (map (lambda (mode)
                       (map (lambda (make) (measured mode (make 10000)))
                            (list words-of (lambda (n) (make-string n #\W))
                                  (lambda (n) (words-of (* 10 n)))
                                  (lambda (n) (make-string (* 10 n) #\W)))))
                     '(board list tree))])
    (tasks-reset!)
    (close-window! w)
    (steps 2)
    (check (format "a keystroke in Tasks measures as many characters with a \
                    100,000-character title on a task others wait for as \
                    with one of 10,000, in the Board, the List and the Tree, \
                    words or one long word: ~a" counts)
           (for-all (lambda (c) (and (= (car c) (caddr c))
                                     (= (cadr c) (cadddr c))))
                    counts))))

;;; ------------------------------------------------------------------
;;; surfaces and strips

;; The canvas's pixel at logical point (x, y).
(define (canvas-pixel x y)
  (cairo-surface-flush *canvas*)
  (pixel-at *canvas* (exact (floor (* x *scale*)))
            (exact (floor (* y *scale*)))))

;; With no surface registered (no desktop layer) the world draws its
;; windows on black, maximizes them to the whole screen, and a press,
;; a key or the wheel where nothing is goes nowhere.
(let* ([ss *surfaces*] [strips *strips*] [keys 0]
       [w (open-window "Alone" 'size '(200 . 100) 'pos '(300 . 200)
                       'draw (lambda (win cw ch)
                               (fill-rect 0 0 cw ch #x336699))
                       'key (lambda (win key mods) (set! keys (+ keys 1))))])
  (set! *surfaces* '())
  (set! *strips* '())
  (invalidate!)
  (steps 1)
  (let-values ([(cx cy cw ch) (content-rect w)])
    (let ([inside (canvas-pixel (+ cx 50) (+ cy 50))]
          [around (map (lambda (p) (canvas-pixel (car p) (cdr p)))
                       '((5 . 5) (640 . 790) (1270 . 400)))])
      (inject! '(mouse-move 20 400) '(mouse-down 20 400 left 1)
               '(mouse-up 20 400 left) '(wheel 0 1) '(key-down #\a ()))
      (steps 1)
      (maximize-window! w)
      (let ([whole (list (window-x w) (window-y w) (window-w w) (window-h w))]
            [hover *hover*])
        (set! *surfaces* ss)
        (set! *strips* strips)
        (close-window! w)
        (steps 1)
        (check (format "with no surface registered the windows are drawn on \
                        black (#x~x, ~s), maximize to the screen (~s), and \
                        input where nothing is goes nowhere (~s; keys \
                        to the window ~a)"
                       inside around whole hover keys)
               (and (= inside #xff336699)
                    (equal? around '(#xff000000 #xff000000 #xff000000))
                    (equal? whole (list 0 0 (screen-w) (screen-h)))
                    (not hover) (= keys 1)))))))

;; A surface of one's own with a strip on the left: it is drawn over the
;; windows, takes the presses on it, and maximized windows go beside it.
(let* ([pressed '()]
       [w (open-window "Beside" 'size '(200 . 100) 'pos '(300 . 200))])
  (define-surface dock 15
    'draw (lambda () (fill-rect 0 0 64 (screen-h) #xcc3300))
    'box (lambda () (values 0 0 64 (screen-h)))
    'at (lambda (x y) (and (< x 64) 'dock))
    'mouse (lambda (kind what x y button clicks)
             (set! pressed (cons (list kind what button) pressed))))
  (define-strip dock 'left (lambda () 64))
  (maximize-window! w)
  (steps 1)
  (let-values ([(dx dy dw dh) (desktop-rect)])
    (let ([at (list (window-x w) (window-y w) (window-w w) (window-h w))]
          [px (canvas-pixel 30 400)])
      (inject! '(mouse-move 30 400) '(mouse-down 30 400 left 1)
               '(mouse-up 30 400 left))
      (steps 1)
      (let ([hover *hover*])
        (set! *surfaces* (remp (lambda (s) (eq? (surface-name s) 'dock))
                               *surfaces*))
        (set! *strips* (remp (lambda (s) (eq? (car s) 'dock)) *strips*))
        (wm-screen-resized!)
        (let ([back (window-x w)])
          (close-window! w)
          (steps 1)
          (check (format "a surface with a strip on the left: maximized \
                          windows beside it ~s, drawn over them (#x~x), \
                          its presses ~s, hovered ~s; gone, they fill the \
                          left again (~a)" at px pressed hover back)
                 (and (equal? at (list 64 (topbar-height) (- (screen-w) 64)
                                       (- (screen-h) (topbar-height)
                                          (taskbar-height))))
                      (equal? (list dx dy dw dh) at)
                      (= px #xffcc3300)
                      (equal? pressed '((down dock left) (move dock #f)))
                      (equal? hover '(surface dock dock))
                      (= back 0))))))))

;;; ------------------------------------------------------------------
;;; settings in layers

;; thunk run with mac given these settings too (its own options, say);
;; your changes and Overrides cleared before and after.
(define (with-mac-settings settings thunk)
  (let ([plain *styles*])
    (dynamic-wind
      (lambda ()
        (register-style! 'mac "Mac"
                         (append settings (style-settings (find-style 'mac))))
        (set-style! 'mac)
        (reset-theme!))
      thunk
      (lambda ()
        (set! *styles* plain)
        (reset-theme!)))))

;; The accent and where it comes from, after each layer gives one.
(check "a setting comes from the first layer that has it: the Overrides, \
        your changes to the style, the style, its palette, the built-ins"
       (let ([at (lambda () (list (theme 'accent) (theme-source 'accent)))])
         (equal?
          (list (with-mac-settings '() at)
                (with-mac-settings '()
                  (lambda () (set-theme! 'palette 'nord) (at)))
                (with-mac-settings '(accent #x112233)
                  (lambda () (set-theme! 'palette 'nord) (at)))
                (with-mac-settings '(accent #x112233)
                  (lambda () (set-theme! 'accent #x445566) (at)))
                (with-mac-settings '(accent #x112233)
                  (lambda ()
                    (set-theme! 'accent #x445566)
                    (override! 'accent #x778899)
                    (let ([over (at)])
                      (override! 'accent)
                      (let ([cleared (at)])
                        (reset-style! 'accent)
                        (list over cleared (at)))))))
          '((#x7aa2f7 (built-in #x7aa2f7))
            (#x88c0d0 (palette nord #x88c0d0))
            (#x112233 (style mac #x112233))
            (#x445566 (changes mac #x445566))
            ((#x778899 (overrides #x778899))
             (#x445566 (changes mac #x445566))
             (#x112233 (style mac #x112233)))))))

;; A style that lists its options honors only those of the common ones:
;; the others keep its value under your changes and Overrides; keys that
;; are not common options are always yours.
(check "a style honors the common options it lists, and every other key"
       (equal?
        (with-mac-settings '(options (accent) corner-radius 3)
          (lambda ()
            (override! 'corner-radius 12)
            (set-theme! 'ui-font-size 20)
            (override! 'accent #x778899)
            (set-theme! 'red #x010203)
            (map theme '(corner-radius ui-font-size accent red))))
        '(3 13 #x778899 #x010203)))

(check "a rule computed from itself is named, and the theme stays"
       (with-mac-settings '()
         (lambda ()
           (set-changes! 'mac (list 'loop-a (rule (theme 'loop-b))
                                    'loop-b (rule (theme 'loop-a))))
           (let* ([before *theme*]
                  [msg (guard (c [#t (condition->string c)])
                         (theme-changed!)
                         #f)])
             (set-changes! 'mac '())
             (and msg (string-contains-ci? msg "computed from itself")
                  (or (string-contains-ci? msg "loop-a")
                      (string-contains-ci? msg "loop-b"))
                  (eq? *theme* before))))))

(check "a base color changed re-derives what its rules derive from it"
       (with-mac-settings '()
         (lambda ()
           (set-theme! 'accent #xff9e64)
           (set-theme! 'bg-3 #x404040)
           (and (= (theme 'selection) (mix #xff9e64 'bg-1 0.65))
                (= (theme 'window-border-focus) (mix #x404040 #xff9e64 0.45))
                (= (theme 'panel-hover) #x404040)
                (= (theme 'button-idle) (mix #x404040 'text 0.15))))))

;; Files today's tree wrote (tests' fixtures, from .work): Tokyo Night
;; with an accent and a radius, Nord with a radius, Genera with its
;; palette and an accent of its own, and one naming a style and a palette
;; since removed. Each loads to what today's tree resolved from it: its
;; own values, the built-in ones elsewhere; its style and palette chosen
;; (mac and Tokyo Night where gone), and only your choices kept as changes.
(define old-theme-files
  '((default (accent . #xff9e64) (selection . #x6a493c)
             (window-border-focus . #x896051) (corner-radius . 4))
    (nord (bg-0 . #x242933) (bg-1 . #x2e3440) (bg-2 . #x353c4a)
          (bg-3 . #x3b4252) (text . #xeceff4) (text-dim . #xd8dee9)
          (text-faint . #x6d7a96) (accent . #x88c0d0) (accent-2 . #xb48ead)
          (red . #xbf616a) (orange . #xd08770) (yellow . #xebcb8b)
          (green . #xa3be8c) (cyan . #x8fbcbb) (teal . #x8fbcbb)
          (magenta . #xb48ead) (selection . #x4e6572)
          (desktop-top . #x252a34) (desktop-bottom . #x3a4660)
          (panel . #x1f232b) (panel-text . #xeceff4) (panel-dim . #x6d7a96)
          (panel-hover . #x3b4252) (window-bg . #x2e3440)
          (window-border . #x49505f) (window-border-focus . #x5e7b8b)
          (title-bg . #x353c4a) (title-bg-focus . #x3b4252)
          (title-text . #xeceff4) (title-text-dim . #x6d7a96)
          (button-close . #xbf616a) (button-min . #xebcb8b)
          (button-max . #xa3be8c) (button-idle . #x565c6a)
          (corner-radius . 4) (preset . nord))
    (genera (bg-0 . #xffffff) (bg-1 . #xffffff) (bg-2 . #xf0f0f0)
            (bg-3 . #xd8d8d8) (text . #x000000) (text-dim . #x222222)
            (text-faint . #x666666) (accent . #x3366cc) (accent-2 . #x444444)
            (red . #x8b0000) (orange . #x6b4400) (yellow . #x5c5c00)
            (green . #x1f5f1f) (cyan . #x1f5566) (teal . #x1f5f55)
            (magenta . #x661f66) (selection . #xb8c9ed)
            (desktop-top . #xffffff) (desktop-bottom . #xffffff)
            (watermark . #x000000) (panel . #x000000) (panel-text . #xffffff)
            (panel-dim . #xb4b4b4) (panel-hover . #x383838)
            (window-bg . #xffffff) (window-border . #xc7c7c7)
            (window-border-focus . #x8ea5d3) (title-bg . #xf0f0f0)
            (title-bg-focus . #xd8d8d8) (title-text . #x000000)
            (title-text-dim . #x666666) (button-close . #x8b0000)
            (button-min . #x5c5c00) (button-max . #x1f5f1f)
            (button-idle . #xb8b8b8) (window-style . genera)
            (preset . genera) (desktop-pattern . flat))
    (deleted (window-style . amiga) (preset . workbench) (corner-radius . 4))))

(define (write-theme-file! data)
  (call-with-output-file *theme-file* (lambda (p) (write data p)) 'replace))

;; The colors a palette gives or derives (not those kits and styles add).
(define palette-keys
  (append (plist-keys base-colors) (plist-keys derived-colors)))

(let* ([fresh (let ([t (resolve-theme 'mac '() '())])
                (compute-rules! t)
                t)]
       [loaded
        (map (lambda (f)
               (let ([file (cdr f)])
                 (write-theme-file! file)
                 (load-saved-theme!)
                 (list (car f)
                       (for-all (lambda (k)
                                  (equal? (theme k)
                                          (let ([p (assq k file)])
                                            (if p
                                                (cdr p)
                                                (hashtable-ref fresh k #f)))))
                                palette-keys)
                       (theme 'window-style) (theme 'palette)
                       (theme 'corner-radius) (theme 'wallpaper)
                       *changes*)))
             old-theme-files)])
  (set-style! 'mac)
  (reset-theme!)
  (check (format "old theme files load to the colors today's tree resolved \
                  from them, and your choices as the style's changes: ~s"
                 loaded)
         (equal? loaded
                 '((default #t mac tokyo-night 4 gradient
                            ((mac palette tokyo-night accent #xff9e64
                                  corner-radius 4)))
                   (nord #t mac nord 4 gradient
                         ((mac palette nord corner-radius 4)))
                   (genera #t genera genera 9 flat
                           ((genera palette genera accent #x3366cc)))
                   (deleted #t mac tokyo-night 4 gradient
                            ((mac palette tokyo-night corner-radius 4)))))))

;; Saved, cleared and loaded: the chosen style, the scale, the Overrides
;; and every style's changes come back; the file is plain data, its colors
;; in hex.
(check "the theme file round-trips: style, scale, Overrides, changes"
       (let ([text (lambda ()
                     (call-with-input-file *theme-file* get-string-all))])
         (set-style! 'genera)
         (set-theme! 'palette 'nord)
         (set-theme! 'corner-radius 4)
         (set-style! 'mac)
         (set-theme! 'accent #xff9e64)
         (override! 'ui-font-size 15)
         (let ([state (list *style-name* *scale-setting* *overrides*
                            *changes*)]
               [file (begin (save-theme!) (text))])
           (set-style! 'windows)
           (reset-theme!)
           (load-saved-theme!)
           (let ([back (list *style-name* *scale-setting* *overrides*
                             *changes*)])
             (reset-theme!)
             (set-style! 'mac)
             (and (equal? back state)
                  (string-contains-ci? file "(accent . #xff9e64)")
                  (string-contains-ci? file "(version . 2)")
                  (equal? (theme 'ui-font-size) 13))))))

;; Every palette, Tokyo Night too, derives what it does not give as
;; today's tree derived it (derive-colors, as it was).
(define (derived-as-today palette)
  (define (ref k) (plist-ref palette k #f))
  (define (unless-given p)
    (cond [(null? p) '()]
          [(ref (car p)) (unless-given (cddr p))]
          [else (cons* (car p) (cadr p) (unless-given (cddr p)))]))
  (append palette
          (unless-given
           (list 'selection (mix (ref 'accent) (ref 'bg-1) 0.65)
                 'panel-text (ref 'text) 'panel-dim (ref 'text-faint)
                 'panel-hover (ref 'bg-3) 'window-bg (ref 'bg-1)
                 'window-border (mix (ref 'bg-3) (ref 'text) 0.08)
                 'window-border-focus (mix (ref 'bg-3) (ref 'accent) 0.45)
                 'title-bg (ref 'bg-2) 'title-bg-focus (ref 'bg-3)
                 'title-text (ref 'text) 'title-text-dim (ref 'text-faint)
                 'button-close (ref 'red) 'button-min (ref 'yellow)
                 'button-max (ref 'green)
                 'button-idle (mix (ref 'bg-3) (ref 'text) 0.15)))))

(check (format "every palette resolves to its colors derived as today's \
                tree derived them (~a)" (themes))
       (for-all (lambda (name)
                  (let* ([given (let ([p (palette-colors name)])
                                  (if (null? p) base-colors p))]
                         [today (derived-as-today given)]
                         [now (preset-colors name)])
                    (for-all (lambda (k)
                               (equal? (plist-ref now k #f)
                                       (plist-ref today k #f)))
                             palette-keys)))
                (themes)))

;;; ------------------------------------------------------------------
;;; styles and parts

(reset-theme!)
(set-style! 'mac)

;; thunk's result, with the styles registered meanwhile gone after it.
(define (with-test-styles thunk)
  (let ([plain *styles*])
    (dynamic-wind void thunk
                  (lambda () (set! *styles* plain) (set-style! 'mac)))))

(define (insets-of w) (call-with-values (lambda () (frame-insets w)) list))
(define (test-insets win) (values 3 17 3 3))

(check "a derived style is its parent with one setting changed, and its \
        parent defined again shows in it"
       (with-test-styles
        (lambda ()
          (define-style test-base "Test base"
            'frame-insets 'test-insets 'corner-radius 5)
          (define-style test-flat "Test flat"
            'parent 'test-base 'corner-radius 0)
          (set-style! 'test-flat)
          (let* ([w (open-window "Derived" 'size '(100 . 50))]
                 [got (list (map style-name (style-chain 'test-flat))
                            (insets-of w) (theme 'corner-radius)
                            (theme-source 'corner-radius)
                            (theme-source 'frame-insets))])
            (define-style test-base "Test base" 'corner-radius 5)
            (let ([again (insets-of w)])
              (close-window! w)
              (equal? (list got again)
                      (list (list '(test-flat test-base) '(3 17 3 3) 0
                                  '(style test-flat 0)
                                  '(style test-base test-insets))
                            (list 0 (title-height) 0 0))))))))

;; A part a style names is looked up at each call: redefined at the REPL,
;; the next frame draws the new definition.
(define test-drawn #f)
(define (test-under win x y w h focused? hover pressed)
  (set! test-drawn 'first))
(check "a part given by name shows its redefinition at the next frame"
       (with-test-styles
        (lambda ()
          (define-style test-part "Test part" 'frame-under 'test-under)
          (set-style! 'test-part)
          (let ([w (open-window "Part" 'size '(100 . 50))])
            (steps 1)
            (let ([first test-drawn])
              (set! test-under (lambda args (set! test-drawn 'second)))
              (invalidate!)
              (steps 1)
              (close-window! w)
              (equal? (list first test-drawn) '(first second)))))))

(define-part (test-meter n) (* n 2))
(check "define-part: the built-in is name*, and name calls the style's part"
       (with-test-styles
        (lambda ()
          (let ([plain (test-meter 3)])
            (define-style test-tenfold "Test" 'test-meter (lambda (n) (* n 10)))
            (set-style! 'test-tenfold)
            (equal? (list plain (test-meter 3) (test-meter* 3)
                          (theme-default 'test-meter))
                    '(6 30 6 test-meter*))))))

(check "a title's right click opens the style's window menu, a part"
       (with-test-styles
        (lambda ()
          (let ([opened #f])
            (define-style test-menu "Test"
              'open-window-menu (lambda (win x y) (set! opened win)))
            (set-style! 'test-menu)
            (let ([w (open-window "Menu" 'size '(200 . 100) 'pos '(300 . 200))])
              (inject! '(mouse-move 400 205) '(mouse-down 400 205 right 1)
                       '(mouse-up 400 205 right))
              (steps 1)
              (close-window! w)
              (eq? opened w))))))

(check "a window of another style is drawn and hit-tested in that style's \
        theme, its content included"
       (let* ([seen #f]
              [w (open-window "Genera here" 'size '(200 . 100)
                              'pos '(300 . 200) 'window-style 'genera
                              'draw (lambda (win cw ch)
                                      (set! seen (list (theme 'palette)
                                                       (theme 'wallpaper)))))])
         (invalidate!)
         (steps 1)
         ;; Genera's label, at the bottom, is its title
         (let ([insets (insets-of w)]
               [hit (window-region w 400 (+ 200 (window-h w) -10))])
           (close-window! w)
           (printf "  ~s\n" (list seen insets hit))
           (and (equal? seen '(genera flat)) (eq? (theme 'palette) 'tokyo-night)
                (equal? insets '(6 6 6 23)) (eq? hit 'title)))))

(check "choosing Genera brings its palette and wallpaper; a palette chosen \
        for it stays with it"
       (begin
         (set-style! 'genera)
         (let ([own (list (theme 'palette) (theme 'wallpaper))])
           (set-theme! 'palette 'nord)
           (set-style! 'mac)
           (let ([mac (list (theme 'palette) (theme 'wallpaper))])
             (set-style! 'genera)
             (let ([back (theme 'palette)])
               (reset-theme!)
               (set-style! 'mac)
               (equal? (list own mac back)
                       '((genera flat) (tokyo-night gradient) nord)))))))

(check "with-style: another style's theme, or this one's with a setting"
       (equal? (list (with-style 'interlisp
                       (lambda () (list (theme 'palette) (theme 'wallpaper))))
                     (with-style 'mac (lambda () (theme 'palette))
                                 'palette 'nord)
                     (theme 'palette))
               '((interlisp shade) nord tokyo-night)))

;;; ------------------------------------------------------------------
;;; fonts and sizes as settings

(check "a font is a setting: two themes asking for one font share its \
        record, and a font changed and back is the very one"
       (let ([ui (theme 'ui)]
             [other (with-style 'windows (lambda () (theme 'ui)))])
         (set-theme! 'ui-font-size 15)
         (let ([bigger (theme 'ui)])
           (reset-style! 'ui-font-size)
           (and (eq? ui other) (eq? ui (theme 'ui))
                (= 15 (font-size bigger)) (eq? (theme 'title) (theme 'ui-bold))
                (eq? (get-font 'ui) ui)))))

(check "a text's layout stays across a color change, and goes with a font \
        change"
       (let* ([ui (make-ui)]
              [lay (lambda ()
                     (ui-text-layout ui 'probe "some words to wrap here" 120
                                     'ui #t))]
              [a (lay)])
         (set-theme! 'accent #x123456)
         (let ([b (lay)])
           (set-theme! 'ui-font-size 15)
           (let ([c (lay)])
             (reset-style! 'accent 'ui-font-size)
             (and (eq? a b) (not (eq? b c)))))))

(check "a title's or bar's height: the setting grown to its text's need, 0 \
        none, and a style's own pixels where it fixes it"
       (with-test-styles
        (lambda ()
          (set-theme! 'title-height 10)
          (set-theme! 'taskbar-height 0)
          (let ([mac (list (title-height) (taskbar-height)
                           (theme 'title-height-min))])
            (reset-style! 'title-height 'taskbar-height)
            (define-style test-fixed "Test" 'options '(palette)
              'title-height 12)
            (set-style! 'test-fixed)
            (override! 'title-height 40)
            (let ([fixed (list (title-height) (theme 'fixed))])
              (override! 'title-height)
              (and (equal? (cdr mac) (list 0 (car mac))) (> (car mac) 10)
                   (= (car fixed) 12) (memq 'title-height (cadr fixed))
                   #t))))))

;;; ------------------------------------------------------------------
;;; the kit, the bars and the menus through parts; places

(for-each close-window! (windows))
(reset-theme!)
(set-style! 'mac)
(steps 1)

;; A window holding a text field, a label under it: (values win ui).
(define (field-window text)
  (let ([ui (make-ui)])
    (values (apply open-window "Field" 'size '(400 . 120) 'pos '(200 . 200)
                   'ui ui
                   'draw (lambda (win w h)
                           (ui-begin! ui)
                           (ui-text-field ui 'f 20 20 300 30 text void)
                           (ui-draw-label 20 70 300 20 '() "a label")
                           (ui-end! ui w h))
                   (ui-handlers ui))
            ui)))

;; The leftmost column inside the field (content x 30 to 300) where its
;; text's ink is, in logical pixels.
(define (ink-left win)
  (let-values ([(cx cy cw ch) (content-rect win)])
    (let ([bg (canvas-pixel (+ cx 24) (+ cy 35))])
      (let loop ([x 24])
        (cond [(> x 300) #f]
              [(exists (lambda (dy) (not (= (canvas-pixel (+ cx x) (+ cy dy))
                                            bg)))
                       '(28 31 34 37 40))
               x]
              [else (loop (+ x 1))])))))

;; The caret's index after a click at content x 60.
(define (index-clicked win ui)
  (let-values ([(cx cy cw ch) (content-rect win)])
    (inject! `(mouse-move ,(+ cx 60) ,(+ cy 35))
             `(mouse-down ,(+ cx 60) ,(+ cy 35) left 1)
             `(mouse-up ,(+ cx 60) ,(+ cy 35) left))
    (steps 2)
    (edit-caret (ui-state-edit ui))))

(define (test-text-areas x y w h flags)
  (let ([a (ui-text-box-areas* x y w h flags)])
    (cons (cons* 'edit (+ (cadr (car a)) 40) (cddr (car a))) (cdr a))))

(check "a style that moves only ui-text-box-areas moves the text and where \
        a click puts the caret"
       (with-test-styles
        (lambda ()
          (let-values ([(win ui) (field-window "MMMMMMMMMMMM")])
            (steps 2)
            (let ([plain (list (ink-left win) (index-clicked win ui))])
              (define-style test-areas "Test"
                'ui-text-box-areas 'test-text-areas)
              (set-style! 'test-areas)
              (steps 2)
              (let ([moved (list (ink-left win) (index-clicked win ui))])
                (close-window! win)
                (printf "  ~s\n" (list plain moved))
                (and (car plain) (car moved)
                     (<= 39 (- (car moved) (car plain)) 41)
                     (< (cadr moved) (cadr plain)))))))))

(check "a style mapping the field place colors a text box's text, and only \
        inside it"
       (with-test-styles
        (lambda ()
          (let ([seen '()])
            (define-style test-place "Test"
              'places (list (list 'field 'text #xff0000))
              'ui-draw-text-box
              (lambda args
                (set! seen (cons (cons 'box (theme 'text)) seen))
                (apply ui-draw-text-box* args))
              'ui-draw-label
              (lambda args
                (set! seen (cons (cons 'label (theme 'text)) seen))
                (apply ui-draw-label* args)))
            (set-style! 'test-place)
            (let-values ([(win ui) (field-window "x")])
              (steps 2)
              (close-window! win)
              (and (equal? (assq 'box seen) '(box . #xff0000))
                   (equal? (assq 'label seen) (cons 'label (theme 'text)))
                   (not (= (theme 'text) #xff0000))))))))

(define (test-topbar-areas)
  (let ([h (topbar-height)])
    (list (list 'launcher 300 0 120 h) (list 'title 0 0 300 h))))

(check "a style that moves the launcher in the top bar's areas moves its \
        click"
       (with-test-styles
        (lambda ()
          (define (opened-at x)
            (inject! `(mouse-move ,x 10) `(mouse-down ,x 10 left 1)
                     `(mouse-up ,x 10 left))
            (steps 1)
            (let ([open? (launcher-open?)])
              (when (menu-open?) (close-menu!))
              (steps 1)
              open?))
          (let ([plain (list (opened-at 40) (opened-at 340))])
            (define-style test-launcher "Test"
              'topbar-areas 'test-topbar-areas)
            (set-style! 'test-launcher)
            (equal? (list plain (list (opened-at 40) (opened-at 340)))
                    '((#t #f) (#f #t)))))))

(check "the menu's entries are where menu-areas puts them: a taller entry \
        moves the click"
       (with-test-styles
        (lambda ()
          (define (entry-at-row row)
            (open-menu! 100 100 '(("One" . void) ("Two" . void)
                                  ("Three" . void)))
            (let-values ([(x y w h) (menu-geometry *menu*)])
              (let ([i (menu-entry-at (+ x 20)
                                      (+ y (theme 'menu-pad) row))])
                (close-menu!)
                i)))
          (let ([plain (entry-at-row 60)])
            (define-style test-menu "Test" 'menu-item-height 40)
            (set-style! 'test-menu)
            (equal? (list plain (entry-at-row 60)) '(2 1))))))

(check "the dialog place reaches the kit's dialog, drawn at ui-end!"
       (with-test-styles
        (lambda ()
          (let ([seen #f] [ui (make-ui)])
            (define-style test-dialog "Test"
              'places (list (list 'dialog 'bg-2 #xffff00))
              'ui-draw-dialog (lambda args
                                (set! seen (theme 'bg-2))
                                (apply ui-draw-dialog* args)))
            (set-style! 'test-dialog)
            (let ([w (apply open-window "Dialog" 'size '(420 . 260)
                            'ui ui
                            'draw (lambda (win w h)
                                    (ui-begin! ui)
                                    (ui-dialog ui 'd "A dialog" "Its message."
                                               (list (list "OK" void)) void)
                                    (ui-end! ui w h))
                            (ui-handlers ui))])
              (steps 2)
              (close-window! w)
              (eqv? seen #xffff00))))))

(check "a preview's extra settings lie over the Overrides, and over what \
        a style honors"
       (with-test-styles
        (lambda ()
          (define-style test-fixed-palette "Test" 'parent 'mac
            'options '(accent))
          (override! 'palette 'gruvbox)
          (let ([seen (list (with-style 'mac (lambda () (theme 'palette))
                                        'palette 'nord)
                            (with-style 'test-fixed-palette
                                        (lambda () (theme 'palette))
                                        'palette 'nord))])
            (override! 'palette)
            (equal? seen '(nord nord))))))

(check "a window of another style hears its input in that style's theme, \
        and a theme change its handler makes stays"
       (let* ([seen #f]
              [w (open-window "Genera input" 'size '(200 . 100)
                              'pos '(300 . 200) 'window-style 'genera
                              'mouse-down
                              (lambda (win x y button clicks)
                                (set! seen (theme 'palette))
                                (set-theme! 'accent #x123456)))])
         (steps 1)
         (let-values ([(cx cy cw ch) (content-rect w)])
           (inject! `(mouse-down ,(+ cx 20) ,(+ cy 20) left 1)
                    `(mouse-up ,(+ cx 20) ,(+ cy 20) left))
           (steps 1))
         (close-window! w)
         (let ([accent (theme 'accent)])
           (reset-style! 'accent)
           (and (eq? seen 'genera) (= accent #x123456)))))

(check "a menu whose place makes its entries taller takes a click where \
        it draws the entry"
       (with-test-styles
        (lambda ()
          (define-style test-tall-menu "Test"
            'places (list (list 'menu 'menu-item-height 50)))
          (set-style! 'test-tall-menu)
          (open-menu! 300 150 '(("One" . void) ("Two" . void)
                                ("Three" . void)))
          (let* ([two (assv 1 (with-place 'menu
                                (lambda () (menu-areas *menu*))))]
                 [hit (menu-entry-at (+ (cadr two) 20) (+ (caddr two) 25))])
            (close-menu!)
            (and (= (list-ref two 4) 50) (eqv? hit 1))))))

(check "an accent change is a change of colors only: the chrome stays"
       (let* ([n 0] [plain wm-chrome-changed!])
         (set! wm-chrome-changed! (lambda () (set! n (+ n 1)) (plain)))
         (set-theme! 'accent #xff9e64)
         (reset-style! 'accent)
         (set! wm-chrome-changed! plain)
         (= n 0)))

(check "a window of no style, focused or opened from another style's \
        handler, is in the chosen style's theme"
       (let* ([seen '()]
              [note (lambda (what)
                      (set! seen (cons (cons what (theme 'palette)) seen)))]
              [b (open-window "Plain" 'size '(100 . 60) 'pos '(600 . 400)
                              'focus (lambda (win) (note 'focus)))]
              [c #f]
              [g (open-window "Genera" 'size '(200 . 100) 'pos '(300 . 200)
                              'window-style 'genera
                              'mouse-down
                              (lambda (win x y button clicks)
                                (focus-window! b)
                                (set! c (open-window
                                         "Opened" 'size '(100 . 60)
                                         'open (lambda (win)
                                                 (note 'open))))))])
         (set! seen '())
         (steps 1)
         (let-values ([(cx cy cw ch) (content-rect g)])
           (inject! `(mouse-down ,(+ cx 20) ,(+ cy 20) left 1)
                    `(mouse-up ,(+ cx 20) ,(+ cy 20) left))
           (steps 1))
         (for-each close-window! (list b c g))
         (and (equal? (assq 'focus seen) '(focus . tokyo-night))
              (equal? (assq 'open seen) '(open . tokyo-night)))))

(check "a theme change of colors from another style's handler changes no \
        chrome"
       (let* ([n 0] [plain wm-chrome-changed!]
              [g (open-window "Genera" 'size '(200 . 100) 'pos '(300 . 200)
                              'window-style 'genera
                              'mouse-down
                              (lambda (win x y button clicks)
                                (set! n 0)
                                (set-theme! 'accent #x123456)))])
         (steps 1)
         (set! wm-chrome-changed! (lambda () (set! n (+ n 1)) (plain)))
         (let-values ([(cx cy cw ch) (content-rect g)])
           (inject! `(mouse-down ,(+ cx 20) ,(+ cy 20) left 1)
                    `(mouse-up ,(+ cx 20) ,(+ cy 20) left))
           (steps 1))
         (set! wm-chrome-changed! plain)
         (close-window! g)
         (reset-style! 'accent)
         (= n 0)))

(check "a text box whose place sets its text size takes a click where it \
        draws its text, as with the size set for the whole style"
       (with-test-styles
        (lambda ()
          (define (caret-at style)
            (set-style! style)
            (let-values ([(win ui) (field-window "MMMMMMMMMMMM")])
              (steps 2)
              (let ([i (index-clicked win ui)])
                (close-window! win)
                i)))
          (define-style test-big "Test" 'ui-font-size 26)
          (define-style test-big-field "Test"
            'places (list (list 'field 'ui-font-size 26)))
          (let* ([whole (caret-at 'test-big)]
                 [placed (caret-at 'test-big-field)])
            (printf "  ~s\n" (list whole placed))
            (= whole placed)))))

(check "the bars' place sizes their strips and places their clicks"
       (with-test-styles
        (lambda ()
          (define (launcher-at x)
            (inject! `(mouse-move ,x 10) `(mouse-down ,x 10 left 1)
                     `(mouse-up ,x 10 left))
            (steps 1)
            (let ([open? (launcher-open?)])
              (when (menu-open?) (close-menu!))
              (steps 1)
              open?))
          (define-style test-big-bars "Test"
            'places (list (list 'bar 'ui-font-size 26)))
          (let ([plain (launcher-at 150)])
            (set-style! 'test-big-bars)
            (let-values ([(dx dy dw dh) (desktop-rect)])
              (and (not plain) (launcher-at 150)
                   (= dy (topbar-height))
                   (> dy (theme 'topbar-height))))))))

(check "a tooltip is measured in its place, where it is drawn"
       (with-test-styles
        (lambda ()
          (let* ([seen #f] [ui (make-ui)] [fake (now-ms)] [real now-ms])
            (define-style test-big-tips "Test"
              'places (list (list 'tooltip 'ui-font-size 26))
              'ui-draw-tooltip
              (lambda (x y w h flags text)
                (set! seen (list (list w h)
                                 (call-with-values
                                   (lambda () (ui-tooltip-size text))
                                   list)))
                (ui-draw-tooltip* x y w h flags text)))
            (set-style! 'test-big-tips)
            (set! now-ms (lambda () fake))
            (let ([w (apply open-window "Tips" 'size '(400 . 200)
                            'pos '(200 . 200) 'ui ui
                            'draw (lambda (win w h)
                                    (ui-begin! ui)
                                    (ui-button ui 'b 20 20 100 30 "Button"
                                               void 'tip "A tip")
                                    (ui-end! ui w h))
                            (ui-handlers ui))])
              (steps 2)
              (let-values ([(cx cy cw ch) (content-rect w)])
                (inject! `(mouse-move ,(+ cx 50) ,(+ cy 30)))
                (steps 2)
                (set! fake (+ fake 2000))
                (invalidate! w)
                (steps 2))
              (set! now-ms real)
              (close-window! w)
              (and seen (equal? (car seen) (cadr seen))))))))

(check "reload reads the source list from load.ss"
       (and (equal? (read-source-list) *schevana-sources*)
            (member "src/wm.ss" *schevana-sources*)
            #t))

(steps 2)
(check "the world still renders after all this" (begin (render-frame!) #t))

(if (zero? failures)
    (printf "all checks passed\n")
    (printf "~a check(s) FAILED\n" failures))
(exit (if (zero? failures) 0 1))
