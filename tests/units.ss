;;; tests/units.ss — the drawing layer piece by piece: the measuring
;;; context, pixmaps, Life's generations and grid, and invalidation.
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
     (let ([ok (guard (c [#t (printf "  error: ~a\n" (condition->string c)) #f]) expr)])
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
  (unless (and (= (cairo-image-surface-get-width a) (cairo-image-surface-get-width b))
               (= (cairo-image-surface-get-height a) (cairo-image-surface-get-height b)))
    (errorf 'max-diff "images differ in size"))
  (cairo-surface-flush a)
  (cairo-surface-flush b)
  (let ([pa (cairo-image-surface-get-data a)] [pb (cairo-image-surface-get-data b)]
        [sa (cairo-image-surface-get-stride a)] [sb (cairo-image-surface-get-stride b)]
        [w (cairo-image-surface-get-width a)] [h (cairo-image-surface-get-height a)])
    (define (masked? x y)
      (exists (lambda (r) (and (<= (car r) x) (< x (caddr r)) (<= (cadr r) y) (< y (cadddr r)))) mask))
    (let row ([y 0] [worst 0])
      (cond
        [(= y h) worst]
        [(zero? (sdl-memcmp (+ pa (* y sa)) (+ pb (* y sb)) (* 4 w))) (row (+ y 1) worst)]
        [else
         (let col ([x 0] [worst worst])
           (if (= x w)
               (row (+ y 1) worst)
               (let ([p (foreign-ref 'unsigned-32 pa (+ (* y sa) (* 4 x)))]
                     [q (foreign-ref 'unsigned-32 pb (+ (* y sb) (* 4 x)))])
                 (col (+ x 1) (if (or (fx= p q) (masked? x y)) worst (fxmax worst (pixel-diff p q)))))))]))))

;; The largest difference of the four channels of two pixels.
(define (pixel-diff p q)
  (let loop ([k 0] [d 0])
    (if (fx= k 32)
        d
        (loop (fx+ k 8) (fxmax d (fxabs (fx- (fxand (fxsrl p k) 255) (fxand (fxsrl q k) 255))))))))

(define (report-worst worst where)
  (printf "  worst difference ~a~a\n" worst (if where (format " at ~s" where) "")))

(define (same-bytes? a b)
  (and (= (cairo-image-surface-get-width a) (cairo-image-surface-get-width b))
       (= (cairo-image-surface-get-height a) (cairo-image-surface-get-height b))
       (zero? (max-diff a b))))

;; The canvas's pixels, copied.
(define (canvas-bytes)
  (cairo-surface-flush *canvas*)
  (let* ([n (* (cairo-image-surface-get-stride *canvas*) (canvas-pixel-height))]
         [bv (make-immobile-bytevector n)])
    (sdl-memcpy (object->reference-address bv) (cairo-image-surface-get-data *canvas*) n)
    bv))

(define (cairo-matrix-xx cr)
  (cairo-get-matrix cr *scratch*)
  (foreign-ref 'double *scratch* 0))

;;; ------------------------------------------------------------------ boot

(schevana-boot! 'headless 1280 800)

;;; ------------------------------------------------------------------ measuring context

;; What text measures with *cr* on the canvas itself, as before.
(define (measured-on-canvas thunk)
  (let ([cr (cairo-create *canvas*)])
    (cairo-scale cr *scale* *scale*)
    (let ([v (fluid-let ([*cr* cr]) (thunk))])
      (cairo-destroy cr)
      v)))

(define (measures-like-canvas?)
  (let ([measure (lambda ()
                   (list (text-width "Schevana λ" 'ui) (text-width "(define x 1)" 'mono)
                         (font-metrics (make-font* "sans" 13)) (font-metrics (make-font* "monospace" 11 'bold))))])
    (equal? (measure) (measured-on-canvas measure))))

(check "before the first frame *cr* is the measuring context"
       (and (zero? *frame-ms*) (not (eqv? *measure-cr* 0)) (eqv? *cr* *measure-cr*)
            (= (cairo-matrix-xx *cr*) *scale*)))
(check "text-width and font-metrics work before the first frame, as on the canvas"
       (and (> (text-width "Schevana" 'ui) 0) (> (font-line-height 'ui) 0) (measures-like-canvas?)))

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
       (and (= *scale* 2.0) (eqv? *cr* *measure-cr*) (= (cairo-matrix-xx *cr*) 2.0)))
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
;; mono's char width and a text-width when 'mono is first measured inside
;; (measure-in measure), on a fresh context scaled like the canvas.
(define (first-measured-in measure-in)
  (install-default-fonts!)
  (let* ([img (cairo-image-surface-create CAIRO_FORMAT_ARGB32 400 300)] [cr (cairo-create img)])
    (cairo-scale cr *scale* *scale*)
    (let ([v (fluid-let ([*cr* cr])
               (measure-in (lambda () (list (font-char-width 'mono) (text-width "(set-theme! 'accent #xff9e64)" 'mono)))))])
      (cairo-destroy cr)
      (cairo-surface-destroy img)
      v)))

(check "the first measurement of a font, in a half-scale style preview, is the canvas's (100, 125, 150, 200%)"
       (let ([results
              (map (lambda (s)
                     (canvas-create! 1280 800 s)
                     (let ([normal (first-measured-in (lambda (measure) (measure)))]
                           [preview (first-measured-in
                                     (lambda (measure)
                                       (draw-window-style-preview 'genera 0 0 200 150 0.5)   ; its title is in 'mono
                                       (call-with-scale 0.5 measure)))])
                       (list s normal preview)))
                   '(1 1.25 1.5 2))])
         (canvas-create! 1280 800 *host-scale*)
         (install-default-fonts!)
         (printf "  ~s\n" results)
         (for-all (lambda (r) (equal? (cadr r) (caddr r))) results)))
(steps 1)

;;; ------------------------------------------------------------------ host window

;; (w h scale ratio) -> window units: logical size × scale / pixel ratio.
(check "the host window opens at the logical size times the scale, in window units"
       (equal? (map (lambda (args) (call-with-values (lambda () (apply host-window-size args)) list))
                    '((1280 800 1 1) (1280 800 2 1) (1280 800 2 2) (1280 800 3/2 2) (1280 800 1 2)
                      (1024 640 5/4 1) (1281 801 7/4 2)))
               '((1280 800) (2560 1600) (1280 800) (960 600) (640 400) (1280 800) (1121 701))))

;; shadow{

;;; ------------------------------------------------------------------ shadow tiles

;; How many 'tiles images were drawn without looking up a tile.
(define untiled 0)

;; An image holding everything the shadow of the box can touch, over a
;; gradient that goes from opaque to translucent, with after-shadow drawn
;; on top. how is 'tiles (draw-shadow), 'stack (draw-shadow-directly,
;; layer by layer over the gradient) or 'stack-alone (the stack drawn on its
;; own, then composited once, as tiles are).
(define (shadow-image how s x y w h r size after-shadow)
  (let*-values ([(bx by bw bh) (shadow-bounds x y w h size)]
                [(pw ph) (values (+ (exact (ceiling (* s (+ bx bw)))) 3) (+ (exact (ceiling (* s (+ by bh)))) 3))])
    (let* ([alone (and (eq? how 'stack-alone) (image pw ph s (lambda () (draw-shadow-directly x y w h r size))))]
           [img (image pw ph s
                       (lambda ()
                         (fill-gradient 0 0 (+ bx bw 3) (+ by bh 3) #x3060a0 (alpha #xe0c040 0.4) 'horizontal)
                         (case how
                           [(tiles)
                            (let ([lookups *shadow-tile-clock*])
                              (draw-shadow x y w h r size)
                              (when (= lookups *shadow-tile-clock*) (set! untiled (+ untiled 1))))]
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
                     (when (> d worst) (set! worst d) (set! where (list 'scale s 'r r 'size size 'box wh 'at x y)))))
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
                     (over-shadow-cases '(1 1.25 1.5 2) '((0 0) (0.25 0.6) (0.5 0.15) (0.77 0.9))
                                        (lambda (s x y w h r size)
                                          (shadow-diff 'tiles 'stack-alone s x y w h r size void)))])
         (report-worst worst where)
         (and (<= worst 2) (= untiled 0))))

(check "translucent boxes keep the shadow under them (within 2 levels)"
       (let-values ([(worst where)
                     (over-shadow-cases '(1 1.25 1.5 2) '((0 0) (0.25 0.6) (0.5 0.15) (0.77 0.9))
                                        (lambda (s x y w h r size)
                                          (shadow-diff 'tiles 'stack-alone s x y w h r size
                                                       (lambda () (fill-round-rect x y w h r (alpha #xf0f0f0 0.35))))))])
         (report-worst worst where)
         (<= worst 2)))

;; Drawn layer by layer over a background, each faint layer of the stack
;; rounds to 8 bits by itself: up to half a level per layer, one layer per
;; 2 pixels of size.
(check "the stack drawn over a background differs only by its own rounding"
       (let ([raw 0])
         (let-values ([(excess where)
                       (over-shadow-cases '(1 2) '((0 0) (0.25 0.6))
                                          (lambda (s x y w h r size)
                                            (let ([d (shadow-diff 'tiles 'stack s x y w h r size void)])
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
         (let* ([one (image 900 700 1.5 (lambda () (draw-shadow 40.3 30.7 200 120 8 14)))]
                [t1 (the-only-tile)])
           ;; same phases (whole multiples of 2 logical pixels at 150%), another box size
           (let ([kept (map (lambda (s) (image (cairo-image-surface-get-width s) (cairo-image-surface-get-height s) 1
                                               (lambda () (cairo-set-source-surface *cr* s 0 0) (cairo-paint *cr*))))
                            (tile-surfaces t1))])
             (flush-shadow-tiles!)
             (let* ([two (image 900 700 1.5 (lambda () (draw-shadow 140.3 230.7 300 180 8 14)))]
                    [t2 (the-only-tile)])
               (let ([ok (and t1 t2 (not (eq? t1 t2))
                              (for-all same-bytes? kept (tile-surfaces t2))
                              (for-all (lambda (f) (= (f t1) (f t2)))
                                       (list shadow-tile-tL0 shadow-tile-tcL shadow-tile-tcR shadow-tile-tR1
                                             shadow-tile-tT0 shadow-tile-tcT shadow-tile-tcB shadow-tile-tB1)))])
                 (for-each cairo-surface-destroy (cons* one two kept))
                 (canvas-create! 1280 800 *host-scale*)
                 ok))))))

(check "a shadow painted in parts equals one painted whole"
       (let* ([whole (image 400 300 1.25 (lambda () (draw-shadow 30.4 20.2 250 150 9 14)))]
              [parts (image 400 300 1.25
                            (lambda ()
                              (for-each (lambda (r)
                                          (call-with-clip (car r) (cadr r) (caddr r) (cadddr r)
                                            (lambda () (draw-shadow 30.4 20.2 250 150 9 14))))
                                        '((0 0 120 240) (120 0 200 100) (120 100 200 140)))))]
              [ok (same-bytes? whole parts)])
         (cairo-surface-destroy whole)
         (cairo-surface-destroy parts)
         ok))

(check "non-uniform transforms fall back to the stacked shadow, bit for bit"
       (let* ([draw (lambda (shadow)
                      (image 500 400 1 (lambda ()
                                         (fill-gradient 0 0 500 400 #x3060a0 #xe0c040 'horizontal)
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
         (let ([a (image 400 300 1 (lambda () (call-with-scale 0.5 (lambda () (draw-shadow 60 40 300 200 8 14)))))])
           (cairo-surface-destroy a)
           (let ([t (the-only-tile)] [keys (vector->list (hashtable-keys *shadow-tiles*))])
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
               (unless (<= *shadow-tile-bytes* shadow-tile-budget) (set! ok #f))))
           (cairo-destroy cr)
           (cairo-surface-destroy s)
           (let* ([n (hashtable-size *shadow-tiles*)]
                  [ok (and ok (< 0 n 300)
                           (= (* 6 n) (owned-tile-surfaces))
                           (= *shadow-tile-bytes*
                              (apply + (map (lambda (e) (shadow-tile-bytes (car e)))
                                            (vector->list (hashtable-values *shadow-tiles*))))))])
             (printf "  ~a of 300 tiles kept, ~,1f MB\n" n (/ *shadow-tile-bytes* 1048576.0))
             (canvas-create! 1280 800 *host-scale*)
             ok))))

;; A tree without shadows, reloaded live, resets without flushing the
;; tiles first, and leaves draw-shadow bound for whoever still calls it.
(define (one-tile-then-unflushed-reset)
  (flush-shadow-tiles!)
  (let* ([one (image 400 300 1 (lambda () (draw-shadow 30 20 250 150 9 14)))] [t (the-only-tile)])
    (fluid-let ([flush-shadow-tiles! void]) (free-owned-surfaces!))
    (values one t)))

(check "tiles freed by a reset that does not flush them are not drawn from"
       (let*-values ([(one t) (one-tile-then-unflushed-reset)]
                     [(two) (image 400 300 1 (lambda () (draw-shadow 30 20 250 150 9 14)))]
                     [(t2) (the-only-tile)])
         (let ([ok (and t t2 (not (eq? t t2)) (= *shadow-tile-bytes* (shadow-tile-bytes t2))
                        (same-bytes? one two))])
           (cairo-surface-destroy one)
           (cairo-surface-destroy two)
           ok)))

(check "nor freed again by a flush, when a new owned surface has their address"
       (let*-values ([(one t) (one-tile-then-unflushed-reset)] [(a) (shadow-tile-template t)])
         (hashtable-set! *gfx-owned* a 'test)
         (flush-shadow-tiles!)
         (let ([ok (eq? 'test (hashtable-ref *gfx-owned* a #f))])
           (hashtable-delete! *gfx-owned* a)
           (cairo-surface-destroy one)
           ok)))

(check "flushing frees every tile surface"
       (begin (flush-shadow-tiles!)
              (and (= 0 (owned-tile-surfaces)) (= 0 *shadow-tile-bytes*) (= 0 (hashtable-size *shadow-tiles*)))))
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
                        (fxsll (exact (round (* 255 g))) 8) (exact (round (* 255 b))))))))

(check "pixmap set and ref round-trip"
       (let ([pm (make-pixmap 3 2)])
         (pixmap-set! pm 2 1 #x123456)
         (pixmap-set-pixel! pm 0 1 #x80402010)
         (and (= (pixmap-w pm) 3) (= (pixmap-h pm) 2)
              (= (pixmap-ref pm 2 1) #xff123456) (= (pixmap-ref pm 0 1) #x80402010)
              (= (pixmap-ref pm 0 0) 0)
              (= (bytevector-u32-native-ref (pixmap-bytes pm) (* 4 (+ 2 3))) #xff123456)
              (raises? (lambda () (pixmap-ref pm 3 0)))
              (raises? (lambda () (pixmap-set! pm 0 -1 'red))))))

(check "pixmaps reject sizes that are not positive whole numbers"
       (and (raises? (lambda () (make-pixmap 0 4))) (raises? (lambda () (make-pixmap 2.5 4)))))

(check "make-pixmap and pixmap-fill! fill every pixel"
       (let ([pm (make-pixmap 7 5 #x00ff00)])
         (define (all? px) (for-all (lambda (i) (= (bytevector-u32-native-ref (pixmap-bytes pm) (* 4 i)) px)) (iota 35)))
         (and (all? #xff00ff00)
              (begin (pixmap-fill! pm (alpha #xffffff 0.5)) (all? #x80808080)))))

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
       (let* ([pm (make-pixmap 2 2 #x0000ff)] [old (pixmap-bytes pm)] [new (make-pixmap-bytevector pm)])
         (bytevector-u32-native-set! new 0 #xffabcdef)
         (and (= (bytevector-length new) 16)
              (eq? (pixmap-swap-bytes! pm new) old)
              (eq? (pixmap-bytes pm) new)
              (= (pixmap-ref pm 0 0) #xffabcdef)
              (raises? (lambda () (pixmap-swap-bytes! pm (make-bytevector 8 0)))))))

;; Each device pixel of the drawn rect shows its pixmap pixel, and nothing
;; outside the rect changes.
(define (draws-nearest? s pm)
  (canvas-create! 1280 800 s)
  (let* ([before (image *canvas-pw* *canvas-ph* s (lambda () (fill-gradient 0 0 300 300 #x204060 #x806040)))]
         [after (image *canvas-pw* *canvas-ph* s
                       (lambda ()
                         (fill-gradient 0 0 300 300 #x204060 #x806040)
                         (draw-pixmap pm 100 100 20 20)))]
         [x0 (exact (* s 100))] [x1 (exact (* s 120))]
         [ok (let loop ([y 0])
               (or (= y (exact (* s 300)))
                   (and (let col ([x 0])
                          (or (= x (exact (* s 300)))
                              (and (if (and (<= x0 x) (< x x1) (<= x0 y) (< y x1))
                                       (= (pixel-at after x y)
                                          (pixmap-ref pm (quotient (- x x0) (* s 10)) (quotient (- y x0) (* s 10))))
                                       (= (pixel-at after x y) (pixel-at before x y)))
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
    (define (at x y) (bytevector-u8-ref grid (+ (mod x cols) (* (mod y rows) cols))))
    (do ([y 0 (+ y 1)]) ((= y rows) next)
      (do ([x 0 (+ x 1)]) ((= x cols))
        (let ([n (apply + (map (lambda (d) (at (+ x (car d)) (+ y (cdr d))))
                               '((-1 . -1) (0 . -1) (1 . -1) (-1 . 0) (1 . 0) (-1 . 1) (0 . 1) (1 . 1))))])
          (bytevector-u8-set! next (+ x (* y cols)) (if (or (= n 3) (and (= n 2) (= 1 (at x y)))) 1 0)))))))

;; A grid from lines of text, # for a live cell.
(define (life-grid . lines)
  (let* ([cols (string-length (car lines))] [g (make-bytevector (* cols (length lines)) 0)])
    (for-each (lambda (line y)
                (for-each (lambda (x) (when (char=? #\# (string-ref line x)) (bytevector-u8-set! g (+ x (* y cols)) 1)))
                          (iota cols)))
              lines (iota (length lines)))
    g))

(define (random-life-grid cols rows)
  (let ([g (make-bytevector (* cols rows) 0)])
    (do ([i 0 (+ i 1)]) ((= i (* cols rows)) g) (bytevector-u8-set! g i (if (< (random 1.0) 0.3) 1 0)))))

;; The grid moved by (dx, dy) on the torus.
(define (life-shift grid cols rows dx dy)
  (let ([g (make-bytevector (* cols rows) 0)])
    (do ([i 0 (+ i 1)]) ((= i (* cols rows)) g)
      (bytevector-u8-set! g (+ (mod (+ (mod i cols) dx) cols) (* cols (mod (+ (quotient i cols) dy) rows)))
                          (bytevector-u8-ref grid i)))))

;; k generations by the kernel, checking every generation against the
;; reference and every pixel against its cell.
(define (life-kernel-ok? grid cols rows k)
  (let ([row-px (life-row-pixels rows)] [dead (color->pixel 'bg-0)] [pix (make-bytevector (* 4 cols rows) 0)])
    (let loop ([i 0] [g grid])
      (or (= i k)
          (let ([next (make-bytevector (* cols rows) 0)])
            (life-step-rows! g next pix row-px dead cols rows 0 rows)
            (and (equal? next (life-reference g cols rows))
                 (for-all (lambda (c)
                            (= (bytevector-u32-native-ref pix (* 4 c))
                               (if (= 1 (bytevector-u8-ref next c)) (fxvector-ref row-px (quotient c cols)) dead)))
                          (iota (* cols rows)))
                 (loop (+ i 1) next)))))))

;; k generations by the kernel alone.
(define (life-kernel-run grid cols rows k)
  (let ([row-px (life-row-pixels rows)] [pix (make-bytevector (* 4 cols rows) 0)])
    (let loop ([i 0] [g grid])
      (if (= i k)
          g
          (let ([next (make-bytevector (* cols rows) 0)])
            (life-step-rows! g next pix row-px 0 cols rows 0 rows)
            (loop (+ i 1) next))))))

(let ([block (life-grid "......" ".##..." ".##..." "......" "......")]
      [blinker (life-grid "....." "....." ".###." "....." ".....")]
      [glider (life-grid ".#......" "..#....." "###....." "........" "........" "........" "........" "........")])
  (check "Life's kernel: a block stays, a blinker turns"
         (and (equal? (life-kernel-run block 6 5 1) block)
              (equal? (life-kernel-run blinker 5 5 1) (life-grid "....." "..#.." "..#.." "..#.." "....."))
              (equal? (life-kernel-run blinker 5 5 2) blinker)))
  (check "Life's kernel: a glider moves one cell down and right in 4 generations, and around an 8 × 8 torus in 32"
         (and (equal? (life-kernel-run glider 8 8 4) (life-shift glider 8 8 1 1))
              (equal? (life-kernel-run glider 8 8 12) (life-shift glider 8 8 3 3))
              (equal? (life-kernel-run glider 8 8 32) glider))))

(random-seed 1717)
(check "Life's kernel equals the reference on random grids over 20 generations, its pixels its cells (72 × 44, 37 × 23, 5 × 3, 3 × 3, 1 × 5, 4 × 1)"
       (for-all (lambda (size) (life-kernel-ok? (random-life-grid (car size) (cadr size)) (car size) (cadr size) 20))
                '((72 44) (37 23) (5 3) (3 3) (1 5) (4 1))))

(check "Life's kernel on rows [y0, y1) writes those rows of the generation and nothing else"
       (let* ([g (random-life-grid 72 44)] [whole (life-reference g 72 44)]
              [part (make-bytevector (* 72 44) 9)] [pix (make-bytevector (* 4 72 44) 0)])
         (life-step-rows! g part pix (life-row-pixels 44) 0 72 44 10 27)
         (for-all (lambda (i)
                    (= (bytevector-u8-ref part i) (if (<= (* 10 72) i (- (* 27 72) 1)) (bytevector-u8-ref whole i) 9)))
                  (iota (* 72 44)))))

(define (bytes-total) (+ (bytes-allocated) (bytes-deallocated)))

(let* ([g (random-life-grid 72 44)] [next (make-bytevector (* 72 44) 0)] [pix (make-bytevector (* 4 72 44) 0)]
       [row-px (life-row-pixels 44)] [dead (color->pixel 'bg-0)])
  (life-step-rows! g next pix row-px dead 72 44 0 44)
  (let ([b0 (bytes-total)])
    (do ([i 0 (fx+ i 1)]) ((fx= i 20)) (life-step-rows! g next pix row-px dead 72 44 0 44))
    (let ([b (- (bytes-total) b0)])
      ;; an 8-argument + made each generation allocate about 300 KB
      (check (format "Life's kernel allocates nothing (~a bytes in 20 generations at 72 × 44)" b) (< b 1024)))))

;; Life's draw handler into a pw × ph image at scale s, and what it gave
;; draw-life-cells: (pixels cols rows cell ox oy).
(define (life-drawn win s)
  (let-values ([(cw ch) (window-content-size win)])
    (let* ([seen #f]
           [a (fluid-let ([draw-life-cells
                           (let ([real draw-life-cells])
                             (lambda args (set! seen args) (apply real args)))])
                (image (exact (ceiling (* s cw))) (exact (ceiling (* s ch))) s
                       (lambda () ((hashtable-ref (window-handlers win) 'draw #f) win cw ch))))])
      (values a seen))))

;; The live cells a window shows: those whose pixel is not bg-0's.
(define (life-shown win)
  (let-values ([(a seen) (life-drawn win 1)])
    (cairo-surface-destroy a)
    (apply (lambda (pixels cols rows cell ox oy)
             (let ([g (make-bytevector (* cols rows) 0)] [dead (color->pixel 'bg-0)])
               (do ([i 0 (+ i 1)]) ((= i (* cols rows)) g)
                 (unless (= dead (pixmap-ref pixels (mod i cols) (quotient i cols)))
                   (bytevector-u8-set! g i 1)))))
           seen)))

(let ([win (life)])
  (window-call win 'key #\space '())   ; paused
  (let ([shown (life-shown win)])
    (window-call win 'key #\n '())
    (check "Life's n key shows the next generation"
           (and (> (apply + (bytevector->u8-list shown)) 100) (equal? (life-shown win) (life-reference shown 72 44)))))
  (window-call win 'key #\n '())
  (let ([b0 (bytes-total)])
    (do ([i 0 (+ i 1)]) ((= i 20)) (window-call win 'key #\n '()))
    (let ([kb (/ (- (bytes-total) b0) 20 1024.0)])
      ;; the handler's call and (invalidate! win) take about 2.4 KB of it
      (check (format "a step of Life (its n key) allocates under 8 KB: ~,1f KB (about 300 KB before its kernel)" kb)
             (< kb 8))))
  (close-window! win))

;; Life's draw handler into an image, next to the cells drawn the old way:
;; bg-0 everywhere, each live cell a rectangle. Compared above the status
;; bar.
(define (life-matches-rects? s content-w content-h)
  (canvas-create! 1280 800 s)
  (let ([win (life)])
    (resize-window! win content-w content-h)
    (let-values ([(cw ch) (window-content-size win)] [(a seen) (life-drawn win s)])
      (let* ([pw (cairo-image-surface-get-width a)] [ph (cairo-image-surface-get-height a)]
             [dead (color->pixel 'bg-0)] [live 0]
             [b (apply (lambda (pixels cols rows cell ox oy)
                         (image pw ph s
                                (lambda ()
                                  (fill-rect 0 0 cw ch 'bg-0)
                                  (do ([y 0 (+ y 1)]) ((= y rows))
                                    (do ([x 0 (+ x 1)]) ((= x cols))
                                      (unless (= dead (pixmap-ref pixels x y))
                                        (set! live (+ live 1))
                                        (fill-rect (+ ox (* x cell) 1) (+ oy (* y cell) 1) (- cell 1) (- cell 1)
                                                   (mix 'green 'cyan (/ y rows)))))))))
                       seen)]
             [grid-bottom (exact (floor (* s (- ch (life-bar-height)))))]
             [ok (and (> live 100) (= 0 (max-diff a b (list 0 grid-bottom pw ph))))])
        (cairo-surface-destroy a)
        (cairo-surface-destroy b)
        (close-window! win)
        (canvas-create! 1280 800 *host-scale*)
        ok))))

(check "Life's pixmap drawing equals per-cell rectangles at 100%"
       (and (life-matches-rects? 1 720 466) (life-matches-rects? 1 803 515)))
(check "Life's pixmap drawing equals per-cell rectangles at 200%"
       (and (life-matches-rects? 2 720 466) (life-matches-rects? 2 803 515)))

;;; ------------------------------------------------------------------ invalidation

(let ([w (open-window "Dirty" 'size '(200 . 100))])
  (define (asks? thunk) (set! *dirty* #f) (thunk) *dirty*)
  (check "(invalidate!), (invalidate! #f), (invalidate! win), (invalidate! win x y w h) and (request-frame!) ask for a frame"
         (for-all asks? (list (lambda () (invalidate!)) (lambda () (invalidate! #f)) (lambda () (invalidate! w))
                              (lambda () (invalidate! w 1 2 3 4)) request-frame!)))
  (window-call w 'key #\a '())   ; no key handler: nothing to see
  (window-on! w 'key (lambda (win key mods) (void)))
  (check "input to a handler asks for a frame" (asks? (lambda () (window-call w 'key #\a '()))))
  (window-on! w 'key (lambda (win key mods) (current-window)))
  (check "handlers run with (current-window) set" (and (eq? w (window-call w 'key #\a '())) (not (current-window))))
  (close-window! w))

(check "reload reads the source list from load.ss"
       (and (equal? (read-source-list) *schevana-sources*) (member "src/wm.ss" *schevana-sources*) #t))

(steps 2)
(check "the world still renders after all this" (begin (render-frame!) #t))

(if (zero? failures) (printf "all checks passed\n") (printf "~a check(s) FAILED\n" failures))
(exit (if (zero? failures) 0 1))
