;;; gfx.ss — the drawing API apps use.
;;;
;;; Immediate mode with an implicit current canvas: the window manager
;;; translates and clips to a window's content area before calling the
;;; window's draw handler, so apps draw in local coordinates:
;;;
;;;   (fill-rect 10 10 100 40 'accent)
;;;   (draw-text 16 20 "hello" 'text 'mono)
;;;
;;; Colours are #xRRGGBB fixnums, theme keys ('accent), or (alpha c 0.5).

(define pi (* 4 (atan 1)))

(defvar *cr* 0)          ; current cairo context
(defvar *canvas* 0)      ; the screen image surface
(defvar *canvas-w* 0)    ; logical size (what apps see)
(defvar *canvas-h* 0)
(defvar *scale* 1.0)     ; logical -> pixel (HiDPI)
(defvar *scratch* (foreign-alloc 64))

(define (canvas-create! w h scale)
  (unless (= *cr* 0)
    (cairo-destroy *cr*)
    (cairo-surface-destroy *canvas*))
  (set! *canvas* (cairo-image-surface-create CAIRO_FORMAT_ARGB32
                                             (exact (round (* w scale)))
                                             (exact (round (* h scale)))))
  (set! *cr* (cairo-create *canvas*))
  (cairo-scale *cr* scale scale)
  (set! *canvas-w* w)
  (set! *canvas-h* h)
  (set! *scale* (inexact scale)))

(define (canvas-pixel-width) (exact (round (* *canvas-w* *scale*))))
(define (canvas-pixel-height) (exact (round (* *canvas-h* *scale*))))

(define (screenshot path)
  (cairo-surface-flush *canvas*)
  (cairo-surface-write-to-png *canvas* path)
  path)

;;; ------------------------------------------------------------------ colours

(define (rgb r g b) (fxior (fxsll r 16) (fxsll g 8) b))
(define (alpha c a) (cons c a))

(define (color-rgba c)
  (cond
    [(fixnum? c)
     (values (fl/ (fixnum->flonum (fxand (fxsra c 16) 255)) 255.0)
             (fl/ (fixnum->flonum (fxand (fxsra c 8) 255)) 255.0)
             (fl/ (fixnum->flonum (fxand c 255)) 255.0)
             1.0)]
    [(symbol? c) (color-rgba (theme c))]
    [(pair? c)
     (let-values ([(r g b a) (color-rgba (car c))])
       (values r g b (* a (cdr c))))]
    [else (errorf 'color "not a colour: ~s" c)]))

(define (color->fixnum c)
  (let-values ([(r g b a) (color-rgba c)])
    (rgb (exact (round (* r 255))) (exact (round (* g 255))) (exact (round (* b 255))))))

;; Hue in degrees, saturation and lightness in [0, 1].
(define (color->hsl c)
  (let-values ([(r g b a) (color-rgba c)])
    (let* ([hi (max r g b)] [lo (min r g b)] [l (/ (+ hi lo) 2)] [d (- hi lo)])
      (if (< d 1e-9)
          (values 0.0 0.0 l)
          (values (cond [(= hi r) (* 60 (mod (/ (- g b) d) 6))]
                        [(= hi g) (* 60 (+ (/ (- b r) d) 2))]
                        [else (* 60 (+ (/ (- r g) d) 4))])
                  (/ d (- 1 (abs (- (* 2 l) 1))))
                  l)))))

(define (hsl->color h s l)
  (let* ([c (* (- 1 (abs (- (* 2 l) 1))) s)]
         [hp (/ (mod h 360) 60)]
         [x (* c (- 1 (abs (- (mod hp 2) 1))))]
         [m (- l (/ c 2))])
    (define (byte v) (clamp (exact (round (* 255 (+ v m)))) 0 255))
    (let-values ([(r g b) (cond [(< hp 1) (values c x 0)] [(< hp 2) (values x c 0)]
                                [(< hp 3) (values 0 c x)] [(< hp 4) (values 0 x c)]
                                [(< hp 5) (values x 0 c)] [else (values c 0 x)])])
      (rgb (byte r) (byte g) (byte b)))))

;; Black or white, whichever reads better on colour c.
(define (readable-on c)
  (let-values ([(r g b a) (color-rgba c)])
    (if (> (+ (* 0.299 r) (* 0.587 g) (* 0.114 b)) 0.45) #x111111 #xf4f4f4)))

;; Blend two colours: t=0 gives c1, t=1 gives c2.
(define (mix c1 c2 t)
  (let-values ([(r1 g1 b1 a1) (color-rgba c1)] [(r2 g2 b2 a2) (color-rgba c2)])
    (define (m a b) (exact (round (* 255 (+ a (* t (- b a)))))))
    (rgb (m r1 r2) (m g1 g2) (m b1 b2))))

(define (set-color! c)
  (let-values ([(r g b a) (color-rgba c)])
    (cairo-set-source-rgba *cr* r g b a)))

;;; ------------------------------------------------------------------ fonts

(define-record-type font
  (nongenerative schevana-font)
  (fields family slant weight size face (mutable cache)))

(defvar *fonts* (make-eq-hashtable))

(define (make-font* family size . opts)
  (let ([slant (if (memq 'italic opts) CAIRO_FONT_SLANT_ITALIC CAIRO_FONT_SLANT_NORMAL)]
        [weight (if (memq 'bold opts) CAIRO_FONT_WEIGHT_BOLD CAIRO_FONT_WEIGHT_NORMAL)])
    (make-font family slant weight size (cairo-toy-font-face-create family slant weight) #f)))

(define (define-font! name family size . opts)
  (hashtable-set! *fonts* name (apply make-font* family size opts)))

(define (get-font f)
  (cond
    [(font? f) f]
    [(hashtable-ref *fonts* f #f)]
    [else (errorf 'get-font "unknown font ~s" f)]))

(define (use-font! f)
  (cairo-set-font-face *cr* (font-face f))
  (cairo-set-font-size *cr* (font-size f)))

;; #(ascent descent height char-width), computed on first use.
(define (font-metrics f)
  (let ([f (get-font f)])
    (or (font-cache f)
        (begin
          (use-font! f)
          (cairo-font-extents *cr* *scratch*)
          (let ([ascent (foreign-ref 'double *scratch* 0)]
                [descent (foreign-ref 'double *scratch* 8)]
                [height (foreign-ref 'double *scratch* 16)])
            (cairo-text-extents *cr* "M" *scratch*)
            (let ([m (vector ascent descent height (foreign-ref 'double *scratch* 32))])
              (font-cache-set! f m)
              m))))))

(define (font-ascent f) (vector-ref (font-metrics f) 0))
(define (font-line-height f) (vector-ref (font-metrics f) 2))
(define (font-char-width f) (vector-ref (font-metrics f) 3))

;; The standard fonts, built from the theme's font keys.
(define (install-default-fonts!)
  (let ([ui (theme 'ui-font)] [ui-size (theme 'ui-font-size)]
        [mono (theme 'mono-font)] [mono-size (theme 'mono-font-size)])
    (define-font! 'mono mono mono-size)
    (define-font! 'mono-small mono (- mono-size 2))
    (define-font! 'mono-bold mono mono-size 'bold)
    (define-font! 'ui ui ui-size)
    (define-font! 'ui-bold ui ui-size 'bold)
    (define-font! 'ui-small ui (- ui-size 2))
    (define-font! 'title ui ui-size 'bold)
    (define-font! 'heading ui (+ ui-size 5) 'bold)
    (define-font! 'big ui (* 2 ui-size) 'bold)
    (define-font! 'huge "serif" 360 'bold)))

(unless (hashtable-contains? *fonts* 'heading) (install-default-fonts!))

;;; ------------------------------------------------------------------ text

;; Draws str with its top-left at (x, y). Returns the x where the text ends.
(define draw-text
  (case-lambda
    [(x y str) (draw-text x y str 'text 'ui)]
    [(x y str color) (draw-text x y str color 'ui)]
    [(x y str color font)
     (let ([f (get-font font)])
       (use-font! f)
       (set-color! color)
       (cairo-move-to *cr* x (+ y (font-ascent f)))
       (cairo-show-text *cr* str)
       (cairo-get-current-point *cr* *scratch* (+ *scratch* 8))
       (foreign-ref 'double *scratch* 0))]))

(define text-width
  (case-lambda
    [(str) (text-width str 'ui)]
    [(str font)
     (use-font! (get-font font))
     (cairo-text-extents *cr* str *scratch*)
     (foreign-ref 'double *scratch* 32)]))

;; Text centred in a box.
(define (draw-text-centered x y w h str color font)
  (let ([tw (text-width str font)] [th (font-line-height font)])
    (draw-text (+ x (/ (- w tw) 2)) (+ y (/ (- h th) 2)) str color font)))

;; Shorten str with an ellipsis until it fits in max-w.
(define (fit-text str max-w font)
  (if (<= (text-width str font) max-w)
      str
      (let loop ([n (- (string-length str) 1)])
        (let ([s (string-append (substring str 0 (max n 0)) "…")])
          (if (or (<= n 0) (<= (text-width s font) max-w)) s (loop (- n 1)))))))

;;; ------------------------------------------------------------------ shapes

(define (rrect-path! x y w h r)
  (let ([r (min r (/ w 2) (/ h 2))])
    (if (<= r 0)
        (cairo-rectangle *cr* x y w h)
        (begin
          (cairo-new-sub-path *cr*)
          (cairo-arc *cr* (- (+ x w) r) (+ y r) r (* -0.5 pi) 0)
          (cairo-arc *cr* (- (+ x w) r) (- (+ y h) r) r 0 (* 0.5 pi))
          (cairo-arc *cr* (+ x r) (- (+ y h) r) r (* 0.5 pi) pi)
          (cairo-arc *cr* (+ x r) (+ y r) r pi (* 1.5 pi))
          (cairo-close-path *cr*)))))

(define (clear! color)
  (set-color! color)
  (cairo-paint *cr*))

(define (fill-rect x y w h color)
  (set-color! color)
  (cairo-rectangle *cr* x y w h)
  (cairo-fill *cr*))

(define stroke-rect
  (case-lambda
    [(x y w h color) (stroke-rect x y w h color 1)]
    [(x y w h color lw)
     (set-color! color)
     (cairo-set-line-width *cr* lw)
     (cairo-rectangle *cr* (+ x (/ lw 2)) (+ y (/ lw 2)) (- w lw) (- h lw))
     (cairo-stroke *cr*)]))

(define (fill-round-rect x y w h r color)
  (set-color! color)
  (rrect-path! x y w h r)
  (cairo-fill *cr*))

(define stroke-round-rect
  (case-lambda
    [(x y w h r color) (stroke-round-rect x y w h r color 1)]
    [(x y w h r color lw)
     (set-color! color)
     (cairo-set-line-width *cr* lw)
     (rrect-path! (+ x (/ lw 2)) (+ y (/ lw 2)) (- w lw) (- h lw) r)
     (cairo-stroke *cr*)]))

(define draw-line
  (case-lambda
    [(x1 y1 x2 y2 color) (draw-line x1 y1 x2 y2 color 1)]
    [(x1 y1 x2 y2 color lw)
     (set-color! color)
     (cairo-set-line-width *cr* lw)
     (cairo-set-line-cap *cr* CAIRO_LINE_CAP_ROUND)
     (cairo-move-to *cr* x1 y1)
     (cairo-line-to *cr* x2 y2)
     (cairo-stroke *cr*)]))

;; points: list of (x . y)
(define draw-polyline
  (case-lambda
    [(points color) (draw-polyline points color 1)]
    [(points color lw)
     (unless (null? points)
       (set-color! color)
       (cairo-set-line-width *cr* lw)
       (cairo-set-line-cap *cr* CAIRO_LINE_CAP_ROUND)
       (cairo-set-line-join *cr* CAIRO_LINE_JOIN_ROUND)
       (cairo-move-to *cr* (caar points) (cdar points))
       (if (null? (cdr points))
           (cairo-line-to *cr* (caar points) (cdar points))
           (for-each (lambda (p) (cairo-line-to *cr* (car p) (cdr p))) (cdr points)))
       (cairo-stroke *cr*))]))

(define (fill-polygon points color)
  (unless (null? points)
    (set-color! color)
    (cairo-move-to *cr* (caar points) (cdar points))
    (for-each (lambda (p) (cairo-line-to *cr* (car p) (cdr p))) (cdr points))
    (cairo-close-path *cr*)
    (cairo-fill *cr*)))

(define (fill-circle cx cy r color)
  (set-color! color)
  (cairo-new-sub-path *cr*)
  (cairo-arc *cr* cx cy r 0 (* 2 pi))
  (cairo-fill *cr*))

(define stroke-circle
  (case-lambda
    [(cx cy r color) (stroke-circle cx cy r color 1)]
    [(cx cy r color lw)
     (set-color! color)
     (cairo-set-line-width *cr* lw)
     (cairo-new-sub-path *cr*)
     (cairo-arc *cr* cx cy r 0 (* 2 pi))
     (cairo-stroke *cr*)]))

;; Linear gradient from c1 to c2; dir is 'vertical or 'horizontal.
(define fill-gradient
  (case-lambda
    [(x y w h c1 c2) (fill-gradient x y w h c1 c2 'vertical 0)]
    [(x y w h c1 c2 dir) (fill-gradient x y w h c1 c2 dir 0)]
    [(x y w h c1 c2 dir radius)
     (let ([pat (if (eq? dir 'horizontal)
                    (cairo-pattern-create-linear x 0 (+ x w) 0)
                    (cairo-pattern-create-linear 0 y 0 (+ y h)))])
       (let-values ([(r g b a) (color-rgba c1)])
         (cairo-pattern-add-color-stop-rgba pat 0 r g b a))
       (let-values ([(r g b a) (color-rgba c2)])
         (cairo-pattern-add-color-stop-rgba pat 1 r g b a))
       (cairo-set-source *cr* pat)
       (rrect-path! x y w h radius)
       (cairo-fill *cr*)
       (cairo-pattern-destroy pat))]))

;; A soft drop shadow made of stacked translucent rounded rects.
(define (draw-shadow x y w h r size)
  (let loop ([i size])
    (when (> i 0)
      (fill-round-rect (- x i) (+ (- y i) (/ size 3)) (+ w (* 2 i)) (+ h (* 2 i)) (+ r i)
                       (alpha #x000000 (/ 0.22 size)))
      (loop (- i 2)))))

;;; ------------------------------------------------------------------ state

(define (call-with-clip x y w h thunk)
  (dynamic-wind
    (lambda ()
      (cairo-save *cr*)
      (cairo-new-path *cr*)
      (cairo-rectangle *cr* x y w h)
      (cairo-clip *cr*))
    thunk
    (lambda () (cairo-restore *cr*))))

(define (call-with-round-clip x y w h r thunk)
  (dynamic-wind
    (lambda ()
      (cairo-save *cr*)
      (cairo-new-path *cr*)
      (rrect-path! x y w h r)
      (cairo-clip *cr*))
    thunk
    (lambda () (cairo-restore *cr*))))

(define (call-with-translation dx dy thunk)
  (dynamic-wind
    (lambda () (cairo-save *cr*) (cairo-translate *cr* dx dy))
    thunk
    (lambda () (cairo-restore *cr*))))

(define (call-with-scale s thunk)
  (dynamic-wind
    (lambda () (cairo-save *cr*) (cairo-scale *cr* s s))
    thunk
    (lambda () (cairo-restore *cr*))))

(define-syntax with-clip
  (syntax-rules ()
    [(_ (x y w h) body ...) (call-with-clip x y w h (lambda () body ...))]))

(define-syntax with-translation
  (syntax-rules ()
    [(_ (dx dy) body ...) (call-with-translation dx dy (lambda () body ...))]))

;;; ------------------------------------------------------------------ layers
;;; An offscreen image that can be drawn once and blitted many times.

(define-record-type layer (nongenerative schevana-layer) (fields surface w h scale))

(define (make-layer* w h)
  (make-layer (cairo-image-surface-create CAIRO_FORMAT_ARGB32
                                          (exact (ceiling (* w *scale*)))
                                          (exact (ceiling (* h *scale*))))
              w h *scale*))

(define (free-layer! l) (cairo-surface-destroy (layer-surface l)))

(define (draw-into-layer! l thunk)
  (let ([cr (cairo-create (layer-surface l))])
    (cairo-scale cr (layer-scale l) (layer-scale l))
    (fluid-let ([*cr* cr]) (thunk))
    (cairo-destroy cr)))

(define (blit-layer! l x y)
  (cairo-save *cr*)
  (cairo-translate *cr* x y)
  (cairo-scale *cr* (/ 1.0 (layer-scale l)) (/ 1.0 (layer-scale l)))
  (cairo-set-source-surface *cr* (layer-surface l) 0 0)
  (cairo-paint *cr*)
  (cairo-restore *cr*))

;; A radial glow: colour at the centre fading to transparent at radius r.
(define (fill-glow cx cy r color)
  (let ([pat (cairo-pattern-create-radial cx cy 0 cx cy r)])
    (let-values ([(cr cg cb ca) (color-rgba color)])
      (cairo-pattern-add-color-stop-rgba pat 0 cr cg cb ca)
      (cairo-pattern-add-color-stop-rgba pat 1 cr cg cb 0))
    (cairo-set-source *cr* pat)
    (cairo-new-path *cr*)
    (cairo-arc *cr* cx cy r 0 (* 2 pi))
    (cairo-fill *cr*)
    (cairo-pattern-destroy pat)))
