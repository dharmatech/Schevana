;;; gfx.ss — the drawing API apps use.
;;;
;;; Immediate mode with an implicit current canvas: the window manager
;;; translates and clips to a window's content area before calling the
;;; window's draw handler, so apps draw in local coordinates:
;;;
;;;   (fill-rect 10 10 100 40 'accent)
;;;   (draw-text 16 20 "hello" 'text 'mono)
;;;
;;; Colors are #xRRGGBB fixnums, theme keys ('accent), or (alpha c 0.5).

(define pi (* 4 (atan 1)))

(defvar *cr* 0)          ; current cairo context
(defvar *canvas* 0)      ; the screen image surface: the last frame drawn
;; the second screen buffer: the next frame is drawn here
(defvar *back* 0)
(defvar *canvas-w* 0)    ; logical size (what apps see)
(defvar *canvas-h* 0)
(defvar *scale* 1.0)     ; logical -> pixel (HiDPI)
(defvar *scratch* (foreign-alloc 64))

;; pixel size of the image
(defvar *canvas-pw* (exact (round (* *canvas-w* *scale*))))
(defvar *canvas-ph* (exact (round (* *canvas-h* *scale*))))

;; A canvas pw × ph pixels big, drawn scale times bigger than the logical
;; pixels apps use; its logical size is the whole part of pw / scale.
;; *cr* only measures between frames. *back* is the second screen buffer
;; (see frames, below); both are new, so the next frame is the whole
;; screen. Interrupts are off, so no process is switched out halfway and
;; no frame is drawn on a canvas half made. At another scale fonts measure
;; differently, and so do the theme's sizes made from them.
(define (canvas-create-pixels! pw ph scale)
  (let ([rescaled? (not (= (inexact scale) *scale*))])
    (with-interrupts-disabled
      (when rescaled? (flush-shadow-tiles!))   ; shadow
      (unless (eqv? *canvas* 0) (cairo-surface-destroy *canvas*))
      (unless (eqv? *back* 0) (cairo-surface-destroy *back*))
      (set! *canvas* (cairo-image-surface-create CAIRO_FORMAT_ARGB32 pw ph))
      (set! *back* (cairo-image-surface-create CAIRO_FORMAT_ARGB32 pw ph))
      (set! *canvas-pw* pw)
      (set! *canvas-ph* ph)
      (set! *canvas-w* (exact (floor (/ pw scale))))
      (set! *canvas-h* (exact (floor (/ ph scale))))
      (set! *scale* (inexact scale))
      (reset-measure-cr!)
      (request-frame!))
    (when rescaled? (fonts-rescaled!) (theme-changed!))))

;; A canvas of w × h logical pixels.
(define (canvas-create! w h scale)
  (canvas-create-pixels! (exact (round (* w scale)))
                         (exact (round (* h scale)))
                         scale))

(define (canvas-pixel-width) *canvas-pw*)
(define (canvas-pixel-height) *canvas-ph*)

(define (screenshot path)
  (cairo-surface-flush *canvas*)
  (cairo-surface-write-to-png *canvas* path)
  path)

;;; Between frames *cr* is a context on a 1 × 1 image with the canvas's
;;; scale, and stray drawing from ticks, timers or the REPL lands nowhere.
;;; text-width and font-metrics always measure on it, whatever *cr* is:
;;; exactly as on the canvas (both are image surfaces), never hinted for a
;;; preview drawn at half scale.

(defvar *measure-surface* 0)
(defvar *measure-cr* 0)

(define (reset-measure-cr!)
  (let ([old *measure-cr*])
    ;; a canvas context left in *cr* by code from before the measuring context
    (unless (or (eqv? *cr* 0) (eqv? *cr* old)) (cairo-destroy *cr*))
    (unless (eqv? old 0) (cairo-destroy old))
    (unless (eqv? *measure-surface* 0)
      (cairo-surface-destroy *measure-surface*))
    (set! *measure-surface*
      (cairo-image-surface-create CAIRO_FORMAT_ARGB32 1 1))
    (set! *measure-cr* (cairo-create *measure-surface*))
    (cairo-scale *measure-cr* *scale* *scale*)
    (set! *cr* *measure-cr*)))

;; (the theme's sizes measure fonts before there is a canvas)
(when (eqv? *measure-cr* 0) (reset-measure-cr!))

;;; Offscreen surfaces the renderer owns, by address, so that a reset (or a
;;; reload that changes how they are used) can free every one of them. A
;;; free runs with interrupts off, so no process is switched out halfway
;;; through one and no frame is drawn from a cache half freed.

(defvar *gfx-owned* (make-eqv-hashtable))   ; surface address -> tag
;; how often free-owned-surfaces! has run
(defvar *owned-frees* 0)

;; An ARGB32 surface, cleared to transparent.
(define (make-owned-surface tag pw ph)
  (let ([s (cairo-image-surface-create CAIRO_FORMAT_ARGB32 pw ph)])
    (hashtable-set! *gfx-owned* s tag)
    s))

;; Does nothing for a surface that is not (or no longer) owned.
(define (free-owned-surface! s)
  (with-interrupts-disabled
    (when (hashtable-contains? *gfx-owned* s)
      (hashtable-delete! *gfx-owned* s)
      (cairo-surface-destroy s))))

;; A cache of owned surfaces that a reset does not flush compares
;; *owned-frees* with its own count to see that they are gone.
(define (free-owned-surfaces!)
  (with-interrupts-disabled
    (flush-shadow-tiles!)   ; shadow
    (vector-for-each free-owned-surface! (hashtable-keys *gfx-owned*))
    (set! *owned-frees* (+ *owned-frees* 1))))

;;; ------------------------------------------------------------------ colors

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
    [else (errorf 'color "not a color: ~s" c)]))

(define (color->fixnum c)
  (let-values ([(r g b a) (color-rgba c)])
    (rgb (exact (round (* r 255)))
         (exact (round (* g 255)))
         (exact (round (* b 255))))))

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
    (let-values ([(r g b) (cond [(< hp 1) (values c x 0)]
                                [(< hp 2) (values x c 0)]
                                [(< hp 3) (values 0 c x)]
                                [(< hp 4) (values 0 x c)]
                                [(< hp 5) (values x 0 c)]
                                [else (values c 0 x)])])
      (rgb (byte r) (byte g) (byte b)))))

;; Black or white, whichever reads better on color c.
(define (readable-on c)
  (let-values ([(r g b a) (color-rgba c)])
    (if (> (+ (* 0.299 r) (* 0.587 g) (* 0.114 b)) 0.45) #x111111 #xf4f4f4)))

;; Blend two colors: t=0 gives c1, t=1 gives c2.
(define (mix c1 c2 t)
  (let-values ([(r1 g1 b1 a1) (color-rgba c1)] [(r2 g2 b2 a2) (color-rgba c2)])
    (define (m a b) (exact (round (* 255 (+ a (* t (- b a)))))))
    (rgb (m r1 r2) (m g1 g2) (m b1 b2))))

(define (set-color! c)
  (let-values ([(r g b a) (color-rgba c)])
    (cairo-set-source-rgba *cr* r g b a)))

;;; ------------------------------------------------------------------ fonts
;;; A font is a setting: the theme's roles ('ui, 'mono, 'title, ...) are
;;; rules, below, that make one with font, and get-font of a role reads
;;; the theme.

(define-record-type (font-type make-font font?)
  (nongenerative schevana-font-2)
  (fields (immutable family font-family) (immutable slant font-slant)
          (immutable weight font-weight) (immutable size font-size)
          (immutable face font-face)
          (mutable cache font-cache font-cache-set!)))

(define (make-font* family size . opts)
  (let ([slant (if (memq 'italic opts)
                   CAIRO_FONT_SLANT_ITALIC
                   CAIRO_FONT_SLANT_NORMAL)]
        [weight (if (memq 'bold opts)
                    CAIRO_FONT_WEIGHT_BOLD
                    CAIRO_FONT_WEIGHT_NORMAL)])
    (make-font family slant weight size
               (cairo-toy-font-face-create family slant weight) #f)))

;; (font family size . opts), opts bold and italic: one record for each,
;; so that every theme asking for it shares its face, its metrics and
;; what is laid out in it.
(define *font-specs* (make-hashtable equal-hash equal?))
(define (font family size . opts)
  (let ([spec (cons* family size opts)])
    (or (hashtable-ref *font-specs* spec #f)
        (let ([f (apply make-font* family size opts)])
          (hashtable-set! *font-specs* spec f)
          f))))

;; A font, or a theme key whose value is one ('ui, 'mono-bold, ...).
(define (get-font f) (if (font? f) f (theme f)))

;; The roles, from the four font settings (a style may give any).
(let loop ([roles
            (list
             'ui (rule (font (theme 'ui-font) (theme 'ui-font-size)))
             'ui-bold (rule (font (theme 'ui-font) (theme 'ui-font-size) 'bold))
             'ui-small (rule (font (theme 'ui-font)
                                   (- (theme 'ui-font-size) 2)))
             'ui-italic (rule (font (theme 'ui-font) (theme 'ui-font-size)
                                    'italic))
             'title (rule (font (theme 'ui-font) (theme 'ui-font-size) 'bold))
             'heading (rule (font (theme 'ui-font) (+ (theme 'ui-font-size) 5)
                                  'bold))
             'big (rule (font (theme 'ui-font) (* 2 (theme 'ui-font-size))
                              'bold))
             'huge (rule (font "serif" 360 'bold))
             'mono (rule (font (theme 'mono-font) (theme 'mono-font-size)))
             'mono-small (rule (font (theme 'mono-font)
                                     (- (theme 'mono-font-size) 2)))
             'mono-bold (rule (font (theme 'mono-font) (theme 'mono-font-size)
                                    'bold)))])
  (unless (null? roles)
    (theme-default! (car roles) (cadr roles))
    (loop (cddr roles))))

;; The screen's scale changed: each font is measured again.
(define (fonts-rescaled!)
  (vector-for-each (lambda (f) (font-cache-set! f #f))
                   (hashtable-values *font-specs*)))

(define (use-font! f) (use-font-on! *cr* f))

(define (use-font-on! cr f)
  (cairo-set-font-face cr (font-face f))
  (cairo-set-font-size cr (font-size f)))

;; #(ascent descent height char-width), measured on first use (on the
;; measuring context, see above).
(define (font-metrics f)
  (let ([f (get-font f)] [cr *measure-cr*])
    (or (font-cache f)
        (begin
          (use-font-on! cr f)
          (cairo-font-extents cr *scratch*)
          (let ([ascent (foreign-ref 'double *scratch* 0)]
                [descent (foreign-ref 'double *scratch* 8)]
                [height (foreign-ref 'double *scratch* 16)])
            (cairo-text-extents cr "M" *scratch*)
            (let ([m (vector ascent descent height
                             (foreign-ref 'double *scratch* 32))])
              (font-cache-set! f m)
              m))))))

(define (font-ascent f) (vector-ref (font-metrics f) 0))
(define (font-line-height f) (vector-ref (font-metrics f) 2))
(define (font-char-width f) (vector-ref (font-metrics f) 3))

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
     (use-font-on! *measure-cr* (get-font font))
     (cairo-text-extents *measure-cr* str *scratch*)
     (foreign-ref 'double *scratch* 32)]))

;; Width of the widest of strs (0 for none).
(define (widest-text strs font)
  (apply max 0 (map (lambda (s) (text-width s font)) strs)))

;; Text centered in a box.
(define (draw-text-centered x y w h str color font)
  (let ([tw (text-width str font)] [th (font-line-height font)])
    (draw-text (+ x (/ (- w tw) 2)) (+ y (/ (- h th) 2)) str color font)))

;; str cut to as many characters as w pixels could show (no glyph but a
;; zero-width one is narrower than about a pixel): all a fit measures.
(define (text-within str w)
  (let ([n (+ 1 (max 0 (exact (floor w))))])
    (if (> (string-length str) n) (substring str 0 n) str)))

;; Shorten str with an ellipsis until it fits in max-w: the longest prefix
;; of str within max-w that does, found by halving (log n measures, not n).
;; measure, if given, is (lambda (s) width) instead of text-width in font.
(define (fit-text str max-w font . measure)
  (let* ([width (if (pair? measure) (car measure)
                    (lambda (s) (text-width s font)))]
         [s (text-within str max-w)]
         [cut (lambda (n) (string-append (substring s 0 n) "…"))])
    (if (and (eq? s str) (<= (width str) max-w))
        str
        (let loop ([lo 0] [hi (- (string-length s) 1)])   ; lo fits, or is 0
          (let ([mid (quotient (+ lo hi 1) 2)])
            (cond [(>= lo hi) (cut lo)]
                  [(<= (width (cut mid)) max-w) (loop mid hi)]
                  [else (loop lo (- mid 1))]))))))

;; Break str into lines no wider than max-w, at spaces. Newlines are kept;
;; a single word wider than max-w gets a line of its own.
(define (wrap-text str max-w font)
  (apply append
         (map (lambda (para)
                (let loop ([words (string-split para #\space)]
                           [line #f]
                           [acc '()])
                  (cond
                    [(null? words) (reverse (if line (cons line acc) acc))]
                    [(not line) (loop (cdr words) (car words) acc)]
                    [else
                     (let ([longer (string-append line " " (car words))])
                       (if (<= (text-width longer font) max-w)
                           (loop (cdr words) longer acc)
                           (loop (cdr words) (car words) (cons line acc))))])))
              (string-split str #\newline))))

;; Draw str word-wrapped to width w, one line every step pixels (default:
;; the font's line height). Returns the y just below the last line.
;; (draw-wrapped-text, desktop/crash.ss, breaks monospace text anywhere.)
(define (draw-paragraph x y w str color font . step)
  (let ([step (if (pair? step) (car step) (font-line-height font))])
    (fold-left (lambda (y line) (draw-text x y line color font) (+ y step))
               y (wrap-text str w font))))

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

;; Many rectangles in one color, filled at once: ((x y w h) ...).
(define (fill-rects rects color)
  (set-color! color)
  (for-each (lambda (r)
              (cairo-rectangle *cr* (car r) (cadr r) (caddr r) (cadddr r)))
            rects)
  (cairo-fill *cr*))

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
           (for-each (lambda (p) (cairo-line-to *cr* (car p) (cdr p)))
                     (cdr points)))
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

(define-record-type layer
  (nongenerative schevana-layer)
  (fields surface w h scale))

(define (make-layer* w h)
  (make-layer (cairo-image-surface-create CAIRO_FORMAT_ARGB32
                                          (exact (ceiling (* w *scale*)))
                                          (exact (ceiling (* h *scale*))))
              w h *scale*))

(define (free-layer! l) (cairo-surface-destroy (layer-surface l)))

;; The cairo_t goes on an error too. (Not by dynamic-wind: a process's
;; engine leaves and re-enters that at every slice.)
(define (draw-into-layer! l thunk)
  (let ([cr (cairo-create (layer-surface l))])
    (cairo-scale cr (layer-scale l) (layer-scale l))
    (guard (c [#t (cairo-destroy cr) (raise c)])
      (fluid-let ([*cr* cr]) (thunk)))
    (cairo-destroy cr)))

(define (blit-layer! l x y)
  (cairo-save *cr*)
  (cairo-translate *cr* x y)
  (cairo-scale *cr* (/ 1.0 (layer-scale l)) (/ 1.0 (layer-scale l)))
  (cairo-set-source-surface *cr* (layer-surface l) 0 0)
  (cairo-paint *cr*)
  (cairo-restore *cr*))

;; A radial glow: color at the center fading to transparent at radius r.
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

;;; ------------------------------------------------------------------
;;; pixel buffers
;;; A pixmap is a small image an app fills pixel by pixel (one pixel per
;;; Life cell, say) and draws scaled up in one call, instead of drawing
;;; thousands of rectangles. Its pixels are a Scheme bytevector of 4·w·h
;;; bytes, row after row, each pixel one native 32-bit word in cairo's
;;; premultiplied ARGB (color->pixel makes one). Tight loops can write
;;; (pixmap-bytes pm) directly; a worker thread can fill a buffer from
;;; make-pixmap-bytevector and hand it over with pixmap-swap-bytes!.

(define-record-type (pixmap new-pixmap pixmap?)
  (nongenerative schevana-pixmap-2)
  (fields width height (mutable bytes)))

(define (pixmap-w pm) (pixmap-width pm))
(define (pixmap-h pm) (pixmap-height pm))

;; Transparent, or filled with color.
(define make-pixmap
  (case-lambda
    [(w h) (make-pixmap w h #f)]
    [(w h color)
     (unless (and (fixnum? w) (fixnum? h) (fx> w 0) (fx> h 0))
       (errorf 'make-pixmap "not a pixmap size: ~s × ~s" w h))
     (let ([pm (new-pixmap w h (make-immobile-bytevector (fx* 4 (fx* w h)) 0))])
       (when color (pixmap-fill! pm color))
       pm)]))

;; A zeroed buffer the size of pm's pixels, for a worker to fill.
(define (make-pixmap-bytevector pm)
  (make-immobile-bytevector (fx* 4 (fx* (pixmap-w pm) (pixmap-h pm))) 0))

;; Wraps bv (4·w·h bytes of pixels) without copying it.
(define (bytevector->pixmap bv w h)
  (unless (and (fixnum? w) (fixnum? h) (fx> w 0) (fx> h 0)
               (bytevector? bv) (= (bytevector-length bv) (* 4 w h)))
    (errorf 'bytevector->pixmap "need ~a bytes for ~s × ~s pixels"
            (* 4 w h) w h))
  (new-pixmap w h bv))

(define (check-pixmap-bytes who pm bv)
  (unless (and (bytevector? bv)
               (= (bytevector-length bv) (* 4 (pixmap-w pm) (pixmap-h pm))))
    (errorf who "need ~a bytes of pixels for a ~a × ~a pixmap"
            (* 4 (pixmap-w pm) (pixmap-h pm)) (pixmap-w pm) (pixmap-h pm))))

;; A color as one pixel. #xRRGGBB is opaque; theme keys and (alpha c a)
;; are premultiplied. Reads the theme, so main thread only.
(define (color->pixel c)
  (if (fixnum? c)
      (fxior #xff000000 (fxand c #xffffff))
      (let-values ([(r g b a) (color-rgba c)])
        (let ([a (clamp a 0.0 1.0)])
          (define (byte v) (exact (round (* 255 v))))
          (fxior (fxsll (byte a) 24) (fxsll (byte (* r a)) 16)
                 (fxsll (byte (* g a)) 8) (byte (* b a)))))))

(define (pixmap-index who pm x y)
  (unless (and (fixnum? x) (fixnum? y)
               (fx< -1 x (pixmap-w pm)) (fx< -1 y (pixmap-h pm)))
    (errorf who "(~s, ~s) is outside a ~a × ~a pixmap"
            x y (pixmap-w pm) (pixmap-h pm)))
  (fx* 4 (fx+ x (fx* y (pixmap-w pm)))))

(define (pixmap-set! pm x y color)
  (pixmap-set-pixel! pm x y (color->pixel color)))

(define (pixmap-set-pixel! pm x y px)
  (bytevector-u32-native-set! (pixmap-bytes pm)
                              (pixmap-index 'pixmap-set-pixel! pm x y) px))

;; The raw pixel.
(define (pixmap-ref pm x y)
  (bytevector-u32-native-ref (pixmap-bytes pm)
                             (pixmap-index 'pixmap-ref pm x y)))

(define (pixmap-fill! pm color)
  (let* ([bv (pixmap-bytes pm)] [px (color->pixel color)]
         [row (fx* 4 (pixmap-w pm))] [n (bytevector-length bv)])
    (do ([i 0 (fx+ i 4)]) ((fx= i row)) (bytevector-u32-native-set! bv i px))
    (let loop ([done row])   ; then double what is filled
      (when (fx< done n)
        (let ([k (fxmin done (fx- n done))])
          (bytevector-copy! bv 0 bv done k)
          (loop (fx+ done k)))))))

;; bv holds raw pixels, exactly as many as pm has.
(define (pixmap-load! pm bv)
  (check-pixmap-bytes 'pixmap-load! pm bv)
  (bytevector-copy! bv 0 (pixmap-bytes pm) 0 (bytevector-length bv)))

;; Installs bv (from make-pixmap-bytevector, say) as pm's pixels without
;; copying; returns the old buffer.
(define (pixmap-swap-bytes! pm bv)
  (check-pixmap-bytes 'pixmap-swap-bytes! pm bv)
  (let ([old (pixmap-bytes pm)])
    (pixmap-bytes-set! pm bv)
    old))

;; Draws the pixmap scaled to w × h (default: its own size), each pixel a
;; sharp block. Cairo reads the bytes in place: they are locked for the
;; call, and an image target composites before it returns.
(define draw-pixmap
  (case-lambda
    [(pm x y) (draw-pixmap pm x y (pixmap-w pm) (pixmap-h pm))]
    [(pm x y w h)
     (when (and (> w 0) (> h 0))
       (let ([bytes (pixmap-bytes pm)] [pw (pixmap-w pm)] [ph (pixmap-h pm)]
             [surface 0] [pat 0])
         (dynamic-wind
           (lambda ()
             (lock-object bytes)
             (set! surface (cairo-image-surface-create-for-data
                            (object->reference-address bytes)
                            CAIRO_FORMAT_ARGB32 pw ph (fx* 4 pw)))
             (set! pat (cairo-pattern-create-for-surface surface))
             (cairo-pattern-set-filter pat CAIRO_FILTER_NEAREST)
             (cairo-save *cr*))
           (lambda ()
             (cairo-translate *cr* x y)
             (cairo-scale *cr* (/ w pw) (/ h ph))
             (cairo-set-source *cr* pat)
             (cairo-rectangle *cr* 0 0 pw ph)
             (cairo-fill *cr*))
           (lambda ()
             (cairo-restore *cr*)
             (cairo-pattern-destroy pat)
             (cairo-surface-destroy surface)
             (unlock-object bytes)))))]))

;;; ------------------------------------------------------------------ frames
;;; A frame is drawn from scratch into *back*, the second screen buffer,
;;; clipped to a box around what changed (render-frame! in wm.ss). Then
;;; each of the box's rows is compared with the screen's, the rows that
;;; changed are copied onto the screen, and the host uploads only those.
;;; All of it runs on the main thread.

(defvar *frame-rows* 0)   ; how many rows the last frame changed

;; (a world reloaded from before the second buffer has none yet)
(when (and (eqv? *back* 0) (not (eqv? *canvas* 0)))
  (set! *back* (cairo-image-surface-create CAIRO_FORMAT_ARGB32
                                           *canvas-pw* *canvas-ph*))
  (request-frame!))

;; Copies the rows from y0 to y1 of *back*, holding a new frame, that
;; differ from the screen's onto the screen, which stays the same surface
;; (swapping the buffers instead measured slower); returns their runs,
;; ((y0 . y1) ...). Whole rows: the frame left the other columns of
;; *back* as they were, equal to the screen's.
(define (copy-changed-rows! y0 y1)
  (cairo-surface-flush *canvas*)
  (let* ([b (cairo-image-surface-get-data *back*)]
         [c (cairo-image-surface-get-data *canvas*)]
         [stride (cairo-image-surface-get-stride *canvas*)]
         [runs (changed-rows b c stride y0 y1)])
    (for-each (lambda (r)
                (sdl-memcpy (+ c (* stride (car r))) (+ b (* stride (car r)))
                            (* stride (- (cdr r) (car r)))))
              runs)
    (cairo-surface-mark-dirty *canvas*)
    (set! *frame-rows*
      (fold-left (lambda (n r) (+ n (- (cdr r) (car r)))) 0 runs))
    runs))

;; Runs of rows from y0 to y1 where two buffers differ, ((y0 . y1) ...),
;; top first; one memcmp a row, from the bottom up.
(define (changed-rows pa pb stride y0 y1)
  (let loop ([y (fx- y1 1)] [end #f] [acc '()])   ; rows y+1 .. end-1 differ
    (cond
      [(fx< y y0) (if end (cons (cons y0 end) acc) acc)]
      [(fx= 0 (sdl-memcmp (+ pa (fx* y stride)) (+ pb (fx* y stride)) stride))
       (loop (fx- y 1) #f (if end (cons (cons (fx+ y 1) end) acc) acc))]
      [else (loop (fx- y 1) (or end (fx+ y 1)) acc)])))
