;;; gfx.ss — the drawing API apps use.
;;;
;;; Immediate mode with an implicit current canvas: the window manager
;;; translates and clips to a window's content area before calling the
;;; window's draw handler, so apps draw in local coordinates:
;;;
;;;   (fill-rect 10 10 100 40 'accent)
;;;   (draw-text 16 20 "hello" 'text 'mono)
;;;
;;; Colors are #xRRGGBB fixnums, theme keys ('accent), (alpha c 0.5),
;;; stipples, patterns used as colors (stipple, below), or gradients
;;; (linear-gradient, below).

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
(defvar *stipple-tiles* (make-hashtable equal-hash equal?))   ; owned too
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
    (hashtable-clear! *stipple-tiles*)
    (vector-for-each free-owned-surface! (hashtable-keys *gfx-owned*))
    (set! *owned-frees* (+ *owned-frees* 1))))

;;; ------------------------------------------------------------------ colors

(define (rgb r g b) (fxior (fxsll r 16) (fxsll g 8) b))
(define (alpha c a) (cons c a))

;; A pattern used as a color: rows of one length, of #\# (ink) and of
;; anything else (paper; #f paper is clear), a logical pixel each, tiled
;; from the screen's device origin, so tiles line up across windows as
;; Medley's textures do. Plain data: it saves and prints.
;;   (stipple 'text 'bg-1 "#..." "...." "..#." "....")
(define (stipple ink paper . rows) (cons* 'stipple ink paper rows))
(define (stipple? c) (and (pair? c) (eq? (car c) 'stipple)))

(define (color-rgba c)
  (cond
    [(fixnum? c)
     (values (fl/ (fixnum->flonum (fxand (fxsra c 16) 255)) 255.0)
             (fl/ (fixnum->flonum (fxand (fxsra c 8) 255)) 255.0)
             (fl/ (fixnum->flonum (fxand c 255)) 255.0)
             1.0)]
    [(symbol? c) (color-rgba (theme c))]
    [(stipple? c)   ; its ink and paper mixed by coverage
     (let*-values ([(cells) (string->list (apply string-append (cdddr c)))]
                   [(k) (/ (length (filter (lambda (ch) (char=? ch #\#)) cells))
                           (length cells))]
                   [(r g b a) (color-rgba (cadr c))]
                   [(r2 g2 b2 a2) (color-rgba (or (caddr c) (alpha 0 0)))]
                   [(wi wp) (values (* k a) (* (- 1 k) a2))])
       (define (m x y)
         (if (zero? (+ wi wp)) 0.0 (/ (+ (* wi x) (* wp y)) (+ wi wp))))
       (values (m r r2) (m g g2) (m b b2) (+ wi wp)))]
    [(pair? c)
     (let-values ([(r g b a) (color-rgba (car c))])
       (values r g b (* a (cdr c))))]
    [else (errorf 'color "not a color: ~s" c)]))

;; Blend two colors: t=0 gives c1, t=1 gives c2.
(define (mix c1 c2 t)
  (let-values ([(r1 g1 b1 a1) (color-rgba c1)] [(r2 g2 b2 a2) (color-rgba c2)])
    (define (m a b) (exact (round (* 255 (+ a (* t (- b a)))))))
    (rgb (m r1 r2) (m g1 g2) (m b1 b2))))

(define (set-color! c)
  (let ([c (let key ([c c]) (if (symbol? c) (key (theme c)) c))])
    (cond
      [(stipple? c) (set-stipple! c)]
      [(and (pair? c) (memq (car c) '(linear-gradient radial-gradient)))
       (set-gradient! c)]
      [else (let-values ([(r g b a) (color-rgba c)])
              (cairo-set-source-rgba *cr* r g b a))])))

;; A stipple's tile on device pixels where px's rule puts them at our
;; scale (logical ceiling((d + 1/2)/s) - 1), whole periods: exact anywhere.
(define (stipple-tile c s ink paper)
  (let* ([w (string-length (cadddr c))] [h (length (cdddr c))]
         [pw (numerator (* w s))] [ph (numerator (* h s))]
         [at (lambda (d n) (mod (- (ceiling (/ (+ d 1/2) s)) 1) n))]
         [us (list->vector (map (lambda (x) (at x w)) (iota pw)))]
         [tile (make-owned-surface 'stipple pw ph)])
    (do ([y 0 (+ y 1)]) ((= y ph) (cairo-surface-mark-dirty tile) tile)
      (let ([row (list-ref (cdddr c) (at y h))])
        (do ([x 0 (+ x 1)]) ((= x pw))
          (foreign-set! 'unsigned-32 (cairo-image-surface-get-data tile)
                        (* 4 (+ x (* y pw)))
                        (if (char=? #\# (string-ref row (vector-ref us x)))
                            ink paper)))))))

;; Tiles are made once per rows, colors and scale (one takes about 1 ms
;; at 133 %, and Genera grays every window not selected), at most 64.
;; Interrupts are off until the pattern holds its tile.
(define (set-stipple! c)
  (cairo-get-matrix *cr* *scratch*)   ; the pattern's space is the device's
  (let* ([s (rationalize (exact (foreign-ref 'double *scratch* 0)) 1/100000)]
         [ink (color->pixel (cadr c))]
         [paper (color->pixel (or (caddr c) (alpha 0 0)))]
         [key (list* s ink paper (cdddr c))] [t *stipple-tiles*]
         [pat (with-interrupts-disabled
                (cairo-pattern-create-for-surface
                 (or (hashtable-ref t key #f)
                     (let ([tile (stipple-tile c s ink paper)])
                       (when (>= (hashtable-size t) 64)
                         (vector-for-each free-owned-surface!
                                          (hashtable-values t))
                         (hashtable-clear! t))
                       (hashtable-set! t key tile)
                       tile))))])
    (cairo-pattern-set-matrix pat *scratch*)
    (cairo-pattern-set-extend pat CAIRO_EXTEND_REPEAT)
    (cairo-pattern-set-filter pat CAIRO_FILTER_NEAREST)
    (cairo-set-source *cr* pat)   ; which keeps the pattern and its tile
    (cairo-pattern-destroy pat)))

;; A gradient used as a color: its colors spread evenly from (x0, y0) to
;; (x1, y1), or from the center (cx, cy) out to radius r, in the
;; coordinates it is drawn in. Plain data, as a stipple is.
;;   (fill-rect 0 0 w h (linear-gradient 0 0 0 h 'accent 'bg-1))
(define (linear-gradient x0 y0 x1 y1 . colors)
  (cons* 'linear-gradient x0 y0 x1 y1 colors))
(define (radial-gradient cx cy r . colors)
  (cons* 'radial-gradient cx cy r colors))

(define (set-gradient! c)
  (let* ([radial? (eq? (car c) 'radial-gradient)]
         [colors (list-tail c (if radial? 4 5))]
         [n (length colors)]
         [pat (if radial?
                  (apply (lambda (cx cy r)
                           (cairo-pattern-create-radial cx cy 0 cx cy r))
                         (list-head (cdr c) 3))
                  (apply cairo-pattern-create-linear (list-head (cdr c) 4)))])
    (for-each (lambda (color i)
                (let-values ([(r g b a) (color-rgba color)])
                  (cairo-pattern-add-color-stop-rgba
                   pat (/ i (max 1 (- n 1))) r g b a)))
              colors (iota n))
    (cairo-set-source *cr* pat)   ; which keeps the pattern
    (cairo-pattern-destroy pat)))

;;; ------------------------------------------------------------------ fonts
;;; A font is a setting: the theme's roles ('ui, 'mono, 'title, ...) are
;;; rules, below, that make one with font, and get-font of a role reads
;;; the theme.

(define-record-type (font-type make-font font?)
  (nongenerative schevana-font-3)
  ;; slant italic or normal, weight bold or normal
  (fields (immutable family font-family) (immutable slant font-slant)
          (immutable weight font-weight) (immutable size font-size)
          (immutable face font-face)
          (mutable cache font-cache font-cache-set!)
          (immutable options font-options)))   ; cairo's: antialiased or not

(defvar plain-font-options (cairo-font-options-create))
(defvar crisp-font-options (cairo-font-options-create))
(cairo-font-options-set-antialias crisp-font-options CAIRO_ANTIALIAS_NONE)

(define (make-font* family size . opts)
  (let ([italic? (memq 'italic opts)]
        [bold? (memq 'bold opts)]
        ;; a Mac's Cairo takes monospace for Courier, which has no λ
        [face-family (if (and (equal? family "monospace")
                              (memq (machine-type)
                                    '(ta6osx tarm64osx a6osx arm64osx)))
                         "Menlo"
                         family)])
    (make-font family (if italic? 'italic 'normal) (if bold? 'bold 'normal)
               size
               (cairo-toy-font-face-create
                face-family
                (if italic? CAIRO_FONT_SLANT_ITALIC CAIRO_FONT_SLANT_NORMAL)
                (if bold? CAIRO_FONT_WEIGHT_BOLD CAIRO_FONT_WEIGHT_NORMAL))
               #f
               (if (memq 'crisp opts) crisp-font-options plain-font-options))))

;; (font family size . opts), opts bold, italic and crisp (not
;; antialiased: Platinum's text): one record for each, so that every
;; theme asking for it shares its face, its metrics and what is laid out
;; in it.
(define *font-specs* (make-hashtable equal-hash equal?))
(define (font family size . opts)
  (let ([spec (cons* family size opts)])
    (or (hashtable-ref *font-specs* spec #f)
        (let ([f (apply make-font* family size opts)])
          (hashtable-set! *font-specs* spec f)
          f))))

;; A font, or a theme key whose value is one ('ui, 'mono-bold, ...).
(define (get-font f) (if (font? f) f (theme f)))

;; The roles, from the four font settings (a style may give any): the ui
;; or mono family at its size plus d (or twice it).
(define (theme-font-rule family d . opts)
  (theme-rule
   (let ([size (theme (if (eq? family 'ui) 'ui-font-size 'mono-font-size))])
     (apply font (theme (if (eq? family 'ui) 'ui-font 'mono-font))
            (if (eq? d 'twice) (* 2 size) (+ size d)) opts))))
(let loop ([roles (list 'ui (theme-font-rule 'ui 0)
                        'ui-bold (theme-font-rule 'ui 0 'bold)
                        'ui-small (theme-font-rule 'ui -2)
                        'ui-italic (theme-font-rule 'ui 0 'italic)
                        'title (theme-font-rule 'ui 0 'bold)
                        'heading (theme-font-rule 'ui 5 'bold)
                        'big (theme-font-rule 'ui 'twice 'bold)
                        'huge (theme-rule (font "serif" 360 'bold))
                        'mono (theme-font-rule 'mono 0)
                        'mono-small (theme-font-rule 'mono -2)
                        'mono-bold (theme-font-rule 'mono 0 'bold))])
  (unless (null? roles)
    (theme-default! (car roles) (cadr roles))
    (loop (cddr roles))))

;; Faint text's font: its own (#f), or what a style's procedure makes of
;; it, a part (Genera, with no gray, sets its hints in italic).
(theme-default! 'faint-text-font #f)
(define (faint-font color f)
  (let ([make (and (eq? color 'text-faint) (theme-part 'faint-text-font))])
    (if make (make f) f)))

;; The screen's scale changed: each font is measured again.
(define (fonts-rescaled!)
  (vector-for-each (lambda (f) (font-cache-set! f #f))
                   (hashtable-values *font-specs*)))

(define (use-font! f) (use-font-on! *cr* f))

(define (use-font-on! cr f)
  (cairo-set-font-face cr (font-face f))
  (cairo-set-font-size cr (font-size f))
  (cairo-set-font-options cr (font-options f)))

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
(define (font-descent f) (vector-ref (font-metrics f) 1))
(define (font-line-height f) (vector-ref (font-metrics f) 2))
(define (font-char-width f) (vector-ref (font-metrics f) 3))

;;; ------------------------------------------------------------------ text

;; Draws str with its top-left at (x, y). Returns the x where the text ends.
(define draw-text
  (case-lambda
    [(x y str) (draw-text x y str 'text 'ui)]
    [(x y str color) (draw-text x y str color 'ui)]
    [(x y str color font)
     (let ([f (faint-font color (get-font font))])
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

;; The ink of str in font as *cr* draws it, hinted at its scale: (values
;; left top width height) from the pen on the baseline (top < 0 above).
(define (text-ink str font)
  (use-font! (get-font font))
  (cairo-text-extents *cr* str *scratch*)
  (apply values (map (lambda (i) (foreign-ref 'double *scratch* i))
                     '(0 8 16 24))))

;;; ------------------------------------------------------------------ shapes

(define (rrect-path! x y w h r)
  ;; corner radius 0 makes a style square: all it draws, an app's too
  (let ([r (if (<= (theme 'corner-radius) 0) 0 (min r (/ w 2) (/ h 2)))])
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
     (stroke-round-rects (list (list x y w h r)) color lw)]))

;; Many rounded rectangles in one color, stroked at once (where their
;; lines cross, painted once): ((x y w h r) ...), each line inside its
;; box, as stroke-round-rect's.
(define (stroke-round-rects boxes color lw)
  (set-color! color)
  (cairo-set-line-width *cr* lw)
  (for-each (lambda (b)
              (apply (lambda (x y w h r)
                       (rrect-path! (+ x (/ lw 2)) (+ y (/ lw 2)) (- w lw)
                                    (- h lw) r))
                     b))
            boxes)
  (cairo-stroke *cr*))

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

;; Turned by angle radians, clockwise on the screen.
(define (call-with-rotation angle thunk)
  (dynamic-wind
    (lambda () (cairo-save *cr*) (cairo-rotate *cr* angle))
    thunk
    (lambda () (cairo-restore *cr*))))

;; Drawn in device pixels: those of the surface being drawn on (the
;; screen, a layer, an image), from its top left corner.
(define (call-with-device-pixels thunk)
  (dynamic-wind
    (lambda () (cairo-save *cr*) (cairo-identity-matrix *cr*))
    thunk
    (lambda () (cairo-restore *cr*))))

;; (values s x0 y0): device pixels per logical pixel where we draw, and
;; where the logical origin is in device pixels. Unrotated drawing only:
;; inside call-with-rotation s is not the scale.
(define (device-transform)
  (cairo-get-matrix *cr* *scratch*)
  (values (foreign-ref 'double *scratch* 0) (foreign-ref 'double *scratch* 32)
          (foreign-ref 'double *scratch* 40)))

;; What thunk draws combines with what is there by op instead of covering
;; it: 'difference (white turns it to its inverse) or 'add.
(define (call-with-operator op thunk)
  (let ([o (case op
             [(difference) CAIRO_OPERATOR_DIFFERENCE]
             [(add) CAIRO_OPERATOR_ADD]
             [else (errorf 'call-with-operator "no operator ~s" op)])])
    (dynamic-wind
      (lambda () (cairo-save *cr*) (cairo-set-operator *cr* o))
      thunk
      (lambda () (cairo-restore *cr*)))))

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

;; Draws thunk on a fresh image w × h pixels big, transparent, and writes
;; it to path as a PNG file.
(define (write-image-png path w h thunk)
  (let* ([surface (cairo-image-surface-create CAIRO_FORMAT_ARGB32 w h)]
         [cr (cairo-create surface)]
         [free! (lambda ()
                  (cairo-destroy cr)
                  (cairo-surface-destroy surface))])
    ;; freed on an error too, as draw-into-layer!'s cairo_t
    (guard (c [#t (free!) (raise c)])
      (fluid-let ([*cr* cr]) (thunk)))
    (cairo-surface-write-to-png surface path)
    (free!)))

;;; ------------------------------------------------------------------ pixels
;;; Raw pixels, each one native 32-bit word in cairo's premultiplied ARGB
;;; (color->pixel makes one), row after row in a Scheme bytevector: what
;;; kits/pixmap.ss's pixmaps hold.

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

;; Draws the pw × ph pixels in bytes scaled to w × h at (x, y), each pixel
;; a sharp block. Cairo reads the bytes in place: they are locked for the
;; call, and an image target composites before it returns.
(define (draw-pixels bytes pw ph x y w h)
  (when (and (> w 0) (> h 0))
    (let ([surface 0] [pat 0])
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
          (unlock-object bytes))))))

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
