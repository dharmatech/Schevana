;;; apps/settings.ss — customize the look of the world.
;;;
;;; Every setting is a theme key (see src/theme.ss), so anything done here
;;; can also be done from the REPL with set-theme!. Changes apply live and
;;; are saved to *theme-file* (~/.schevana-theme.ss), which is loaded at boot.

(define settings-tabs
  '((theme . "Theme") (colors . "Colors") (layout . "Layout") (fonts . "Fonts")
    (display . "Display")))

;; Percentages for the theme key 'scale; 0 follows the desktop.
(define settings-scale-choices
  '(("Auto" . 0) ("100%" . 100) ("125%" . 125) ("150%" . 150) ("175%" . 175)
    ("200%" . 200) ("250%" . 250) ("300%" . 300)))

(define settings-color-groups
  '(("BASE" bg-0 bg-1 bg-2 bg-3 text text-dim text-faint)
    ("ACCENTS" accent accent-2 red orange yellow green cyan teal magenta
               selection)
    ("DESKTOP" desktop-top desktop-bottom watermark)
    ("PANELS" panel panel-text panel-dim panel-hover)
    ("WINDOWS" window-bg window-border window-border-focus title-bg
               title-bg-focus title-text title-text-dim button-close button-min
               button-max button-idle)))

;; (key label min max)
(define settings-metrics
  '((corner-radius "Corner radius" 0 20)
    (title-height "Title bar height" 22 44)
    (shadow "Window shadow" 0 32)   ; shadow
    (topbar-height "Top bar height" 22 40)
    (taskbar-height "Taskbar height" 28 56)))

;;; Sizes are worked out from the current fonts, so the layout still holds
;;; after the Fonts tab makes text bigger. Each is a max with the number it
;;; comes to at the default fonts. When the window is too small for what it
;;; shows, the right-hand pane scrolls and label columns stack.

;; y for a line of font, centered in a row of height h at y.
(define (settings-text-y y h font)
  (+ y (floor (/ (- h (font-line-height font)) 2))))

(define (settings-sidebar-width)
  (max 168
       (+ 40 (text-width "Settings" 'heading))
       (+ 48 (widest-text (map cdr settings-tabs) 'ui-bold))
       (+ 56 (text-width "Reset to defaults" 'ui))))

(define (settings-button-height) (max 30 (round (+ (font-line-height 'ui) 15))))
(define (settings-tab-height) (max 32 (round (+ (font-line-height 'ui) 12))))
(define (settings-color-row-height)
  (max 26 (round (+ (font-line-height 'ui) 11))))

(define (settings-color-list-width)
  (max 220 (+ 54 (widest-text (map symbol->string
                                   (filter symbol? (settings-color-rows)))
                              'ui))))

;; Color editor: label column, hex field and its reset button.
(define (settings-hsl-label-width)
  (max 96 (+ 24 (widest-text '("Hue" "Saturation" "Lightness") 'ui))))
(define (settings-hsl-readout-width) (max 64 (+ 28 (text-width "100%" 'ui))))
(define (settings-hex-width) (max 116 (+ 60 (text-width "#000000" 'mono))))
(define (settings-reset-color-width)
  (max 140 (+ 40 (text-width "Reset to preset" 'ui))))
(define (settings-color-editor-width)
  (+ (settings-hsl-label-width) (settings-hex-width) 10
     (settings-reset-color-width)))

;; The size a new Settings window opens at: wide enough for the Colors
;; tab beside its list, and tall enough for most tabs without scrolling.
(define (settings-natural-width)
  (max 840 (+ (settings-sidebar-width) 28 (settings-color-list-width) 28
              (settings-color-editor-width) 28)))
(define (settings-natural-height)
  (max 600 (* 30 (round (font-line-height 'ui)))))

;; Label column and readout of a slider row.
(define (settings-label-width labels) (max 180 (+ 24 (widest-text labels 'ui))))
(define (settings-readout-width) (max 60 (+ 24 (text-width "00 px" 'ui))))

;; A labeled row: the label in a column label-w wide, or on a line of its
;; own above the control when fewer than min-w pixels would be left for the
;; control. draw-control gets (x y w) for a control control-h tall, with
;; trailing-w pixels kept free after it. Returns the y below the row.
(define (settings-row x y w label-w label control-h min-w trailing-w
                      draw-control)
  (let ([lh (font-line-height 'ui)])
    (if (>= (- w label-w trailing-w) min-w)
        (let ([row-h (max control-h (round lh))])
          (draw-text x (settings-text-y y row-h 'ui) label 'text-dim 'ui)
          (draw-control (+ x label-w) (+ y (floor (/ (- row-h control-h) 2)))
                        (- w label-w trailing-w))
          (+ y row-h))
        (let ([cy (round (+ y lh 6))])
          (draw-text x y label 'text-dim 'ui)
          (draw-control x cy (- w trailing-w))
          (+ cy control-h)))))

(define-record-type settings-st
  (nongenerative schevana-settings-2)
  (fields (mutable tab)
          (mutable key)       ; color being edited
          (mutable scroll)    ; color list scroll offset
          (mutable hex)       ; hex digits being typed, or #f
          ;; #(key rgb h s l): keeps hue stable at zero saturation
          (mutable hsl)
          (mutable pane-scroll)))

(define (settings-set! key value)
  (if (eq? key 'accent)
      (set-accent! value)   ; keeps the keys derived from the accent in step
      (set-theme! key value))
  (save-theme-soon!))

;; Title and subtitle of a tab; returns the y where its content starts.
(define (settings-heading x y w title subtitle)
  (draw-text x y title 'text 'heading)
  (round (+ 24 (draw-paragraph x (+ y (font-line-height 'heading) 6) w subtitle
                               'text-faint 'ui))))

(define (settings-color-rows)
  (apply append settings-color-groups))

;; Fill a track with n small steps of (color-at t), t in [0, 1].
(define (paint-steps x y w h color-at)
  (let ([n 48])
    (do ([i 0 (+ i 1)]) ((= i n))
      (fill-rect (+ x (* i (/ w n))) y (+ 1 (/ w n)) h
                 (color-at (/ (+ i 0.5) n))))))

;; Cards (presets, window styles) are wide enough for their labels.
(define (settings-card-label-height base)
  (max base (round (+ (font-line-height 'ui) (- base 18)))))
(define (settings-card-width labels) (max 180 (+ 24 (widest-text labels 'ui))))

;; Lay cards out in a grid; draw-card gets (item x y). Returns the y below.
(define (settings-card-grid items x y w cw ch draw-card)
  (let* ([gap 12] [cols (max 1 (quotient (+ w gap) (+ cw gap)))])
    (let loop ([is items] [i 0])
      (unless (null? is)
        (draw-card (car is) (+ x (* (mod i cols) (+ cw gap)))
                   (+ y (* (quotient i cols) (+ ch gap))))
        (loop (cdr is) (+ i 1))))
    (+ y (* (quotient (+ (length items) cols -1) cols) (+ ch gap)))))

;;; ------------------------------------------------------------------ theme tab

(define (draw-preset-card x y w h name selected? hover?)
  (let* ([colors (preset-colors name)]
         [c (lambda (k) (plist-ref colors k #x808080))]
         [label-h (settings-card-label-height 30)] [r 10])
    (call-with-round-clip x y w h r
      (lambda ()
        (fill-desktop-pattern x y w h
                              (plist-ref colors 'desktop-pattern 'gradient)
                              (c 'desktop-top) (c 'desktop-bottom))
        (let ([wx (+ x 18)] [wy (+ y 14)] [ww (- w 36)] [wh (- h label-h 22)])
          (fill-round-rect wx (+ wy 3) ww wh 5 (alpha #x000000 0.25))
          (fill-round-rect wx wy ww wh 5 (c 'window-bg))
          (call-with-round-clip wx wy ww wh 5
            (lambda () (fill-rect wx wy ww 12 (c 'title-bg-focus))))
          (for-each (lambda (i k)
                      (fill-circle (+ wx 8 (* i 8)) (+ wy 6) 2.5 (c k)))
                    '(0 1 2) '(button-close button-min button-max))
          (for-each (lambda (i k frac)
                      (fill-round-rect (+ wx 8 (if (= i 2) 10 0))
                                       (+ wy 20 (* i 9))
                                       (* frac (- ww 16)) 4 2 (c k)))
                    '(0 1 2 3) '(accent-2 accent green text-dim)
                    '(0.45 0.7 0.5 0.35)))
        (fill-rect x (- (+ y h) label-h) w label-h (c 'panel))
        (draw-text (+ x 12) (+ (- (+ y h) label-h)
                               (/ (- label-h (font-line-height 'ui)) 2))
                   (preset-title name) (c 'panel-text) 'ui)))
    (stroke-round-rect x y w h r (cond [selected? 'accent]
                                       [hover? 'text-faint]
                                       [else 'window-border])
                       (if selected? 2 1))))

(define (settings-draw-theme ui x y w)
  (let* ([y0 (settings-heading x y w "Theme"
               "Pick a starting point; fine-tune it under Colors.")]
         [cw (settings-card-width (map preset-title (themes)))]
         [ch (+ 90 (settings-card-label-height 30))]
         [ay (+ 10 (settings-card-grid (themes) x y0 w cw ch
                     (lambda (name cx cy)
                       (draw-preset-card cx cy cw ch name
                                         (eq? name (theme 'preset))
                                         (ui-hover? ui cx cy cw ch))
                       (ui-area ui (list 'preset name) cx cy cw ch
                                (lambda ()
                                  (apply-theme-preset! name)
                                  (save-theme-soon!))))))]
         [palette (preset-colors (theme 'preset))]
         [per-row (max 1 (quotient (+ w 8) 36))]
         [cy0 (round (+ ay (font-line-height 'ui-bold) 27))])
    (draw-text x ay "Accent color" 'text 'ui-bold)
    (let loop ([ks '(accent cyan teal green yellow orange red magenta accent-2)]
               [i 0])
      (if (null? ks)
          (+ cy0 (* 40 (quotient (- i 1) per-row)) 15)
          (let ([color (plist-ref palette (car ks) #x808080)]
                [sx (+ x (* 36 (mod i per-row)))]
                [cy (+ cy0 (* 40 (quotient i per-row)))])
            (fill-circle (+ sx 14) cy 11 color)
            (when (eqv? color (theme 'accent))
              (stroke-circle (+ sx 14) cy 15 'text 2))
            (ui-area ui (list 'accent (car ks)) sx (- cy 14) 28 28
                     (lambda () (set-accent! color) (save-theme-soon!)))
            (loop (cdr ks) (+ i 1)))))))

;;; ------------------------------------------------------------------
;;; colors tab

;; avail: the visible height of the pane, which the list fills.
(define (settings-draw-colors ui st x y w avail)
  (let* ([ly (settings-heading x y w "Colors"
               "Pick a color to edit it. Every window updates as you drag.")]
         [lw (settings-color-list-width)]
         [row-h (settings-color-row-height)]
         [rows (settings-color-rows)]
         [total (+ 12 (* row-h (length rows)))]
         ;; the editor sits beside the list, or below it when there's no room
         [beside? (>= (- w lw 28) (settings-color-editor-width))]
         [lh (if beside?
                 (max (* 6 row-h) (- (+ y avail) ly))
                 (min total (* 8 row-h)))]
         [scroll (clamp (settings-st-scroll st) 0 (max 0 (- total lh)))])
    (settings-st-scroll-set! st scroll)
    (fill-round-rect x ly lw lh 8 'bg-0)
    ;; for the wheel; the rows sit on top
    (ui-area ui 'color-list x ly lw lh #f)
    (call-with-clip x (+ ly 4) lw (- lh 8)
      (lambda ()
        (let loop ([rs rows] [ry (- (+ ly 6) scroll)])
          (unless (or (null? rs) (> ry (+ ly lh)))
            (let ([r (car rs)])
              (when (> (+ ry row-h) ly)
                (if (string? r)
                    (draw-text (+ x 12)
                               (+ ry (- row-h (font-line-height 'ui-small) 4))
                               r 'text-faint 'ui-small)
                    (let ([selected? (eq? r (settings-st-key st))]
                          [top (max ry ly)]
                          [bottom (min (+ ry row-h) (+ ly lh))])
                      (cond
                        [selected?
                         (fill-round-rect (+ x 4) ry (- lw 8) row-h 5
                                          (alpha 'accent 0.22))]
                        [(ui-hover? ui x top lw (- bottom top))
                         (fill-round-rect (+ x 4) ry (- lw 8) row-h 5 'bg-2)])
                      (fill-round-rect (+ x 12) (+ ry (/ (- row-h 16) 2))
                                       16 16 4 r)
                      (stroke-round-rect (+ x 12) (+ ry (/ (- row-h 16) 2))
                                         16 16 4 (alpha 'text 0.25))
                      (draw-text (+ x 38)
                                 (+ ry (/ (- row-h (font-line-height 'ui)) 2))
                                 (symbol->string r)
                                 (if selected? 'text 'text-dim) 'ui)
                      (when (> bottom top)
                        (ui-area ui (list 'color r) x top lw (- bottom top)
                                 (lambda ()
                                   (settings-st-key-set! st r)
                                   (settings-st-hex-set! st #f)))))))
              (loop (cdr rs) (+ ry row-h)))))))
    (if beside?
        (max (+ ly lh)
             (settings-draw-color-editor ui st (+ x lw 28) ly (- w lw 28)))
        (settings-draw-color-editor ui st x (+ ly lh 28) w))))

;; Returns the y below the editor.
(define (settings-draw-color-editor ui st x y w)
  (let* ([key (settings-st-key st)]
         [value (theme key)]
         [rgbv (color->fixnum value)]
         [hsl (let ([c (settings-st-hsl st)])
                (if (and c (eq? (vector-ref c 0) key)
                         (eqv? (vector-ref c 1) rgbv))
                    c
                    (let-values ([(hh ss ll) (color->hsl value)])
                      (let ([v (vector key rgbv hh ss ll)])
                        (settings-st-hsl-set! st v)
                        v))))]
         [label-w (settings-hsl-label-width)]
         [readout-w (settings-hsl-readout-width)]
         [sx (+ x label-w)]
         [by (+ y (max 44 (round (+ 2 (font-line-height 'heading) 21))))]
         [bw (settings-hex-width)]
         [bh (max 30 (round (+ (font-line-height 'mono) 14)))]
         [reset-w (settings-reset-color-width)]
         ;; the reset button goes under the hex field when it can't go beside it
         [reset-below? (> (+ label-w bw 10 reset-w) w)]
         [hex-bottom (if reset-below? (+ by bh 10 bh) (+ by bh))]
         [row0 (+ (max (+ y 78) hex-bottom) 26)])
    (define (update! i v)
      (vector-set! hsl i v)
      (let ([c (hsl->color (vector-ref hsl 2) (vector-ref hsl 3)
                           (vector-ref hsl 4))])
        (vector-set! hsl 1 c)
        (settings-set! key c)))
    (define (slider-row ry label id v lo hi readout paint on-change)
      (+ 18 (settings-row x ry w label-w label 24 60 readout-w
              (lambda (cx cy cw)
                (ui-slider ui id cx cy cw v lo hi on-change paint)
                (draw-text (+ cx cw 16) (settings-text-y cy 24 'ui) readout
                           'text-dim 'ui)))))
    ;; swatch, name, hex field, reset
    (fill-round-rect x y 78 78 12 value)
    (stroke-round-rect x y 78 78 12 (alpha 'text 0.25))
    (draw-text sx (+ y 2) (symbol->string key) 'text 'heading)
    (let ([editing (settings-st-hex st)])
      (fill-round-rect sx by bw bh 6 'bg-0)
      (stroke-round-rect sx by bw bh 6 (if editing 'accent 'window-border))
      (let ([tx (draw-text (+ sx 10)
                           (+ by (/ (- bh (font-line-height 'mono)) 2))
                           (string-append "#"
                                          (or editing
                                              (substring (hex-color rgbv) 2 8)))
                           (if editing 'text 'text-dim) 'mono)])
        (when editing (fill-rect (+ tx 1) (+ by 7) 2 (- bh 14) 'accent)))
      (ui-area ui 'hex sx by bw bh (lambda () (settings-st-hex-set! st "")))
      (ui-button ui 'reset-color (if reset-below? sx (+ sx bw 10))
                 (if reset-below? (+ by bh 10) by)
                 reset-w bh "Reset to preset"
                 (lambda ()
                   (settings-set! key (plist-ref (preset-colors (theme 'preset))
                                                 key (theme-default key))))))
    (let* ([hh (vector-ref hsl 2)]
           [ss (vector-ref hsl 3)]
           [ll (vector-ref hsl 4)]
           [y1 (slider-row row0 "Hue" 'hue hh 0 360
                           (format "~a°" (exact (round hh)))
                           (lambda (tx ty tw th)
                             (paint-steps tx ty tw th
                                          (lambda (t)
                                            (hsl->color (* 360 t) (max ss 0.4)
                                                        (clamp ll 0.35 0.65)))))
                           (lambda (v) (update! 2 v)))]
           [y2 (slider-row y1 "Saturation" 'saturation ss 0 1
                           (format "~a%" (exact (round (* 100 ss))))
                           (lambda (tx ty tw th)
                             (paint-steps tx ty tw th
                                          (lambda (t) (hsl->color hh t ll))))
                           (lambda (v) (update! 3 v)))]
           [y3 (slider-row y2 "Lightness" 'lightness ll 0 1
                           (format "~a%" (exact (round (* 100 ll))))
                           (lambda (tx ty tw th)
                             (paint-steps tx ty tw th
                                          (lambda (t) (hsl->color hh ss t))))
                           (lambda (v) (update! 4 v)))])
      ;; rgb and the expression that reads this key, on two lines if need be
      (let*-values ([(r g b a) (color-rgba rgbv)]
                    [(rgb) (format "rgb(~a, ~a, ~a)" (exact (round (* r 255)))
                                   (exact (round (* g 255)))
                                   (exact (round (* b 255))))]
                    [(expr) (format "(theme '~a)" key)]
                    [(one-line) (string-append rgb "   " expr)]
                    [(ty) (+ y3 8)]
                    [(lh) (font-line-height 'mono-small)])
        (if (<= (text-width one-line 'mono-small) w)
            (begin (draw-text x ty one-line 'text-faint 'mono-small)
                   (+ ty lh))
            (begin
              (draw-text x ty (fit-text rgb w 'mono-small)
                         'text-faint 'mono-small)
              (draw-text x (+ ty lh 4) (fit-text expr w 'mono-small)
                         'text-faint 'mono-small)
              (+ ty lh 4 lh)))))))

;;; ------------------------------------------------------------------
;;; layout & fonts tabs

;; A slider for a pixel size; label-w is from settings-label-width.
;; Chrome heights can't go below what the fonts need (chrome-min-height),
;; so their sliders start there and show the height in effect.
;; Returns the y below the row.
(define (settings-slider-row ui x y w label-w label key lo hi)
  (let* ([lo (max lo (chrome-min-height key))]
         [hi (max hi (+ lo 10))]
         [v (max (theme key) lo)])
    (settings-row x y w label-w label 24 120 (settings-readout-width)
      (lambda (sx sy sw)
        (ui-slider ui (list 'metric key) sx sy sw v lo hi
                   (lambda (nv)
                     (let ([nv (exact (round nv))])
                       (unless (= nv (theme key)) (settings-set! key nv)))))
        (draw-text (+ sx sw 18) (settings-text-y sy 24 'ui) (format "~a px" v)
                   'text-dim 'ui)))))

(define (draw-style-card ui x y w h style selected?)
  (let ([name (window-style-name style)]
        [label-h (settings-card-label-height 28)]
        [r 10])
    (call-with-round-clip x y w h r
      (lambda ()
        (fill-desktop-pattern x y w h (theme 'desktop-pattern)
                              'desktop-top 'desktop-bottom)
        (draw-window-style-preview name x y w (- h label-h) 0.5)
        (fill-rect x (- (+ y h) label-h) w label-h 'panel)
        (draw-text (+ x 12) (+ (- (+ y h) label-h)
                               (/ (- label-h (font-line-height 'ui)) 2))
                   (window-style-title style) 'panel-text 'ui)))
    (stroke-round-rect x y w h r
                       (cond [selected? 'accent]
                             [(ui-hover? ui x y w h) 'text-faint]
                             [else 'window-border])
                       (if selected? 2 1))))

(define (settings-draw-layout ui x y w)
  (let* ([y0 (settings-heading x y w "Layout"
               "Window style, and the sizes of the chrome and the panels.")]
         [cw (settings-card-width (map window-style-title *window-styles*))]
         [ch (+ 88 (settings-card-label-height 28))]
         [label-w (settings-label-width (map cadr settings-metrics))]
         [ry (+ 14 (settings-card-grid *window-styles* x y0 w cw ch
                     (lambda (style cx cy)
                       (let ([name (window-style-name style)])
                         (draw-style-card ui cx cy cw ch style
                                          (eq? name (theme 'window-style)))
                         (ui-area ui (list 'style name) cx cy cw ch
                                  (lambda ()
                                    (settings-set! 'window-style name)))))))])
    (fold-left (lambda (ry m)
                 (+ 20 (settings-slider-row ui x ry w label-w (cadr m) (car m)
                                            (caddr m) (cadddr m))))
               ry settings-metrics)))

(define (settings-draw-fonts ui x y w)
  (let* ([ry (+ 6 (settings-heading x y w "Fonts" "Interface text and code."))]
         [label-w (settings-label-width '("Interface typeface"
                                           "Interface size" "Code size"))]
         [seg-h (max 30 (round (+ (font-line-height 'ui) 15)))]
         [choices '(("Sans" . "sans-serif") ("Serif" . "serif")
                    ("Mono" . "monospace"))]
         [seg-w (* 3 (+ 34 (widest-text (map car choices) 'ui)))]
         [ry (+ 22 (settings-row x ry w label-w "Interface typeface" seg-h seg-w
                                 (settings-readout-width)
                                 ;; it may use the readout column too
                                 (lambda (sx sy sw)
                                   (ui-segmented ui 'ui-font sx sy
                                     (min (max 330 seg-w)
                                          (+ sw (settings-readout-width)))
                                     seg-h choices
                                     (theme 'ui-font)
                                     (lambda (v)
                                       (settings-set! 'ui-font v))))))]
         [ry (+ 22 (settings-slider-row ui x ry w label-w "Interface size"
                                        'ui-font-size 9 36))]
         [py (+ 28 (settings-slider-row ui x ry w label-w "Code size"
                                        'mono-font-size 9 44))]
         [cw (font-char-width 'mono)] [lh (repl-line-height)]
         [code-y (round (+ 14 (* 2 (font-line-height 'ui)) 8 22))]
         [ph (max 190 (+ code-y (* 3 lh) 14))])
    (define (code-line i text)
      (let ([colors (scheme-colors text 3)])
        (vector-fill-range! colors 0 3 'accent)
        (draw-repl-line (+ x 18) (+ py code-y (* i lh))
                        text colors 999 0 1 cw lh)))
    (fill-round-rect x py w ph 8 'bg-0)
    (stroke-round-rect x py w ph 8 'window-border)
    (call-with-clip x py w ph
      (lambda ()
        (draw-text (+ x 18) (+ py 14)
                   "The quick brown fox jumps over the lazy dog." 'text 'ui)
        (draw-text (+ x 18) (+ py 14 (font-line-height 'ui) 8)
                   "Window titles and menus use the bold weight."
                   'text-dim 'ui-bold)
        (code-line 0 "λ> (define (square x) (* x x))  ; code")
        (code-line 1 "λ> (map square '(1 2 3))")
        (draw-text (+ x 18) (+ py code-y (* 2 lh)) "(1 4 9)" 'teal 'mono)))
    (+ py ph)))

;;; ------------------------------------------------------------------
;;; display tab

(define (settings-draw-display ui x y w)
  (let* ([ry (+ 6 (settings-heading x y w "Display"
                    "How big everything is drawn, and full screen."))]
         [label-w (settings-label-width '("Scale" "Full screen"))]
         [seg-h (max 30 (round (+ (font-line-height 'ui) 15)))]
         [segmented-w (lambda (choices)
                        (* (length choices)
                           (+ 34 (widest-text (map car choices) 'ui))))]
         [scale-w (segmented-w settings-scale-choices)]
         [stack? (< (- w label-w) scale-w)]   ; both rows stack, or neither
         [ry (settings-row x ry w label-w "Scale" seg-h scale-w 0
                           (lambda (sx sy sw)
                             (ui-segmented ui 'scale sx sy (min scale-w sw)
                                           seg-h settings-scale-choices
                                           (theme 'scale)
                                           (lambda (v)
                                             (settings-set! 'scale v)))))]
         [ry (+ 22 (draw-paragraph x (+ ry 10) w
                                   (format "Auto follows your desktop, which \
                                            asks for ~a%. Everything is drawn \
                                            from outlines, so text stays sharp \
                                            at any scale; 1-pixel lines are \
                                            crispest at whole multiples like \
                                            200%."
                                           (system-scale-percent))
                                   'text-faint 'ui))]
         [fs-choices '(("Off" . #f) ("On" . #t))])
    (settings-row x ry w label-w "Full screen" seg-h
                  (if stack? (+ w 1) (segmented-w fs-choices)) 0
                  (lambda (sx sy sw)
                    (ui-segmented ui 'fullscreen sx sy
                                  (min (max 160 (segmented-w fs-choices)) sw)
                                  seg-h fs-choices
                                  (fullscreen?)
                                  (lambda (v) (set-fullscreen! v)))))))

;;; ------------------------------------------------------------------ the app

(define (settings-draw-sidebar ui st sw h)
  (fill-rect 0 0 sw h 'bg-0)
  (draw-line (- sw 0.5) 0 (- sw 0.5) h 'window-border)
  (draw-text 20 18 "Settings" 'text 'heading)
  (let* ([tab-h (settings-tab-height)]
         [tabs-bottom
          (let loop ([ts settings-tabs]
                     [ty (round (+ 18 (font-line-height 'heading) 27))])
            (if (null? ts)
                (- ty 6)
                (let* ([t (caar ts)] [selected? (eq? t (settings-st-tab st))]
                       [hover? (ui-hover? ui 10 ty (- sw 20) tab-h)])
                  (when (or selected? hover?)
                    (fill-round-rect 10 ty (- sw 20) tab-h 7
                                     (if selected? (alpha 'accent 0.22) 'bg-2)))
                  (when selected?
                    (fill-round-rect 10 (+ ty (/ tab-h 2) -8) 3 16 1.5 'accent))
                  (draw-text 24 (+ ty (/ (- tab-h (font-line-height 'ui)) 2))
                             (cdar ts)
                             (if selected? 'text 'text-dim)
                             (if selected? 'ui-bold 'ui))
                  (ui-area ui (list 'tab t) 10 ty (- sw 20) tab-h
                           (lambda ()
                             (settings-st-tab-set! st t)
                             (settings-st-hex-set! st #f)
                             (settings-st-pane-scroll-set! st 0)))
                  (loop (cdr ts) (+ ty tab-h 6)))))]
         ;; The hint at the bottom, wrapped to the sidebar, and the button
         ;; above it; both move down rather than cover the tabs.
         [step (round (+ (font-line-height 'ui-small) 3))]
         [hint (wrap-text "Changes apply live and are saved automatically."
                          (- sw 26) 'ui-small)]
         [bh (settings-button-height)]
         [by (max (- h 20 (* step (length hint)) 12 bh) (+ tabs-bottom 16))]
         [hint-y (+ by bh 12)])
    (ui-button ui 'reset-all 14 by (- sw 28) bh "Reset to defaults"
               (lambda () (reset-theme!) (save-theme-soon!)))
    (for-each (lambda (line i)
                (draw-text 18 (+ hint-y (* i step)) line 'text-faint 'ui-small))
              hint (iota (length hint)))))

;; A thin scrollbar at x for the pane; drag the thumb (or click the track).
(define (settings-draw-scrollbar ui st x h content-h max-scroll)
  (let* ([th (max 30 (* h (/ h content-h)))]
         [ty (* (- h th) (/ (settings-st-pane-scroll st) max-scroll))]
         [hot? (or (ui-active? ui 'pane-scrollbar) (ui-hover? ui x 0 10 h))])
    (fill-round-rect (+ x 2) (+ ty 2) 5 (- th 4) 2.5
                     (alpha 'text (if hot? 0.4 0.2)))
    (ui-area ui 'pane-scrollbar x 0 10 h #f
             (lambda (mx my)
               (settings-st-pane-scroll-set!
                st (clamp (round (* max-scroll (/ (- my (/ th 2)) (- h th))))
                          0 max-scroll))))))

(define (settings-draw ui st w h)
  (ui-begin! ui)
  (fill-rect 0 0 w h 'bg-1)
  (let* ([sw (settings-sidebar-width)]
         [x (+ sw 28)] [cw (- w x 28)]
         [scroll (settings-st-pane-scroll st)]
         [y (- 20 scroll)])
    (settings-draw-sidebar ui st sw h)
    (when (call-with-clip (+ sw 1) 0 (- w sw 1) h
            (lambda ()
              (let* ([bottom
                      (case (settings-st-tab st)
                        [(theme) (settings-draw-theme ui x y cw)]
                        [(colors) (settings-draw-colors ui st x y cw (- h 38))]
                        [(layout) (settings-draw-layout ui x y cw)]
                        [(fonts) (settings-draw-fonts ui x y cw)]
                        [(display) (settings-draw-display ui x y cw)]
                        ;; a tab name from older code, open across a reload
                        [else (settings-draw-theme ui x y cw)])]
                     [content-h (+ (- bottom y) 20 18)]
                     [max-scroll (max 0 (- content-h h))])
                (cond
                  [(> scroll max-scroll)
                   (settings-st-pane-scroll-set! st max-scroll)
                   #t]
                  [else
                   (when (> max-scroll 0)
                     (settings-draw-scrollbar ui st (- w 10) h content-h
                                              max-scroll))
                   #f]))))
      ;; the wheel went past the end, which only drawing finds: drawn
      ;; again where the pane stops, not shown for a frame past it
      (settings-draw ui st w h))))

(define (parse-hex s)
  (case (string-length s)
    [(6) (string->number s 16)]
    [(3) (string->number
          (list->string
           (apply append (map (lambda (c) (list c c)) (string->list s))))
          16)]
    [else #f]))

(define (settings-key st win key mods)
  (let ([hex (settings-st-hex st)])
    (cond
      [hex
       (case key
         [(return)
          (let ([v (parse-hex hex)])
            (when v (settings-set! (settings-st-key st) v)))
          (settings-st-hex-set! st #f)]
         [(escape) (settings-st-hex-set! st #f)]
         [(backspace)
          (when (> (string-length hex) 0)
            (settings-st-hex-set!
             st (substring hex 0 (- (string-length hex) 1))))])]
      [(and (eq? (settings-st-tab st) 'colors) (memq key '(up down)))
       (let* ([keys (filter symbol? (settings-color-rows))]
              [i (or (list-index (lambda (k) (eq? k (settings-st-key st)))
                                 keys)
                     0)])
         (settings-st-key-set!
          st (list-ref keys (clamp (+ i (if (eq? key 'up) -1 1))
                                   0 (- (length keys) 1)))))])
    (invalidate! win)))

(define (settings-text st win str)
  (let ([hex (settings-st-hex st)])
    (when hex
      (let ([s (string-append
                hex
                (list->string
                 (filter (lambda (c)
                           (or (char<=? #\0 c #\9) (char<=? #\a c #\f)))
                         (string->list (string-downcase str)))))])
        (settings-st-hex-set! st (if (> (string-length s) 6)
                                     (substring s 0 6)
                                     s))
        (invalidate! win)))))

(define-app (settings) "Settings"
  (let ([existing (find-window 'settings)])
    (if (and existing (settings-st? (window-ref existing 'state #f)))
        (begin (focus-window! existing) existing)
        (let ([ui (make-ui)] [st (make-settings-st 'theme 'accent 0 #f #f 0)])
          ;; a window opened by code from before a (reload!) that changed
          ;; settings-st can't draw any more: replace it
          (when existing (close-window! existing))
          (open-window "Settings"
            'size (cons (settings-natural-width) (settings-natural-height))
            'min-size '(640 . 420)
            'ui ui
            'state st
            'draw (lambda (win w h) (settings-draw ui st w h))
            'key (lambda (win key mods) (settings-key st win key mods))
            'text (lambda (win str) (settings-text st win str))
            'wheel (lambda (win dx dy)
                     ;; over the color list it scrolls the list, elsewhere the
                     ;; pane
                     (let ([r (and (eq? (settings-st-tab st) 'colors)
                                   (ui-find ui 'color-list))])
                       (if (and r (point-in? (ui-state-mx ui) (ui-state-my ui)
                                             (ui-region-x r) (ui-region-y r)
                                             (ui-region-w r) (ui-region-h r)))
                           (settings-st-scroll-set!
                            st (max 0 (- (settings-st-scroll st)
                                         (* (settings-color-row-height) dy))))
                           (settings-st-pane-scroll-set!
                            st (max 0 (- (settings-st-pane-scroll st)
                                         (* 3 (round (font-line-height 'ui))
                                            dy))))))
                     (invalidate! win))
            'mouse-down (lambda (win x y button clicks)
                          (settings-st-hex-set! st #f)
                          (when (eq? button 'left) (ui-mouse-down ui x y)))
            'mouse-move (lambda (win x y) (ui-mouse-move ui x y))
            'mouse-up (lambda (win x y button) (ui-mouse-up ui)))))))
