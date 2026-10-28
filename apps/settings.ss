;;; apps/settings.ss — customise the look of the world.
;;;
;;; Every setting is a theme key (see src/theme.ss), so anything done here
;;; can also be done from the REPL with set-theme!. Changes apply live and
;;; are saved to *theme-file* (~/.schevana-theme.ss), which is loaded at boot.

(define settings-tabs
  '((theme . "Theme") (colours . "Colours") (layout . "Layout") (fonts . "Fonts")))

(define settings-colour-groups
  '(("BASE" bg-0 bg-1 bg-2 bg-3 text text-dim text-faint)
    ("ACCENTS" accent accent-2 red orange yellow green cyan teal magenta selection)
    ("DESKTOP" desktop-top desktop-bottom watermark)
    ("PANELS" panel panel-text panel-dim panel-hover)
    ("WINDOWS" window-bg window-border window-border-focus title-bg title-bg-focus
               title-text title-text-dim button-close button-min button-max button-idle)))

;; (key label min max)
(define settings-metrics
  '((corner-radius "Corner radius" 0 20)
    (title-height "Title bar height" 22 44)
    (shadow "Window shadow" 0 32)
    (topbar-height "Top bar height" 22 40)
    (taskbar-height "Taskbar height" 28 56)))

(define settings-sidebar-width 168)

(define-record-type settings-st
  (nongenerative schevana-settings)
  (fields (mutable tab)
          (mutable key)       ; colour being edited
          (mutable scroll)    ; colour list scroll offset
          (mutable hex)       ; hex digits being typed, or #f
          (mutable hsl)))     ; #(key rgb h s l): keeps hue stable at zero saturation

(define (settings-set! key value)
  (if (eq? key 'accent)
      (set-accent! value)   ; keeps the keys derived from the accent in step
      (set-theme! key value))
  (save-theme-soon!))

(define (settings-heading x y title subtitle)
  (draw-text x y title 'text 'heading)
  (draw-text x (+ y (font-line-height 'heading) 6) subtitle 'text-faint 'ui))

(define (settings-colour-rows)
  (apply append settings-colour-groups))

;; Fill a track with n small steps of (color-at t), t in [0, 1].
(define (paint-steps x y w h color-at)
  (let ([n 48])
    (do ([i 0 (+ i 1)]) ((= i n))
      (fill-rect (+ x (* i (/ w n))) y (+ 1 (/ w n)) h (color-at (/ (+ i 0.5) n))))))

;;; ------------------------------------------------------------------ theme tab

(define (draw-preset-card x y w h name selected? hover?)
  (let* ([colors (preset-colors name)]
         [c (lambda (k) (plist-ref colors k #x808080))]
         [label-h 30] [r 10])
    (call-with-round-clip x y w h r
      (lambda ()
        (fill-gradient x y w h (c 'desktop-top) (c 'desktop-bottom))
        (let ([wx (+ x 18)] [wy (+ y 14)] [ww (- w 36)] [wh (- h label-h 22)])
          (fill-round-rect wx (+ wy 3) ww wh 5 (alpha #x000000 0.25))
          (fill-round-rect wx wy ww wh 5 (c 'window-bg))
          (call-with-round-clip wx wy ww wh 5
            (lambda () (fill-rect wx wy ww 12 (c 'title-bg-focus))))
          (for-each (lambda (i k) (fill-circle (+ wx 8 (* i 8)) (+ wy 6) 2.5 (c k)))
                    '(0 1 2) '(button-close button-min button-max))
          (for-each (lambda (i k frac)
                      (fill-round-rect (+ wx 8 (if (= i 2) 10 0)) (+ wy 20 (* i 9))
                                       (* frac (- ww 16)) 4 2 (c k)))
                    '(0 1 2 3) '(accent-2 accent green text-dim) '(0.45 0.7 0.5 0.35)))
        (fill-rect x (- (+ y h) label-h) w label-h (c 'panel))
        (draw-text (+ x 12) (+ (- (+ y h) label-h) (/ (- label-h (font-line-height 'ui)) 2))
                   (preset-title name) (c 'panel-text) 'ui)))
    (stroke-round-rect x y w h r (cond [selected? 'accent] [hover? 'text-faint] [else 'window-border])
                       (if selected? 2 1))))

(define (settings-draw-theme ui x y w h)
  (settings-heading x y "Theme" "Pick a starting point; fine-tune it under Colours.")
  (let* ([gap 12] [cw 180] [ch 120]
         [cols (max 1 (quotient (+ w gap) (+ cw gap)))]
         [y0 (+ y 66)])
    (let loop ([ps theme-presets] [i 0])
      (unless (null? ps)
        (let* ([name (caar ps)]
               [cx (+ x (* (mod i cols) (+ cw gap)))]
               [cy (+ y0 (* (quotient i cols) (+ ch gap)))])
          (draw-preset-card cx cy cw ch name (eq? name (theme 'preset)) (ui-hover? ui cx cy cw ch))
          (ui-area ui (list 'preset name) cx cy cw ch
                   (lambda () (apply-theme-preset! name) (save-theme-soon!)))
          (loop (cdr ps) (+ i 1)))))
    (let* ([rows (quotient (+ (length theme-presets) cols -1) cols)]
           [ay (+ y0 (* rows (+ ch gap)) 10)]
           [palette (preset-colors (theme 'preset))])
      (draw-text x ay "Accent colour" 'text 'ui-bold)
      (let loop ([ks '(accent cyan teal green yellow orange red magenta accent-2)] [sx x])
        (unless (null? ks)
          (let ([color (plist-ref palette (car ks) #x808080)])
            (fill-circle (+ sx 14) (+ ay 42) 11 color)
            (when (eqv? color (theme 'accent)) (stroke-circle (+ sx 14) (+ ay 42) 15 'text 2))
            (ui-area ui (list 'accent (car ks)) sx (+ ay 28) 28 28
                     (lambda () (set-accent! color) (save-theme-soon!)))
            (loop (cdr ks) (+ sx 36))))))))

;;; ------------------------------------------------------------------ colours tab

(define (settings-draw-colours ui st x y w h)
  (settings-heading x y "Colours" "Pick a colour to edit it. Every window updates as you drag.")
  (let* ([ly (+ y 66)] [lw 220] [lh (- (+ y h) ly)]
         [row-h 26]
         [rows (settings-colour-rows)]
         [total (+ 12 (* row-h (length rows)))]
         [scroll (clamp (settings-st-scroll st) 0 (max 0 (- total lh)))])
    (settings-st-scroll-set! st scroll)
    (fill-round-rect x ly lw lh 8 'bg-0)
    (call-with-clip x (+ ly 4) lw (- lh 8)
      (lambda ()
        (let loop ([rs rows] [ry (- (+ ly 6) scroll)])
          (unless (or (null? rs) (> ry (+ ly lh)))
            (let ([r (car rs)])
              (when (> (+ ry row-h) ly)
                (if (string? r)
                    (draw-text (+ x 12) (+ ry 9) r 'text-faint 'ui-small)
                    (let ([selected? (eq? r (settings-st-key st))]
                          [top (max ry ly)] [bottom (min (+ ry row-h) (+ ly lh))])
                      (cond
                        [selected? (fill-round-rect (+ x 4) ry (- lw 8) row-h 5 (alpha 'accent 0.22))]
                        [(ui-hover? ui x top lw (- bottom top)) (fill-round-rect (+ x 4) ry (- lw 8) row-h 5 'bg-2)])
                      (fill-round-rect (+ x 12) (+ ry 5) 16 16 4 r)
                      (stroke-round-rect (+ x 12) (+ ry 5) 16 16 4 (alpha 'text 0.25))
                      (draw-text (+ x 38) (+ ry (/ (- row-h (font-line-height 'ui)) 2))
                                 (symbol->string r) (if selected? 'text 'text-dim) 'ui)
                      (when (> bottom top)
                        (ui-area ui (list 'colour r) x top lw (- bottom top)
                                 (lambda ()
                                   (settings-st-key-set! st r)
                                   (settings-st-hex-set! st #f)))))))
              (loop (cdr rs) (+ ry row-h)))))))
    (settings-draw-colour-editor ui st (+ x lw 28) ly (- w lw 28) lh)))

(define (settings-draw-colour-editor ui st x y w h)
  (let* ([key (settings-st-key st)]
         [value (theme key)]
         [rgbv (color->fixnum value)]
         [hsl (let ([c (settings-st-hsl st)])
                (if (and c (eq? (vector-ref c 0) key) (eqv? (vector-ref c 1) rgbv))
                    c
                    (let-values ([(hh ss ll) (color->hsl value)])
                      (let ([v (vector key rgbv hh ss ll)])
                        (settings-st-hsl-set! st v)
                        v))))]
         [sx (+ x 96)] [sw (max 60 (- w 96 64))])
    (define (update! i v)
      (vector-set! hsl i v)
      (let ([c (hsl->color (vector-ref hsl 2) (vector-ref hsl 3) (vector-ref hsl 4))])
        (vector-set! hsl 1 c)
        (settings-set! key c)))
    (define (slider-row i label id v lo hi readout paint on-change)
      (let ([ry (+ y 104 (* i 42))])
        (draw-text x (+ ry 4) label 'text-dim 'ui)
        (ui-slider ui id sx ry sw v lo hi on-change paint)
        (draw-text (+ sx sw 16) (+ ry 4) readout 'text-dim 'ui)))
    ;; swatch, name, hex field, reset
    (fill-round-rect x y 78 78 12 value)
    (stroke-round-rect x y 78 78 12 (alpha 'text 0.25))
    (draw-text sx (+ y 2) (symbol->string key) 'text 'heading)
    (let* ([by (+ y 44)] [bw 116] [bh 30] [editing (settings-st-hex st)])
      (fill-round-rect sx by bw bh 6 'bg-0)
      (stroke-round-rect sx by bw bh 6 (if editing 'accent 'window-border))
      (let ([tx (draw-text (+ sx 10) (+ by (/ (- bh (font-line-height 'mono)) 2))
                           (string-append "#" (or editing (substring (hex-color rgbv) 2 8)))
                           (if editing 'text 'text-dim) 'mono)])
        (when editing (fill-rect (+ tx 1) (+ by 7) 2 (- bh 14) 'accent)))
      (ui-area ui 'hex sx by bw bh (lambda () (settings-st-hex-set! st "")))
      (ui-button ui 'reset-colour (+ sx bw 10) by 140 bh "Reset to preset"
                 (lambda ()
                   (settings-set! key (plist-ref (preset-colors (theme 'preset)) key (theme-default key))))))
    (let ([hh (vector-ref hsl 2)] [ss (vector-ref hsl 3)] [ll (vector-ref hsl 4)])
      (slider-row 0 "Hue" 'hue hh 0 360 (format "~a°" (exact (round hh)))
                  (lambda (tx ty tw th)
                    (paint-steps tx ty tw th (lambda (t) (hsl->color (* 360 t) (max ss 0.4) (clamp ll 0.35 0.65)))))
                  (lambda (v) (update! 2 v)))
      (slider-row 1 "Saturation" 'saturation ss 0 1 (format "~a%" (exact (round (* 100 ss))))
                  (lambda (tx ty tw th) (paint-steps tx ty tw th (lambda (t) (hsl->color hh t ll))))
                  (lambda (v) (update! 3 v)))
      (slider-row 2 "Lightness" 'lightness ll 0 1 (format "~a%" (exact (round (* 100 ll))))
                  (lambda (tx ty tw th) (paint-steps tx ty tw th (lambda (t) (hsl->color hh ss t))))
                  (lambda (v) (update! 4 v))))
    (let-values ([(r g b a) (color-rgba rgbv)])
      (draw-text x (+ y 238) (format "rgb(~a, ~a, ~a)   (theme '~a)"
                                     (exact (round (* r 255))) (exact (round (* g 255)))
                                     (exact (round (* b 255))) key)
                 'text-faint 'mono-small))))

;;; ------------------------------------------------------------------ layout & fonts tabs

(define (settings-slider-row ui x y w label key lo hi)
  (let ([sw (- w 240)] [v (theme key)])
    (draw-text x (+ y 4) label 'text-dim 'ui)
    (ui-slider ui (list 'metric key) (+ x 180) y sw v lo hi
               (lambda (nv)
                 (let ([nv (exact (round nv))])
                   (unless (= nv (theme key)) (settings-set! key nv)))))
    (draw-text (+ x 180 sw 18) (+ y 4) (format "~a px" v) 'text-dim 'ui)))

(define (settings-draw-layout ui x y w h)
  (settings-heading x y "Layout" "Sizes of the window chrome and the panels.")
  (let loop ([ms settings-metrics] [ry (+ y 72)])
    (unless (null? ms)
      (let ([m (car ms)])
        (settings-slider-row ui x ry w (cadr m) (car m) (caddr m) (cadddr m))
        (loop (cdr ms) (+ ry 46))))))

(define (settings-draw-fonts ui x y w h)
  (settings-heading x y "Fonts" "Interface text and code.")
  (let ([ry (+ y 72)])
    (draw-text x (+ ry 7) "Interface typeface" 'text-dim 'ui)
    (ui-segmented ui 'ui-font (+ x 180) ry (min 330 (- w 240)) 30
                  '(("Sans" . "sans-serif") ("Serif" . "serif") ("Mono" . "monospace"))
                  (theme 'ui-font)
                  (lambda (v) (settings-set! 'ui-font v)))
    (settings-slider-row ui x (+ ry 52) w "Interface size" 'ui-font-size 9 18)
    (settings-slider-row ui x (+ ry 98) w "Code size" 'mono-font-size 9 22)
    (let* ([py (+ ry 150)] [ph (min 190 (- (+ y h) py))]
           [cw (font-char-width 'mono)] [lh (+ 3 (font-line-height 'mono))])
      (define (code-line i text)
        (let ([colors (scheme-colors text 3)])
          (vector-fill-range! colors 0 3 'accent)
          (draw-repl-line (+ x 18) (+ py 74 (* i lh)) text colors 999 0 1 cw lh)))
      (fill-round-rect x py w ph 8 'bg-0)
      (stroke-round-rect x py w ph 8 'window-border)
      (draw-text (+ x 18) (+ py 14) "The quick brown fox jumps over the lazy dog." 'text 'ui)
      (draw-text (+ x 18) (+ py 14 (font-line-height 'ui) 8) "Window titles and menus use the bold weight."
                 'text-dim 'ui-bold)
      (code-line 0 "λ> (define (square x) (* x x))  ; code")
      (code-line 1 "λ> (map square '(1 2 3))")
      (draw-text (+ x 18) (+ py 74 (* 2 lh)) "(1 4 9)" 'teal 'mono))))

;;; ------------------------------------------------------------------ the app

(define (settings-draw-sidebar ui st w h)
  (let ([sw settings-sidebar-width])
    (fill-rect 0 0 sw h 'bg-0)
    (draw-line (- sw 0.5) 0 (- sw 0.5) h 'window-border)
    (draw-text 20 18 "Settings" 'text 'heading)
    (let loop ([ts settings-tabs] [ty 66])
      (unless (null? ts)
        (let* ([t (caar ts)] [selected? (eq? t (settings-st-tab st))]
               [hover? (ui-hover? ui 10 ty (- sw 20) 32)])
          (when (or selected? hover?)
            (fill-round-rect 10 ty (- sw 20) 32 7 (if selected? (alpha 'accent 0.22) 'bg-2)))
          (when selected? (fill-round-rect 10 (+ ty 8) 3 16 1.5 'accent))
          (draw-text 24 (+ ty (/ (- 32 (font-line-height 'ui)) 2)) (cdar ts)
                     (if selected? 'text 'text-dim) (if selected? 'ui-bold 'ui))
          (ui-area ui (list 'tab t) 10 ty (- sw 20) 32
                   (lambda () (settings-st-tab-set! st t) (settings-st-hex-set! st #f)))
          (loop (cdr ts) (+ ty 38)))))
    (ui-button ui 'reset-all 14 (- h 94) (- sw 28) 30 "Reset to defaults"
               (lambda () (reset-theme!) (save-theme-soon!)))
    (draw-text 18 (- h 52) "Changes apply live and" 'text-faint 'ui-small)
    (draw-text 18 (- h 36) "are saved automatically." 'text-faint 'ui-small)))

(define (settings-draw ui st w h)
  (ui-begin! ui)
  (fill-rect 0 0 w h 'bg-1)
  (settings-draw-sidebar ui st w h)
  (let* ([x (+ settings-sidebar-width 28)] [y 20]
         [cw (- w x 28)] [ch (- h y 18)])
    (call-with-clip (+ settings-sidebar-width 1) 0 (- w settings-sidebar-width 1) h
      (lambda ()
        (case (settings-st-tab st)
          [(theme) (settings-draw-theme ui x y cw ch)]
          [(colours) (settings-draw-colours ui st x y cw ch)]
          [(layout) (settings-draw-layout ui x y cw ch)]
          [(fonts) (settings-draw-fonts ui x y cw ch)])))))

(define (parse-hex s)
  (case (string-length s)
    [(6) (string->number s 16)]
    [(3) (string->number (list->string (apply append (map (lambda (c) (list c c)) (string->list s)))) 16)]
    [else #f]))

(define (settings-key st win key mods)
  (let ([hex (settings-st-hex st)])
    (cond
      [hex
       (case key
         [(return)
          (let ([v (parse-hex hex)]) (when v (settings-set! (settings-st-key st) v)))
          (settings-st-hex-set! st #f)]
         [(escape) (settings-st-hex-set! st #f)]
         [(backspace)
          (when (> (string-length hex) 0)
            (settings-st-hex-set! st (substring hex 0 (- (string-length hex) 1))))])]
      [(and (eq? (settings-st-tab st) 'colours) (memq key '(up down)))
       (let* ([keys (filter symbol? (settings-colour-rows))]
              [i (or (list-index (lambda (k) (eq? k (settings-st-key st))) keys) 0)])
         (settings-st-key-set! st (list-ref keys (clamp (+ i (if (eq? key 'up) -1 1)) 0 (- (length keys) 1)))))])
    (invalidate! win)))

(define (settings-text st win str)
  (let ([hex (settings-st-hex st)])
    (when hex
      (let ([s (string-append hex (list->string (filter (lambda (c) (or (char<=? #\0 c #\9) (char<=? #\a c #\f)))
                                                        (string->list (string-downcase str)))))])
        (settings-st-hex-set! st (if (> (string-length s) 6) (substring s 0 6) s))
        (invalidate! win)))))

(define-app (settings) "Settings"
  (let ([existing (find-window 'settings)])
    (if existing
        (begin (focus-window! existing) existing)
        (let ([ui (make-ui)] [st (make-settings-st 'theme 'accent 0 #f #f)])
          (open-window "Settings"
            'size '(840 . 540)
            'min-size '(640 . 420)
            'ui ui
            'draw (lambda (win w h) (settings-draw ui st w h))
            'key (lambda (win key mods) (settings-key st win key mods))
            'text (lambda (win str) (settings-text st win str))
            'wheel (lambda (win dx dy)
                     (when (eq? (settings-st-tab st) 'colours)
                       (settings-st-scroll-set! st (max 0 (- (settings-st-scroll st) (* 26 dy))))
                       (invalidate! win)))
            'mouse-down (lambda (win x y button clicks)
                          (settings-st-hex-set! st #f)
                          (when (eq? button 'left) (ui-mouse-down ui x y)))
            'mouse-move (lambda (win x y) (ui-mouse-move ui x y))
            'mouse-up (lambda (win x y button) (ui-mouse-up ui)))))))
