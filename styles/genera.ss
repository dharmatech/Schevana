;;; styles/genera.ss — the Genera window style.
;;; Symbolics Genera, the Lisp Machine system of the 1980s. Programming
;;; the User Interface (Genera 7, 1986; the same in Genera 8) puts a pane's
;;; label "in the lower, left-hand corner of the pane (directly inside the
;;; border)", inside a 4-pixel white margin. Real Genera 8 screens, measured
;;; in captures from 1998, 2004 and 2024, draw that border 2 pixels thick
;;; (the manual's 1 is a bare pane's default) and the label in italic, in
;;; normal video. No title bar, no buttons and no mark of the selected
;;; window: Genera never had them (the buttons in some pictures belong to
;;; the X or Mac window around the whole Genera screen). The label strip is
;;; what you drag here, and right-clicking it gives the window menu; Genera
;;; did both from its System menu instead. The research is in
;;; .work/reports/ ("Symbolics Genera window appearance").
;;;
;;; The screen is Genera's too: no top bar, and at the bottom the who line,
;;; what the mouse buttons do in reverse video over a line of fields in
;;; character columns. A window not selected is grayed over; buttons are
;;; command words, boxed under the pointer and reversed while pressed.
;;; Its numbers are settings, so a style derived from it changes them (the
;;; CADR, styles/cadr.ss).

;; The label strip: the label's line and 2 (the CADR's label spacing).
;; Its top in the window: under the white margin, directly inside the
;; border (Genera), or under the content (the CADR, its margin below).
(define (genera-label-height)
  (exact (max 14 (round (+ (font-line-height (theme 'genera-label-font))
                           2)))))

(define (genera-label-y win)
  (- (window-h win) (theme 'genera-border) (genera-label-height)
     (if (eq? (theme 'genera-label-under) 'content) (theme 'genera-margin) 0)))

(define (genera-insets win)
  (let* ([b (theme 'genera-border)] [m (theme 'genera-margin)] [s (+ b m)])
    (values s s s (+ b (genera-label-height) m))))

;; The label strip is the window's title area. The corners and the bottom
;; edge stay free for resizing.
(define (genera-regions win)
  (list (list 'title 14 (genera-label-y win) (- (window-w win) 28)
              (- (genera-label-height) 4))))

(define (genera-draw-under win x y w h focused? hover pressed)
  (let ([b (theme 'genera-border)] [m (theme 'genera-margin)]
        [f (theme 'genera-label-font)] [lh (genera-label-height)])
    (px-fill x y w h 'text)   ; on whole device pixels at any scale
    (px-fill (+ x b) (+ y b) (- w b b) (- h b b) (window-bg win))
    (call-with-clip x y w h   ; even "…" may not fit a narrow window
      (lambda ()
        (draw-text (+ x b m)
                   (+ y (genera-label-y win)
                      (if (eq? (theme 'genera-label-align) 'top)
                          0
                          (floor (/ (- lh (font-line-height f)) 2))))
                   (fit-text (window-title win) (- w (* 2 (+ b m))) f)
                   'text f)))))

;; A window not selected is grayed over, as Genera grays one it is not
;; using: a sparse stipple of ink on clear paper (about 6 %, report:47).
(define (genera-draw-over win x y w h focused? hover pressed)
  (unless focused?
    (let-values ([(l t r b) (frame-insets win)])
      (fill-rect (+ x l) (+ y t) (- w l r) (- h t b) 'genera-gray))))

;; A command word: plain until the pointer is on it (or the keyboard), a
;; 1-pixel box then, reversed while pressed or chosen (report:51, :65).
(define (genera-button x y w h flags label glyph)
  (let* ([on? (lambda (f) (memq f flags))]
         [live? (not (on? 'disabled))]
         [rev? (and live? (or (on? 'pressed) (on? 'selected)))]
         [ink (cond [rev? 'window-bg] [live? 'text] [else 'text-faint])]
         [g (ui-glyph-size)])
    (cond [rev? (px-fill x y w h 'text)]
          [(and live? (or (on? 'hover) (on? 'focused)))
           (px-ring x y w h 'text)])
    (if label
        (draw-text-centered x y w h (fit-text label (- w 8) 'ui) ink 'ui)
        (when glyph
          (ui-draw-glyph (+ x (/ (- w g) 2)) (+ y (/ (- h g) 2)) g g
                         (list (cons 'ink ink)) glyph)))))

;;; the who line

;; Each of its two lines: the code font's line and 2.
(define (genera-line) (exact (round (+ (font-line-height 'mono) 2))))

;; Its fields, at character columns as the CADR's (lispm.md 2.1): what
;; the mouse buttons do across the top line; below, the selected window,
;; a field that lists the windows, and the status.
(define (genera-who-line-areas)
  (let ([lh (genera-line)] [y (- (screen-h) (taskbar-height))]
        [cw (font-char-width 'mono)])
    (list (list 'mouse-doc 0 y (screen-w) lh)
          (list 'title cw (+ y lh) (* 24 cw) lh)
          (list 'windows (* 26 cw) (+ y lh) (* 11 cw) lh)
          (list 'status (* 38 cw) (+ y lh) (max 0 (- (screen-w) (* 39 cw)))
                lh))))

(define (genera-draw-who-line)
  (let* ([as (taskbar-areas)] [lh (genera-line)] [w (screen-w)]
         [y (- (screen-h) (taskbar-height))] [cw (font-char-width 'mono)]
         [ty (lambda (y) (+ y (floor (/ (- lh (font-line-height 'mono)) 2))))]
         [field (lambda (what s)
                  (apply (lambda (fx fy fw fh)
                           (draw-text fx (ty fy) (fit-text s fw 'mono) 'text
                                      'mono))
                         (cdr (assq what as))))])
    (fill-rect 0 y w lh 'text)
    (draw-text cw (ty y) (fit-text (mouse-doc-text (mouse-doc)) (- w cw cw)
                                   'mono)
               'bg-1 'mono)
    (fill-rect 0 (+ y lh) w lh 'bg-1)
    (field 'title (if *focus* (window-title *focus*) "No selected window"))
    (field 'windows (let ([n (length *windows*)])
                      (format "~a window~a" n (if (= n 1) "" "s"))))
    (field 'status (string-join (shown-status-strings) "  "))))

;; It shows the mouse documentation and the status (the clock).
(define (genera-who-line-tick)
  (status-tick taskbar-box)
  (mouse-doc-tick taskbar-box))

;; "Mouse-L: Move; Mouse-R: Window menu."
(define (genera-mouse-doc-text doc)
  (if (null? doc)
      ""
      (string-append
       (string-join (map (lambda (b) (format "Mouse-~a: ~a" (car b) (cdr b)))
                         doc)
                    "; ")
       ".")))

;; A window not selected is grayed over with this: a sparse stipple of
;; ink on clear paper; a color, which you may change and save.
(color-default! 'genera-gray (stipple 'text #f "#..." "...." "...." "...."))

;; Its palette on a plain white screen.
(define-style genera "Genera"
  'palette 'genera  'wallpaper 'flat
  'genera-border 2  'genera-margin 4
  'genera-label-font 'ui-italic  'genera-label-align 'center
  'genera-label-under 'margin
  'frame-insets 'genera-insets  'frame-regions 'genera-regions
  'frame-under 'genera-draw-under  'frame-over 'genera-draw-over
  'ui-draw-button 'genera-button
  ;; the screen: no top bar; the who line at the bottom
  'topbar-height 0
  'taskbar-height (theme-rule (* 2 (genera-line)))
  'draw-taskbar 'genera-draw-who-line  'taskbar-areas 'genera-who-line-areas
  'taskbar-tick 'genera-who-line-tick  'mouse-doc-text 'genera-mouse-doc-text
  ;; the feel
  'title-double-click 'none  'bell 'screen  'menu-hover 'outline
  'mouse-doc #t  'scrollbar-side 'left
  'options '(palette accent wallpaper ui-font ui-font-size mono-font
             mono-font-size
             (genera-border "Border" 1 4)
             (genera-margin "White margin" 0 8)
             (genera-label-font "Label font"
              (("Italic" . ui-italic) ("Upright" . ui) ("Code" . mono)))))
