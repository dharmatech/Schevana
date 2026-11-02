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

(define genera-border 2)
(define genera-margin 4)

(define (genera-label-height)
  (exact (max 14 (- (title-height) 16)
              (round (+ (font-line-height 'ui-italic) 2)))))

(define (genera-insets win)
  (let ([m (+ genera-border genera-margin)])
    (values m m m (+ genera-border (genera-label-height) genera-margin))))

;; The label strip is the window's title area. The corners and the bottom
;; edge stay free for resizing.
(define (genera-regions win)
  (let ([w (window-w win)] [h (window-h win)] [lh (genera-label-height)])
    (list (list 'title 14 (- h genera-border lh) (- w 28) (- lh 4)))))

(define (genera-draw-under win x y w h focused? hover pressed)
  (let ([lh (genera-label-height)] [b genera-border])
    (fill-rect x y w h 'text)
    (fill-rect (+ x b) (+ y b) (- w b b) (- h b b) (window-bg win))
    (call-with-clip x y w h   ; even "…" may not fit a narrow window
      (lambda ()
        (draw-text (+ x b genera-margin)
                   (+ y (- h b lh)
                      (floor (/ (- lh (font-line-height 'ui-italic)) 2)))
                   (fit-text (window-title win)
                             (- w (* 2 (+ b genera-margin)))
                             'ui-italic)
                   'text 'ui-italic)))))

;; Its palette on a plain white screen.
(define-style genera "Genera"
  'palette 'genera
  'wallpaper 'flat
  'frame-insets 'genera-insets
  'frame-regions 'genera-regions
  'frame-under 'genera-draw-under)
