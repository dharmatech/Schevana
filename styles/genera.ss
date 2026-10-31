;;; styles/genera.ss — the Genera window style.
;;; Symbolics Genera, the Lisp Machine system of the 1980s, as its manuals
;;; describe it. Programming the User Interface (Genera 7, 1986): a pane's
;;; default margins are "a 1-pixel-wide border and a margin between the
;;; border and displayed output to the pane of 4 pixels", and its label
;;; "appears ... in the lower, left-hand corner of the pane (directly
;;; inside the border)" in the pane's default character style, a fixed-
;;; width font. No title bar and no buttons: the label strip is what you
;;; drag, and right-clicking it gives the window menu (Genera explained its
;;; mouse clicks in the documentation line instead).

(define genera-border 1)
(define genera-margin 4)

(define (genera-label-height)
  (exact (max 14 (- (title-height) 16) (round (+ (font-line-height 'mono) 2)))))

(define (genera-insets win)
  (let ([m (+ genera-border genera-margin)])
    (values m m m (+ genera-border (genera-label-height) genera-margin))))

;; The label strip is the window's title area. The corners and the bottom
;; edge stay free for resizing.
(define (genera-buttons win)
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
                      (floor (/ (- lh (font-line-height 'mono)) 2)))
                   (fit-text (window-title win)
                             (- w (* 2 (+ b genera-margin)))
                             'mono)
                   'text 'mono)))))

(define-window-style genera "Genera"
  'insets genera-insets
  'buttons genera-buttons
  'draw-under genera-draw-under)
