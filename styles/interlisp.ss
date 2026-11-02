;;; styles/interlisp.ss — the Interlisp window style.
;;; Interlisp-D, later Medley (Xerox PARC, 1980s; on Unix workstations
;;; from 1988), matched against a real Medley screen: a 2-pixel border and
;;; a 15-pixel title bar in solid ink with the title in paper color, and
;;; no buttons: everything was done from the right-button menu (here:
;;; right-click the title). Ink is the theme's text color, paper the
;;; window's background, so any palette gets the inverse-video look.

(define interlisp-border 2)

(define (interlisp-title-height)
  (exact (max 15 (- (title-height) 15)
              (round (+ (font-line-height 'ui-small) 2)))))

(define (interlisp-insets win)
  (values interlisp-border (interlisp-title-height)
          interlisp-border interlisp-border))

(define (interlisp-draw-under win x y w h focused? hover pressed)
  (let ([th (interlisp-title-height)])
    (fill-rect x y w h 'text)
    (call-with-clip x y w th   ; even "…" may not fit a narrow window
      (lambda ()
        (draw-text (+ x 5)
                   (+ y (floor (/ (- th (font-line-height 'ui-small)) 2)))
                   (fit-text (window-title win) (- w 10) 'ui-small)
                   (window-bg win) 'ui-small)))))

;; Its palette over the dotted gray shade.
(define-style interlisp "Interlisp"
  'palette 'interlisp
  'wallpaper 'shade
  'frame-insets 'interlisp-insets
  'frame-under 'interlisp-draw-under)
