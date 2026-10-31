;;; styles/mac.ss — the Mac window style.
;;; Traffic lights on the left, glyphs on hover, centered title.

(define (draw-resize-grip x y)
  (for-each (lambda (d)
              (draw-line (- x d) (- y 3) (- x 3) (- y d) (alpha 'text 0.18) 1))
            '(7 11)))

(define (mac-radius win) (if (maximized? win) 0 (theme 'corner-radius)))

(define (mac-buttons win)
  (let ([cy (/ (title-height) 2)])
    (map (lambda (kind cx) (list kind (- cx 9) (- cy 9) 18 18))
         '(close minimize maximize) '(18 38 58))))

(define (mac-draw-under win x y w h focused? hover pressed)
  (let ([th (title-height)] [r (mac-radius win)]
        [glyphs? (memq hover '(close minimize maximize))])
    (window-shadow win x y w h r focused?)   ; shadow
    (fill-round-rect x y w h r (window-bg win))
    (call-with-round-clip x y w h r
      (lambda ()
        (fill-gradient x y w th
                       (if focused? (mix 'title-bg-focus 'text 0.04) 'title-bg)
                       (if focused? 'title-bg-focus 'title-bg))
        (draw-line x (+ y th -0.5) (+ x w) (+ y th -0.5) (alpha #x000000 0.5))
        (for-each (lambda (cx color glyph)
                    (let ([cy (+ y (/ th 2))])
                      (fill-circle (+ x cx) cy 6
                        (if (or focused? glyphs?) color 'button-idle))
                      (when glyphs?
                        (draw-text-centered (+ x cx -6) (- cy 7) 12 13 glyph
                                            (alpha #x000000 0.6) 'glyph))))
                  '(18 38 58) '(button-close button-min button-max)
                  '("×" "–" "+"))
        (draw-text-centered (+ x 70) y (- w 140) th
                            (fit-text (window-title win) (- w 160) 'title)
                            (if focused? 'title-text 'title-text-dim)
                            'title)))))

(define (mac-draw-over win x y w h focused? hover pressed)
  (stroke-round-rect x y w h (mac-radius win)
                     (if focused? 'window-border-focus 'window-border))
  ;; (the grip reaches 11 pixels in)
  (unless (or (maximized? win) (< w 14) (< h 14))
    (draw-resize-grip (+ x w) (+ y h))))

(define-window-style mac "Mac"
  'radius mac-radius
  'buttons mac-buttons
  'draw-under mac-draw-under
  'draw-over mac-draw-over)
