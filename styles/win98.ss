;;; styles/win98.ss — the Windows 98 window style.
;;; Matched against real Windows 98 screenshots: a 4-pixel frame of two
;;; bevels (light gray and white above, black and dark gray below) around
;;; the gray face; an 18-pixel caption, navy fading to blue while active
;;; and gray when not, holding the app's icon and a bold title; 16 × 14
;;; beveled buttons on the right, minimize touching maximize and close 2
;;; pixels apart. No rounded corners, no hover effects. The
;;; face is the theme's title-bg; the caption runs from accent to accent-2.

;; A 1-pixel bevel: light on the top and left, dark on the bottom and right.
;; Swap the colors for a recessed look.
(define (bevel-rect x y w h light dark)
  (fill-rect x y w 1 light)
  (fill-rect x y 1 h light)
  (fill-rect x (+ y h -1) w 1 dark)
  (fill-rect (+ x w -1) y 1 h dark))

(define win98-border 4)

;; The four bevel pens for a face color, lightest first: for the classic
;; #c0c0c0 that is white, #dfdfdf, #808080 and black.
(define (win98-pens face)
  (values #xffffff (mix face #xffffff 0.49) (mix face #x000000 1/3) #x000000))

(define (win98-caption-height)
  (exact (max 18 (- (title-height) 12)
              (round (+ (font-line-height 'title) 3)))))

;; (values width height): 16 × 14 in the classic 18-pixel caption.
(define (win98-button-size)
  (let ([bh (- (win98-caption-height) 4)])
    (values (exact (round (* bh 8/7))) bh)))

(define (win98-insets win)
  (values win98-border (+ win98-border (win98-caption-height) 1)
          win98-border win98-border))

(define (win98-buttons win)
  (let*-values ([(bw bh) (win98-button-size)]
                [(y) (+ win98-border 2)]
                [(close-x) (- (window-w win) win98-border 2 bw)]
                [(max-x) (- close-x 2 bw)])
    (list (list 'close close-x y bw bh)
          (list 'maximize max-x y bw bh)
          (list 'minimize (- max-x bw) y bw bh))))

;; A beveled box, raised or (pressed) sunken.
(define (win98-box x y w h face down?)
  (let-values ([(hi lt sh dk) (win98-pens face)])
    (fill-rect x y w h face)
    (if down?
        (begin (bevel-rect x y w h dk hi)
               (bevel-rect (+ x 1) (+ y 1) (- w 2) (- h 2) sh lt))
        (begin (bevel-rect x y w h hi dk)
               (bevel-rect (+ x 1) (+ y 1) (- w 2) (- h 2) lt sh)))))

;; Button glyphs as rectangles in the pixels of a 16 × 14 button.
(define win98-glyphs
  '((minimize (4 9 6 2))
    (maximize (3 2 9 2) (3 2 1 9) (11 2 1 9) (3 10 9 1))
    (restore (5 1 6 2) (5 1 1 3) (10 1 1 6) (8 6 3 1)
             (3 4 6 2) (3 4 1 6) (8 4 1 6) (3 9 6 1))
    (close (4 3 2 1) (10 3 2 1) (5 4 2 1) (9 4 2 1) (6 5 4 1) (7 6 2 1)
           (6 7 4 1) (5 8 2 1) (9 8 2 1) (4 9 2 1) (10 9 2 1))))

(define (win98-draw-button win kind bx by bw bh face pressed)
  (let* ([down? (eq? pressed kind)]
         [glyph (if (and (eq? kind 'maximize) (maximized? win)) 'restore kind)]
         [k (max 1 (quotient bh 14))]           ; whole pixels stay crisp
         [ox (+ bx (quotient (- bw (* 16 k)) 2) (if down? 1 0))]
         [oy (+ by (quotient (- bh (* 14 k)) 2) (if down? 1 0))])
    (win98-box bx by bw bh face down?)
    (when (eq? glyph 'restore)   ; the front window hides the back one
      (fill-rect (+ ox (* 4 k)) (+ oy (* 6 k)) (* 4 k) (* 3 k) face))
    (for-each (lambda (r)
                (fill-rect (+ ox (* k (car r))) (+ oy (* k (cadr r)))
                           (* k (caddr r)) (* k (cadddr r)) #x000000))
              (cdr (assq glyph win98-glyphs)))))

(define (win98-draw-under win x y w h focused? hover pressed)
  (let*-values ([(face) 'title-bg]
                [(hi lt sh dk) (win98-pens face)]
                [(b) win98-border]
                [(ch) (win98-caption-height)]
                [(cx cy cw) (values (+ x b) (+ y b) (- w b b))]
                [(is) (- ch 2)]
                [(icon) (app-color (window-app win))]
                [(title) (window-title win)]
                [(buttons) (win98-buttons win)])
    ;; a window too narrow for its buttons and icon shows only what fits
    (call-with-clip x y w h
      (lambda ()
        (fill-rect x y w h face)
        (bevel-rect x y w h lt dk)
        (bevel-rect (+ x 1) (+ y 1) (- w 2) (- h 2) hi sh)
        (if focused?
            (fill-gradient cx cy cw ch 'accent 'accent-2 'horizontal)
            (fill-gradient cx cy cw ch sh face 'horizontal))
        ;; the app's icon: a square in its color with its initial
        (fill-rect (+ cx 2) (+ cy 1) is is icon)
        (stroke-rect (+ cx 2.5) (+ cy 1.5) (- is 1) (- is 1)
                     (mix icon #x000000 0.4))
        (draw-text-centered (+ cx 2) (+ cy 1) is is
                            (if (string=? title "")
                                "·"
                                (string (char-upcase (string-ref title 0))))
                            (readable-on icon) 'ui-small)
        (draw-text (+ cx is 5) (+ cy (/ (- ch (font-line-height 'title)) 2))
                   (fit-text title
                             (- (list-ref (car (last-pair buttons)) 1) is b 10)
                             'title)
                   (if focused? #xffffff face) 'title)
        (for-each (lambda (bt)
                    (win98-draw-button win (car bt)
                                       (+ x (list-ref bt 1))
                                       (+ y (list-ref bt 2))
                                       (list-ref bt 3) (list-ref bt 4)
                                       face pressed))
                  buttons)))))

(define-window-style win98 "Windows 98"
  'insets win98-insets
  'buttons win98-buttons
  'draw-under win98-draw-under)
