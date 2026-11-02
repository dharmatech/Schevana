;;; styles/windows.ss — the Windows window style.
;;; App icon and title on the left; minimize, maximize and close on the
;;; right as full-height buttons, close turning red on hover.

(define windows-button-width 46)

(define (windows-radius win)
  (if (maximized? win) 0 (min 8 (theme 'corner-radius))))

(define (windows-regions win)
  (let ([w (window-w win)] [bw windows-button-width] [th (title-height)])
    (list (list 'close (- w bw) 0 bw th)
          (list 'maximize (- w bw bw) 0 bw th)
          (list 'minimize (- w bw bw bw) 0 bw th))))

(define (windows-draw-button win kind bx by bw bh focused? hover pressed)
  (let* ([down? (eq? pressed kind)]
         [hot? (or down? (eq? hover kind))]
         [close? (eq? kind 'close)]
         [ink (cond [(and close? hot?) #xffffff]
                    [focused? 'title-text]
                    [else 'title-text-dim])]
         [cx (+ bx (quotient bw 2))]
         [cy (+ by (quotient bh 2))])
    (when hot?
      (fill-rect bx by bw bh
                 (if close?
                     (if down? #xf1707a #xe81123)
                     (alpha 'text (if down? 0.16 0.09)))))
    (case kind
      [(minimize) (fill-rect (- cx 5) cy 10 1 ink)]
      [(maximize)
       (if (maximized? win)
           (begin
             (stroke-rect (- cx 5) (- cy 3) 8 8 ink)
             (fill-rect (- cx 3) (- cy 5) 8 1 ink)
             (fill-rect (+ cx 4) (- cy 5) 1 8 ink))
           (stroke-rect (- cx 5) (- cy 5) 10 10 ink))]
      [(close)
       (draw-line (- cx 5) (- cy 5) (+ cx 5) (+ cy 5) ink 1)
       (draw-line (+ cx 5) (- cy 5) (- cx 5) (+ cy 5) ink 1)])))

(define (windows-draw-under win x y w h focused? hover pressed)
  (let* ([th (title-height)] [r (windows-radius win)]
         [title (window-title win)]
         [icon (app-color (window-app win))]
         [is (max 16 (round (+ (font-line-height 'ui-small) 3)))])
    (window-shadow win x y w h r focused?)   ; shadow
    (fill-round-rect x y w h r (window-bg win))
    (call-with-round-clip x y w h r
      (lambda ()
        (fill-rect x y w th (if focused? 'title-bg-focus 'title-bg))
        (fill-round-rect (+ x 12) (+ y (quotient (- th is) 2)) is is 4 icon)
        (draw-text-centered (+ x 12) (+ y (quotient (- th is) 2)) is is
                            (if (string=? title "")
                                "·"
                                (string (char-upcase (string-ref title 0))))
                            (readable-on icon) 'ui-small)
        (draw-text (+ x is 22) (+ y (/ (- th (font-line-height 'ui)) 2))
                   (fit-text title (- w is 22 (* 3 windows-button-width) 12)
                             'ui)
                   (if focused? 'title-text 'title-text-dim) 'ui)
        (for-each (lambda (b)
                    (windows-draw-button win (car b)
                                         (+ x (list-ref b 1))
                                         (+ y (list-ref b 2))
                                         (list-ref b 3) (list-ref b 4)
                                         focused? hover pressed))
                  (windows-regions win))))))

(define (windows-draw-over win x y w h focused? hover pressed)
  (stroke-round-rect x y w h (windows-radius win)
                     (if focused? 'window-border-focus 'window-border)))

(define-style windows "Windows"
  'frame-radius 'windows-radius
  'frame-regions 'windows-regions
  'frame-under 'windows-draw-under
  'frame-over 'windows-draw-over)
