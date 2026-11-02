;;; styles/draft.ss — the Draft window style.
;;; Every window is a technical drawing of itself: a fine outline, and
;;; along its top a dimension line from the left edge to the title block,
;;; with the title and the window's live size in logical pixels in a gap
;;; at its middle, "REPL · 662 × 470"; resize the window and its dimension
;;; follows. The buttons are the title block, the boxed table in the corner
;;; of a drawing, its top and right sides the outline's. No title bar
;;; fill: the band is the sheet's margin. One ink for everything, as on
;;; paper: the accent while the window has the focus, faded while not.

(define draft-arrow 8)   ; an arrowhead's length; it is 5 pixels wide
(define draft-pad 6)     ; between the dimension line and its text

;; The band on top. Odd, so that the dimension line runs through its
;; middle row, with the block's cells as tall above it as below.
(define (draft-band)
  (let ([t (title-height)]) (if (even? t) (+ t 1) t)))

;; A cell of the title block. Even, so that its inside is odd and a
;; glyph's 1-pixel lines fall on its middle pixel.
(define (draft-cell) (max 20 (* 2 (round (* 2/5 (title-height))))))

;; A 1-pixel outline, none at the screen's edges.
(define (draft-edge win) (if (maximized? win) 0 1))
(define (draft-insets win)
  (let ([e (draft-edge win)]) (values e (draft-band) e e)))

;; Where the title block's left rule is, in the window.
(define (draft-block-x win) (- (window-w win) 1 (* 3 (draft-cell))))

(define (draft-regions win)
  (let ([bx (draft-block-x win)] [c (draft-cell)] [b (draft-band)])
    (map (lambda (kind i) (list kind (+ bx (* i c)) 0 c b))
         '(minimize maximize close) '(0 1 2))))

;; The baseline that centers the title's capitals on the line through cy,
;; on a whole device pixel, from their height measured on *cr*: hinting
;; makes them 9 pixels tall at 100% but 10 logical pixels at 200%.
(define (draft-baseline cy)
  (use-font! (get-font 'title))
  (cairo-text-extents *cr* "H" *scratch*)
  (let ([base (+ cy (/ (- (foreign-ref 'double *scratch* 8)) 2))])
    (/ (round (* base *scale*)) *scale*)))

;; The dimension's text in room, as (values title size): both where they
;; fit, else the title cut to fit, else nothing: the line reads better
;; unbroken than cut by a stub such as "A …". The title comes first.
(define (draft-label win room)
  (let* ([title (window-title win)]
         [size (format "~a~a × ~a" (if (string=? title "") "" " · ")
                       (exact (round (window-w win)))
                       (exact (round (window-h win))))]
         [cut (fit-text title room 'title)])
    (cond [(<= (+ (text-width title 'title) (text-width size 'ui)) room)
           (values title size)]
          [(or (string=? cut title) (> (string-length cut) 5))
           (values cut "")]
          [else (values "" "")])))

;; An arrowhead with its tip at (x, y) and its base dir lengths away.
(define (draft-arrowhead x y dir ink)
  (let ([bx (+ x (* dir draft-arrow))])
    (fill-polygon `((,x . ,y) (,bx . ,(- y 5/2)) (,bx . ,(+ y 5/2))) ink)))

;; A button's mark, 2k + 1 pixels across around the middle of the pixel
;; at (cx, cy), in 1-pixel lines like the rest of the drawing.
(define (draft-glyph win kind cx cy k ink)
  (let ([s (+ k k 1)] [x0 (- cx k 1/2)] [y0 (- cy k 1/2)])
    (case kind
      [(minimize) (fill-rect x0 (- cy 1/2) s 1 ink)]
      [(maximize)
       (if (maximized? win)   ; restore: a sheet in front of another
           (begin (stroke-rect x0 (+ y0 2) (- s 2) (- s 2) ink)
                  (fill-rect (+ x0 2) y0 (- s 2) 1 ink)
                  (fill-rect (+ x0 s -1) y0 1 (- s 2) ink))
           (stroke-rect x0 y0 s s ink))]
      [(close) (draw-line (- cx k) (- cy k) (+ cx k) (+ cy k) ink)
               (draw-line (+ cx k) (- cy k) (- cx k) (+ cy k) ink)])))

;; The title block: a box the band's height, ruled into cells. The cell
;; under the mouse lights in the accent, even on an unfocused window.
(define (draft-block win x y hover pressed ink)
  (let ([c (draft-cell)] [b (draft-band)])
    (stroke-rect (+ x (draft-block-x win)) y (+ 1 (* 3 c)) b ink)
    (for-each
     (lambda (bt)
       (let* ([kind (car bt)] [cx (+ x (cadr bt))]
              [down? (eq? pressed kind)] [hot? (or down? (eq? hover kind))]
              [red? (and hot? (eq? kind 'close))])
         (fill-rect cx y 1 b ink)
         (when hot?
           (fill-rect (+ cx 1) (+ y 1) (- c 1) (- b 2)
                      (cond [(not red?) (alpha 'accent (if down? 0.3 0.15))]
                            [down? (mix 'button-close #x000000 0.2)]
                            [else 'button-close])))
         (draft-glyph win kind (+ cx (/ c 2) 1/2) (+ y (quotient b 2) 1/2)
                      (quotient c 6)
                      (cond [red? (readable-on 'button-close)]
                            [hot? 'accent]
                            [else ink]))))
     (draft-regions win))))

(define (draft-draw-under win x y w h focused? hover pressed)
  ;; Unfocused lines fade toward the sheet, as faint ink alone is too
  ;; close to Genera's black; their text keeps the dim title color.
  (let*-values ([(ink) (if focused? 'accent
                           (mix 'text-faint (window-bg win) 0.3))]
                [(a) draft-arrow]
                [(left) (+ x (draft-edge win))]   ; the arrows' tips
                [(right) (+ x (draft-block-x win))]
                [(ly) (+ y (quotient (draft-band) 2))]   ; the line's row
                ;; with at least 6 pixels of line either side of the text
                [(title size)
                 (draft-label win (- right left (* 2 (+ a 6 draft-pad))))]
                [(lw) (ceiling (+ (text-width title 'title)
                                  (text-width size 'ui)))]
                [(pad) (if (zero? lw) 0 draft-pad)]
                [(lx) (round (/ (+ left right (- lw)) 2))]
                [(base) (draft-baseline (+ ly 1/2))])
    (window-shadow win x y w h 0 focused?)   ; shadow
    (fill-rect x y w h (window-bg win))
    (call-with-clip x y w h   ; a window narrower than its block cuts it
      (lambda ()
        (unless (maximized? win) (stroke-rect x y w h ink))
        (when (> (- right left) (* 2 a))
          (fill-rect (+ left a -1) ly (- lx pad left a -1) 1 ink)
          (fill-rect (+ lx lw pad) ly (- right a -1 lx lw pad) 1 ink)
          (draft-arrowhead left (+ ly 1/2) 1 ink)
          (draft-arrowhead right (+ ly 1/2) -1 ink))
        (draw-text (draw-text lx (- base (font-ascent 'title)) title
                              (if focused? 'title-text 'title-text-dim)
                              'title)
                   (- base (font-ascent 'ui)) size
                   (if focused? ink 'title-text-dim) 'ui)
        (draft-block win x y hover pressed ink)))))

(define-style draft "Draft"
  'frame-insets 'draft-insets
  'frame-regions 'draft-regions
  'frame-under 'draft-draw-under)
