;;; styles/cons.ss — the Cons window style.
;;; The title bar is a box-and-pointer diagram, drawn the way SICP draws a
;;; list: the window is ("Title" - +). Its first pair holds the title in
;;; its car and points to two more pairs, whose cars minimize and maximize;
;;; the chain ends, as every list does, in the nil slash, and clicking that
;;; end of the list closes the window. Drawn, not painted: outlined cells
;;; on the window's own background, in accent ink with a bolder stroke
;;; while the window has the focus, in faint ink when it has not. Under the
;;; mouse a cell fills with ink; nil fills red, its slash crossed out.

;; A cell is a square the title's line fits in, and an even number of
;; pixels so that the glyphs center: 20 at the default fonts. The chain
;; sits a quarter cell from the left, in the middle of the title bar.
(define (cons-line) (exact (round (font-line-height 'title))))
(define (cons-cell) (* 2 (quotient (+ (cons-line) 6) 2)))
(define (cons-left) (quotient (cons-cell) 4))

;; A pointer's length: 0.7 of a cell, shorter where the window is so
;; narrow that the chain would not fit with a car a cell wide.
(define (cons-gap win)
  (let ([c (cons-cell)])
    (max (quotient c 3)
         (min (quotient (* 7 c) 10)
              (quotient (- (window-w win) (* 2 (cons-left)) (* 6 c)) 2)))))

;; The title's car: as wide as the title, at least a cell, and narrow
;; enough to leave room for the rest of the chain.
(define (cons-car-width win)
  (let ([c (cons-cell)])
    (max c (min (exact (ceiling (+ (text-width (window-title win) 'title)
                                   (* 4/5 c))))
                (- (window-w win) (* 2 (cons-left)) (* 5 c)
                   (* 2 (cons-gap win)))))))

;; Where the three pairs start: the title's, minimize's and maximize's.
(define (cons-pairs win)
  (let* ([c (cons-cell)] [a (cons-gap win)]
         [x (+ (cons-left) (cons-car-width win) c a)])
    (list (cons-left) x (+ x c c a))))

(define (cons-regions win)
  (let ([c (cons-cell)] [th (title-height)] [xs (cons-pairs win)])
    (list (list 'minimize (cadr xs) 0 c th)
          (list 'maximize (caddr xs) 0 c th)
          (list 'close (+ (caddr xs) c) 0 c th))))

;; A pair at x: a car w wide and a cdr, c high, each maybe tinted. The
;; divider lies in the car, so a cell's car is a square inside.
(define (cons-pair! x y w c ink lw car-tint cdr-tint)
  (when car-tint (fill-rect x y w c car-tint))
  (when cdr-tint (fill-rect (+ x w) y c c cdr-tint))
  (stroke-rect x y (+ w c) c ink lw)
  (fill-rect (+ x w (- lw)) y lw c ink))

;; From a dot in the middle of the cdr whose inside starts at x, an arrow
;; to the box at x1.
(define (cons-pointer! x x1 y c ink lw)
  (let* ([dx (+ x (/ (- c lw) 2))] [ly (+ y (quotient (- c lw) 2))]
         [my (+ ly (/ lw 2))] [head (quotient c 3)] [half (quotient c 6)])
    (fill-circle dx my (+ (/ c 10) (/ lw 2)) ink)
    (fill-rect (floor dx) ly (- x1 head (floor dx) -1) lw ink)
    (fill-polygon (list (cons x1 my) (cons (- x1 head) (- my half))
                        (cons (- x1 head) (+ my half)))
                  ink)))

;; - or + in the cell at x, 2 pixels thick like the bold title's strokes.
(define (cons-sign! x y c plus? color lw)
  (let* ([i (- c lw lw)] [g (* 2 (quotient c 5))]
         [o (+ lw (quotient (- i g) 2))] [m (+ lw (quotient (- i 2) 2))])
    (fill-rect (+ x o) (+ y m) g 2 color)
    (when plus? (fill-rect (+ x m) (+ y o) 2 g color))))

;; nil: the slash across the cdr whose inside starts at x, crossed out in
;; the color cross. Clipped to the inside, so the box's lines stay clean.
(define (cons-nil! x y c ink lw cross)
  (let ([w (- c lw)] [h (- c lw lw)] [y (+ y lw)])
    (call-with-clip x y w h
      (lambda ()
        (draw-line x (+ y h) (+ x w) y (or cross ink) lw)
        (when cross (draw-line x y (+ x w) (+ y h) cross lw))))))

;; The y to draw the title at so that its capitals sit in the middle of a
;; cell at cy, c high. The cap height is measured on *cr*, at the scale it
;; is drawn: hinting makes it 9 px at 100% but 10 logical px at 200%. The
;; baseline goes on a whole device pixel, never below the middle.
(define (cons-title-y cy c)
  (let-values ([(l top w h) (text-ink "H" 'title)])
    (- (px-baseline (+ cy (/ c 2) (/ (- top) 2)) floor)
       (font-ascent 'title))))

(define (cons-tint kind hover pressed color)
  (cond [(eq? pressed kind) (alpha color 0.6)]
        [(eq? hover kind) color]
        [else #f]))

(define (cons-draw-under win x y w h focused? hover pressed)
  (let* ([th (title-height)] [c (cons-cell)] [cy (+ y (quotient (- th c) 2))]
         [lw (if focused? 2 1)] [ink (if focused? 'accent 'text-faint)]
         [fg (if focused? 'title-text 'title-text-dim)] [bg (window-bg win)]
         [cw (cons-car-width win)]
         [xs (map (lambda (p) (+ x p)) (cons-pairs win))]
         [t (car xs)] [b (cadr xs)] [m (caddr xs)]
         [s (fit-text (window-title win) (- cw (* 4/5 c)) 'title)]
         [close (cons-tint 'close hover pressed 'button-close)])
    (define (sign x kind plus? cdr-tint)
      (let ([tint (cons-tint kind hover pressed 'accent)])
        (cons-pair! x cy c c ink lw tint cdr-tint)
        (cons-sign! x cy c plus? (if tint bg fg) lw)))
    (window-shadow win x y w h 0 focused?)   ; shadow
    (fill-rect x y w h bg)
    (fill-rect x (+ y th -1) w 1 'window-border)
    (call-with-clip x y w th
      (lambda ()
        (cons-pair! t cy cw c ink lw #f #f)
        (draw-text (+ t (floor (/ (- cw (text-width s 'title)) 2)))
                   (cons-title-y cy c) s fg 'title)
        (cons-pointer! (+ t cw) b cy c ink lw)
        (sign b 'minimize #f #f)
        (cons-pointer! (+ b c) m cy c ink lw)
        (sign m 'maximize #t close)
        (cons-nil! (+ m c) cy c ink lw (and close bg))))))

(define (cons-draw-over win x y w h focused? hover pressed)
  (stroke-rect x y w h (if focused? 'window-border-focus 'window-border)))

(define-style cons "Cons"
  ;; its corners are square: every common option but the radius
  'options (remq 'corner-radius (map car common-options))
  'frame-regions 'cons-regions
  'frame-under 'cons-draw-under
  'frame-over 'cons-draw-over)
