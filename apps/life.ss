;;; apps/life.ss — Conway's Game of Life. Shows mouse input and animation.
;;; Space: pause · R: randomize · C: clear · N: single step · click/drag: draw

(define-app (life) "Life"
  (let* ([cols 72] [rows 44]
         [grid (make-bytevector (* cols rows) 0)]
         [next (make-bytevector (* cols rows) 0)]
         [running? #t] [generation 0] [last-step 0] [delay 80]
         [painting #f]                 ; #f, or the value being painted
         [cell 10] [ox 0] [oy 0])      ; layout from the last draw
    (define (idx x y) (+ (mod x cols) (* (mod y rows) cols)))
    (define (cell-ref x y) (bytevector-u8-ref grid (idx x y)))
    (define (neighbours x y)
      (+ (cell-ref (- x 1) (- y 1)) (cell-ref x (- y 1)) (cell-ref (+ x 1) (- y 1))
         (cell-ref (- x 1) y) (cell-ref (+ x 1) y)
         (cell-ref (- x 1) (+ y 1)) (cell-ref x (+ y 1)) (cell-ref (+ x 1) (+ y 1))))
    (define (step!)
      (do ([y 0 (+ y 1)]) ((= y rows))
        (do ([x 0 (+ x 1)]) ((= x cols))
          (let ([n (neighbours x y)] [alive? (= 1 (cell-ref x y))])
            (bytevector-u8-set! next (idx x y) (if (or (= n 3) (and alive? (= n 2))) 1 0)))))
      (let ([t grid]) (set! grid next) (set! next t))
      (set! generation (+ generation 1)))
    (define (randomize!)
      (do ([i 0 (+ i 1)]) ((= i (* cols rows)))
        (bytevector-u8-set! grid i (if (< (random 1.0) 0.22) 1 0)))
      (set! generation 0))
    (define (cell-at x y)
      (let ([cx (exact (floor (/ (- x ox) cell)))] [cy (exact (floor (/ (- y oy) cell)))])
        (and (>= cx 0) (< cx cols) (>= cy 0) (< cy rows) (cons cx cy))))
    (define (paint! x y)
      (let ([c (cell-at x y)])
        (when (and c painting)
          (bytevector-u8-set! grid (idx (car c) (cdr c)) painting))))
    (randomize!)
    (open-window "Life"
      'size (cons (* cols 10) (+ 26 (* rows 10)))
      'cursor 'crosshair
      'tick (lambda (win)
              (when (and running? (> (- (now-ms) last-step) delay))
                (set! last-step (now-ms))
                (step!)
                (invalidate! win)))
      'draw (lambda (win w h)
              (set! cell (max 2 (min (/ w cols) (/ (- h 26) rows))))
              (set! ox (/ (- w (* cell cols)) 2))
              (set! oy 0)
              (fill-rect 0 0 w h 'bg-0)
              (do ([y 0 (+ y 1)]) ((= y rows))
                (let ([color (mix 'green 'cyan (/ y rows))])
                  (do ([x 0 (+ x 1)]) ((= x cols))
                    (when (= 1 (cell-ref x y))
                      (fill-rect (+ ox (* x cell) 1) (+ oy (* y cell) 1) (- cell 1) (- cell 1) color)))))
              (fill-rect 0 (- h 26) w 26 'bg-1)
              (draw-text 10 (- h 21)
                         (format "generation ~a   ~a   space pause · r random · c clear · n step"
                                 generation (if running? "▶" "❚❚"))
                         'text-faint 'ui-small))
      'mouse-down (lambda (win x y button clicks)
                    (let ([c (cell-at x y)])
                      (when c
                        (set! painting (if (= 1 (cell-ref (car c) (cdr c))) 0 1))
                        (paint! x y)
                        (invalidate! win))))
      'mouse-move (lambda (win x y) (when painting (paint! x y) (invalidate! win)))
      'mouse-up (lambda (win x y button) (set! painting #f))
      'key (lambda (win key mods)
             (case key
               [(#\space) (set! running? (not running?))]
               [(#\r) (randomize!)]
               [(#\c) (bytevector-fill! grid 0) (set! generation 0)]
               [(#\n) (step!)])
             (invalidate! win)))))
