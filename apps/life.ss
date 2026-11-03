;;; apps/life.ss — Conway's Game of Life. Shows mouse input and animation.
;;; Space: pause · R: randomize · C: clear · N: single step · click/drag: draw

(define (life-bar-height) (max 26 (round (+ (font-line-height 'ui-small) 13))))

;; The status line under the grid.
(define (life-status-text generation running?)
  (format "generation ~a   ~a   space pause · r random · c clear · n step"
          generation (if running? "▶" "❚❚")))

;; The live-cell pixel of each row, top to bottom (color->pixel reads the
;; theme).
(define (life-row-pixels rows)
  (let ([v (make-fxvector rows)])
    (do ([y 0 (fx+ y 1)]) ((fx= y rows) v)
      (fxvector-set! v y (color->pixel (mix 'green 'cyan (/ y rows)))))))

;; Rows [y0, y1) of the next generation on a torus into next, and one pixel
;; per cell into pix. Touches only its arguments and allocates nothing:
;; fixnum operations, at most three arguments to fx+ (a + or fx+ of four
;; or more allocates its arguments: an 8-argument + took 300 KB a generation).
(define (life-step-rows! grid next pix row-px dead cols rows y0 y1)
  (let ([last-x (fx- cols 1)] [last-y (fx- rows 1)])
    (do ([y y0 (fx+ y 1)]) ((fx= y y1))
      (let* ([mid (fx* y cols)]
             [up (if (fx= y 0) (fx* last-y cols) (fx- mid cols))]
             [dn (if (fx= y last-y) 0 (fx+ mid cols))]
             [live (fxvector-ref row-px y)])
        (define (col x)   ; live cells in column x of rows y-1, y, y+1
          (fx+ (bytevector-u8-ref grid (fx+ up x))
               (bytevector-u8-ref grid (fx+ mid x))
               (bytevector-u8-ref grid (fx+ dn x))))
        (let loop ([x 0] [left (col last-x)] [here (col 0)])
          (when (fx< x cols)
            (let* ([right (col (if (fx= x last-x) 0 (fx+ x 1)))]
                   [i (fx+ mid x)]
                   [self (bytevector-u8-ref grid i)]
                   [n (fx- (fx+ left here right) self)]
                   [alive (if (or (fx= n 3) (and (fx= n 2) (fx= self 1))) 1 0)])
              (bytevector-u8-set! next i alive)
              (bytevector-u32-native-set! pix (fx* 4 i)
                                          (if (fx= alive 1) live dead))
              (loop (fx+ x 1) here right))))))))

;; The cells (a pixmap, one pixel each) scaled up at (ox, oy), then the
;; one-pixel gaps between them.
(define (draw-life-cells pixels cols rows cell ox oy)
  (pixmap-draw pixels ox oy (* cell cols) (* cell rows))
  ;; columns and rows apart: crossing rects in one path take cairo's slow
  ;; path under the window's rounded clip (5× slower)
  (fill-rects (map (lambda (x) (list (+ ox (* x cell)) oy 1 (* cell rows)))
                   (iota cols))
              'bg-0)
  (fill-rects (map (lambda (y) (list ox (+ oy (* y cell)) (* cell cols) 1))
                   (iota rows))
              'bg-0))

(define-app (life) "Life"
  (let* ([cols 72] [rows 44] [n (* cols rows)]
         [grid (make-bytevector n 0)]        ; a byte per cell, 1 for alive
         [next (make-bytevector n 0)]
         ;; one pixel per cell: what draw shows
         [pixels (make-pixmap cols rows)]
         ;; a step writes the next pixels here
         [back (make-pixmap-bytevector pixels)]
         [row-px #f] [dead 0] [palette-key #f]
         [running? #t] [generation 0] [last-step 0] [delay 80]
         [painting #f]                 ; #f, or the value being painted
         [cell 10] [ox 0] [oy 0])      ; layout from the last draw
    (define (index c) (+ (car c) (* (cdr c) cols)))
    (define (pixel-of i)
      (if (fx= 1 (bytevector-u8-ref grid i))
          (fxvector-ref row-px (fxquotient i cols))
          dead))
    (define (repaint-pixels!)
      (let ([bv (pixmap-bytes pixels)])
        (do ([i 0 (fx+ i 1)]) ((fx= i n))
          (bytevector-u32-native-set! bv (fx* 4 i) (pixel-of i)))))
    ;; 44 mixes, again only when a color it uses changed (by any means: a
    ;; raw hashtable-set! of *theme* too)
    (define (palette!)
      (let ([key (list (theme 'green) (theme 'cyan) (theme 'bg-0))])
        (unless (equal? key palette-key)
          (set! row-px (life-row-pixels rows))
          (set! dead (color->pixel 'bg-0))
          (set! palette-key key)
          (repaint-pixels!))))
    (define (step!)
      (life-step-rows! grid next back row-px dead cols rows 0 rows)
      (let ([t grid]) (set! grid next) (set! next t))
      (set! back (pixmap-swap-bytes! pixels back))
      (set! generation (+ generation 1)))
    (define (randomize!)
      (do ([i 0 (+ i 1)]) ((= i n))
        (bytevector-u8-set! grid i (if (< (random 1.0) 0.22) 1 0)))
      (set! generation 0)
      (repaint-pixels!))
    (define (cell-at x y)
      (let ([cx (exact (floor (/ (- x ox) cell)))]
            [cy (exact (floor (/ (- y oy) cell)))])
        (and (>= cx 0) (< cx cols) (>= cy 0) (< cy rows) (cons cx cy))))
    (define (paint! x y)
      (let ([c (cell-at x y)])
        (when (and c painting)
          (bytevector-u8-set! grid (index c) painting)
          (pixmap-set-pixel! pixels (car c) (cdr c) (pixel-of (index c))))))
    (palette!)
    (randomize!)
    (open-window "Life"
      'size (cons (* cols 10) (+ (life-bar-height) (* rows 10)))
      'cursor 'crosshair
      'tick (lambda (win)
              (when (and running? (> (- (now-ms) last-step) delay))
                (set! last-step (now-ms))
                (step!)
                (invalidate! win)))
      'draw (lambda (win w h)
              (palette!)
              (set! cell
                    (max 2 (floor (min (/ w cols)
                                       (/ (- h (life-bar-height)) rows)))))
              (set! ox (floor (/ (- w (* cell cols)) 2)))
              (set! oy 0)
              ;; the margins around the grid (the grid covers the rest)
              (fill-rect 0 0 w oy 'bg-0)
              (fill-rect 0 oy ox (* cell rows) 'bg-0)
              (fill-rect (+ ox (* cell cols)) oy (- w ox (* cell cols))
                         (* cell rows) 'bg-0)
              (fill-rect 0 (+ oy (* cell rows)) w (- h oy (* cell rows)) 'bg-0)
              (draw-life-cells pixels cols rows cell ox oy)
              (fill-rect 0 (- h (life-bar-height)) w (life-bar-height) 'bg-1)
              (draw-text 10 (+ (- h (life-bar-height)) 5)
                         (life-status-text generation running?)
                         'text-faint 'ui-small))
      'mouse-down (lambda (win x y button clicks)
                    (let ([c (cell-at x y)])
                      (when c
                        (set! painting
                          (if (= 1 (bytevector-u8-ref grid (index c))) 0 1))
                        (paint! x y)
                        (invalidate! win))))
      'mouse-move (lambda (win x y)
                    (when painting (paint! x y) (invalidate! win)))
      'mouse-up (lambda (win x y button) (set! painting #f))
      'key (lambda (win key mods)
             (case key
               [(#\space) (set! running? (not running?))]
               [(#\r) (randomize!)]
               [(#\c) (bytevector-fill! grid 0)
                      (set! generation 0)
                      (repaint-pixels!)]
               [(#\n) (step!)])
             (invalidate! win)))))
