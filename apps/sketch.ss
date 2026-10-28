;;; apps/sketch.ss — freehand drawing. Strokes are plain Scheme data:
;;; (window-ref win 'strokes) => ((colour width (x . y) ...) ...)

(define sketch-palette '(text red orange yellow green teal accent accent-2))

(define-app (sketch) "Sketch"
  (let ([color 'accent] [width 3] [current #f])
    (define (strokes win) (window-ref win 'strokes '()))
    (define (swatch-at x y)
      (and (< y 34)
           (let ([i (exact (floor (/ (- x 10) 28)))])
             (and (>= i 0) (< i (length sketch-palette)) (list-ref sketch-palette i)))))
    (open-window "Sketch"
      'size '(560 . 380)
      'cursor 'crosshair
      'strokes '()
      'draw (lambda (win w h)
              (fill-rect 0 0 w h 'bg-1)
              (for-each (lambda (s) (draw-polyline (cddr s) (car s) (cadr s)))
                        (reverse (strokes win)))
              (when current (draw-polyline (cddr current) (car current) (cadr current)))
              (fill-rect 0 0 w 34 'bg-2)
              (let loop ([cs sketch-palette] [x 10])
                (unless (null? cs)
                  (fill-circle (+ x 12) 17 9 (car cs))
                  (when (eq? (car cs) color) (stroke-circle (+ x 12) 17 12 'text 1.5))
                  (loop (cdr cs) (+ x 28))))
              (draw-text (max (+ 20 (* 28 (length sketch-palette))) (- w 210)) 11
                         "drag to draw · c clears · 1-9 width" 'text-faint 'ui-small))
      'mouse-down (lambda (win x y button clicks)
                    (cond
                      [(swatch-at x y) => (lambda (c) (set! color c))]
                      [(eq? button 'left) (set! current (list color width (cons x y)))])
                    (invalidate! win))
      'mouse-move (lambda (win x y)
                    (when current
                      (set-cdr! (cdr current) (cons (cons x y) (cddr current)))
                      (invalidate! win)))
      'mouse-up (lambda (win x y button)
                  (when current
                    (window-set! win 'strokes (cons current (strokes win)))
                    (set! current #f)))
      'key (lambda (win key mods)
             (cond
               [(eqv? key #\c) (window-set! win 'strokes '())]
               [(and (char? key) (char<=? #\1 key #\9))
                (set! width (* 1.5 (- (char->integer key) (char->integer #\0))))])
             (invalidate! win)))))
