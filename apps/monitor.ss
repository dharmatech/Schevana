;;; apps/monitor.ss — the world's state, live: windows, processes, memory.
;;; Click a window row to focus it, a process row to kill it.

(define-app (monitor) "Monitor"
  (let ([rows '()] [last 0])   ; rows: ((y0 y1 . action) ...) from the last draw
    (define (section title y w)
      (draw-text 14 y title 'accent 'ui-bold)
      (draw-line 14 (+ y 22.5) (- w 14) (+ y 22.5) 'window-border)
      (+ y 28))
    (define (row y cols texts color)
      (for-each (lambda (x t) (draw-text x y t color 'mono-small)) cols texts)
      (+ y 19))
    (open-window "Monitor"
      'size '(520 . 420)
      'tick (lambda (win)
              (when (> (- (now-ms) last) 500)
                (set! last (now-ms))
                (invalidate! win)))
      'draw (lambda (win w h)
              (set! rows '())
              (fill-rect 0 0 w h 'bg-1)
              (let* ([y (section "Windows" 10 w)]
                     [y (row y '(14 50 230 330) '("id" "title" "app" "geometry") 'text-faint)]
                     [y (fold-left
                         (lambda (y win2)
                           (set! rows (cons (cons* y (+ y 19) (lambda () (focus-window! win2))) rows))
                           (row y '(14 50 230 330)
                                (list (number->string (window-id win2))
                                      (truncate-string (window-title win2) 22)
                                      (format "~a" (or (window-app win2) "-"))
                                      (format "~ax~a+~a+~a ~a" (window-w win2) (window-h win2)
                                              (window-x win2) (window-y win2)
                                              (if (eq? (window-state win2) 'normal) "" (window-state win2))))
                                (cond [(eq? win2 (focused-window)) 'text]
                                      [(window-error win2) 'red]
                                      [else 'text-dim])))
                         y (sort (lambda (a b) (< (window-id a) (window-id b))) (windows)))]
                     [y (section "Processes" (+ y 12) w)]
                     [y (if (null? (procs))
                            (row y '(14) '("none — try (spawn \"x\" (lambda () (let l () (l)))) in a REPL") 'text-faint)
                            (fold-left
                             (lambda (y p)
                               (set! rows (cons (cons* y (+ y 19) (lambda () (kill! p))) rows))
                               (row y '(14 50 230 330)
                                    (list (number->string (proc-id p)) (proc-name p)
                                          (format "~,1fM ticks" (/ (proc-ticks p) 1e6))
                                          (format "~,1fs  (click kills)" (/ (- (now-ms) (proc-started p)) 1000.0)))
                                    'orange))
                             y (procs)))]
                     [y (section "World" (+ y 12) w)])
                (row y '(14 200)
                     (list (format "heap ~,1f MB   gcs ~a" (/ (bytes-allocated) 1048576.0) (collections))
                           (format "uptime ~as   frame ~,1f ms   apps ~a"
                                   (quotient (uptime-ms) 1000) *frame-ms* (length (apps))))
                     'text-dim)))
      'mouse-down (lambda (win x y button clicks)
                    (let ([r (find (lambda (r) (and (>= y (car r)) (< y (cadr r)))) rows)])
                      (when r ((cddr r))))))))
