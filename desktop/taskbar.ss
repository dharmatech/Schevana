;;; taskbar.ss — the bar at the bottom: a button for each window (left
;;; focuses or minimizes, middle closes, right opens its menu) and a hint.

(define (taskbar-height)
  (max (theme 'taskbar-height) (chrome-min-height 'taskbar-height)))

(define (taskbar-box)
  (let ([h (taskbar-height)]) (values 0 (- (screen-h) h) (screen-w) h)))

(defvar *taskbar-items* '())   ; ((x0 x1 window) ...) from the last frame

(define (draw-taskbar)
  (let* ([w (screen-w)] [h (taskbar-height)] [y0 (- (screen-h) h)]
         [lh (font-line-height 'ui)]
         [ws (sort (lambda (a b) (< (window-id a) (window-id b))) *windows*)]
         [n (length ws)]
         [hint "F1 apps  ·  Ctrl+Tab switch  ·  right-click desktop"]
         [button-w (lambda (hint-w)
                     (if (zero? n)
                         0
                         (min 210 (/ (- w 16 hint-w (* 6 (- n 1))) n))))]
         ;; the hint gives way when the buttons would be too narrow for their
         ;; titles
         [hint-w (let ([hw (+ 24 (text-width hint 'ui-small))]
                       [want (min 210 (+ 48 (widest-text (map window-title ws)
                                                         'ui)))])
                   (if (>= (button-w hw) want) hw 0))]
         [bw (button-w hint-w)])
    (fill-rect 0 y0 w h (alpha 'panel 0.94))
    (draw-line 0 (+ y0 0.5) w (+ y0 0.5) (alpha 'panel-text 0.12))
    (when (> hint-w 0)
      (draw-text (- w hint-w) (+ y0 (/ (- h (font-line-height 'ui-small)) 2))
                 hint 'panel-dim 'ui-small))
    (let loop ([ws ws] [x 8] [items '()])
      (if (null? ws)
          (set! *taskbar-items* items)
          (let* ([win (car ws)]
                 [focused? (eq? win *focus*)]
                 [hover? (equal? *hover* (list 'surface 'taskbar win))]
                 [min? (not (visible? win))]
                 [color (app-color (window-app win))])
            (when (or focused? hover?)
              (fill-round-rect x (+ y0 5) bw (- h 10) 6
                               (if focused?
                                   'panel-hover
                                   (alpha 'panel-hover 0.6))))
            (when focused?
              (fill-round-rect (+ x 12) (- (+ y0 h) 6) (- bw 24) 2 1
                               (panel-accent)))
            (fill-circle (+ x 15) (+ y0 (/ h 2)) 4
                         (if min? (alpha color 0.35) color))
            (draw-text (+ x 27) (+ y0 (/ (- h lh) 2))
                       (fit-text (window-title win) (- bw 36) 'ui)
                       (if min? 'panel-dim 'panel-text) 'ui)
            (loop (cdr ws) (+ x bw 6) (cons (list x (+ x bw) win) items)))))))

;; The window whose button is at (x, y), else 'bar: the bar takes every
;; point of its strip.
(define (taskbar-at x y)
  (and (>= y (- (screen-h) (taskbar-height)))
       (let ([it (find (lambda (it) (and (>= x (car it)) (< x (cadr it))))
                       *taskbar-items*)])
         (if (and it (memq (caddr it) *windows*)) (caddr it) 'bar))))

(define (taskbar-mouse kind what x y button clicks)
  (when (and (eq? kind 'down) (window? what))
    (case button
      [(left) (if (and (eq? what *focus*) (visible? what))
                  (minimize-window! what)
                  (focus-window! what))]
      [(middle) (close-window! what)]
      [(right)
       (open-window-menu! what x (- (screen-h) (taskbar-height)))])))

(define-surface taskbar 10
  'draw 'draw-taskbar 'box 'taskbar-box 'at 'taskbar-at
  'mouse 'taskbar-mouse)
(define-strip taskbar 'bottom 'taskbar-height)
