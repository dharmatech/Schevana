;;; lib/gravity-l1-simple.ss — Gravity L1 (simple): Gravity L0's sim, the
;;; library lib/gravity.sls, in the world. The world owns the loop, so the
;;; program's loop becomes a window's handlers: 'frame takes one step and
;;; asks for the next frame, 'draw draws the bodies, and the line L0 prints
;;; every 100 steps. A handler's call has a budget, so a frame takes one
;;; step, not every step there is.
;;;
;;; It isn't sandboxed: it loads into a world namespace, all of Chez and
;;; every name of the world's, which a person makes (Environments' Fork…
;;; and Load…, or a REPL). There the library is still imported, and its
;;; names and these stay out of "/" (docs/GRAVITY.md):
;;;
;;;   (define lab (env-fork (env-root) 'world 'name 'gravity))
;;;   (env-load lab (source-path "lib/gravity-l1-simple.ss"))
;;;   (app-launch! 'gravity-l1-simple)

(import (gravity))

;; How many bodies, and how far in time a step moves them: L0's.
(define count 200)
(define dt 0.01)

;; The line L0 prints, to draw
(define (report k bodies)
  (string-append "step " (number->string k)
                 "  energy " (number->string (energy bodies))))

;; Where a body shows in a field w × h: the unit circle fills most of it,
;; around its center, y up.
(define (draw-body b w h)
  (let ([z (body-place b)] [scale (* 0.4 (min w h))])
    (fill-circle (+ (/ w 2) (* scale (real-part z)))
                 (- (/ h 2) (* scale (imag-part z)))
                 2 'accent)))

(define-app (gravity-l1-simple) "Gravity L1 (simple)"
  (let ([bodies (ring count)])
    (open-window "Gravity L1 (simple)"
      'size '(520 . 520)
      'bodies bodies
      'steps 0
      'line (report 0 bodies)
      'open (lambda (win) (window-frame-soon! win))
      'frame (lambda (win t)
               (window-frame-soon! win)
               (let ([bodies (step (window-ref win 'bodies) dt gravity)]
                     [k (+ (window-ref win 'steps) 1)])
                 (window-set! win 'bodies bodies)
                 (window-set! win 'steps k)
                 (when (= (mod k 100) 0)
                   (window-set! win 'line (report k bodies)))))
      'draw (lambda (win w h)
              (fill-box 0 0 w h 'bg-0)
              (for-each (lambda (b) (draw-body b w h))
                        (window-ref win 'bodies))
              (draw-text 10 8 (window-ref win 'line) 'text-dim 'ui-small)))))
