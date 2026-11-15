;;; lib/gravity-l0-simple.sps — Gravity L0 (simple): the sim of
;;; lib/gravity.sls run by a plain R6RS program, outside Schevana. It owns
;;; its loop, takes every step in one call, and prints the energy every 100
;;; steps. From the tree's root:
;;;
;;;   scheme --libdirs lib --program lib/gravity-l0-simple.sps [count steps]
;;;
;;; docs/GRAVITY.md says what each level of Schevana takes away from it.

(import (rnrs) (gravity))

;; How many bodies, and how far in time a step moves them: a turn of the
;; ring is about 1,300 steps.
(define count 200)
(define dt 0.01)

(define (report k bodies)
  (display "step ")
  (display k)
  (display "  energy ")
  (display (energy bodies))
  (newline))

(define (run n steps)
  (let loop ([bodies (ring n)] [k 0])
    (when (= (mod k 100) 0)
      (report k bodies))
    (when (< k steps)
      (loop (step bodies dt gravity) (+ k 1)))))

(let ([args (map string->number (cdr (command-line)))])
  (if (= (length args) 2)
      (run (car args) (cadr args))
      (run count 2000)))
