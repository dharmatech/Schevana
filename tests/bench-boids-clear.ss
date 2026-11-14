;;; tests/bench-boids-clear.ss — Boids (clear)'s step against Boids', the
;;; same flocks at the counts tests/bench.ss steps (150, 450, 2,000 and
;;; 5,000), under the default rules and the heaviest:
;;;
;;;   scheme --script tests/bench-boids-clear.ss out
;;;
;;; Each is timed in rounds, Boids' and then Boids (clear)'s in one round,
;;; the other way around in the next, so that the machine's drift falls on
;;; both; printed are the medians of the rounds and their ratio. "alone" is
;;; the worklet called on the whole flock, on this thread; "crew" a step
;;; (boids-step-on: the steering, on the crew of 8, and the wrap). Prints,
;;; asserts nothing. output-dir only receives scratch files.

(define *schevana-root*
  (let ([dir (path-parent (path-parent (car (command-line))))])
    (if (string=? dir "") "." dir)))

(define out-dir
  (if (pair? (command-line-arguments)) (car (command-line-arguments)) "."))

(load (string-append *schevana-root* "/main.ss"))
(load (string-append *schevana-root* "/tests/reach.ss"))

(set! *theme-file-path* (string-append out-dir "/theme.ss"))
(set! *grant-file-path* (string-append out-dir "/permissions.ss"))

(define (bench-app app) (env-environment (grant-env app)))
(define (bench-ref app name) (top-level-value name (bench-app app)))

(define bench-heaviest
  '(cohesion 0.2
    cohesion-radius 200.0
    alignment 1.0
    alignment-radius 200.0
    separation 1.0
    separation-radius 50.0
    view-angle 360.0
    clear-view 1.0
    back-off 1.0
    blocker-radius 100.0
    blocker-cone 180.0
    min-speed 1.0
    max-speed 4.0))

;; A round's thunk is called k times, k chosen once so that a round
;; takes about 30 ms.
(define (bench-reps thunk)
  (let ([t0 (now-ns)])
    (thunk)
    (max 1 (exact (round (/ 30e6 (max 1 (- (now-ns) t0))))))))

;; ms a call of thunk takes over k calls.
(define (bench-time thunk k)
  (let ([t0 (now-ns)])
    (do ([i 0 (fx+ i 1)]) ((fx= i k)) (thunk))
    (/ (- (now-ns) t0) k 1e6)))

(define (bench-median l)
  (let ([l (sort < l)]) (list-ref l (quotient (length l) 2))))

;; The medians of rounds of theirs and mine, alternating which goes first.
(define (bench-pair theirs mine rounds)
  (theirs)
  (mine)
  (let ([k (bench-reps theirs)])
    (let loop ([r 0] [ts '()] [ms '()])
      (if (= r rounds)
          (values (bench-median ts) (bench-median ms))
          (if (even? r)
              (let* ([t (bench-time theirs k)] [m (bench-time mine k)])
                (loop (+ r 1) (cons t ts) (cons m ms)))
              (let* ([m (bench-time mine k)] [t (bench-time theirs k)])
                (loop (+ r 1) (cons t ts) (cons m ms))))))))

(define (bench-row label theirs mine)
  (let-values ([(t m) (bench-pair theirs mine 21)])
    (printf "~a  Boids ~8,3f ms  clear ~8,3f ms  clear/Boids ~5,3f\n"
            label t m (/ m t))))

(random-seed 2024)
(let ([size (crew-size)])
  (for-each
   (lambda (rules-name rules)
     (for-each
      (lambda (n)
        (let* ([flock ((bench-ref 'boids 'boids-scatter) n 1200 700)]
               [next (make-flvector (flvector-length flock))]
               [steering ((bench-ref 'boids 'boids-steering) rules)]
               [theirs (bench-ref 'boids 'boids-steer!)]
               [mine (bench-ref 'boids-clear 'boids-steer!)]
               [their-step (bench-ref 'boids 'boids-step-on)]
               [my-step (bench-ref 'boids-clear 'boids-step-on)])
          (crew-size-set! 0)
          (bench-row (format "~8a ~5d alone" rules-name n)
                     (lambda () (theirs 0 n flock next steering))
                     (lambda () (mine 0 n flock next steering)))
          (crew-size-set! 8)
          (bench-row (format "~8a ~5d crew " rules-name n)
                     (lambda () (their-step flock rules 1200 700))
                     (lambda () (my-step flock rules 1200 700)))))
      '(150 450 2000 5000)))
   '(default heaviest)
   (list (bench-ref 'boids 'boids-default-rules) bench-heaviest))
  (crew-size-set! size))

(exit)
