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
;;; (boids-step-on: the steering, on the crew of 8, and the wrap).
;;;
;;; Then the grid of docs/BOIDS-CLEAR.md, at 150, 450 and 1,000 boids: a
;;; step (with the wrap) written simple, on lists, against one written
;;; flat, on this thread alone and on the crew of 8. "simple" is Boids
;;; (simple), level 2, on its records; "L3" Boids L3 (simple), the same step
;;; in worklets on pairs, alone and on the crew; "Boids" Boids, alone and on
;;; the crew. Each group's steps are timed in rounds, the first turning
;;; round by round, and what one step of each way allocates. The simple
;;; steps are slow, so fewer rounds.
;;;
;;; Last, Boids L4 (flat), whose step is a compute shader: a step (with the
;;; wrap) of the same flocks, at the grid's counts and up to its cap; and
;;; its window's field, 760 × 520, listed by tile and drawn, a flock that
;;; has flocked for 300 steps, over what an image of its ground alone
;;; takes. The GPU is opened as the headless world opens lavapipe, with no
;;; window; "cpu" after output-dir times lavapipe instead:
;;;
;;;   scheme --script tests/bench-boids-clear.ss out cpu
;;;
;;; SDL's GPU API has no timestamps, so a time is the clock's: 8 steps sent
;;; at once and a read of the last one's buffer, which waits for them all,
;;; over 8; an image waits for its drawing.
;;;
;;; Prints, asserts nothing. output-dir only receives scratch files.

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

;; The medians of rounds of thunks, each called its own k times a round,
;; which goes first turning round by round.
(define (bench-rounds thunks rounds)
  (let* ([ths (list->vector thunks)]
         [n (vector-length ths)]
         [ks (vector-map bench-reps ths)]
         [times (make-vector n '())])
    (do ([r 0 (+ r 1)]) ((= r rounds))
      (do ([j 0 (+ j 1)]) ((= j n))
        (let ([i (mod (+ r j) n)])
          (vector-set! times i
                       (cons (bench-time (vector-ref ths i) (vector-ref ks i))
                             (vector-ref times i))))))
    (map bench-median (vector->list times))))

;; The medians of rounds of theirs and mine, alternating which goes first.
;; Each has its own k, so that a much slower step's rounds stay short too.
(define (bench-pair theirs mine rounds)
  (theirs)
  (mine)
  (let ([kt (bench-reps theirs)] [km (bench-reps mine)])
    (let loop ([r 0] [ts '()] [ms '()])
      (if (= r rounds)
          (values (bench-median ts) (bench-median ms))
          (if (even? r)
              (let* ([t (bench-time theirs kt)] [m (bench-time mine km)])
                (loop (+ r 1) (cons t ts) (cons m ms)))
              (let* ([m (bench-time mine km)] [t (bench-time theirs kt)])
                (loop (+ r 1) (cons t ts) (cons m ms))))))))

(define (bench-row label theirs mine)
  (let-values ([(t m) (bench-pair theirs mine 21)])
    (printf "~a  Boids ~8,3f ms  clear ~8,3f ms  clear/Boids ~5,3f\n"
            label t m (/ m t))))

;; The bytes a call of thunk allocates, on this thread alone.
(define (bench-bytes thunk)
  (let ([b0 (+ (bytes-allocated) (bytes-deallocated))])
    (thunk)
    (- (+ (bytes-allocated) (bytes-deallocated)) b0)))

;; A row of the grid: n boids under rules, every way.
(define (bench-grid-row label rules n)
  (let* ([flock ((bench-ref 'boids 'boids-scatter) n 1200 700)]
         [records (bench-simple-flock 'boids-simple flock)]
         [pairs (bench-simple-flock 'boids-l3-simple flock)]
         [simple (bench-ref 'boids-simple 'boids-step-on)]
         [l3 (bench-ref 'boids-l3-simple 'boids-step-on)]
         [boids (bench-ref 'boids 'boids-step-on)]
         [simple-step (lambda () (simple records rules 1200 700))]
         [l3-step (lambda () (l3 pairs rules 1200 700))]
         [boids-step (lambda () (boids flock rules 1200 700))])
    (crew-size-set! 0)
    (let ([alone (bench-rounds (list simple-step l3-step boids-step) 7)]
          [bytes (list (bench-bytes l3-step) (bench-bytes boids-step))])
      (crew-size-set! 8)
      (let ([crew (bench-rounds (list l3-step boids-step) 7)])
        (printf "~a  simple ~7,2f  L3 ~7,2f, crew ~6,2f  Boids ~6,2f, \
                 crew ~5,2f ms  makes ~5,1f MB, Boids ~5,1f KB\n"
                label (car alone) (cadr alone) (car crew) (caddr alone)
                (cadr crew) (/ (car bytes) 1e6) (/ (cadr bytes) 1e3))))))

;; Boids' flock, an flvector, as app's, a list of boids made by its
;; make-boid and make-vec: records in Boids (simple), pairs in Boids L3
;; (simple).
(define (bench-simple-flock app flock)
  (let ([make-boid (bench-ref app 'make-boid)]
        [make-vec (bench-ref app 'make-vec)])
    (let loop ([at (- (flvector-length flock) 4)] [boids '()])
      (if (< at 0)
          boids
          (loop (- at 4)
                (cons (make-boid (make-vec (flvector-ref flock at)
                                           (flvector-ref flock (+ at 1)))
                                 (make-vec (flvector-ref flock (+ at 2))
                                           (flvector-ref flock (+ at 3))))
                      boids))))))

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
  (for-each
   (lambda (rules-name rules)
     (for-each
      (lambda (n)
        (bench-grid-row (format "~8a ~5d grid" rules-name n) rules n))
      '(150 450 1000)))
   '(default heaviest)
   (list (bench-ref 'boids 'boids-default-rules) bench-heaviest))
  (crew-size-set! size))

;;; ------------------------------------------------------------------ L4

(define bench-l4-kind
  (if (and (pair? (command-line-arguments))
           (member "cpu" (cdr (command-line-arguments))))
      'cpu
      'gpu))

;; The GPU's device, opened as the headless world opens lavapipe's: on the
;; windowless video driver.
(when (eq? bench-l4-kind 'gpu)
  (core0-window-init! "offscreen")
  (gpu-open! 'gpu))
(schevana-boot! 'headless 1280 800)

(define (bench-l4 name) (bench-ref 'boids-l4-flat name))

(define (bench-l4-buffer flock)
  (let ([b (make-gpu-buffer (flvector-length flock))])
    (do ([i 0 (+ i 1)]) ((= i (flvector-length flock)) b)
      (gpu-buffer-set! b i (flvector-ref flock i)))))

;; ms a step of flock under rules on the grid's field, 1200 × 700: 8 from
;; one buffer into the other and back, sent at once, and a read that waits.
(define (bench-l4-step flock rules)
  (let* ([n (quotient (flvector-length flock) 4)]
         [a (bench-l4-buffer flock)]
         [b (bench-l4-buffer flock)]
         [args (cons* n 1200.0 700.0 ((bench-l4 'boids-steering) rules))])
    (define (eight)
      (do ([i 0 (+ i 1)]) ((= i 8))
        (apply gpu-compute-soon! (bench-l4 'boids-steer) n 1
               (if (even? i) a b) (if (even? i) b a) args))
      (gpu-computes-send!)
      (gpu-buffer-read! a))
    (let ([ms (car (bench-rounds (list eight) 7))])
      (gpu-buffer-release! a)
      (gpu-buffer-release! b)
      (/ ms 8))))

;; ms to list and draw n boids in win's field, 760 × 520, once they have
;; flocked for 300 steps, over an image of the ground alone.
(define (bench-l4-draw win n)
  (let ([draw (lambda ()
                (drawing->bytes 760 520 1
                                (lambda ()
                                  ((bench-l4 'boids-draw-field) win 760 520))))]
        [ground (lambda ()
                  (drawing->bytes 760 520 1
                                  (lambda () (fill-box 0 0 760 520 'bg-0))))])
    ((bench-l4 'boids-resize!) win n)
    ((bench-l4 'boids-scatter!) win)
    (do ([i 0 (+ i 1)]) ((= i 300))
      ((bench-l4 'boids-step!) win 760 520)
      (when (= 0 (mod i 30))
        (gpu-computes-send!)
        (gpu-buffer-read! ((bench-l4 'boids-current) win))))
    (let ([times (bench-rounds (list draw ground) 7)])
      (- (car times) (cadr times)))))

(printf "Boids L4 (flat) on ~a\n" (car (platform-gpu-device)))
(random-seed 2024)
(for-each
 (lambda (rules-name rules)
   (for-each
    (lambda (n)
      (printf "~8a ~5d L4    step ~8,3f ms\n" rules-name n
              (bench-l4-step ((bench-ref 'boids 'boids-scatter) n 1200 700)
                             rules)))
    '(150 450 1000 2000 4096 8192)))
 '(default heaviest)
 (list (bench-ref 'boids 'boids-default-rules) bench-heaviest))
(let ([win ((bench-l4 'boids-l4-flat))])
  (close-window! (find-window "Boids L4 (flat) — Rules"))
  (window-call win 'key #\space '())   ; paused: the bench steps it
  (for-each
   (lambda (n)
     (printf "default  ~5d L4    list and draw ~8,3f ms\n" n
             (bench-l4-draw win n)))
   '(150 1000 2000 4096 8192))
  (close-window! win))

(exit)
