;;; apps/boids.ss — Boids, Craig Reynolds' flocking simulation (1987).
;;; Ported from Boids in Niva (github.com/dharmatech/boids-niva), which was
;;; in turn inspired by Boids in Aloe, a Lisp whose kernel is sending a
;;; message (github.com/dharmatech/2026-09-02-aloe-racket).
;;;
;;; Besides Reynolds' three rules (cohesion, alignment, separation), two
;;; react to blockers, boids close and straight ahead: clear view steps
;;; aside, back off steps away. They come from Gary William Flake's "The
;;; Computational Beauty of Nature" (MIT Press, 1998), written from his
;;; description, not copied from his (GPL) source.
;;;
;;; The simulation is pure: a flock is a list of boids, and a step makes a
;;; new one. Only the window keeps state.

;;; ------------------------------------------------------------------ vectors

;; A 2D vector of flonums: a position, a velocity or a push.
(define-record-type vec
  (nongenerative schevana-boids-vec)
  (fields x y))

(record-writer (record-type-descriptor vec)
  (lambda (v port wr) (fprintf port "#<vec ~a ~a>" (vec-x v) (vec-y v))))

(define vec-zero (make-vec 0.0 0.0))

(define (vec+ a b) (make-vec (+ (vec-x a) (vec-x b)) (+ (vec-y a) (vec-y b))))
(define (vec- a b) (make-vec (- (vec-x a) (vec-x b)) (- (vec-y a) (vec-y b))))
(define (vec* v k) (make-vec (* (vec-x v) k) (* (vec-y v) k)))
(define (vec/ v k) (make-vec (/ (vec-x v) k) (/ (vec-y v) k)))

(define (vec-dot a b) (+ (* (vec-x a) (vec-x b)) (* (vec-y a) (vec-y b))))

;; The squared distance, so neighbor checks need no square root.
(define (vec-dist2 a b) (let ([d (vec- a b)]) (vec-dot d d)))

;; The centroid of a non-empty list of points.
(define (vec-mean vs) (vec/ (fold-left vec+ vec-zero vs) (inexact (length vs))))

;;; ------------------------------------------------------------------ boids

;; One bird: where it is and where it's heading.
(define-record-type boid
  (nongenerative schevana-boids-boid)
  (fields position velocity))

(record-writer (record-type-descriptor boid)
  (lambda (b port wr)
    (let ([p (boid-position b)] [v (boid-velocity b)])
      (fprintf port "#<boid at ~a ~a moving ~a ~a>"
               (vec-x p) (vec-y p) (vec-x v) (vec-y v)))))

;; Other is in b's field of view: fov degrees wide, centered on b's
;; heading. A boid standing still sees everything.
(define (boid-sees? b other fov)
  (let* ([v (boid-velocity b)] [h2 (vec-dot v v)])
    (or (= h2 0.0) (>= fov 360.0)
        (let* ([to-other (vec- (boid-position other) (boid-position b))]
               [lengths (sqrt (* h2 (vec-dot to-other to-other)))]
               [half-angle (/ (* (/ fov 2.0) pi) 180.0)])
          (> (vec-dot v to-other) (* (cos half-angle) lengths))))))

;; The boids in flock that b can see closer than r, not counting itself.
(define (boid-neighbors b flock r fov)
  (filter (lambda (other)
            (let ([d2 (vec-dist2 (boid-position b) (boid-position other))])
              (and (> d2 0.0) (< d2 (* r r)) (boid-sees? b other fov))))
          flock))

;; The rules' raw pushes. boid-steer weighs them and adds them up.

;; Toward the center of the neighbors.
(define (boid-cohere b others)
  (if (null? others)
      vec-zero
      (vec- (vec-mean (map boid-position others)) (boid-position b))))

;; Toward the neighbors' average velocity.
(define (boid-align b others)
  (if (null? others)
      vec-zero
      (vec- (vec-mean (map boid-velocity others)) (boid-velocity b))))

;; Away from every boid in flock closer than r, seen or not: you feel a
;; bump from behind.
(define (boid-separate b flock r)
  (let ([p (boid-position b)])
    (fold-left (lambda (sum other)
                 (let ([d2 (vec-dist2 p (boid-position other))])
                   (if (and (> d2 0.0) (< d2 (* r r)))
                       (vec+ sum (vec- p (boid-position other)))
                       sum)))
               vec-zero flock)))

;; A unit step sideways out from behind each blocker: square to the line
;; between them, on the side b is heading.
(define (boid-step-aside b blockers)
  (let ([p (boid-position b)] [v (boid-velocity b)])
    (fold-left (lambda (sum other)
                 (let* ([away (vec- p (boid-position other))]
                        [d (sqrt (vec-dot away away))]
                        [quarter-turn (make-vec (- 0.0 (vec-y away))
                                                (vec-x away))]
                        [side (if (< (vec-dot v quarter-turn) 0.0)
                                  (vec* quarter-turn -1.0)
                                  quarter-turn)])
                   (vec+ sum (vec/ side d))))
               vec-zero blockers)))

;; A unit step straight away from each blocker.
(define (boid-back-off b blockers)
  (let ([p (boid-position b)])
    (fold-left (lambda (sum other)
                 (let* ([away (vec- p (boid-position other))]
                        [d (sqrt (vec-dot away away))])
                   (vec+ sum (vec/ away d))))
               vec-zero blockers)))

(define (boid-advance b velocity)
  (make-boid (vec+ (boid-position b) velocity) velocity))

;;; ------------------------------------------------------------------ rules

;; The flocking rules, as a plist. Each rule has a weight (how hard it
;; steers) and a radius (how far a boid looks for it). view-angle is the
;; field of view in degrees, centered on the heading: 180 sees only
;; ahead, 360 all around. Blockers are boids closer than blocker-radius
;; inside a cone straight ahead, blocker-cone degrees wide.
;;
;; The speed limits belong to the screen, not the sim (boids-step leaves
;; them out). With view-angle 180, cohesion pulls stragglers forward and
;; nothing pulls the leaders back, so speed grows without bound: classic
;; boids cap it. Back off, unchecked, brakes boids almost to a standstill:
;; Flake's boids keep a minimum speed.
;;
;; These are the original settings: see only ahead, barely any
;; separation, and the blocker rules off (their radius and cone are
;; Flake's).
(define boids-default-rules
  '(cohesion 0.01     cohesion-radius 50.0
    alignment 0.05    alignment-radius 50.0
    separation 0.001  separation-radius 5.0
    view-angle 180.0
    clear-view 0.0    back-off 0.0
    blocker-radius 40.0  blocker-cone 90.0
    min-speed 1.0     max-speed 4.0))

(define (boids-rule rules key)
  (or (plist-ref rules key #f) (errorf 'boids-rule "no rule ~s" key)))

;;; ------------------------------------------------------------------ flocks

;; b's next state, given everyone's current state.
(define (boid-steer b flock rules)
  (define (rule key) (boids-rule rules key))
  (define (seen radius fov) (boid-neighbors b flock (rule radius) fov))
  ;; a rule's push times its weight, worked out only for a weight
  (define (push weight thunk)
    (let ([w (rule weight)]) (if (= w 0.0) vec-zero (vec* (thunk) w))))
  (let* ([fov (rule 'view-angle)]
         ;; blockers are in a cone ahead: a boid can't react to what it
         ;; can't see
         [cone (min (rule 'blocker-cone) fov)])
    (boid-advance b
      (fold-left vec+ (boid-velocity b)
        (list (push 'cohesion
                    (lambda () (boid-cohere b (seen 'cohesion-radius fov))))
              (push 'alignment
                    (lambda () (boid-align b (seen 'alignment-radius fov))))
              (push 'separation
                    (lambda ()
                      (boid-separate b flock (rule 'separation-radius))))
              (push 'clear-view
                    (lambda ()
                      (boid-step-aside b (seen 'blocker-radius cone))))
              (push 'back-off
                    (lambda ()
                      (boid-back-off b (seen 'blocker-radius cone)))))))))

;; The next flock: every boid steered by the same current state.
(define (boids-step flock rules)
  (map (lambda (b) (boid-steer b flock rules)) flock))

;; Three boids close together, for the tests and the REPL:
;; (boids-step boids-demo boids-default-rules)
(define boids-demo
  (list (make-boid (make-vec 0.0 0.0) (make-vec 1.0 0.0))
        (make-boid (make-vec 1.0 1.0) (make-vec 0.0 1.0))
        (make-boid (make-vec 2.0 0.0) (make-vec 1.0 1.0))))

;;; ------------------------------------------------------------------ screen

(define (boid-speed-at-least b lo)
  (let* ([v (boid-velocity b)] [s2 (vec-dot v v)])
    ;; standing still, it has no heading, so it stays put
    (if (and (> s2 0.0) (< s2 (* lo lo)))
        (make-boid (boid-position b) (vec* v (/ lo (sqrt s2))))
        b)))

(define (boid-speed-at-most b hi)
  (let* ([v (boid-velocity b)] [s2 (vec-dot v v)])
    (if (> s2 (* hi hi))
        (make-boid (boid-position b) (vec* v (/ hi (sqrt s2))))
        b)))

;; Leaving one edge of a w × h screen brings b back on the opposite edge.
(define (boid-wrap b w h)
  (let ([p (boid-position b)])
    (make-boid (make-vec (mod (vec-x p) w) (mod (vec-y p) h))
               (boid-velocity b))))

;; One frame on a w × h screen: a sim step, then the speed limits and
;; the wrap.
(define (boids-step-on flock rules w h)
  (let ([lo (boids-rule rules 'min-speed)] [hi (boids-rule rules 'max-speed)]
        [w (inexact w)] [h (inexact h)])
    (map (lambda (b)
           (boid-wrap (boid-speed-at-most (boid-speed-at-least b lo) hi) w h))
         (boids-step flock rules))))

;; n boids scattered over a w × h screen, moving in random directions.
(define (boids-scatter n w h)
  (map (lambda (i)
         (make-boid (make-vec (random (inexact w)) (random (inexact h)))
                    (make-vec (- (random 4.0) 2.0) (- (random 4.0) 2.0))))
       (iota n)))

;; n boids: the first n of flock, or all of it and random newcomers.
(define (boids-resize flock n w h)
  (let ([have (length flock)])
    (if (<= n have)
        (list-head flock n)
        (append flock (boids-scatter (- n have) w h)))))

;;; ------------------------------------------------------------------ the app

;; How often the flock steps, in ms: about 60 times a second, however
;; often the window ticks.
(define boids-step-ms 15)

(define boids-count 150)

;; A dot, with a line toward where it's heading.
(define (draw-boid b)
  (let* ([p (boid-position b)] [tip (vec+ p (vec* (boid-velocity b) 4.0))])
    (draw-line (vec-x p) (vec-y p) (vec-x tip) (vec-y tip) 'orange 2)
    (fill-circle (vec-x p) (vec-y p) 3 'yellow)))

(define-app (boids) "Boids"
  (let* ([w 900] [h 600]
         [flock (boids-scatter boids-count w h)]
         [rules boids-default-rules]
         [last-step 0])
    (open-window "Boids"
      'size (cons w h)
      'tick (lambda (win)
              (when (>= (- (now-ms) last-step) boids-step-ms)
                (set! last-step (now-ms))
                (let-values ([(w h) (window-content-size win)])
                  (set! flock (boids-step-on flock rules w h)))
                (invalidate! win)))
      'draw (lambda (win w h)
              (fill-rect 0 0 w h 'bg-0)
              (for-each draw-boid flock)))))
