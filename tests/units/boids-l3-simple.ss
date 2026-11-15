;;; tests/units/boids-l3-simple.ss — Boids L3 (simple): in its sandbox,
;;; its step's worklets, its flocking against the rules written out plainly
;;; (tests/units.ss), on the crew and on the caller alone, and against
;;; Boids' own, number for number; how it draws, its keys, a step at its
;;; cap, and a flock kept through a reload. Its step is slow, so the flocks
;;; here are small and few.

;; Boids L3 (simple) runs in its sandbox (services/grant.ss): its names,
;; forwarded here. Boids' are read from its own sandbox.
(define boids-l3-box (grant-env 'boids-l3-simple))
(define boids-box (env-environment (grant-env 'boids)))
(define (boids-own name) (top-level-value name boids-box))
(define (boids-l3-own name)
  (top-level-value name (env-environment boids-l3-box)))

(check "Boids L3 (simple) runs in its sandbox, holding its bundles, allowed \
        by the table, its step's procedures worklets, and its names stay \
        there"
       (let* ([held (environment-symbols (env-environment boids-l3-box))]
              [world (environment-symbols *world-env*)]
              [win (boids-l3-simple)]
              [rw (find-window "Boids L3 (simple) — Rules")]
              [ok (and (equal? (env-path boids-l3-box)
                               "/apps/boids-l3-simple")
                       (equal? (grant-requested 'boids-l3-simple)
                               '(window draw ui timers crew))
                       (null? (grant-undecided 'boids-l3-simple))
                       (for-all (lambda (n) (worklet? (boids-l3-own n)))
                                '(make-vec vec+ vec-mean boid-neighbors
                                  boid-cohere boids-rule boid-steer
                                  boids-steer-slice!))
                       (not (worklet? (boids-l3-own 'boids-step)))
                       (memq 'boid-neighbors held)
                       (not (memq 'boid-neighbors world))
                       (not (memq 'boids-steer-slice! world))
                       (eq? (window-app win) 'boids-l3-simple)
                       rw (not (window-app rw)))])
         (close-window! win)
         ok))

(for-each (lambda (n)
            (define-top-level-syntax n
              (forwarder-to (datum->syntax (env-anchor boids-l3-box) n))))
          '(make-vec vec-x vec-y make-boid boid-position boid-velocity
            boids-step boids-demo boids-default-rules boids-scatter
            boids-step-on boids-random-rules boids-max-count))

;; A flock as Boids' flvector, four numbers a boid, and back.
(define (l3->flat flock)
  (apply flvector
         (apply append
                (map (lambda (b)
                       (let ([p (boid-position b)] [v (boid-velocity b)])
                         (list (vec-x p) (vec-y p) (vec-x v) (vec-y v))))
                     flock))))

(define (flat->l3 flat)
  (map (lambda (b)
         (make-boid (make-vec (car b) (cadr b))
                    (make-vec (caddr b) (cadddr b))))
       (ref-flock flat)))

(check "Boids L3 (simple): the demo's first step is the Niva version's"
       (equal? (flvector->list
                (l3->flat (boids-step boids-demo boids-default-rules)))
               '(0.9869999999999999 0.054 0.9869999999999999 0.054
                 1.0 2.002 0.0 1.002
                 3.003 0.999 1.003 0.999)))

(random-seed 2024)
(check "Boids L3 (simple)'s steps are the reference's to the last digit, \
        under the default rules and random ones, on the crew and on the \
        caller alone"
       (let ([size (crew-size)])
         (define (agree? workers)
           (crew-size-set! workers)
           (for-all
            (lambda (trial)
              (let ([rules (if (= trial 0)
                               boids-default-rules
                               (boids-random-rules))]
                    [flock (boids-scatter 300 400 300)])
                (equal? (ref-flock
                         (l3->flat (boids-step-on flock rules 400 300)))
                        (ref-step-on (ref-flock (l3->flat flock)) rules
                                     400.0 300.0))))
            (iota 8)))
         (let ([ok (and (agree? 8) (agree? 0))])
           (crew-size-set! size)
           ok)))

;; The same flock, frame after frame, stays the same as Boids' own.
(random-seed 2024)
(check "Boids L3 (simple)'s steps are Boids', number for number, 8 frames \
        of 300 boids, under the default rules and the heaviest"
       (let ([heaviest '(cohesion 0.2
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
                         max-speed 4.0)])
         (for-all
          (lambda (rules)
            (let ([start ((boids-own 'boids-scatter) 300 400 300)])
              (let loop ([theirs start] [mine (flat->l3 start)] [k 0])
                (or (= k 8)
                    (let ([theirs ((boids-own 'boids-step-on) theirs rules
                                   400 300)]
                          [mine (boids-step-on mine rules 400 300)])
                      (and (equal? (flvector->list (l3->flat mine))
                                   (flvector->list theirs))
                           (loop theirs mine (+ k 1))))))))
          (list boids-default-rules heaviest))))

;; Not yet ticked, both status lines say the same.
(check "Boids L3 (simple) draws a flock as Boids does, pixel for pixel"
       (let* ([theirs (boids)]
              [mine (boids-l3-simple)]
              [flock ((boids-own 'boids-scatter) 150 760 520)])
         (window-set! theirs 'flock flock)
         (window-set! mine 'flock (flat->l3 flock))
         (let-values ([(w h) (window-content-size mine)])
           (define (drawn win)
             (image (exact (ceiling w)) (exact (ceiling h)) 1
                    (lambda ()
                      (parameterize ([current-window win])
                        ((hashtable-ref (window-handlers win) 'draw #f)
                         win w h)))))
           (let ([a (drawn theirs)] [b (drawn mine)])
             (close-window! theirs)
             (close-window! mine)
             (same-bytes? a b)))))

(let ([win (boids-l3-simple)] [ahead 0])
  (define (key! k) (window-call win 'key k '()))
  ;; a second on from the last tick's, so that each one steps
  (define (tick!)
    (set! ahead (+ ahead 1000))
    (fluid-let ([now-ms (let ([t (now-ms)]) (lambda () (+ t ahead)))])
      (steps 1)))
  (check "Boids L3 (simple): R picks random rules, D the defaults again, \
          and the rules window closes with H"
         (begin
           (key! #\r)
           (and (not (equal? (window-ref win 'rules) boids-default-rules))
                (begin (key! #\d)
                       (eq? (window-ref win 'rules) boids-default-rules))
                (begin (key! #\h)
                       (not (find-window "Boids L3 (simple) — Rules"))))))
  (check "Boids L3 (simple) steps when ticked"
         (let ([before (window-ref win 'flock)])
           (tick!)
           (and (not (eq? (window-ref win 'flock) before))
                (not (window-error win)))))
  ;; A step on core1 has the crew's 100 ms and the handler's fuel.
  (check "Boids L3 (simple) steps its most boids when ticked, on the crew"
         (let ([size (crew-size)])
           (crew-size-set! 8)
           (window-set! win 'flock (boids-scatter boids-max-count 760 520))
           (let ([before (window-ref win 'flock)])
             (tick!)
             (crew-size-set! size)
             (and (not (eq? (window-ref win 'flock) before))
                  (= (length (window-ref win 'flock)) boids-max-count)
                  (not (window-error win))))))
  (close-window! win))

;; A flock is pairs, and the worklets are defined again: loading the file
;; again leaves it good.
(check "Boids L3 (simple): a flock made before the file loads again still \
        steps after, the same"
       (let* ([flock (boids-scatter 100 300 300)]
              [before (l3->flat (boids-step flock boids-default-rules))])
         (load-source "apps/boids-l3-simple.ss")
         (equal? (flvector->list
                  (l3->flat (boids-step flock boids-default-rules)))
                 (flvector->list before))))
