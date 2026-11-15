;;; tests/units/boids-simple.ss — Boids (simple): in its sandbox, its
;;; flocking against the rules written out plainly (tests/units.ss) and
;;; against Boids' own, number for number; how it draws, its keys, and a
;;; flock kept through a reload. Its step is slow on purpose, so the
;;; flocks here are small and few.

;; Boids (simple) runs in its sandbox (services/grant.ss): its names,
;; forwarded here. Boids' are read from its own sandbox.
(define boids-simple-box (grant-env 'boids-simple))
(define boids-box (env-environment (grant-env 'boids)))
(define (boids-own name) (top-level-value name boids-box))

(check "Boids (simple) runs in its sandbox, holding its bundles, allowed by \
        the table, without the crew, and its names stay there"
       (let* ([held (environment-symbols (env-environment boids-simple-box))]
              [world (environment-symbols *world-env*)]
              [win (boids-simple)]
              [rw (find-window "Boids (simple) — Rules")]
              [ok (and (equal? (env-path boids-simple-box) "/apps/boids-simple")
                       (equal? (grant-requested 'boids-simple)
                               '(window draw ui timers))
                       (null? (grant-undecided 'boids-simple))
                       (memq 'boids-step held) (memq 'boid-neighbors held)
                       (memq 'vec-mean held)
                       (not (memq 'crew-for* held))
                       (not (memq 'boid-neighbors world))
                       (not (memq 'vec-mean world))
                       (eq? (window-app win) 'boids-simple)
                       rw (not (window-app rw)))])
         (close-window! win)
         ok))

(for-each (lambda (n)
            (define-top-level-syntax n
              (forwarder-to (datum->syntax (env-anchor boids-simple-box) n))))
          '(make-vec vec-x vec-y make-boid boid? boid-position boid-velocity
            boids-step boids-demo boids-default-rules boids-scatter
            boids-step-on boids-random-rules))

;; A flock as Boids' flvector, four numbers a boid, and back.
(define (simple->flat flock)
  (apply flvector
         (apply append
                (map (lambda (b)
                       (let ([p (boid-position b)] [v (boid-velocity b)])
                         (list (vec-x p) (vec-y p) (vec-x v) (vec-y v))))
                     flock))))

(define (flat->simple flat)
  (map (lambda (b)
         (make-boid (make-vec (car b) (cadr b))
                    (make-vec (caddr b) (cadddr b))))
       (ref-flock flat)))

(check "Boids (simple): the demo's first step is the Niva version's"
       (equal? (flvector->list
                (simple->flat (boids-step boids-demo boids-default-rules)))
               '(0.9869999999999999 0.054 0.9869999999999999 0.054
                 1.0 2.002 0.0 1.002
                 3.003 0.999 1.003 0.999)))

(random-seed 2024)
(check "Boids (simple)'s steps are the reference's to the last digit, under \
        the default rules and random ones"
       (for-all
        (lambda (trial)
          (let ([rules (if (= trial 0)
                           boids-default-rules
                           (boids-random-rules))]
                [flock (boids-scatter 300 400 300)])
            (equal? (ref-flock
                     (simple->flat (boids-step-on flock rules 400 300)))
                    (ref-step-on (ref-flock (simple->flat flock)) rules
                                 400.0 300.0))))
        (iota 12)))

;; The same flock, frame after frame, stays the same as Boids' own.
(random-seed 2024)
(check "Boids (simple)'s steps are Boids', number for number, 8 frames of \
        300 boids, under the default rules and the heaviest"
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
              (let loop ([theirs start] [mine (flat->simple start)] [k 0])
                (or (= k 8)
                    (let ([theirs ((boids-own 'boids-step-on) theirs rules
                                   400 300)]
                          [mine (boids-step-on mine rules 400 300)])
                      (and (equal? (flvector->list (simple->flat mine))
                                   (flvector->list theirs))
                           (loop theirs mine (+ k 1))))))))
          (list boids-default-rules heaviest))))

;; Not yet ticked, both status lines say the same.
(check "Boids (simple) draws a flock as Boids does, pixel for pixel"
       (let* ([theirs (boids)]
              [mine (boids-simple)]
              [flock ((boids-own 'boids-scatter) 150 760 520)])
         (window-set! theirs 'flock flock)
         (window-set! mine 'flock (flat->simple flock))
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

(let ([win (boids-simple)])
  (define (key! k) (window-call win 'key k '()))
  (check "Boids (simple): R picks random rules, D the defaults again, and \
          the rules window closes with H"
         (begin
           (key! #\r)
           (and (not (equal? (window-ref win 'rules) boids-default-rules))
                (begin (key! #\d)
                       (eq? (window-ref win 'rules) boids-default-rules))
                (begin (key! #\h)
                       (not (find-window "Boids (simple) — Rules"))))))
  (check "Boids (simple) steps when ticked"
         (let ([before (window-ref win 'flock)])
           (fluid-let ([now-ms (let ([t (now-ms)]) (lambda () (+ t 1000)))])
             (steps 1))
           (and (not (eq? (window-ref win 'flock) before))
                (not (window-error win)))))
  (close-window! win))

;; The records are nongenerative: loading the file again defines them
;; again, the same, so boids made before are boids after.
(check "Boids (simple): a flock made before the file loads again still \
        steps after, the same"
       (let* ([flock (boids-scatter 100 300 300)]
              [before (simple->flat (boids-step flock boids-default-rules))])
         (load-source "apps/boids-simple.ss")
         (and (for-all boid? flock)
              (equal? (flvector->list
                       (simple->flat (boids-step flock boids-default-rules)))
                      (flvector->list before)))))
