;;; tests/units/boids-clear.ss — Boids (clear): in its sandbox, its
;;; flocking against the rules written out plainly (tests/units.ss) and
;;; against Boids' own, number for number; what it allocates, how it
;;; draws, and its flat records' names.

;; Boids (clear) runs in its sandbox (services/grant.ss): its names,
;; forwarded here. Boids' are read from its own sandbox.
(define boids-clear-box (grant-env 'boids-clear))
(define boids-box (env-environment (grant-env 'boids)))
(define (boids-own name) (top-level-value name boids-box))

(check "Boids (clear) runs in its sandbox, holding its bundles, allowed by \
        the table, and its names stay there, its worklets too"
       (let* ([held (environment-symbols (env-environment boids-clear-box))]
              [world (environment-symbols *world-env*)]
              [win (boids-clear)]
              [rw (find-window "Boids (clear) — Rules")]
              [ok (and (equal? (env-path boids-clear-box) "/apps/boids-clear")
                       (equal? (grant-requested 'boids-clear)
                               '(window draw ui timers crew))
                       (null? (grant-undecided 'boids-clear))
                       (memq 'boids-step held) (memq 'boids-steer! held)
                       (memq 'boid-x held)
                       (not (memq 'boid-x world))
                       (not (memq 'define-flat-record world))
                       (worklet? (top-level-value
                                  'boids-steer!
                                  (env-environment boids-clear-box)))
                       (worklet? (top-level-value
                                  'boids-keep-on-screen!
                                  (env-environment boids-clear-box)))
                       (eq? (window-app win) 'boids-clear)
                       rw (not (window-app rw)))])
         (close-window! win)
         ok))

(for-each (lambda (n)
            (define-top-level-syntax n
              (forwarder-to (datum->syntax (env-anchor boids-clear-box) n))))
          '(boid-x boid-y boid-vx boid-vy boid-x-set! boids-size boids-steer!
            boids-steering boids-step boids-demo boids-default-rules
            boids-scatter boids-step-on boids-random-rules
            boids-keep-on-screen! boids-set-rule flat-names->flvector))

(check "Boids (clear): the demo's first step is the Niva version's"
       (equal? (flvector->list (boids-step boids-demo boids-default-rules))
               '(0.9869999999999999 0.054 0.9869999999999999 0.054
                 1.0 2.002 0.0 1.002
                 3.003 0.999 1.003 0.999)))

(check "Boids (clear): a boid's names are its four numbers, in order"
       (let ([flock (flvector 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0)])
         (boid-x-set! flock 1 9.0)
         (and (= (boids-size flock) 2)
              (equal? (map (lambda (get) (get flock 1))
                           (list boid-x boid-y boid-vx boid-vy))
                      '(9.0 6.0 7.0 8.0)))))

;; The rules a step reads, by name, are the numbers Boids' step reads by
;; place: the names point where the numbers were.
(random-seed 2024)
(check "Boids (clear): its steering's names hold Boids' steering's numbers"
       (for-all (lambda (trial)
                  (let ([rules (if (= trial 0)
                                   boids-default-rules
                                   (boids-random-rules))])
                    (equal? (flvector->list (boids-steering rules))
                            (flvector->list ((boids-own 'boids-steering)
                                             rules)))))
                (iota 50)))

(check "Boids (clear): its records of names miss no field, and add none"
       (and (raises? (lambda ()
                       (flat-names->flvector 'steering '(a b) '(a 1.0))))
            (raises? (lambda ()
                       (flat-names->flvector 'steering '(a) '(a 1.0 c 2.0))))
            (equal? (flvector->list
                     (flat-names->flvector 'steering '(a b) '(b 2.0 a 1.0)))
                    '(1.0 2.0))))

(random-seed 2024)
(check "Boids (clear)'s steps are the reference's to the last digit, under \
        the default rules and random ones, on the crew and on the caller \
        alone"
       (let ([size (crew-size)])
         (define (agree? workers)
           (crew-size-set! workers)
           (for-all
            (lambda (trial)
              (let ([rules (if (= trial 0)
                               boids-default-rules
                               (boids-random-rules))]
                    [flock (boids-scatter 300 400 300)])
                (equal? (ref-flock (boids-step-on flock rules 400 300))
                        (ref-step-on (ref-flock flock) rules 400.0 300.0))))
            (iota 12)))
         (let ([ok (and (agree? 8) (agree? 0))])
           (crew-size-set! size)
           ok)))

;; Bigger flocks than the reference can step soon, under the heaviest
;; rules too, every number the same as Boids'.
(random-seed 2024)
(check "Boids (clear)'s steps are Boids', number for number, 30 frames of \
        2,000 boids, under the default rules and the heaviest"
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
            (let loop ([flock (boids-scatter 2000 1200 700)] [k 0])
              (or (= k 30)
                  (let ([mine (boids-step-on flock rules 1200 700)]
                        [theirs ((boids-own 'boids-step-on) flock rules
                                 1200 700)])
                    (and (equal? (flvector->list mine)
                                 (flvector->list theirs))
                         (loop mine (+ k 1)))))))
          (list boids-default-rules heaviest))))

(check "Boids (clear): a step's worklet allocates only its sums, and the \
        wrap what Boids' does (Chez's flmod boxes)"
       (let* ([flock (boids-scatter 300 200 200)]
              [next (make-flvector (flvector-length flock))]
              [rules (boids-steering
                      (fold-left (lambda (rules key)
                                   (boids-set-rule rules key 0.5))
                                 (boids-set-rule boids-default-rules
                                                 'view-angle 300.0)
                                 '(separation clear-view back-off)))])
         (define (bytes-of thunk)
           (thunk)
           (let ([b0 (bytes-total)])
             (do ([i 0 (fx+ i 1)]) ((fx= i 20)) (thunk))
             (- (bytes-total) b0)))
         (let ([steer (bytes-of
                       (lambda () (boids-steer! 0 300 flock next rules)))]
               [wrap (bytes-of
                      (lambda ()
                        (boids-keep-on-screen! 0 300 next 1.0 4.0
                                               200.0 200.0)))]
               [their-wrap (bytes-of
                            (lambda ()
                              ((boids-own 'boids-keep-on-screen!)
                               next 1.0 4.0 200.0 200.0)))])
           (printf "  ~a bytes in 20 steps of 300 boids, ~a in 20 wraps, \
                    Boids' ~a\n" steer wrap their-wrap)
           (and (< steer 4096) (< wrap (+ their-wrap 1024))))))

;; Not yet ticked, both status lines say the same.
(check "Boids (clear) draws a flock as Boids does, pixel for pixel"
       (let* ([theirs (boids)]
              [mine (boids-clear)]
              [flock (boids-scatter 150 760 520)])
         (window-set! theirs 'flock flock)
         (window-set! mine 'flock flock)
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

(let ([win (boids-clear)])
  (define (key! k) (window-call win 'key k '()))
  (check "Boids (clear): R picks random rules, D the defaults again, and \
          the rules window closes with H"
         (begin
           (key! #\r)
           (and (not (equal? (window-ref win 'rules) boids-default-rules))
                (begin (key! #\d)
                       (eq? (window-ref win 'rules) boids-default-rules))
                (begin (key! #\h)
                       (not (find-window "Boids (clear) — Rules"))))))
  (check "Boids (clear) steps when ticked"
         (let ([before (window-ref win 'flock)])
           (fluid-let ([now-ms (let ([t (now-ms)]) (lambda () (+ t 1000)))])
             (steps 1))
           (not (eq? (window-ref win 'flock) before))))
  (close-window! win))

;; A flock is plain numbers, and the flat records' macros are declared
;; again: loading the file again leaves it good.
(check "Boids (clear): a flock made before the file loads again still \
        steps after, the same"
       (let* ([flock (boids-scatter 100 300 300)]
              [before (flvector->list (boids-step flock boids-default-rules))])
         (load-source "apps/boids-clear.ss")
         (equal? (flvector->list (boids-step flock boids-default-rules))
                 before)))
