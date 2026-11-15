;;; tests/units/boids-l4-flat.ss — Boids L4 (flat): in its sandbox with the
;;; GPU, its cap in its shaders, its step against the flocking rules
;;; written out plainly (tests/units.ss) on the twin, and the device against
;;; the twin; its tiles' lists, its picture against Boids' own, its keys, a
;;; flock read back, a step at its cap, and a reload. The twin is slow, so
;;; its flocks are small.

;; Boids L4 (flat) runs in its sandbox (services/grant.ss): its names,
;; forwarded here. Boids' are read from its own sandbox.
(define boids-l4-box (grant-env 'boids-l4-flat))
(define boids-box (env-environment (grant-env 'boids)))
(define (boids-own name) (top-level-value name boids-box))

(check "Boids L4 (flat) runs in its sandbox with the GPU's bundle, allowed \
        by the table, and nothing there waits for the GPU; its names and \
        shaders stay there; its rules window is no app's"
       (let* ([held (environment-symbols (env-environment boids-l4-box))]
              [world (environment-symbols *world-env*)]
              [win (boids-l4-flat)]
              [rw (find-window "Boids L4 (flat) — Rules")]
              [ok (and (equal? (env-path boids-l4-box) "/apps/boids-l4-flat")
                       (equal? (grant-requested 'boids-l4-flat)
                               '(window draw ui timers gpu))
                       (null? (grant-undecided 'boids-l4-flat))
                       (memq 'define-compute-shader held)
                       (memq 'gpu-buffer-read-then! held)
                       (not (memq 'gpu-compute! held))
                       (not (memq 'gpu-buffer-read! held))
                       (not (memq 'crew-for* held))
                       (memq 'boids-steer held)
                       (not (memq 'boids-steer world))
                       (not (memq 'boids-view world))
                       (eq? (window-app win) 'boids-l4-flat)
                       rw (not (window-app rw)))])
         (close-window! win)
         ok))

(for-each (lambda (n)
            (define-top-level-syntax n
              (forwarder-to (datum->syntax (env-anchor boids-l4-box) n))))
          '(boids-steer boids-bin boids-starts boids-view boids-steering
            boids-default-rules boids-random-rules boids-set-rule
            boids-max-count boids-tile boids-tiles-most boids-current
            boids-flock-then boids-resize!))

;; The bounds of a shader's loops, outermost first.
(define (l4-loop-bounds s)
  (let walk ([t (shader-tree s)])
    (cond [(not (pair? t)) '()]
          [(and (eq? (car t) 'loop) (pair? (cdr t)) (pair? (cddr t))
                (fixnum? (caddr t)))
           (cons (caddr t) (apply append (map walk (cdddr t))))]
          [else (apply append (map walk t))])))

(check "Boids L4 (flat)'s cap is the bound of each of its shaders' loops \
        over the boids, written in each, and the running sum's is the most \
        tiles"
       (and (equal? (l4-loop-bounds boids-steer) (list boids-max-count))
            (equal? (l4-loop-bounds boids-bin) (list boids-max-count))
            (equal? (l4-loop-bounds (gfx-paint-shader boids-view))
                    (list boids-max-count))
            (equal? (l4-loop-bounds boids-starts) (list boids-tiles-most))))

;;; ------------------------------------------------------------------ steps

;; n boids, as Boids scatters them over a w × h screen, and k steps of
;; Boids' after, each number then made a float: what a buffer holds, so
;; that the reference and the shader start from the same numbers.
(define (l4-flock n w h k rules)
  (let loop ([flock ((boids-own 'boids-scatter) n w h)] [k k])
    (if (> k 0)
        (loop ((boids-own 'boids-step-on) flock rules w h) (- k 1))
        (do ([i 0 (+ i 1)]) ((= i (flvector-length flock)) flock)
          (flvector-set! flock i (flsingle (flvector-ref flock i)))))))

;; A flock as a buffer's bytes, floats, and back.
(define (l4-bytes flock)
  (let ([bv (make-bytevector (* 4 (flvector-length flock)) 0)])
    (do ([i 0 (+ i 1)]) ((= i (flvector-length flock)) bv)
      (bytevector-ieee-single-native-set! bv (* 4 i) (flvector-ref flock i)))))

(define (l4-flvector bv)
  (let ([flock (make-flvector (quotient (bytevector-length bv) 4))])
    (do ([i 0 (+ i 1)]) ((= i (flvector-length flock)) flock)
      (flvector-set! flock i (bytevector-ieee-single-native-ref bv (* 4 i))))))

;; What a step of flock on a w × h field is handed but its buffers.
(define (l4-args flock w h rules)
  (cons* (quotient (flvector-length flock) 4) (inexact w) (inexact h)
         (boids-steering rules)))

;; A step on the twin, the shader as Scheme, in floats.
(define (l4-twin-step flock rules w h)
  (let* ([n (quotient (flvector-length flock) 4)]
         [in (l4-bytes flock)]
         [out (make-bytevector (bytevector-length in) 0)]
         [run (twin-procedure boids-steer)])
    (do ([i 0 (+ i 1)]) ((= i n))
      (apply run i 0 n 1 in out (l4-args flock w h rules)))
    (l4-flvector out)))

;; A buffer holding flock's numbers.
(define (l4-buffer flock)
  (let ([b (make-gpu-buffer (flvector-length flock))])
    (do ([i 0 (+ i 1)]) ((= i (flvector-length flock)) b)
      (gpu-buffer-set! b i (flvector-ref flock i)))))

;; A step on the device, read back.
(define (l4-device-step flock rules w h)
  (let ([in (l4-buffer flock)]
        [out (make-gpu-buffer (flvector-length flock))])
    (apply gpu-compute! boids-steer (quotient (flvector-length flock) 4) 1
           in out (l4-args flock w h rules))
    (let ([next (l4-flvector (gpu-buffer-read! out))])
      (gpu-buffer-release! in)
      (gpu-buffer-release! out)
      next)))

;; The reference's step, as an flvector.
(define (l4-ref-step flock rules w h)
  (apply flvector
         (apply append (ref-step-on (ref-flock flock) rules
                                    (inexact w) (inexact h)))))

;; Two flocks the same but for floats' rounding: each place within a
;; thousandth of a unit of the other's, on the w × h field's wrap, where 0
;; and w are one, and each velocity too. Floats keep 24 bits: a place under
;; 1,024 is rounded to 6e-5, and a velocity's sums gather a rounding from
;; each neighbor they add. Measured, the twin and the device came within
;; 7.5e-5 of the reference in places and 6.4e-5 in velocities, over 40
;; flocks of 300, scattered and flocking, under the default, the heaviest
;; and random rules. What exceeds it is a neighbor in reach for one and not
;; the other, a pair at a radius's edge within a rounding, which floats
;; would explain too, but which moves a boid by far more; none of these
;; flocks has one.
(define (l4-close? a b w h)
  (let loop ([i 0])
    (or (= i (flvector-length a))
        (let* ([d (abs (- (flvector-ref a i) (flvector-ref b i)))]
               [size (case (mod i 4) [(0) w] [(1) h] [else #f])])
          (and (< (if size (min d (- size d)) d) 1e-3)
               (loop (+ i 1)))))))

(define l4-heaviest
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

;; Flocks of 200 on a 400 × 300 field and the rules each steps by:
;; scattered, under the default rules, the heaviest and two random; and
;; flocking, 30 steps on, under the default and the heaviest.
(random-seed 2024)
(define l4-trials
  (map (lambda (rules k) (cons (l4-flock 200 400 300 k rules) rules))
       (list boids-default-rules l4-heaviest (boids-random-rules)
             (boids-random-rules) boids-default-rules l4-heaviest)
       '(0 0 0 0 30 30)))

(check "Boids L4 (flat): a step on the twin is the reference's, but for \
        floats' rounding, scattered and flocking, under the default, the \
        heaviest and random rules"
       (for-all (lambda (trial)
                  (l4-close? (l4-twin-step (car trial) (cdr trial) 400 300)
                             (l4-ref-step (car trial) (cdr trial) 400 300)
                             400.0 300.0))
                l4-trials))

;; But for rounding: a division and a root are a device's to round as it
;; likes, and a product and a sum may be fused (tests/units/shader.ss).
(check "Boids L4 (flat): a step on the device is the twin's"
       (for-all (lambda (trial)
                  (l4-close? (l4-device-step (car trial) (cdr trial) 400 300)
                             (l4-twin-step (car trial) (cdr trial) 400 300)
                             400.0 300.0))
                (list (car l4-trials) (cadr l4-trials))))

;; Boids' flmod is written from flfloor, which in floats can give the
;; field's width itself or a hair below 0. No rule here moves a boid but
;; its velocity: one just left of 0, one just short of the far edge, one
;; landing on it.
(check "Boids L4 (flat): the wrap keeps every place on the field, at its \
        edges too, and where the reference's is"
       (let* ([still (fold-left (lambda (rules key)
                                  (boids-set-rule rules key 0.0))
                                boids-default-rules
                                '(cohesion alignment separation min-speed))]
              [flock (flvector 0.0 10.0 -1e-6 0.0
                               (flsingle 399.99997) 20.0 1e-6 0.0
                               399.0 30.0 1.0 0.0
                               5.0 0.0 0.0 -1e-6)]
              [next (l4-twin-step flock still 400 300)])
         (and (for-all (lambda (i)
                         (let ([x (flvector-ref next (* 4 i))]
                               [y (flvector-ref next (+ 1 (* 4 i)))])
                           (and (<= 0.0 x) (< x 400.0)
                                (<= 0.0 y) (< y 300.0))))
                       (iota 4))
              (l4-close? next (l4-ref-step flock still 400 300) 400.0 300.0)
              (l4-close? (l4-device-step flock still 400 300) next
                         400.0 300.0))))

;;; ------------------------------------------------------------------ tiles

;; The tiles' lists of flock, across × down tiles at scale, on the twin:
;; (values starts lists), as bytevectors.
(define (l4-twin-lists flock across down scale)
  (let* ([n (quotient (flvector-length flock) 4)]
         [tiles (* across down)]
         [in (l4-bytes flock)]
         [counts (make-bytevector (* 4 tiles) 0)]
         [starts (make-bytevector (* 4 (+ tiles 1)) 0)]
         [lists (make-bytevector (* 4 (max 1 (* 16 n))) 0)]
         [bin (twin-procedure boids-bin)])
    (define (each-tile out listing)
      (do ([t 0 (+ t 1)]) ((= t tiles))
        (bin (mod t across) (quotient t across) across down in starts out
             n across listing (inexact boids-tile) scale)))
    (each-tile counts 0)
    ((twin-procedure boids-starts) 0 0 1 1 counts starts tiles)
    (each-tile lists 1)
    (values starts lists)))

;; The same on the device, read back.
(define (l4-device-lists flock across down scale)
  (let* ([n (quotient (flvector-length flock) 4)]
         [tiles (* across down)]
         [in (l4-buffer flock)]
         [counts (make-gpu-buffer tiles)]
         [starts (make-gpu-buffer (+ tiles 1))]
         [lists (make-gpu-buffer (* 16 n))]
         [tile (inexact boids-tile)])
    (gpu-compute! boids-bin across down in starts counts n across 0 tile
                  scale)
    (gpu-compute! boids-starts 1 1 counts starts tiles)
    (gpu-compute! boids-bin across down in starts lists n across 1 tile
                  scale)
    (let ([s (gpu-buffer-read! starts)] [l (gpu-buffer-read! lists)])
      (for-each gpu-buffer-release! (list in counts starts lists))
      (values s l))))

;; The n whole numbers from k up.
(define (l4-from k n) (map (lambda (i) (+ k i)) (iota n)))

;; Tile t's list, by its start and the next one's.
(define (l4-tile-list starts lists t)
  (let ([from (bytevector-s32-native-ref starts (* 4 t))]
        [to (bytevector-s32-native-ref starts (* 4 (+ t 1)))])
    (map (lambda (k) (bytevector-s32-native-ref lists (* 4 k)))
         (l4-from from (- to from)))))

(check "Boids L4 (flat): each tile lists the boids whose drawing touches \
        it, in the flock's order, however many crowd it, and the lists \
        follow one another"
       ;; 4 × 2 tiles, 16 pixels a side; a boid at rest is a dot 3 in
       ;; radius, and a pixel for its edge
       (let ([flock (apply flvector
                           (append '(8.0 8.0 0.0 0.0       ; in tile 0
                                     24.0 8.0 0.0 0.0      ; 1
                                     8.0 8.0 3.0 0.0       ; 0, 1 by its line
                                     60.0 24.0 0.0 0.0     ; 7
                                     200.0 200.0 0.0 0.0)  ; off the field
                                   (apply append           ; 300 in 6
                                          (make-list 300
                                                     '(40.0 24.0 0.0 0.0)))))])
         (let-values ([(starts lists) (l4-twin-lists flock 4 2 1.0)])
           (let ([tile (lambda (t) (l4-tile-list starts lists t))])
             (and (equal? (tile 0) '(0 2))
                  (equal? (tile 1) '(1 2))
                  (for-all (lambda (t) (null? (tile t))) '(2 3 4 5))
                  (equal? (tile 6) (l4-from 5 300))
                  (equal? (tile 7) '(3))
                  (= (bytevector-s32-native-ref starts (* 4 8)) 305))))))

(check "Boids L4 (flat): the device lists a flocking flock's tiles as the \
        twin does, at twice the scale too"
       (let ([flock (l4-flock 150 200 120 40 boids-default-rules)])
         (for-all (lambda (scale)
                    (let ([across (exact (ceiling (/ (* 200 scale) 16)))]
                          [down (exact (ceiling (/ (* 120 scale) 16)))])
                      (let-values ([(s1 l1) (l4-twin-lists flock across down
                                                           scale)]
                                   [(s2 l2) (l4-device-lists flock across down
                                                             scale)])
                        (and (bytevector=? s1 s2)
                             (for-all (lambda (t)
                                        (equal? (l4-tile-list s1 l1 t)
                                                (l4-tile-list s2 l2 t)))
                                      (iota (* across down)))))))
                  '(1.0 2.0))))

;;; ------------------------------------------------------------------ the app

;; The flock window, paused, its rules window closed, given flock.
(define (l4-window-with flock)
  (let ([win (boids-l4-flat)] [n (quotient (flvector-length flock) 4)])
    (close-window! (find-window "Boids L4 (flat) — Rules"))
    (window-call win 'key #\space '())
    (boids-resize! win n)
    (let ([b (window-ref win 'buffers)])
      (do ([i 0 (+ i 1)]) ((= i (flvector-length flock)))
        (gpu-buffer-set! (car b) i (flvector-ref flock i))
        (gpu-buffer-set! (cdr b) i (flvector-ref flock i))))
    win))

;; What win's draw handler draws, as an image.
(define (l4-drawn win)
  (let-values ([(w h) (window-content-size win)])
    (image (exact (ceiling w)) (exact (ceiling h)) 1
           (lambda ()
             (parameterize ([current-window win])
               ((hashtable-ref (window-handlers win) 'draw #f) win w h))))))

;; The core rounds to a byte at each boid it blends, the paint once at the
;; end: so a pixel where boids' edges overlap can differ by a level or two.
(check "Boids L4 (flat) draws a flocking flock as Boids does, within 4 \
        levels a channel, its field above the status line"
       (let* ([flock (l4-flock 300 760 520 100 boids-default-rules)]
              [theirs (boids)]
              [mine (l4-window-with flock)])
         (close-window! (find-window "Boids — Rules"))
         (window-set! theirs 'flock flock)
         (let ([a (l4-drawn theirs)] [b (l4-drawn mine)] [worst 0]
               [ground (color-word 'bg-0)] [dots 0])
           (close-window! theirs)
           (close-window! mine)
           (let-values ([(w h) (values (picture-w a) (picture-h a))])
             (do ([y 0 (+ y 1)]) ((= y (- h 30)))
               (do ([x 0 (+ x 1)]) ((= x w))
                 (let ([p (pixel-at a x y)] [q (pixel-at b x y)])
                   (unless (= q ground) (set! dots (+ dots 1)))
                   (for-each (lambda (s)
                               (set! worst
                                     (max worst
                                          (abs (- (fxand (fxsrl p s) 255)
                                                  (fxand (fxsrl q s) 255))))))
                             '(0 8 16))))))
           (printf "  worst ~a levels; ~a pixels not ground\n" worst dots)
           (and (<= worst 4) (> dots 1000)))))

;; The paint's twin is handed what a frame hands it: a list of one record,
;; the box, at the target's corner, no clip. But for a level: a root is
;; the device's to round, and an edge's cover can fall either side of a
;; half.
(check "Boids L4 (flat): its paint gives on the twin what the device \
        drew, within a level a channel"
       (let* ([flock (l4-flock 60 120 80 20 boids-default-rules)]
              [across 8]
              [ground #xff141820]
              [line #xffe05060]
              [dot #xfff0c040])
         (let-values ([(starts lists) (l4-device-lists flock across 5 1.0)])
           (let* ([in (l4-buffer flock)]
                  [s (make-gpu-buffer (quotient (bytevector-length starts) 4))]
                  [l (make-gpu-buffer (quotient (bytevector-length lists) 4))])
             (do ([i 0 (+ i 1)]) ((= (* 4 i) (bytevector-length starts)))
               (gpu-buffer-set! s i (bytevector-s32-native-ref starts (* 4 i))))
             (do ([i 0 (+ i 1)]) ((= (* 4 i) (bytevector-length lists)))
               (gpu-buffer-set! l i (bytevector-s32-native-ref lists (* 4 i))))
             (let ([p (image 120 80 1
                             (lambda ()
                               (fill-shader 0 0 120 80 boids-view in s l across
                                            boids-tile 1.0 ground line dot)))]
                   [twin (twin-procedure (gfx-paint-shader boids-view))]
                   [records (make-bytevector (* 4 rec-row-words) 0)]
                   [flock-bytes (l4-bytes flock)])
               (for-each gpu-buffer-release! (list in s l))
               (for-all
                (lambda (y)
                  (for-all
                   (lambda (x)
                     (let ([out (twin (+ x 0.5) (+ y 0.5) 0 records records 0 0
                                      flock-bytes starts lists across
                                      boids-tile 1.0 ground line dot)]
                           [got (pixel-at p x y)])
                       (for-all (lambda (v shift)
                                  (<= (abs (- (exact (round (* 255 v)))
                                              (fxand (fxsrl got shift) 255)))
                                      1))
                                out '(16 8 0 24))))
                   (iota 120)))
                (iota 80)))))))

(let ([win (boids-l4-flat)] [ahead 0])
  (define (key! k) (window-call win 'key k '()))
  ;; a second on from the last tick's, so that each one steps
  (define (tick!)
    (set! ahead (+ ahead 1000))
    (fluid-let ([now-ms (let ([t (now-ms)]) (lambda () (+ t ahead)))])
      (steps 1)))
  ;; the flock as it is now, from the GPU, waited for here as no handler may
  (define (flock-now)
    (let ([got #f])
      (boids-flock-then win (lambda (flock) (set! got flock)))
      (let loop ([k 0])
        (if (or got (= k 10)) got (begin (steps 1) (loop (+ k 1)))))))
  (check "Boids L4 (flat): R picks random rules, D the defaults again, and \
          the rules window closes with H"
         (begin
           (key! #\r)
           (and (not (equal? (window-ref win 'rules) boids-default-rules))
                (begin (key! #\d)
                       (eq? (window-ref win 'rules) boids-default-rules))
                (begin (key! #\h)
                       (not (find-window "Boids L4 (flat) — Rules"))))))
  (check "Boids L4 (flat): its flock comes back a step later, not at once, \
          four numbers a boid"
         (let ([got #f])
           (boids-flock-then win (lambda (flock) (set! got flock)))
           (and (not got)
                (begin (steps 2) (flvector? got))
                (= (flvector-length got) (* 4 (window-ref win 'count))))))
  (check "Boids L4 (flat) steps when ticked, and space pauses it"
         (let* ([before (flock-now)]
                [flipped (window-ref win 'flipped?)])
           (tick!)
           (let ([moved (and (not (eq? flipped (window-ref win 'flipped?)))
                             (not (equal? (flock-now) before)))])
             (key! #\space)
             (let ([paused (flock-now)])
               (tick!)
               (let ([still (equal? (flock-now) paused)])
                 (key! #\space)
                 (and moved still (not (window-error win))))))))
  (check "Boids L4 (flat) steps and draws its most boids when ticked"
         (begin
           (boids-resize! win boids-max-count)
           (tick!)
           (let ([flock (flock-now)])
             (and (= (flvector-length flock) (* 4 boids-max-count))
                  (not (window-error win))))))
  (close-window! win))

;; Its shaders are defined again, and a window open across the reload goes
;; on with them.
(check "Boids L4 (flat): loading the file again leaves it good: a step is \
        the same after, and an open window goes on stepping"
       (let* ([flock (car (car l4-trials))]
              [rules (cdr (car l4-trials))]
              [before (l4-device-step flock rules 400 300)]
              [win (boids-l4-flat)])
         (load-source "apps/boids-l4-flat.ss")
         (let ([after (l4-device-step flock rules 400 300)]
               [flipped (window-ref win 'flipped?)])
           (let ([t (+ (now-ms) 1000)])
             (fluid-let ([now-ms (lambda () t)]) (steps 1)))
           (let ([ok (and (equal? after before)
                          (not (eq? flipped (window-ref win 'flipped?)))
                          (not (window-error win)))])
             (close-window! win)
             ok))))
