;;; apps/boids-l4-flat.ss — Boids L4 (flat): Boids' step on the GPU. In
;;; docs/BOIDS-CLEAR.md's terms it is level 4, the GPU, written flat, as
;;; Boids is at level 3: a flock is numbers side by side, four a boid, and
;;; one loop goes over every pair. The flock lives in two GPU buffers; a
;;; compute shader steps it each frame, from one into the other, and a paint
;;; draws it. The CPU scatters boids, sets the rules and asks for the
;;; shaders.
;;;
;;; What the GPU asks of it:
;;; - Only 32-bit numbers, in buffers: no lists, records or procedures as
;;;   values. So a flock is Boids' flvector as floats, and a step is the
;;;   reference's to within what floats round, not to the last digit
;;;   (tests/units/boids-l4-flat.ss).
;;; - The loop over the other boids is do-bounded, its bound a number
;;;   written in the shader: the most boids is in the code
;;;   (boids-max-count).
;;; - No flmod: the wrap is written with flfloor.
;;; - A handler can't wait for the GPU: what comes back, comes back later,
;;;   through gpu-buffer-read-then! (boids-flock-then).
;;;
;;; Boids is Craig Reynolds' flocking simulation (1987), ported from Boids
;;; in Niva (github.com/dharmatech/boids-niva), which was in turn inspired
;;; by Boids in Aloe, a Lisp whose kernel is sending a message
;;; (github.com/dharmatech/2026-09-02-aloe-racket). Besides Reynolds' three
;;; rules (cohesion, alignment, separation), two react to blockers, boids
;;; close and straight ahead: clear view steps aside, back off steps away.
;;; They come from Gary William Flake's "The Computational Beauty of
;;; Nature" (MIT Press, 1998), written from his description, not copied
;;; from his (GPL) source.
;;;
;;; The window keeps the flock's buffers and its rules:
;;; (boids-flock-then win (lambda (flock) (set! seen flock)))  ; an flvector
;;; (window-set! win 'rules (boids-random-rules))
;;; R: random rules · D: the defaults · S: scatter · Space: pause
;;; H: the rules window, beside the flock (it closes with the flock)
;;;
;;; It runs in a sandbox (services/grant.ss), where the names above are and
;;; its shaders touch only the buffers it hands them: a REPL in
;;; /apps/boids-l4-flat has them.

(app-requests window draw ui timers gpu)

;;; ------------------------------------------------------------------ rules

;; The flocking rules, as a plist. Each rule has a weight (how hard it
;; steers) and a radius (how far a boid looks for it). view-angle is the
;; field of view in degrees, centered on the heading: 180 sees only
;; ahead, 360 all around. Blockers are boids closer than blocker-radius
;; inside a cone straight ahead, blocker-cone degrees wide.
;;
;; With view-angle 180, cohesion pulls stragglers forward and nothing pulls
;; the leaders back, so speed grows without bound: classic boids cap it.
;; Back off, unchecked, brakes boids almost to a standstill: Flake's boids
;; keep a minimum speed.
;;
;; These are the original settings: see only ahead, barely any
;; separation, and the blocker rules off (their radius and cone are
;; Flake's).
(define boids-default-rules
  '(cohesion 0.01
    cohesion-radius 50.0
    alignment 0.05
    alignment-radius 50.0
    separation 0.001
    separation-radius 5.0
    view-angle 180.0
    clear-view 0.0
    back-off 0.0
    blocker-radius 40.0
    blocker-cone 90.0
    min-speed 1.0
    max-speed 4.0))

(define (boids-rule rules key)
  (let ([at (memq key rules)])
    (if at (cadr at) (errorf 'boids-rule "no rule ~s" key))))

;; Each rule's range, in the order the rules window shows them. A weight
;; starts at 0, and its useful values span orders of magnitude.
(define boids-ranges
  '((cohesion 0.0 0.2 weight)
    (cohesion-radius 0.0 200.0)
    (alignment 0.0 1.0 weight)
    (alignment-radius 0.0 200.0)
    (separation 0.0 1.0 weight)
    (separation-radius 0.0 50.0)
    (view-angle 0.0 360.0)
    (clear-view 0.0 1.0 weight)
    (back-off 0.0 1.0 weight)
    (blocker-radius 0.0 100.0)
    (blocker-cone 0.0 180.0)
    (min-speed 0.0 5.0)
    (max-speed 0.5 10.0)))

;; Rules picked at random within their ranges. A weight is picked on a
;; cube, so that small ones come up about as often as they would on a
;; log scale. Max speed comes after min speed and is picked from it up,
;; so the two never cross.
(define (boids-random-rules)
  (fold-left
   (lambda (rules range)
     (apply (lambda (key lo hi . weight)
              (let* ([lo (if (eq? key 'max-speed)
                             (max lo (boids-rule rules 'min-speed))
                             lo)]
                     [r (random 1.0)]
                     [t (if (null? weight) r (* r r r))])
                (append rules (list key (+ lo (* t (- hi lo)))))))
            range))
   '() boids-ranges))

;; Rules with key's value v, the others as they were. Min and max speed
;; can't cross: setting one past the other pushes the other along.
(define (boids-set-rule rules key v)
  (define (with rules key v)
    (cond [(null? rules) (errorf 'boids-set-rule "no rule ~s" key)]
          [(eq? (car rules) key) (cons* key v (cddr rules))]
          [else (cons* (car rules) (cadr rules) (with (cddr rules) key v))]))
  (let ([rules (with rules key v)])
    (case key
      [(min-speed) (if (> v (boids-rule rules 'max-speed))
                       (with rules 'max-speed v)
                       rules)]
      [(max-speed) (if (< v (boids-rule rules 'min-speed))
                       (with rules 'min-speed v)
                       rules)]
      [else rules])))

;;; ------------------------------------------------------------------ the GPU

;; The most boids, the bound of the shaders' loops over them: a loop's bound
;; is a number written in the shader, not one it is handed, so it is
;; written three times below, and the tests hold the three to this.
;;
;; A frame's GPU work grows toward the count squared. At 8,192, measured
;; here on a laptop's GPU (a GTX 1650 Ti) at 100%, a step takes 4.4 ms with
;; the default rules and 6.7 with the heaviest, and listing and drawing the
;; flock 4.1: 8.5 to 11 ms, within a 60 Hz frame. Twice as many take 27 and
;; 33, two frames, and the GPU draws the whole desktop too, which would wait
;; for them. On lavapipe, the CPU's Vulkan, 4,096 take 34 to 44 ms a frame
;; and 8,192 100 to 180 (tests/bench-boids-clear.ss).
(define boids-max-count 8192)

;; What a step's shader is handed of the rules, in its parameters' order,
;; worked out once, as Boids' boids-steering: the weights; the cosines of
;; half a field of view and of half the blockers' cone, which is in view
;; too, -2 for all around, which any heading beats (Boids has -inf, a
;; number the shader language doesn't speak of); each rule's reach, the
;; squared radius, 0 for a rule without weight, so that nothing is in
;; reach; the speed limits.
(define (boids-steering rules)
  (define (rule key) (inexact (boids-rule rules key)))
  (define (reach weight radius)
    (if (= (rule weight) 0.0) 0.0 (* (rule radius) (rule radius))))
  (define (half-cosine angle)
    (if (>= angle 360.0) -2.0 (cos (/ (* (/ angle 2.0) pi) 180.0))))
  (let ([fov (rule 'view-angle)])
    (list (rule 'cohesion)
          (rule 'alignment)
          (rule 'separation)
          (rule 'clear-view)
          (rule 'back-off)
          (half-cosine fov)
          (half-cosine (min (rule 'blocker-cone) fov))
          (reach 'cohesion 'cohesion-radius)
          (reach 'alignment 'alignment-radius)
          (reach 'separation 'separation-radius)
          (reach 'clear-view 'blocker-radius)
          (reach 'back-off 'blocker-radius)
          (rule 'min-speed)
          (rule 'max-speed))))

;; A step of the flock on a w × h field, as Boids' boids-step-on: each boid
;; steered by everyone's current state into next, its speed kept within the
;; limits and its place wrapped round the field, a cell a boid. Boids' sums,
;; an flvector, are the loop's variables here: a loop has no body, only its
;; variables' steps, so each sum's step makes its own test. What a pair
;; adds is worked out again for each sum, which the GPU's compiler does
;; once.
(define-compute-shader (boids-steer i row)
  ([flock fl-buffer]
   [next fl-buffer write]
   [n fixnum]
   [w flonum]
   [h flonum]
   [cohesion flonum]
   [alignment flonum]
   [separation flonum]
   [clear-view flonum]
   [back-off flonum]
   [view flonum]                ; the cosines of half the field of view
   [blocker-view flonum]        ; and of half the blockers' cone
   [cohesion-reach flonum]      ; squared
   [alignment-reach flonum]
   [separation-reach flonum]
   [clear-view-reach flonum]
   [back-off-reach flonum]
   [min-speed flonum]
   [max-speed flonum])
  (define (at j k) (fl-ref flock (fx+ (fx* 4 j) k)))
  ;; sum, and v if it counts
  (define (add sum counts? v) (if counts? (fl+ sum v) sum))
  ;; toward the mean of a sum over its count, from own, by weight
  (define (toward sum count own weight)
    (if (fl> count 0.0) (fl* (fl- (fl/ sum count) own) weight) 0.0))
  ;; v brought into [0, size), as Boids' flmod, which a shader hasn't: in
  ;; floats, v - size × floor(v / size) can come out as size itself, or a
  ;; hair below 0, and on the wrap both are 0
  (define (wrap v size)
    (let ([m (fl- v (fl* size (flfloor (fl/ v size))))])
      (if (and (fl>= m 0.0) (fl< m size)) m 0.0)))
  (let* ([x (at i 0)]
         [y (at i 1)]
         [vx (at i 2)]
         [vy (at i 3)]
         [h2 (fl+ (fl* vx vx) (fl* vy vy))])
    (define (dx j) (fl- (at j 0) x))
    (define (dy j) (fl- (at j 1) y))
    (define (d2 j) (fl+ (fl* (dx j) (dx j)) (fl* (dy j) (dy j))))
    ;; not itself, and closer than reach (squared)
    (define (near? j reach) (and (fl> (d2 j) 0.0) (fl< (d2 j) reach)))
    ;; and in the field of view whose half-cosine is c: a boid standing
    ;; still sees everything
    (define (seen? j reach c)
      (and (near? j reach)
           (or (fl= h2 0.0)
               (fl> (fl+ (fl* vx (dx j)) (fl* vy (dy j)))
                    (fl* c (flsqrt (fl* h2 (d2 j))))))))
    (define (away-x j) (fl- 0.0 (dx j)))
    (define (away-y j) (fl- 0.0 (dy j)))
    ;; square to the line between them, on the side it heads: 1 or -1
    (define (side j)
      (if (fl< (fl+ (fl* vx (fl- 0.0 (away-y j))) (fl* vy (away-x j))) 0.0)
          -1.0
          1.0))
    (do-bounded 8192
        ([j 0 (fx+ j 1)]
         ;; toward the center of the neighbors
         [center-x 0.0 (add center-x (seen? j cohesion-reach view) (at j 0))]
         [center-y 0.0 (add center-y (seen? j cohesion-reach view) (at j 1))]
         [centered 0.0 (add centered (seen? j cohesion-reach view) 1.0)]
         ;; toward their average velocity
         [heading-x 0.0 (add heading-x (seen? j alignment-reach view)
                             (at j 2))]
         [heading-y 0.0 (add heading-y (seen? j alignment-reach view)
                             (at j 3))]
         [headed 0.0 (add headed (seen? j alignment-reach view) 1.0)]
         ;; away from every boid this close, seen or not: you feel a bump
         ;; from behind
         [crowd-x 0.0 (add crowd-x (near? j separation-reach) (away-x j))]
         [crowd-y 0.0 (add crowd-y (near? j separation-reach) (away-y j))]
         ;; a unit step sideways out from behind each blocker
         [aside-x 0.0 (add aside-x (seen? j clear-view-reach blocker-view)
                           (fl/ (fl* (fl- 0.0 (away-y j)) (side j))
                                (flsqrt (d2 j))))]
         [aside-y 0.0 (add aside-y (seen? j clear-view-reach blocker-view)
                           (fl/ (fl* (away-x j) (side j)) (flsqrt (d2 j))))]
         ;; a unit step straight away from each blocker
         [back-x 0.0 (add back-x (seen? j back-off-reach blocker-view)
                          (fl/ (away-x j) (flsqrt (d2 j))))]
         [back-y 0.0 (add back-y (seen? j back-off-reach blocker-view)
                          (fl/ (away-y j) (flsqrt (d2 j))))])
      ((fx>= j n)
       (let* ([nvx (fl+ vx
                        (toward center-x centered x cohesion)
                        (toward heading-x headed vx alignment)
                        (fl* crowd-x separation)
                        (fl* aside-x clear-view)
                        (fl* back-x back-off))]
              [nvy (fl+ vy
                        (toward center-y centered y cohesion)
                        (toward heading-y headed vy alignment)
                        (fl* crowd-y separation)
                        (fl* aside-y clear-view)
                        (fl* back-y back-off))]
              ;; the speed limits: at least min-speed, but standing still,
              ;; it has no heading, so it stays put; then at most max-speed
              [s2 (fl+ (fl* nvx nvx) (fl* nvy nvy))]
              [k (if (and (fl> s2 0.0) (fl< s2 (fl* min-speed min-speed)))
                     (fl/ min-speed (flsqrt s2))
                     1.0)]
              [lx (fl* nvx k)]
              [ly (fl* nvy k)]
              [s2 (fl+ (fl* lx lx) (fl* ly ly))]
              [k (if (fl> s2 (fl* max-speed max-speed))
                     (fl/ max-speed (flsqrt s2))
                     1.0)]
              [o (fx* 4 i)])
         ;; moved by the velocity before the limits, as in Boids
         (fl-set! next o (wrap (fl+ x nvx) w))
         (fl-set! next (fx+ o 1) (wrap (fl+ y nvy) h))
         (fl-set! next (fx+ o 2) (fl* lx k))
         (fl-set! next (fx+ o 3) (fl* ly k)))))))

;; Device pixels a tile's side, and the most tiles a field has, the bound
;; of the running sum's loop: an 8K screen's, 7680 × 4320, at 16.
(define boids-tile 16)
(define boids-tiles-most 131072)

;; Each tile across × down of the field counts the boids whose drawing
;; touches it, into counts; or, given where its list starts, lists them
;; there in the flock's order, into lists. It is one shader, so that the
;; lists are what was counted. A shader has no list that grows and no word
;; two cells may add to, so the lists come of a count, a running sum
;; (boids-starts) and the listing, every boid in every tile it touches. A
;; tile writes only its own words, so that no two cells touch one. A boid's
;; drawing is its dot and its heading's line, 4 steps long; scale is device
;; pixels a unit.
(define-compute-shader (boids-bin tx ty)
  ([flock fl-buffer]
   [starts fx-buffer]
   [out fx-buffer write]
   [n fixnum]
   [across fixnum]
   [listing fixnum]               ; 0 to count, 1 to list
   [tile flonum]
   [scale flonum])
  (define (at j k) (fl-ref flock (fx+ (fx* 4 j) k)))
  (let* ([t (fx+ tx (fx* ty across))]
         [start (fx-ref starts t)]
         ;; the tile, grown by what a drawing reaches past its points: the
         ;; dot's radius, and a pixel for its edge
         [reach (fl+ (fl* 3.0 scale) 1.0)]
         [x0 (fl- (fl* (fixnum->flonum tx) tile) reach)]
         [y0 (fl- (fl* (fixnum->flonum ty) tile) reach)]
         [x1 (fl+ (fl* (fixnum->flonum (fx+ tx 1)) tile) reach)]
         [y1 (fl+ (fl* (fixnum->flonum (fx+ ty 1)) tile) reach)])
    (define (tip-x j) (fl+ (at j 0) (fl* 4.0 (at j 2))))
    (define (tip-y j) (fl+ (at j 1) (fl* 4.0 (at j 3))))
    (define (touches? j)
      (and (fl<= (fl* scale (flmin (at j 0) (tip-x j))) x1)
           (fl>= (fl* scale (flmax (at j 0) (tip-x j))) x0)
           (fl<= (fl* scale (flmin (at j 1) (tip-y j))) y1)
           (fl>= (fl* scale (flmax (at j 1) (tip-y j))) y0)))
    (let ([count (do-bounded 8192
                     ([j 0 (fx+ j 1)]
                      [c 0 (if (touches? j)
                               (begin
                                 (when (fx= listing 1)
                                   (fx-set! out (fx+ start c) j))
                                 (fx+ c 1))
                               c)])
                   ((fx>= j n) c))])
      (when (fx= listing 0)
        (fx-set! out t count)))))

;; Where each of tiles tiles' lists starts, from their counts, in one cell:
;; the counts before it, added up, and after the last, all of them.
(define-compute-shader (boids-starts i row)
  ([counts fx-buffer]
   [starts fx-buffer write]
   [tiles fixnum])
  (fx-set! starts tiles
           (do-bounded 131072
               ([t 0 (fx+ t 1)]
                [sum 0 (begin
                         (fx-set! starts t sum)
                         (fx+ sum (fx-ref counts t)))])
             ((fx>= t tiles) sum))))

;; The field, a pixel at a time: the ground, and over it the boids the
;; pixel's tile lists, in the flock's order, each as Boids draws one: a
;; line 2 wide toward where it's heading, then a dot 3 in radius. Each
;; covers as much of the pixel as the core's own line and circle do, by
;; how far the pixel's center is inside its edge.
(define-paint (boids-view x y)
  ([flock fl-buffer]
   [starts fx-buffer]
   [lists fx-buffer]
   [across fixnum]
   [tile fixnum]
   [scale flonum]
   [ground color]
   [line color]
   [dot color])
  (define (at j k) (fl-ref flock (fx+ (fx* 4 j) k)))
  (let* ([px (fl+ (fixnum->flonum x) 0.5)]
         [py (fl+ (fixnum->flonum y) 0.5)]
         [t (fx+ (fxquotient x tile) (fx* (fxquotient y tile) across))]
         [start (fx-ref starts t)]
         [end (fx-ref starts (fx+ t 1))])
    ;; the boid at k in the lists, its place on the field in device pixels,
    ;; and its heading's tip
    (define (boid k) (fx-ref lists k))
    (define (bx j) (fl* scale (at j 0)))
    (define (by j) (fl* scale (at j 1)))
    (define (tip-x j) (fl* scale (fl+ (at j 0) (fl* 4.0 (at j 2)))))
    (define (tip-y j) (fl* scale (fl+ (at j 1) (fl* 4.0 (at j 3)))))
    ;; the segment from j's place to its tip, half scale wide either side
    (define (line-cover j)
      (let* ([ax (fl- px (bx j))]
             [ay (fl- py (by j))]
             [ux (fl- (tip-x j) (bx j))]
             [uy (fl- (tip-y j) (by j))]
             [len2 (fl+ (fl* ux ux) (fl* uy uy))]
             [s (if (fl> len2 0.0)
                    (clamp01 (fl/ (fl+ (fl* ax ux) (fl* ay uy)) len2))
                    0.0)]
             [ex (fl- ax (fl* ux s))]
             [ey (fl- ay (fl* uy s))])
        (clamp01 (fl- 0.5 (fl- (flsqrt (fl+ (fl* ex ex) (fl* ey ey)))
                               scale)))))
    (define (dot-cover j)
      (let ([ex (fl- px (bx j))] [ey (fl- py (by j))])
        (clamp01 (fl- 0.5 (fl- (flsqrt (fl+ (fl* ex ex) (fl* ey ey)))
                               (fl* 3.0 scale))))))
    ;; a channel of what's under, with j's line and then its dot over it
    (define (over under shift j)
      (mixed (mixed under (channel line shift) (line-cover j))
             (channel dot shift)
             (dot-cover j)))
    (define (byte v) (flonum->fixnum (flround (fl* 255.0 (clamp01 v)))))
    (do-bounded 8192
        ([k start (fx+ k 1)]
         [r (channel ground 16) (over r 16 (boid k))]
         [g (channel ground 8) (over g 8 (boid k))]
         [b (channel ground 0) (over b 0 (boid k))])
      ((fx>= k end)
       (fxior -16777216 (fxsll (byte r) 16) (fxsll (byte g) 8) (byte b))))))

;;; ------------------------------------------------------------------ flocks

;; A flock's two buffers, room for the most boids each: a step reads one
;; and writes the other.
(define (boids-buffers)
  (cons (make-gpu-buffer (* 4 boids-max-count))
        (make-gpu-buffer (* 4 boids-max-count))))

;; Boids from up to to scattered over a w × h field, moving in random
;; directions, as Boids' boids-scatter picks them: into both buffers, since
;; a step waiting for the frame may read either.
(define (boids-scatter-into! buffers from to w h)
  (do ([i from (+ i 1)]) ((>= i to))
    (let* ([x (random (inexact w))]
           [y (random (inexact h))]
           [vx (- (random 4.0) 2.0)]
           [vy (- (random 4.0) 2.0)])
      (for-each (lambda (b)
                  (gpu-buffer-set! b (* 4 i) x)
                  (gpu-buffer-set! b (+ (* 4 i) 1) y)
                  (gpu-buffer-set! b (+ (* 4 i) 2) vx)
                  (gpu-buffer-set! b (+ (* 4 i) 3) vy))
                (list (car buffers) (cdr buffers))))))

;; The buffer the newest step wrote, which the next reads.
(define (boids-current win)
  (let ([b (window-ref win 'buffers)])
    (if (window-ref win 'flipped?) (cdr b) (car b))))

;; The flock in win as Boids keeps one, an flvector of four numbers a boid,
;; handed to then once the GPU gives it back, a step or two on, or #f if
;; its buffer was let go of meanwhile: a handler can't wait for the GPU.
(define (boids-flock-then win then)
  (let ([n (window-ref win 'count)])
    (gpu-buffer-read-then! (boids-current win)
      (lambda (bytes)
        (then (and bytes (boids-bytes->flock bytes n)))))))

(define (boids-bytes->flock bytes n)
  (let ([flock (make-flvector (* 4 n))])
    (do ([k 0 (+ k 1)]) ((= k (* 4 n)) flock)
      (flvector-set! flock k
                     (bytevector-ieee-single-native-ref bytes (* 4 k))))))

;; A step of the flock in win on its w × h field, with the next frame: the
;; current buffer into the other, which is current then.
(define (boids-step! win w h)
  (let* ([b (window-ref win 'buffers)]
         [from (boids-current win)]
         [to (if (eq? from (car b)) (cdr b) (car b))]
         [n (window-ref win 'count)])
    (apply gpu-compute-soon! boids-steer n 1 from to n (inexact w) (inexact h)
           (boids-steering (window-ref win 'rules)))
    (window-set! win 'flipped? (not (window-ref win 'flipped?)))))

;; The words the tiles' lists take at most for n boids at scale: a boid's
;; drawing, its dot and a line 4 steps long at the fastest a rule allows,
;; spans so many tiles a side.
(define (boids-list-words n scale)
  (let* ([fastest (caddr (assq 'max-speed boids-ranges))]
         [span (+ (* scale (+ (* 4 fastest) 6)) 2)]
         [side (+ 1 (exact (ceiling (/ span boids-tile))))])
    (* (max n 1) side side)))

;; The window's buffer under key, at least words, made again when the field
;; or the flock outgrows it.
(define (boids-buffer! win key words)
  (let ([b (window-ref win key)])
    (if (and b (>= (cdr b) words))
        (car b)
        (let ([new (make-gpu-buffer words)])
          (when b (gpu-buffer-release! (car b)))
          (window-set! win key (cons new words))
          new))))

;; The field, w × h: the flock counted and listed by tile, with the frame,
;; then drawn by the paint, Boids' colors on Boids' ground.
(define (boids-draw-field win w h)
  (let* ([s (inexact (draw-scale))]
         [across (quotient (+ (exact (ceiling (* w s))) (- boids-tile 1))
                           boids-tile)]
         [down (quotient (+ (exact (ceiling (* h s))) (- boids-tile 1))
                         boids-tile)]
         [tiles (* across down)]
         [n (window-ref win 'count)]
         [flock (boids-current win)]
         [counts (boids-buffer! win 'counts tiles)]
         [starts (boids-buffer! win 'starts (+ tiles 1))]
         [lists (boids-buffer! win 'lists (boids-list-words n s))]
         [tile (inexact boids-tile)])
    (gpu-compute-soon! boids-bin across down flock starts counts n across 0
                       tile s)
    (gpu-compute-soon! boids-starts 1 1 counts starts tiles)
    (gpu-compute-soon! boids-bin across down flock starts lists n across 1
                       tile s)
    (fill-shader 0 0 w h boids-view flock starts lists across boids-tile s
                 'bg-0 'red 'yellow)))

;;; ------------------------------------------------------------------ the app

;; How often the flock steps, in ms: about 60 times a second, however
;; often the window ticks.
(define boids-step-ms 15)

(define boids-count 150)   ; how many a new window starts with

(define (boids-bar-height) (max 26 (round (+ (font-line-height 'ui-small) 13))))

;; The status line under the flock. Where Boids shows a step's time, this
;; says where it runs: the CPU only asks for a step, and the GPU tells it
;; nothing of how long one took.
(define (boids-status-text n running?)
  (format "~a boids   ~a   r random · d defaults · s scatter · h rules · \
           space pause"
          n (if running? "on the GPU" "paused")))

;; The flock's field in win: its content above the status line.
(define (boids-field-size win)
  (let-values ([(w h) (window-content-size win)])
    (values w (max 1 (- h (boids-bar-height))))))

;; What the keys and the rules window's buttons do to the flock in win.
(define (boids-randomize! win) (window-set! win 'rules (boids-random-rules)))
(define (boids-defaults! win) (window-set! win 'rules boids-default-rules))

(define (boids-scatter! win)
  (let-values ([(w h) (boids-field-size win)])
    (boids-scatter-into! (window-ref win 'buffers) 0 (window-ref win 'count)
                         w h)
    (invalidate! win)))

;; n boids: the first n of the flock, or all of it and random newcomers.
(define (boids-resize! win n)
  (let-values ([(w h) (boids-field-size win)])
    (let ([was (window-ref win 'count)])
      (when (> n was)
        (boids-scatter-into! (window-ref win 'buffers) was n w h))
      (window-set! win 'count n))))

(define-app (boids-l4-flat) "Boids L4 (flat)"
  (let ([w 760] [h 520]
        [running? #t] [last-step 0]
        [rules-win #f])    ; the rules window, while it is open
    (define (toggle-rules! win)
      (if rules-win
          (close-window! rules-win)
          (set! rules-win
                (boids-open-rules win (lambda () (set! rules-win #f))))))
    (let* ([buffers (boids-buffers)]
           [win
            (open-window "Boids L4 (flat)"
              'size (cons w (+ h (boids-bar-height)))
              'buffers buffers
              'flipped? #f
              'count boids-count
              'counts #f           ; the tiles' buffers: boids-buffer!
              'starts #f
              'lists #f
              'rules boids-default-rules
              'open (lambda (win) (window-frame-soon! win))
              'frame (lambda (win t)
                       (window-frame-soon! win)
                       (when (and running?
                                  (>= (- t last-step)
                                      (* boids-step-ms 1000000)))
                         (set! last-step t)
                         (let-values ([(w h) (boids-field-size win)])
                           (boids-step! win w h))))
              'draw (lambda (win w h)
                      (let ([bar (boids-bar-height)])
                        (boids-draw-field win w (- h bar))
                        (fill-box 0 (- h bar) w bar 'bg-1)
                        (draw-text 10 (+ (- h bar) 5)
                                   (boids-status-text (window-ref win 'count)
                                                      running?)
                                   'text-faint 'ui-small)))
              'key (lambda (win key mods)
                     (case key
                       [(#\r) (boids-randomize! win)]
                       [(#\d) (boids-defaults! win)]
                       [(#\s) (boids-scatter! win)]
                       [(#\h) (toggle-rules! win)]
                       [(#\space) (set! running? (not running?))])
                     (invalidate! win))
              'close (lambda (win)
                       (when rules-win (close-window! rules-win))
                       (gpu-buffer-release! (car buffers))
                       (gpu-buffer-release! (cdr buffers))
                       (for-each (lambda (key)
                                   (let ([b (window-ref win key)])
                                     (when b (gpu-buffer-release! (car b)))))
                                 '(counts starts lists))))])
      (boids-scatter-into! buffers 0 boids-count w h)
      ;; the rules show at first, as Niva's panel does. Moved to fit on
      ;; the desktop, they'd cover the flock: it moves left to clear them.
      (toggle-rules! win)
      (let-values ([(dx dy dw dh) (desktop-rect)])
        (let ([short (- (+ (window-x win) (window-w win) 12)
                        (window-x rules-win))])
          (when (> short 0)
            (move-window! win (max dx (- (window-x win) short))
                          (window-y win)))))
      (focus-window! win)   ; the flock has the keyboard
      win)))

;;; ------------------------------------------------------------------
;;; the rules window
;;; Boids', without the threads: a slider for the count and for each rule,
;;; and the keys' buttons. It reads and sets the flock window's properties,
;;; so it shows them as they are, whoever changed them: a key, a button or
;;; the REPL.

;; A rule's name as its row shows it: cohesion-radius as cohesion radius.
(define (boids-setting-label key)
  (list->string (map (lambda (c) (if (char=? c #\-) #\space c))
                     (string->list (symbol->string key)))))

(define (boids-weight? range) (pair? (cdddr range)))

;; Where v sits on range's slider, from 0 to 1, and back. A weight's
;; slider is a cube: its middle is an eighth of its top, so the small
;; weights that matter get most of it, as on Niva's log slider.
(define (boids-slider-t range v)
  (let ([t (/ (- v (cadr range)) (- (caddr range) (cadr range)))])
    (if (boids-weight? range) (expt (max t 0.0) 1/3) t)))

(define (boids-slider-value range t)
  (+ (cadr range)
     (* (- (caddr range) (cadr range))
        (if (boids-weight? range) (* t t t) t))))

;; A rule's value as its row shows it.
(define (boids-readout range v)
  (case (car range)
    [(min-speed max-speed) (format "~,1f" v)]
    [(view-angle blocker-cone) (format "~a°" (exact (round v)))]
    [else (if (boids-weight? range)
              (format "~,4f" v)
              (format "~a" (exact (round v))))]))

(define (boids-label-width)
  (+ 12 (text-widest (cons "boids"
                           (map (lambda (r) (boids-setting-label (car r)))
                                boids-ranges))
                     'ui)))

(define (boids-readout-width)
  (+ 12 (text-widest (list "0.0000" (number->string boids-max-count)) 'ui)))

(define boids-buttons
  '("Randomize (R)"
    "Defaults (D)"
    "Scatter (S)"))

;; As wide as the widest label's button, in the style's own.
(define (boids-button-width)
  (apply max (map (lambda (label)
                    (let-values ([(w h) (ui-button-size label #f)]) w))
                  boids-buttons)))

;; The rules window's content size: room for its rows (as
;; boids-draw-rules lays them out), with sliders 140 wide, and for three
;; buttons as wide as the widest label.
(define (boids-rules-size)
  (let ([pad (ui-pad)] [gap (ui-gap)] [sh (ui-slider-height)])
    (values (+ pad pad
               (max (+ (boids-label-width) 140 (boids-readout-width))
                    (+ (* 3 (boids-button-width)) (* 2 gap))))
            (+ pad (+ sh gap) (+ (ui-control-height) (* 3 gap))
               (* (length boids-ranges) (+ sh gap))
               gap (font-line-height 'ui-small) pad))))

;; The rows, top to bottom: the count, the buttons, a slider for each
;; rule, and a hint. Returns the y below them.
(define (boids-draw-rules ui flock-win w)
  (let* ([pad (ui-pad)] [gap (ui-gap)] [sh (ui-slider-height)]
         [lw (boids-label-width)] [rw (boids-readout-width)]
         [rules (window-ref flock-win 'rules)]
         [n (window-ref flock-win 'count)])
    (define (row y id label t readout set)
      (ui-draw-label pad y lw sh '(dim) label)
      (ui-slider ui id (+ pad lw) y (- w pad lw rw pad) t 0.0 1.0 set)
      (ui-draw-label (- w pad rw) y rw sh '(right) readout)
      (+ y sh gap))
    (let* ([y (row pad 'count "boids"
                   (/ (- n 10) (- boids-max-count 10.0))
                   (number->string n)
                   (lambda (t)
                     (boids-resize! flock-win
                       (exact (round (+ 10 (* t (- boids-max-count 10))))))))]
           [y (let ([bw (/ (- w pad pad gap gap) 3)] [ch (ui-control-height)])
                (for-each (lambda (i label id act!)
                            (ui-button ui id (+ pad (* i (+ bw gap))) y bw ch
                                       label (lambda () (act! flock-win))))
                          '(0 1 2) boids-buttons '(randomize defaults scatter)
                          (list boids-randomize! boids-defaults!
                                boids-scatter!))
                (+ y ch (* 3 gap)))]
           [y (fold-left
               (lambda (y range)
                 (let* ([key (car range)] [v (boids-rule rules key)])
                   (row y key (boids-setting-label key)
                        (boids-slider-t range v) (boids-readout range v)
                        ;; the rules as they are then, not as drawn
                        (lambda (t)
                          (window-set! flock-win 'rules
                            (boids-set-rule (window-ref flock-win 'rules) key
                                            (boids-slider-value range t)))))))
               y boids-ranges)])
      (ui-draw-label pad (+ y gap) (- w pad pad) (font-line-height 'ui-small)
                     '(hint) "h hides this window · space pauses")
      (+ y gap (font-line-height 'ui-small) pad))))

;; Opens the rules window for the flock in flock-win, beside it, and
;; returns it; on-close is called when it closes. Its keys are the
;; flock's.
(define (boids-open-rules flock-win on-close)
  (let ([ui (make-ui)]
        [shown #f])   ; the rules and the count it last drew
    (define (now-shown)
      (cons (window-ref flock-win 'rules) (window-ref flock-win 'count)))
    (let-values ([(w h) (boids-rules-size)])
      (let ([win
             (apply open-window "Boids L4 (flat) — Rules"
                    'size (cons w h)
                    'pos (cons (+ (window-x flock-win) (window-w flock-win) 12)
                               (window-y flock-win))
                    'app #f
                    'draw (lambda (win w h)
                            (set! shown (now-shown))
                            (ui-begin! ui)
                            (ui-draw-surface 0 0 w h '(page))
                            (boids-draw-rules ui flock-win w)
                            (ui-end! ui w h))
                    'close (lambda (win) (on-close))
                    (append
                     (ui-handlers ui 'key (lambda (win key mods)
                                            (window-call flock-win 'key key
                                                         mods)
                                            #t))
                     ;; a change from elsewhere shows, and fonts grown in
                     ;; Settings get room for the rows
                     (list 'open (lambda (win) (window-frame-soon! win))
                           'frame (lambda (win t)
                                   (window-frame-soon! win)
                                   (ui-frame ui win)
                                   (let-values ([(w h) (boids-rules-size)])
                                     (boids-grow-to! win w h))
                                   (unless (equal? shown (now-shown))
                                     (invalidate! win))))))])
        (boids-keep-on-desktop! win)
        win))))

;; Grows win's content to at least w × h, moved left or up as far as it
;; takes to stay on the desktop. It never shrinks: a size set by hand
;; holds.
(define (boids-grow-to! win w h)
  (let-values ([(cw ch) (window-content-size win)])
    (when (and (or (> w cw) (> h ch)) (not (shaded? win)))
      (resize-window! win (max w cw) (max h ch))
      (boids-keep-on-desktop! win))))

;; Moves win left or up as far as it takes to stay on the desktop.
(define (boids-keep-on-desktop! win)
  (let-values ([(dx dy dw dh) (desktop-rect)])
    (move-window! win
      (max dx (min (window-x win) (- (+ dx dw) (window-w win))))
      (max dy (min (window-y win) (- (+ dy dh) (window-h win)))))))
