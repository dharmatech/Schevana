;;; apps/boids-clear.ss — Boids (apps/boids.ss), its step written with
;;; names instead of numbers. A study in keeping the flat step's speed and
;;; getting back the original's clarity (apps/boids.ss as first ported,
;;; with vec and boid records): the same flock, to the last digit, at the
;;; same speed. docs/BOIDS-CLEAR.md has what was tried and measured.
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
;;; The simulation is pure: a flock is an flvector of boids, four numbers
;;; each (where it is, where it's heading), and a step makes a new one, on
;;; the crew when the flock is big enough. The window keeps the flock and
;;; its rules, as plain data:
;;; (boid-x (window-ref win 'flock) 0)
;;; (window-set! win 'rules (boids-random-rules))
;;; R: random rules · D: the defaults · S: scatter · Space: pause
;;; H: the rules window, beside the flock (it closes with the flock)
;;;
;;; It runs in a sandbox (services/grant.ss), where the names above are: a
;;; REPL in /apps/boids-clear has them.

(app-requests window draw ui timers crew)

;;; ------------------------------------------------------------------ flat
;;; Flat records: flonums side by side in an flvector, named once. A flock
;;; is boids end to end; the rules a step reads are one record of numbers.
;;; A name costs nothing: where it is used, it becomes the flvector-ref at
;;; its field's offset.
;;;
;;;   (define-flat-record boid (boid-x boid-x-set!) ...)
;;;       records end to end: (boid-x flock i), (boid-x-set! flock i x)
;;;   (define-flat-names steering cohesion ...)
;;;       one record, whose fields a worklet reads as variables;
;;;       (make-flat-names steering 'cohesion 0.01 ...) makes one
;;;   (flat-size boid)      its numbers, 4
;;;
;;; A worklet reaches none of the app's names, so define-flat-worklet
;;; declares again in it the records it names, from their one declaration
;;; here; there they are macros:
;;;
;;;   (define-flat-worklet (boid (steering rules)) (step! lo hi flock rules)
;;;     ...)
;;;       in it, (boid-x flock i) is (flvector-ref flock (fx* 4 i)), and
;;;       cohesion is (flvector-ref rules 0)
;;;   (define-flat-variables sums center-x ...)
;;;       in a worklet's body: an flvector, sums, whose numbers are the
;;;       variables center-x ..., which set! sets. A flonum variable set!
;;;       in a loop would box.
;;;
;;; The app is given syntax-rules alone, which makes no name from another
;;; (boid and x into boid-x): an accessor's name is written out, as in
;;; R6RS's define-record-type. Lists are matched as a pair's tail, (x .
;;; more), not with an ellipsis: these macros are written again inside a
;;; template, where an ellipsis would have to be escaped.

;; How many things follow: (flat-count a b c) is (fx+ 1 (fx+ 1 (fx+ 1
;; 0))), which the compiler folds to 3.
(define-syntax flat-count
  (syntax-rules ()
    [(_) 0]
    [(_ x . more) (fx+ 1 (flat-count . more))]))

;; A record's declaration is a macro of its name, which hands it on:
;; (boid k arg ...) is (k (define-flat-record boid field ...) arg ...).
(define-syntax define-flat-record
  (syntax-rules ()
    [(_ name . fields)
     (begin
       (define-syntax name
         (syntax-rules ()
           [(_ k . args) (k (define-flat-record name . fields) . args)]))
       (flat-record-procedures (flat-count . fields) 0 . fields))]))

;; Each field's accessor and setter, at offset on, of records size long.
(define-syntax flat-record-procedures
  (syntax-rules ()
    [(_ size offset) (begin)]
    [(_ size offset (get set) . more)
     (begin
       (define (get v i) (flvector-ref v (fx+ (fx* size i) offset)))
       (define (set v i x) (flvector-set! v (fx+ (fx* size i) offset) x))
       (flat-record-procedures size (fx+ offset 1) . more))]))

(define-syntax define-flat-names
  (syntax-rules ()
    [(_ name . fields)
     (define-syntax name
       (syntax-rules ()
         [(_ k . args) (k (define-flat-names name . fields) . args)]))]))

(define-syntax flat-size
  (syntax-rules ()
    [(_ name) (name flat-size-of)]))

(define-syntax flat-size-of
  (syntax-rules ()
    [(_ (declare name . fields)) (flat-count . fields)]))

(define-syntax make-flat-names
  (syntax-rules ()
    [(_ name . plist) (name make-flat-names-from . plist)]))

(define-syntax make-flat-names-from
  (syntax-rules ()
    [(_ (declare name . fields) . plist)
     (flat-names->flvector 'name 'fields (list . plist))]))

;; The flvector of fields' values, each given by name in plist, in any
;; order: one missing, or one name has no field of, is an error.
(define (flat-names->flvector name fields plist)
  (define (value-of field plist)
    (cond [(null? plist)
           (errorf 'make-flat-names "~a's ~a is missing" name field)]
          [(eq? (car plist) field) (cadr plist)]
          [else (value-of field (cddr plist))]))
  (let check ([l plist])
    (unless (null? l)
      (unless (memq (car l) fields)
        (errorf 'make-flat-names "~a has no field ~a" name (car l)))
      (check (cddr l))))
  (apply flvector (map (lambda (field) (value-of field plist)) fields)))

;; (define-flat-worklet (record ...) (name . formals) body ...): a worklet
;; in which the records listed are named as here; a record of names
;; listed with a variable, (steering rules), is read from it.
(define-syntax define-flat-worklet
  (syntax-rules ()
    [(_ records head . body) (flat-worklet-gather records () head . body)]))

;; The records' declarations, asked of each in turn.
(define-syntax flat-worklet-gather
  (syntax-rules ()
    [(_ () declarations head . body)
     (flat-worklet declarations head . body)]
    [(_ ((record variable) . more) declarations head . body)
     (record flat-worklet-open variable more declarations head . body)]
    [(_ (record . more) declarations head . body)
     (record flat-worklet-add more declarations head . body)]))

(define-syntax flat-worklet-add
  (syntax-rules ()
    [(_ declaration more (gathered ...) head . body)
     (flat-worklet-gather more (gathered ... declaration) head . body)]))

;; Names opened here, not in the worklet, where each would be a name its
;; record's macro made, which the body's own can't refer to.
(define-syntax flat-worklet-open
  (syntax-rules ()
    [(_ (declare name . fields) variable more (gathered ...) head . body)
     (flat-worklet-gather more
                          (gathered ... (flat-variables variable 0 . fields))
                          head . body)]))

;; The worklet: these macros, as macros of its own, then the records'
;; declarations, then its body. define-worklet compiles it as plain text,
;; where they find each other by name.
(define-syntax flat-worklet
  (syntax-rules ()
    [(_ (declaration ...) head . body)
     (define-worklet head
       (define-syntax flat-count
         (syntax-rules ()
           [(_) 0]
           [(_ x . more) (fx+ 1 (flat-count . more))]))
       (define-syntax define-flat-record
         (syntax-rules ()
           [(_ name . fields)
            (begin
              (define-syntax name
                (syntax-rules ()
                  [(_ k . args)
                   (k (define-flat-record name . fields) . args)]))
              (flat-record-macros (flat-count . fields) 0 . fields))]))
       (define-syntax flat-record-macros
         (syntax-rules ()
           [(_ size offset) (begin)]
           [(_ size offset (get set) . more)
            (begin
              (define-syntax get
                (syntax-rules ()
                  [(_ v i) (flvector-ref v (fx+ (fx* size i) offset))]))
              (define-syntax set
                (syntax-rules ()
                  [(_ v i x) (flvector-set! v (fx+ (fx* size i) offset) x)]))
              (flat-record-macros size (fx+ offset 1) . more))]))
       (define-syntax flat-size
         (syntax-rules ()
           [(_ name) (name flat-size-of)]))
       (define-syntax flat-size-of
         (syntax-rules ()
           [(_ (declare name . fields)) (flat-count . fields)]))
       ;; each field a variable: its slot of the flvector v
       (define-syntax flat-variables
         (syntax-rules ()
           [(_ v offset) (begin)]
           [(_ v offset field . more)
            (begin
              (define-syntax field
                (identifier-syntax
                 [_ (flvector-ref v offset)]
                 [(set! _ x) (flvector-set! v offset x)]))
              (flat-variables v (fx+ offset 1) . more))]))
       (define-syntax define-flat-variables
         (syntax-rules ()
           [(_ v . fields)
            (begin
              (define v (make-flvector (flat-count . fields) 0.0))
              (flat-variables v 0 . fields))]))
       declaration ...
       . body)]))

;;; ------------------------------------------------------------------ boids

;; One bird: where it is and where it's heading.
(define-flat-record boid
  (boid-x boid-x-set!)
  (boid-y boid-y-set!)
  (boid-vx boid-vx-set!)
  (boid-vy boid-vy-set!))

(define (boids-size flock) (quotient (flvector-length flock) (flat-size boid)))

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

;;; ------------------------------------------------------------------ flocks

;; The rules as a step reads them, worked out once: each rule's weight;
;; the cosines of half a field of view and of half the blockers' cone,
;; which is in view too: a boid can't react to what it can't see (all
;; around, -inf: any heading beats it); the longest reach; and each
;; rule's reach, its radius squared, 0 for a rule without weight, so that
;; nothing is in reach.
(define-flat-names steering
  cohesion
  alignment
  separation
  clear-view
  back-off
  view-cosine
  blocker-cosine
  longest-reach
  cohesion-reach
  alignment-reach
  separation-reach
  clear-view-reach
  back-off-reach)

(define (boids-steering rules)
  (define (rule key) (inexact (boids-rule rules key)))
  (define (reach weight radius)
    (if (= (rule weight) 0.0) 0.0 (* (rule radius) (rule radius))))
  (define (half-cosine angle)
    (if (>= angle 360.0) -inf.0 (cos (/ (* (/ angle 2.0) pi) 180.0))))
  (let ([fov (rule 'view-angle)]
        [cohesion-reach (reach 'cohesion 'cohesion-radius)]
        [alignment-reach (reach 'alignment 'alignment-radius)]
        [separation-reach (reach 'separation 'separation-radius)]
        [clear-view-reach (reach 'clear-view 'blocker-radius)]
        [back-off-reach (reach 'back-off 'blocker-radius)])
    (make-flat-names steering
      'cohesion (rule 'cohesion)
      'alignment (rule 'alignment)
      'separation (rule 'separation)
      'clear-view (rule 'clear-view)
      'back-off (rule 'back-off)
      'view-cosine (half-cosine fov)
      'blocker-cosine (half-cosine (min (rule 'blocker-cone) fov))
      'longest-reach (max cohesion-reach alignment-reach separation-reach
                          clear-view-reach back-off-reach)
      'cohesion-reach cohesion-reach
      'alignment-reach alignment-reach
      'separation-reach separation-reach
      'clear-view-reach clear-view-reach
      'back-off-reach back-off-reach)))

;; Boids [lo, hi) of flock steered into next, each by everyone's current
;; state: its velocity plus each rule's push times its weight, and its
;; position moved by that. Every pair of boids is looked at, so the loop
;; over them keeps its flonums unboxed: nothing crosses a call or sits in
;; a variable a loop closes over, and what adds up lives in sums, the one
;; thing a call allocates.
(define-flat-worklet (boid (steering rules))
  (boids-steer! lo hi flock next rules)
  (define n (fxquotient (flvector-length flock) (flat-size boid)))
  ;; what a boid's neighbors add up to, rule by rule: for cohesion, where
  ;; they are and how many; for alignment, how they move and how many;
  ;; the pushes away from them, aside and back
  (define-flat-variables sums
    center-x
    center-y
    centered
    heading-x
    heading-y
    headed
    crowd-x
    crowd-y
    aside-x
    aside-y
    back-x
    back-y)
  (define-syntax add!
    (syntax-rules () [(_ sum v) (set! sum (fl+ sum v))]))
  ;; toward the mean of what is summed, over count
  (define-syntax toward
    (syntax-rules ()
      [(_ sum count own weight)
       (if (fl> count 0.0) (fl* (fl- (fl/ sum count) own) weight) 0.0)]))
  (do ([i lo (fx+ i 1)]) ((fx= i hi))
    (flvector-fill! sums 0.0)
    (do ([j 0 (fx+ j 1)]) ((fx= j n))
      (let* ([dx (fl- (boid-x flock j) (boid-x flock i))]
             [dy (fl- (boid-y flock j) (boid-y flock i))]
             [d2 (fl+ (fl* dx dx) (fl* dy dy))])
        ;; not itself, and in somebody's reach: most pairs stop here
        (when (and (fl> d2 0.0) (fl< d2 longest-reach))
          (let* ([vx (boid-vx flock i)]
                 [vy (boid-vy flock i)]
                 [h2 (fl+ (fl* vx vx) (fl* vy vy))]
                 [ahead (fl+ (fl* vx dx) (fl* vy dy))]
                 [lengths (flsqrt (fl* h2 d2))]
                 [away-x (fl- 0.0 dx)]
                 [away-y (fl- 0.0 dy)])
            ;; in the field of view whose half-cosine is cosine: a boid
            ;; standing still sees everything
            (let-syntax ([sees? (syntax-rules ()
                                  [(_ cosine)
                                   (or (fl= h2 0.0)
                                       (fl> ahead (fl* cosine lengths)))])])
              ;; toward the center of the neighbors
              (when (and (fl< d2 cohesion-reach) (sees? view-cosine))
                (add! center-x (boid-x flock j))
                (add! center-y (boid-y flock j))
                (add! centered 1.0))
              ;; toward their average velocity
              (when (and (fl< d2 alignment-reach) (sees? view-cosine))
                (add! heading-x (boid-vx flock j))
                (add! heading-y (boid-vy flock j))
                (add! headed 1.0))
              ;; away from every boid this close, seen or not: you feel
              ;; a bump from behind
              (when (fl< d2 separation-reach)
                (add! crowd-x away-x)
                (add! crowd-y away-y))
              ;; a unit step sideways out from behind each blocker:
              ;; square to the line between them, on the side it heads
              (when (and (fl< d2 clear-view-reach) (sees? blocker-cosine))
                (let* ([turn-x (fl- 0.0 away-y)]
                       [side (if (fl< (fl+ (fl* vx turn-x) (fl* vy away-x))
                                      0.0)
                                 -1.0
                                 1.0)]
                       [d (flsqrt d2)])
                  (add! aside-x (fl/ (fl* turn-x side) d))
                  (add! aside-y (fl/ (fl* away-x side) d))))
              ;; a unit step straight away from each blocker
              (when (and (fl< d2 back-off-reach) (sees? blocker-cosine))
                (let ([d (flsqrt d2)])
                  (add! back-x (fl/ away-x d))
                  (add! back-y (fl/ away-y d)))))))))
    (let* ([x (boid-x flock i)]
           [y (boid-y flock i)]
           [vx (boid-vx flock i)]
           [vy (boid-vy flock i)]
           [nvx (fl+ (fl+ (fl+ (fl+ (fl+ vx (toward center-x centered
                                                    x cohesion))
                                    (toward heading-x headed vx alignment))
                               (fl* crowd-x separation))
                          (fl* aside-x clear-view))
                     (fl* back-x back-off))]
           [nvy (fl+ (fl+ (fl+ (fl+ (fl+ vy (toward center-y centered
                                                    y cohesion))
                                    (toward heading-y headed vy alignment))
                               (fl* crowd-y separation))
                          (fl* aside-y clear-view))
                     (fl* back-y back-off))])
      (boid-x-set! next i (fl+ x nvx))
      (boid-y-set! next i (fl+ y nvy))
      (boid-vx-set! next i nvx)
      (boid-vy-set! next i nvy))))

;; Boids a call of the crew: at least this many pairs, about 80 µs. So a
;; small flock runs on the caller alone, where waking the crew would cost
;; more than it gives.
(define boids-chunk-pairs 24000)

(define (boids-chunk n) (max 1 (quotient boids-chunk-pairs (max n 1))))

;; The next flock: every boid steered by the same current state.
(define (boids-step flock rules)
  (let ([next (make-flvector (flvector-length flock))]
        [n (boids-size flock)])
    (crew-for* (boids-chunk n) 0 n boids-steer! flock next
               (boids-steering rules))
    next))

;; Three boids close together, for the tests and the REPL:
;; (boids-step boids-demo boids-default-rules)
(define boids-demo
  (flvector 0.0 0.0 1.0 0.0
            1.0 1.0 0.0 1.0
            2.0 0.0 1.0 1.0))

;;; ------------------------------------------------------------------ screen

;; Leaving one edge of a w × h screen brings boids [lo, hi) back on the
;; opposite edge, and their speed stays within least and most; standing
;; still, a boid has no heading, so it stays put. A worklet for the
;; boid's names, which a procedure here has only as procedures: it runs
;; on the caller.
(define-flat-worklet (boid) (boids-keep-on-screen! lo hi flock least most w h)
  ;; the velocity of boid i times k
  (define-syntax scale!
    (syntax-rules ()
      [(_ i k)
       (let ([k* k])
         (boid-vx-set! flock i (fl* (boid-vx flock i) k*))
         (boid-vy-set! flock i (fl* (boid-vy flock i) k*)))]))
  (do ([i lo (fx+ i 1)]) ((fx= i hi))
    (boid-x-set! flock i (flmod (boid-x flock i) w))
    (boid-y-set! flock i (flmod (boid-y flock i) h))
    (let* ([vx (boid-vx flock i)]
           [vy (boid-vy flock i)]
           [s2 (fl+ (fl* vx vx) (fl* vy vy))])
      (when (and (fl> s2 0.0) (fl< s2 (fl* least least)))
        (scale! i (fl/ least (flsqrt s2)))))
    (let* ([vx (boid-vx flock i)]
           [vy (boid-vy flock i)]
           [s2 (fl+ (fl* vx vx) (fl* vy vy))])
      (when (fl> s2 (fl* most most))
        (scale! i (fl/ most (flsqrt s2)))))))

;; One frame on a w × h screen: a sim step, then the speed limits and
;; the wrap.
(define (boids-step-on flock rules w h)
  (let ([next (boids-step flock rules)])
    (boids-keep-on-screen! 0 (boids-size next) next
                           (inexact (boids-rule rules 'min-speed))
                           (inexact (boids-rule rules 'max-speed))
                           (inexact w)
                           (inexact h))
    next))

;; n boids scattered over a w × h screen, moving in random directions.
(define (boids-scatter n w h)
  (let ([flock (make-flvector (* (flat-size boid) n))]
        [w (inexact w)]
        [h (inexact h)])
    (do ([i 0 (+ i 1)]) ((= i n) flock)
      (boid-x-set! flock i (random w))
      (boid-y-set! flock i (random h))
      (boid-vx-set! flock i (- (random 4.0) 2.0))
      (boid-vy-set! flock i (- (random 4.0) 2.0)))))

;; n boids: the first n of flock, or all of it and random newcomers.
(define (boids-resize flock n w h)
  (if (= n (boids-size flock))
      flock
      (let ([next (boids-scatter n w h)])
        (do ([k 0 (+ k 1)])
            ((= k (* (flat-size boid) (min n (boids-size flock)))) next)
          (flvector-set! next k (flvector-ref flock k))))))

;;; ------------------------------------------------------------------ the app

;; How often the flock steps, in ms: about 60 times a second, however
;; often the window ticks.
(define boids-step-ms 15)

(define boids-count 150)   ; how many a new window starts with

(define (boids-bar-height) (max 26 (round (+ (font-line-height 'ui-small) 13))))

;; The status line under the flock.
(define (boids-status-text n step-ms running?)
  (format "~a boids   ~a   r random · d defaults · s scatter · h rules · \
           space pause"
          n (if running? (format "~,1f ms a step" step-ms) "paused")))

;; The flock's field in win: its content above the status line.
(define (boids-field-size win)
  (let-values ([(w h) (window-content-size win)])
    (values w (max 1 (- h (boids-bar-height))))))

;; What the keys and the rules window's buttons do to the flock in win.
(define (boids-randomize! win) (window-set! win 'rules (boids-random-rules)))
(define (boids-defaults! win) (window-set! win 'rules boids-default-rules))

(define (boids-scatter! win)
  (let-values ([(w h) (boids-field-size win)])
    (window-set! win 'flock
      (boids-scatter (boids-size (window-ref win 'flock)) w h))))

(define (boids-resize! win n)
  (let-values ([(w h) (boids-field-size win)])
    (window-set! win 'flock (boids-resize (window-ref win 'flock) n w h))))

;; A dot, with a line toward where it's heading.
(define (draw-boid flock i)
  (let ([x (boid-x flock i)] [y (boid-y flock i)])
    (draw-line x y
               (+ x (* (boid-vx flock i) 4.0))
               (+ y (* (boid-vy flock i) 4.0))
               'red 2)
    (fill-circle x y 3 'yellow)))

(define-app (boids-clear) "Boids (clear)"
  (let ([w 760] [h 520]
        [running? #t] [last-step 0]
        [step-ms 0.0]      ; a step's time, smoothed
        [rules-win #f])    ; the rules window, while it is open
    (define (step! win)
      (let-values ([(w h) (boids-field-size win)])
        (let ([t0 (now-ns)])
          (window-set! win 'flock
            (boids-step-on (window-ref win 'flock) (window-ref win 'rules)
                           w h))
          ;; one step's time jumps around (collections): over about 20
          (set! step-ms (+ (* step-ms 0.95)
                           (* (/ (- (now-ns) t0) 1e6) 0.05))))))
    (define (toggle-rules! win)
      (if rules-win
          (close-window! rules-win)
          (set! rules-win
                (boids-open-rules win (lambda () (set! rules-win #f))))))
    (let ([win
           (open-window "Boids (clear)"
             'size (cons w (+ h (boids-bar-height)))
             'flock (boids-scatter boids-count w h)
             'rules boids-default-rules
             'open (lambda (win) (window-frame-soon! win))
             'frame (lambda (win t)
                      (window-frame-soon! win)
                      (when (and running?
                                 (>= (- t last-step) (* boids-step-ms 1000000)))
                        (set! last-step t)
                        (step! win)))
             'draw (lambda (win w h)
                     (let ([bar (boids-bar-height)]
                           [flock (window-ref win 'flock)])
                       (fill-box 0 0 w (- h bar) 'bg-0)
                       (do ([i 0 (+ i 1)]) ((= i (boids-size flock)))
                         (draw-boid flock i))
                       (fill-box 0 (- h bar) w bar 'bg-1)
                       (draw-text 10 (+ (- h bar) 5)
                                  (boids-status-text (boids-size flock) step-ms
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
             'close (lambda (win) (when rules-win (close-window! rules-win))))])
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
;;; A slider for the count, for the crew's threads and for each rule, and the
;;; keys' buttons. It reads and sets the flock window's properties, so it shows
;;; them as they are, whoever changed them: a key, a button or the REPL.

;; A step's cost grows as the count squared, a frame's as the count. At
;; 5,000, measured here, a step takes 9 ms on the crew of 8 with the default
;; rules and 16 with the heaviest (both blocker rules, 360°, radii of 200);
;; on a thread alone, 72 and 127. What fills the frame then is the drawing,
;; one line and one circle a boid on core1: 23 ms at 100%, about
;; 50 at 200%.
(define boids-max-count 5000)

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
  (+ 12 (text-widest (cons* "boids" "threads"
                            (map (lambda (r) (boids-setting-label (car r)))
                                 boids-ranges))
                     'ui)))

(define (boids-readout-width)
  (+ 12 (text-widest '("0.0000" "1 of 48") 'ui)))

;; The threads a step of n boids uses, as its row shows them: one while a
;; step is one call of the crew.
(define (boids-threads-readout n)
  (let ([threads (+ 1 (crew-size))])
    (if (or (<= n (boids-chunk n)) (= threads 1))
        (if (= threads 1) "1" (format "1 of ~a" threads))
        (number->string threads))))

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
            (+ pad (* 2 (+ sh gap)) (+ (ui-control-height) (* 3 gap))
               (* (length boids-ranges) (+ sh gap))
               gap (font-line-height 'ui-small) pad))))

;; The rows, top to bottom: the count, the threads, the buttons, a slider
;; for each rule, and a hint. Returns the y below them.
(define (boids-draw-rules ui flock-win w)
  (let* ([pad (ui-pad)] [gap (ui-gap)] [sh (ui-slider-height)]
         [lw (boids-label-width)] [rw (boids-readout-width)]
         [rules (window-ref flock-win 'rules)]
         [n (boids-size (window-ref flock-win 'flock))])
    (define (row y id label t readout set . tip)
      (ui-draw-label pad y lw sh '(dim) label)
      (ui-slider ui id (+ pad lw) y (- w pad lw rw pad) t 0.0 1.0 set #f
                 'tip (and (pair? tip) (car tip)))
      (ui-draw-label (- w pad rw) y rw sh '(right) readout)
      (+ y sh gap))
    (let* ([y (row pad 'count "boids"
                   (/ (- n 10) (- boids-max-count 10.0))
                   (number->string n)
                   (lambda (t)
                     (boids-resize! flock-win
                       (exact (round (+ 10 (* t (- boids-max-count 10))))))))]
           ;; the crew's, for every app; the caller makes one more
           [y (let ([most (- (max 2 (platform-cpu-count)) 1)])
                (row y 'threads "threads"
                     (/ (crew-size) (inexact most))
                     (boids-threads-readout n)
                     (lambda (t) (crew-size-set! (exact (round (* t most)))))
                     "The threads a step runs on: the crew's, which every \
                      app shares"))]
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
        [shown #f])   ; the rules, the count and the threads it last drew
    (define (now-shown)
      (list (window-ref flock-win 'rules)
            (boids-size (window-ref flock-win 'flock))
            (crew-size)))
    (let-values ([(w h) (boids-rules-size)])
      (let ([win
             (apply open-window "Boids (clear) — Rules"
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
