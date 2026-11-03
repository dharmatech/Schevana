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
;;; new one. The window keeps the flock and its rules, as plain data:
;;; (window-ref win 'flock) => (#<boid at 451.2 87.0 moving 1.6 -3.1> ...)
;;; (window-set! win 'rules (boids-random-rules))
;;; R: random rules · D: the defaults · S: scatter · H: the rules window
;;; · Space: pause

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

;; Each rule's range, in the order the rules window shows them. A weight
;; starts at 0, and its useful values span orders of magnitude.
(define boids-ranges
  '((cohesion 0.0 0.2 weight)    (cohesion-radius 0.0 200.0)
    (alignment 0.0 1.0 weight)   (alignment-radius 0.0 200.0)
    (separation 0.0 1.0 weight)  (separation-radius 0.0 50.0)
    (view-angle 0.0 360.0)
    (clear-view 0.0 1.0 weight)  (back-off 0.0 1.0 weight)
    (blocker-radius 0.0 100.0)   (blocker-cone 0.0 180.0)
    (min-speed 0.0 5.0)          (max-speed 0.5 10.0)))

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
      (boids-scatter (length (window-ref win 'flock)) w h))))

(define (boids-resize! win n)
  (let-values ([(w h) (boids-field-size win)])
    (window-set! win 'flock (boids-resize (window-ref win 'flock) n w h))))

;; A dot, with a line toward where it's heading.
(define (draw-boid b)
  (let* ([p (boid-position b)] [tip (vec+ p (vec* (boid-velocity b) 4.0))])
    (draw-line (vec-x p) (vec-y p) (vec-x tip) (vec-y tip) 'red 2)
    (fill-circle (vec-x p) (vec-y p) 3 'yellow)))

(define-app (boids) "Boids"
  (let ([w 760] [h 520]
        [running? #t] [last-step 0]
        [step-ms 0.0]      ; a step's time, smoothed
        [rules-win #f])    ; the rules window, while it is open
    (define (step! win)
      (let-values ([(w h) (boids-field-size win)])
        (let ([t0 (current-time 'time-monotonic)])
          (window-set! win 'flock
            (boids-step-on (window-ref win 'flock) (window-ref win 'rules)
                           w h))
          ;; one step's time jumps around (collections): over about 20
          (set! step-ms (+ (* step-ms 0.95) (* (ms-since t0) 0.05))))))
    (define (toggle-rules! win)
      (if rules-win
          (close-window! rules-win)
          (set! rules-win
                (boids-open-rules win (lambda () (set! rules-win #f))))))
    (let ([win
           (open-window "Boids"
             'size (cons w (+ h (boids-bar-height)))
             'flock (boids-scatter boids-count w h)
             'rules boids-default-rules
             'tick (lambda (win)
                     (when (and running?
                                (>= (- (now-ms) last-step) boids-step-ms))
                       (set! last-step (now-ms))
                       (step! win)))
             'draw (lambda (win w h)
                     (let ([bar (boids-bar-height)]
                           [flock (window-ref win 'flock)])
                       (fill-rect 0 0 w (- h bar) 'bg-0)
                       (for-each draw-boid flock)
                       (fill-rect 0 (- h bar) w bar 'bg-1)
                       (draw-text 10 (+ (- h bar) 5)
                                  (boids-status-text (length flock) step-ms
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
;;; A slider for the count and for each rule, and the keys' buttons. It
;;; reads and sets the flock window's properties, so it shows them as they
;;; are, whoever changed them: a key, a button or the REPL.

;; A step's cost grows as the count squared. At 300, measured here, a
;; step takes about 16 ms with the default rules and 30 with the heaviest
;; (both blocker rules, 360°, radii of 200): about a 60 Hz frame, and two.
;; Niva's slider stops at 500 for the same reason, where its steps took
;; 12 and 17.
(define boids-max-count 300)

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
  (+ 12 (widest-text (cons "boids"
                           (map (lambda (r) (boids-setting-label (car r)))
                                boids-ranges))
                     'ui)))

(define (boids-readout-width) (+ 12 (text-width "0.0000" 'ui)))

(define boids-buttons '("Randomize (R)" "Defaults (D)" "Scatter (S)"))

;; The rules window's content size: room for its rows (as
;; boids-draw-rules lays them out), with sliders 140 wide, and for three
;; buttons as wide as the widest label.
(define (boids-rules-size)
  (let ([pad (ui-pad)] [gap (ui-gap)] [sh (ui-slider-height)])
    (values (+ pad pad
               (max (+ (boids-label-width) 140 (boids-readout-width))
                    (+ (* 3 (+ 32 (widest-text boids-buttons 'ui)))
                       (* 2 gap))))
            (+ pad (+ sh gap) (+ (ui-control-height) (* 3 gap))
               (* (length boids-ranges) (+ sh gap))
               gap (font-line-height 'ui-small) pad))))

;; The rows, top to bottom: the count, the buttons, a slider for each
;; rule, and a hint. Returns the y below them.
(define (boids-draw-rules ui flock-win w)
  (let* ([pad (ui-pad)] [gap (ui-gap)] [sh (ui-slider-height)]
         [lw (boids-label-width)] [rw (boids-readout-width)]
         [rules (window-ref flock-win 'rules)]
         [n (length (window-ref flock-win 'flock))])
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
      (cons (window-ref flock-win 'rules)
            (length (window-ref flock-win 'flock))))
    (let-values ([(w h) (boids-rules-size)])
      (parameterize ([new-window-position
                      (cons (+ (window-x flock-win) (window-w flock-win) 12)
                            (window-y flock-win))])
        (apply open-window "Boids — Rules"
               'size (cons w h)
               'app #f
               'draw (lambda (win w h)
                       (set! shown (now-shown))
                       (ui-begin! ui)
                       (boids-draw-rules ui flock-win w)
                       (ui-end! ui w h))
               'close (lambda (win) (on-close))
               (append
                (ui-handlers ui 'key (lambda (win key mods)
                                       (window-call flock-win 'key key mods)
                                       #t))
                ;; the kit's tick, then: a change from elsewhere shows
                (list 'tick (lambda (win)
                              (ui-tick ui win)
                              (unless (equal? shown (now-shown))
                                (invalidate! win))))))))))
