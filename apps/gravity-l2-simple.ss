;;; apps/gravity-l2-simple.ss — Gravity L2 (simple): Gravity L1 in a
;;; sandbox of its own, /apps/gravity-l2-simple (services/grant.ss), with
;;; what the sandbox forces and nothing more (docs/GRAVITY.md):
;;;
;;; - A sandbox has no library and no import, so lib/gravity.sls's body is
;;;   here as top-level forms, line for line, without its library form.
;;; - Its record is nongenerative: the file loads again at each reload, and
;;;   bodies made before it are then still bodies.
;;;
;;; The window is Gravity L1's. A REPL in /apps/gravity-l2-simple has the
;;; names below:
;;; (body-place (car (window-ref (find-window "Gravity L2 (simple)")
;;;                              'bodies)))

(app-requests window draw)

;;; ------------------------------------------------------------------ the sim

;; A body's place and velocity are complex numbers, x+yi; its mass is a
;; real one.
(define-record-type body
  (nongenerative schevana-gravity-body)
  (fields place velocity mass))

;; The ring's mass, shared among its bodies, so that it turns the same
;; however many there are: about what holds a ring of radius 1 together
;; at their speed, 0.5. It breathes as it turns, and after a turn breaks
;; into clumps.
(define ring-mass 0.5)

;; n bodies on the unit circle, each moving along it
(define (ring n)
  (let loop ([k 0] [bodies '()])
    (if (= k n)
        bodies
        (let ([z (make-polar 1.0 (* 6.283185307179586 (/ k n)))])
          (loop (+ k 1)
                (cons (make-body z (* +0.5i z) (/ ring-mass n)) bodies))))))

;; A close pass is softened by this much, a length squared, so that it
;; stays finite.
(define softening 0.01)

;; |z| squared, from its parts. (magnitude z) squared rounds differently,
;; and a version in flonums, without complex numbers, works it out this way.
(define (magnitude-squared z)
  (let ([x (real-part z)] [y (imag-part z)])
    (+ (* x x) (* y y))))

;; The pull toward a body of unit mass d away, softened
(define (gravity d)
  (let ([s (+ (magnitude-squared d) softening)])
    (/ d (* s (sqrt s)))))

;; Every body pulled by all the others under the law pull, a procedure
(define (step bodies dt pull)
  (map (lambda (b)
         (let* ([a (fold-left
                    (lambda (sum o)
                      (if (eq? o b)
                          sum
                          (+ sum (* (body-mass o)
                                    (pull (- (body-place o)
                                             (body-place b)))))))
                    0
                    bodies)]
                [v (+ (body-velocity b) (* dt a))])
           (make-body (+ (body-place b) (* dt v)) v (body-mass b))))
       bodies))

;; What two bodies of unit mass d apart have for being near: gravity is
;; its slope, so a step trades it for speed and back.
(define (potential d)
  (/ -1.0 (sqrt (+ (magnitude-squared d) softening))))

;; The bodies' total energy: each one's for its speed, and each pair's for
;; how near they are. Gravity keeps it, so a step that loses or gains some
;; shows how far a version's numbers wander.
(define (energy bodies)
  (+ (fold-left (lambda (sum b)
                  (+ sum (* 0.5 (body-mass b)
                            (magnitude-squared (body-velocity b)))))
                0
                bodies)
     (let pairs ([bodies bodies] [sum 0])
       (if (null? bodies)
           sum
           (let ([b (car bodies)])
             (pairs (cdr bodies)
                    (fold-left (lambda (sum o)
                                 (+ sum (* (body-mass b) (body-mass o)
                                           (potential (- (body-place o)
                                                         (body-place b))))))
                               sum
                               (cdr bodies))))))))

;;; ------------------------------------------------------------------ window

;; How many bodies, and how far in time a step moves them: L0's.
(define count 200)
(define dt 0.01)

;; The line L0 prints, to draw
(define (report k bodies)
  (string-append "step " (number->string k)
                 "  energy " (number->string (energy bodies))))

;; Where a body shows in a field w × h: the unit circle fills most of it,
;; around its center, y up.
(define (draw-body b w h)
  (let ([z (body-place b)] [scale (* 0.4 (min w h))])
    (fill-circle (+ (/ w 2) (* scale (real-part z)))
                 (- (/ h 2) (* scale (imag-part z)))
                 2 'accent)))

(define-app (gravity-l2-simple) "Gravity L2 (simple)"
  (let ([bodies (ring count)])
    (open-window "Gravity L2 (simple)"
      'size '(520 . 520)
      'bodies bodies
      'steps 0
      'line (report 0 bodies)
      'open (lambda (win) (window-frame-soon! win))
      'frame (lambda (win t)
               (window-frame-soon! win)
               (let ([bodies (step (window-ref win 'bodies) dt gravity)]
                     [k (+ (window-ref win 'steps) 1)])
                 (window-set! win 'bodies bodies)
                 (window-set! win 'steps k)
                 (when (= (mod k 100) 0)
                   (window-set! win 'line (report k bodies)))))
      'draw (lambda (win w h)
              (fill-box 0 0 w h 'bg-0)
              (for-each (lambda (b) (draw-body b w h))
                        (window-ref win 'bodies))
              (draw-text 10 8 (window-ref win 'line) 'text-dim 'ui-small)))))
