;;; lib/gravity.sls — Gravity's sim, a plain R6RS library: bodies on a
;;; plane, each pulled by all the others, a step at a time. Gravity L0
;;; (simple), lib/gravity-l0-simple.sps, runs it as a program of its own;
;;; Gravity L1 (simple) imports it in the world; Gravity L2 (simple) is a
;;; copy of it in a sandbox. docs/GRAVITY.md says what each level takes
;;; away from it, and why.
;;;
;;; Its body isn't indented, so that L2's copy reads the same, line for
;;; line, and a diff of the two shows only what the sandbox changed.

(library (gravity)
  (export make-body body? body-place body-velocity body-mass
          ring gravity step energy)
  (import (rnrs))

;; A body's place and velocity are complex numbers, x+yi; its mass is a
;; real one.
(define-record-type body (fields place velocity mass))

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

)
