;;; tests/units/gravity.ss — Gravity L0, L1 and L2 (simple), the first
;;; versions of the sim that shows what each level takes away
;;; (docs/GRAVITY.md): L0 run by plain Chez, outside Schevana; L1 in a
;;; world namespace, as a person loads it; L2 in its sandbox. The three
;;; agree to the last digit, the sandbox holds, ticks step the windows,
;;; and a reload keeps them stepping where they were.

;; What L0 prints for count bodies and steps, run by plain Chez from the
;; tree's root as its header says: a line every 100 steps.
(define (gravity-l0-lines count steps)
  (let ([file (string-append out-dir "/gravity-l0.txt")])
    (when (file-exists? file) (delete-file file))
    (units-system-within 60
      (format "cd ~a && exec \"$(command -v chez || echo scheme)\" \
               --libdirs lib --program lib/gravity-l0-simple.sps ~a ~a > ~a"
              (units-sh-word *schevana-root*) count steps
              (units-sh-word file)))
    (if (file-exists? file)
        (call-with-input-file file
          (lambda (p)
            (let loop ([acc '()])
              (let ([l (get-line p)])
                (if (eof-object? l) (reverse acc) (loop (cons l acc)))))))
        '())))

;; 200 bodies, as L1's and L2's windows have.
(define gravity-l0-printed (gravity-l0-lines 200 400))

(check "Gravity L0 (simple) runs under plain Chez, outside Schevana, a \
        program and a library of R6RS's alone, and prints the energy \
        every 100 steps"
       (let ([forms (lambda (f) (call-with-input-file (source-path f)
                                  (lambda (p) (list (read p) (read p)))))])
         (and (equal? (car (forms "lib/gravity-l0-simple.sps"))
                      '(import (rnrs) (gravity)))
              (equal? (list-ref (car (forms "lib/gravity.sls")) 3)
                      '(import (rnrs)))
              (= (length gravity-l0-printed) 5)
              (for-all (lambda (l k)
                         (let ([p (open-input-string l)])
                           (and (eq? (read p) 'step) (eqv? (read p) k)
                                (eq? (read p) 'energy) (flonum? (read p)))))
                       gravity-l0-printed '(0 100 200 300 400)))))

;; L1, loaded as a person would load it, into a world namespace of its
;; own; L2's sandbox, by name, as a reload may make it again.
(define gravity-lab (env-fork (env-root) 'world 'name 'gravity))
(env-load gravity-lab (source-path "lib/gravity-l1-simple.ss"))
(define (gravity-l1-ref name) (env-eval gravity-lab name))
(define (gravity-l2-box) (grant-env 'gravity-l2-simple))
(define (gravity-l2-ref name)
  (top-level-value name (env-environment (gravity-l2-box))))

;; The library's names, and those of L1's window.
(define gravity-names
  '(make-body body-place ring gravity step energy count dt report draw-body))

(check "Gravity L1 (simple) imports the library in a world namespace, \
        where its names stay, and the launcher opens it"
       (let ([win (begin (app-launch! 'gravity-l1-simple)
                         (find-window "Gravity L1 (simple)"))])
         (and (eq? (env-kind gravity-lab) 'world)
              (for-all (lambda (n)
                         (top-level-bound? n (env-environment gravity-lab)))
                       (cons 'gravity-l1-simple gravity-names))
              (not (exists (lambda (n) (top-level-bound? n *world-env*))
                           (cons 'gravity-l1-simple gravity-names)))
              win
              (eq? (window-app win) 'gravity-l1-simple)
              (begin (close-window! win) #t))))

(check "Gravity L2 (simple) runs in its sandbox, holding the window and \
        draw bundles alone, allowed by the table, and its names stay there"
       (let* ([held (environment-symbols (env-environment (gravity-l2-box)))]
              [world (environment-symbols *world-env*)]
              [win (gravity-l2-simple)]
              [ok (and (equal? (env-path (gravity-l2-box))
                               "/apps/gravity-l2-simple")
                       (equal? (grant-requested 'gravity-l2-simple)
                               '(window draw))
                       (null? (grant-undecided 'gravity-l2-simple))
                       (for-all (lambda (n) (memq n held)) gravity-names)
                       (memq 'fill-circle held)
                       (not (memq 'now-ms held))
                       (not (memq 'crew-for* held))
                       (not (exists (lambda (n) (memq n world))
                                    gravity-names))
                       (eq? (window-app win) 'gravity-l2-simple))])
         (close-window! win)
         ok))

(check "Gravity L2's sandbox holds: no import or library, no files, no \
        eval, no clock, none of what L0 and L1 had of Chez beyond it"
       (for-all (lambda (form)
                  (raises? (lambda () (env-eval (gravity-l2-box) form))))
                '((import (gravity))
                  (library (gravity-x) (export) (import (rnrs)))
                  (open-input-file "/etc/passwd")
                  (eval 1 (scheme-environment))
                  (record-writer (record-type-descriptor body) #f)
                  (now-ms))))

;; Both windows, opened together, so that each tick steps both.
(define gravity-l1-win (gravity-l1-ref '(gravity-l1-simple)))
(define gravity-l2-win (gravity-l2-simple))

;; Ticks until both windows have taken k steps, as many as asked.
(define (gravity-tick-to! k)
  (let ([n (- k (window-ref gravity-l2-win 'steps))])
    (unless (>= n 0) (errorf 'gravity-tick-to! "already past step ~a" k))
    (steps n)))

;; A window's bodies as data both versions' can be compared by: (place
;; velocity mass) each.
(define (gravity-state ref win)
  (map (lambda (b)
         (list ((ref 'body-place) b) ((ref 'body-velocity) b)
               ((ref 'body-mass) b)))
       (window-ref win 'bodies)))

(check "Gravity L0, L1 and L2 agree to the last digit: after as many \
        ticks as L0's steps, both windows draw the lines L0 printed, and \
        hold the same bodies"
       (for-all (lambda (k line)
                  (gravity-tick-to! k)
                  (and (string=? (window-ref gravity-l1-win 'line) line)
                       (string=? (window-ref gravity-l2-win 'line) line)
                       (equal? (gravity-state gravity-l1-ref gravity-l1-win)
                               (gravity-state gravity-l2-ref
                                              gravity-l2-win))))
                '(0 100 200 300)
                (list-head gravity-l0-printed 4)))

;; One tick more.
(check "Gravity L1 and L2 take a step a tick, each in its frame handler, \
        well within a handler's fuel"
       (let ([before (window-ref gravity-l2-win 'bodies)]
             [k (window-ref gravity-l2-win 'steps)])
         (steps 1)
         (and (= (window-ref gravity-l1-win 'steps) (+ k 1))
              (= (window-ref gravity-l2-win 'steps) (+ k 1))
              (not (eq? (window-ref gravity-l2-win 'bodies) before))
              (not (window-error gravity-l1-win))
              (not (window-error gravity-l2-win))
              (within-fuel? (lambda ()
                              (do ([i 0 (+ i 1)]) ((= i 20))
                                ((gravity-l2-ref 'step) before 0.01
                                 (gravity-l2-ref 'gravity))))))))

;; At step 301 both hold the same bodies.
(check "Gravity L2 draws what Gravity L1 draws, pixel for pixel, and that \
        is more than the ground"
       (let-values ([(w h) (window-content-size gravity-l2-win)])
         (define (drawn win)
           (image (exact (ceiling w)) (exact (ceiling h)) 1
                  (lambda ()
                    (parameterize ([current-window win])
                      ((hashtable-ref (window-handlers win) 'draw #f)
                       win w h)))))
         (let ([a (drawn gravity-l1-win)] [b (drawn gravity-l2-win)]
               [ground (image (exact (ceiling w)) (exact (ceiling h)) 1
                              (lambda () (fill-box 0 0 w h 'bg-0)))])
           (and (same-bytes? a b) (not (same-bytes? a ground))))))

;; L2's file loads again into its sandbox at a reload, so its record type
;; is defined again: nongenerative, it's the same type, and the bodies the
;; window holds are still bodies. L1's namespace isn't loaded again; what
;; it names of the world's forwards to the world's new definitions.
(check "a reload keeps both stepping where they were: Gravity L2's file \
        loads again, its bodies are still bodies, and both windows go on \
        to draw L0's line for step 400"
       (let ([step-was (gravity-l2-ref 'step)])
         (reload!)
         (gravity-tick-to! 400)
         (and (not (eq? (gravity-l2-ref 'step) step-was))
              (for-all (gravity-l2-ref 'body?)
                       (window-ref gravity-l2-win 'bodies))
              (not (window-error gravity-l1-win))
              (not (window-error gravity-l2-win))
              (= (window-ref gravity-l1-win 'steps) 400)
              (string=? (window-ref gravity-l1-win 'line)
                        (list-ref gravity-l0-printed 4))
              (string=? (window-ref gravity-l2-win 'line)
                        (list-ref gravity-l0-printed 4))
              (equal? (gravity-state gravity-l1-ref gravity-l1-win)
                      (gravity-state gravity-l2-ref gravity-l2-win)))))

(close-window! gravity-l1-win)
(close-window! gravity-l2-win)
