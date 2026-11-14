;;; tests/lib.ss — what the suites share, loaded once a suite has found the
;;; tree's root (*schevana-root*): the world loaded but not booted, the
;;; tests' reach into the core (tests/reach.ss), the scratch files, the
;;; checks and the verdict. Each suite boots the world itself, as it needs.
;;;
;;;   (load (string-append *schevana-root* "/tests/lib.ss"))
;;;   ... checks ...
;;;   (test-verdict!)

(load (string-append *schevana-root* "/main.ss"))
(load (string-append *schevana-root* "/tests/reach.ss"))

;; Where a run writes: its first argument, else here.
(define out-dir
  (if (pair? (command-line-arguments)) (car (command-line-arguments)) "."))

;; Never touch the user's real theme file, nor their apps' permissions.
(set! *theme-file-path* (string-append out-dir "/theme.ss"))
(when (file-exists? *theme-file-path*) (delete-file *theme-file-path*))
(set! *grant-file-path* (string-append out-dir "/permissions.ss"))
(when (file-exists? *grant-file-path*) (delete-file *grant-file-path*))

(define failures 0)

;; With SCHEVANA_CHECK set, a run stops after the first check whose name
;; has its text: what ran before it is what a whole run runs, so the check
;; sees what it would there, and the run's verdict is theirs. A long name,
;; and one left empty means none, lest a stray one end every run early.
(define test-only
  (let ([text (getenv "SCHEVANA_CHECK")])
    (and text (not (string=? text "")) text)))
(define (test-only-done name)
  (when (and test-only (string-contains-ci? name test-only))
    (printf "stopped after the check SCHEVANA_CHECK names\n")
    (exit (if (zero? failures) 0 1))))

(define-syntax check
  (syntax-rules ()
    [(_ name expr)
     (let ([ok (guard (c [#t (printf "  error: ~a\n" (condition->string c))
                             #f])
                 expr)])
       (unless ok (set! failures (+ failures 1)))
       (printf "~a ~a\n" (if ok "ok  " "FAIL") name)
       (test-only-done name))]))

;; A form outside a check that raises fails, at its line, and the suite goes
;; on with the next: a region a font leaves out of view ends only what needs
;; it. What the form itself loads or evaluates (an app, a reload) raises as
;; it would. A suite turns it on with (current-eval test-form-eval) once
;; it has booted, or loads a file through it.
(define test-form-running #f)
(define test-form-eval
  (let ([eval* (current-eval)])
    (lambda (x . env)
      (if test-form-running
          (apply eval* x env)
          (guard (c [#t (set! failures (+ failures 1))
                        (printf "FAIL a form at ~a raised: ~a\n"
                                (test-form-place x) (condition->string c))])
            (fluid-let ([test-form-running #t]) (apply eval* x env)))))))

;; Where the loader read x: "tests/smoke.ss:120".
(define (test-form-place x)
  (or (and (annotation? x)
           (call-with-values
            (lambda ()
              (locate-source-object-source (annotation-source x) #t #t))
            (case-lambda
              [(path line char) (format "~a:~a" path line)]
              [r #f])))
      "a line not known"))

;; The world's loop, n times.
(define (steps n) (do ([i 0 (+ i 1)]) ((= i n)) (schevana-step! 0)))

(define (raises? thunk) (guard (c [#t #t]) (thunk) #f))

;; The run's end: its verdict, and its exit status, 1 if a check failed or
;; SCHEVANA_CHECK named none.
(define (test-verdict!)
  (when test-only
    (printf "no check's name has ~s\n" test-only)
    (exit 1))
  (printf "~a\n" (if (zero? failures)
                     "all checks passed"
                     (format "~a check(s) FAILED" failures)))
  (exit (if (zero? failures) 0 1)))
