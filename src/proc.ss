;;; proc.ss — processes and the watchdog, built on Chez Scheme engines.
;;;
;;; Schevana is single-threaded. Long computations become *processes*:
;;; engines that get a slice of fuel every frame, so an infinite loop in the
;;; REPL never freezes the world and can be killed with Ctrl+C.
;;;
;;; Every app handler (draw, key, tick, ...) also runs under a fuel budget via
;;; safe-call: if it loops forever or raises, only that window "crashes".

;; defvar, not define: a reload must not swap out parameters that running
;; engines are still parameterizing.
(defvar in-engine? (make-parameter #f))
(defvar current-proc (make-parameter #f))

(define handler-fuel 50000000)   ; per handler call; roughly 0.1s of pure Scheme
(define slice-fuel 10000)        ; per process slice; roughly 15us

(define (make-timeout-condition who)
  (condition (make-error) (make-who-condition who)
             (make-message-condition "exceeded its time budget (infinite loop?)")))

;; Calls thunk. If it raises or runs out of fuel, returns (on-error condition).
(define (safe-call who thunk on-error)
  (if (in-engine?)
      (guard (c [#t (on-error c)]) (thunk))
      (let ([r ((make-engine
                 (lambda ()
                   (parameterize ([in-engine? #t])
                     (guard (c [#t (cons 'error c)])
                       (call-with-values thunk (lambda vals (cons 'ok vals)))))))
                handler-fuel
                (lambda (left r) r)
                (lambda (eng) (cons 'error (make-timeout-condition who))))])
        (if (eq? (car r) 'ok)
            (apply values (cdr r))
            (on-error (cdr r))))))

;; For trusted system code: errors are reported, but there is no fuel limit.
(define (safely who thunk)
  (guard (c [#t (report-error! who c)]) (thunk)))

;;; ------------------------------------------------------------------ processes

(define-record-type proc
  (nongenerative schevana-proc)
  (fields id name (mutable engine) (mutable state) on-done
          (mutable ticks) started (mutable wake)))

(record-writer (record-type-descriptor proc)
  (lambda (p port wr)
    (fprintf port "#<proc ~a ~s ~a>" (proc-id p) (proc-name p) (proc-state p))))

(defvar *procs* '())
(defvar *next-pid* 1)

;; (spawn name thunk [on-done]) starts a process. on-done receives
;; (status value): 'ok and the list of results, 'error and a condition,
;; or 'killed and #f. Output goes wherever current-output-port pointed
;; when spawn was called (so processes started in a REPL print there).
(define spawn
  (case-lambda
    [(name thunk) (spawn name thunk (lambda (status value) (void)))]
    [(name thunk on-done)
     (let* ([out (current-output-port)]
            [p (make-proc *next-pid* name #f 'running on-done 0 (now-ms) 0)])
       (set! *next-pid* (+ *next-pid* 1))
       (proc-engine-set! p
         (make-engine
          (lambda ()
            (parameterize ([in-engine? #t] [current-proc p]
                           [current-output-port out] [current-error-port out])
              (guard (c [#t (cons 'error c)])
                (call-with-values thunk (lambda vals (cons 'ok vals))))))))
       (set! *procs* (append *procs* (list p)))
       p)]))

(define (procs) *procs*)

(define (find-proc p)
  (if (proc? p) p (find (lambda (q) (eqv? (proc-id q) p)) *procs*)))

(define (proc-finish! p state value)
  (when (memq p *procs*)
    (set! *procs* (remq p *procs*))
    (proc-state-set! p state)
    (safe-call 'on-done (lambda () ((proc-on-done p) state value))
               (lambda (c) (report-error! 'proc c)))
    (invalidate!)))

(define (kill! p)
  (let ([p (find-proc p)])
    (when p (proc-finish! p 'killed #f))))

(define (run-slice! p)
  (when (memq p *procs*)
    ((proc-engine p) slice-fuel
     (lambda (left r) (proc-finish! p (car r) (cdr r)))
     (lambda (eng)
       (proc-engine-set! p eng)
       (proc-ticks-set! p (+ (proc-ticks p) slice-fuel))))))

(define (proc-ready? p) (<= (proc-wake p) (now-ms)))

(define (procs-ready?) (exists proc-ready? *procs*))

;; Run ready processes round-robin until budget-ms has elapsed.
(define (run-procs! budget-ms)
  (let ([deadline (+ (now-ms) budget-ms)])
    (let loop ()
      (let ([ready (filter proc-ready? *procs*)])
        (when (and (pair? ready) (< (now-ms) deadline))
          (for-each run-slice! ready)
          (loop))))))

;; Give up the CPU for ms milliseconds (only inside a process).
(define (sleep-ms ms)
  (let ([p (current-proc)])
    (unless p (errorf 'sleep-ms "can only sleep inside a process (use spawn)"))
    (let ([until (+ (now-ms) ms)])
      (proc-wake-set! p until)
      (let loop ()
        (engine-block)
        (when (< (now-ms) until) (loop))))))

;; Give up the rest of this slice.
(define (yield) (when (current-proc) (engine-block)))
