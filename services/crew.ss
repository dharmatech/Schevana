;;; services/crew.ss — a crew of worker threads for data-parallel loops.
;;;
;;;   (crew-for 0 rows step! grid next)   (step! lo hi grid next) on disjoint
;;;                                       [lo, hi) covering the range
;;;   (crew-for* 8 0 rows step! grid next)       at least 8 indices a call
;;;   (crew-map square list)              (map square list) on the crew
;;;   (crew-size) (crew-size-set! 4)      how many workers
;;;
;;; What the crew calls is a worklet (define-worklet, src/proc.ss): it
;;; touches only what it is handed, and each call writes only its own
;;; slice. What it is handed holds no procedure but worklets: crew-for
;;; refuses any other among its arguments, or in the pairs, vectors and
;;; boxes they hold, the containers a worklet can open. The caller takes
;;; part, and crew-for returns when every call has returned. A call
;;; should take 20 to 200 µs and allocate nothing:
;;; allocation stops every thread. A call that raises stops new ones from
;;; starting, and its condition is raised in the caller once the running
;;; ones are done. On main the caller waits at most crew-main-ms, then
;;; cancels its calls (a worker's timer escapes from one within
;;; microseconds) and raises, as a runaway handler does. In a process (a
;;; REPL evaluation) the caller only waits, a millisecond at a time, and
;;; killing the process cancels its calls. With no workers, or a range of
;;; one chunk, crew-for is (worklet start end arg ...).
;;;
;;; The crew's state changes only by box-cas!: no thread waits on another
;;; but the idle workers, on the one lock, outside any call. Workers run
;;; no engine: every crew-fuel ticks of a call, a worker's timer looks
;;; whether the call's region was cancelled.

(define crew-fuel 20000)          ; timer ticks between a worker's looks
(define crew-main-ms 100)         ; how long main waits for a region
(define crew-idle-ns 50000000)    ; an idle worker's wait, at most

;; A loop's range, chunk, worklet and the arguments after lo and hi; boxes:
;; the next index, the calls running, the first condition raised, whether
;; its caller cancelled it; main's deadline (ms) or #f, and the process
;; waiting for it or #f.
(define-record-type crew-region
  (nongenerative schevana-crew-region-2)
  (fields start end chunk proc args next running error cancelled deadline
          process))

(defvar *crew-size* #f)             ; #f until set: crew-default-size
(defvar *crew-workers* '())         ; their threads, the oldest first
(defvar *crew-regions* (box '()))   ; the loops running, the newest first
(defvar *crew-mutex* (make-mutex))
(defvar *crew-wake* (make-condition))
;; on a worker: the region of the call it runs, and the way out of it
(defvar crew-current-region (make-thread-parameter #f))
(defvar crew-current-escape (make-thread-parameter #f))

;; SCHEVANA_WORKERS, or one less than the machine's threads, at most 8
;; (more allocating workers slow the main thread down).
(define (crew-default-size)
  (let ([n (let ([s (getenv "SCHEVANA_WORKERS")]) (and s (string->number s)))])
    (if (and (fixnum? n) (>= n 0))
        n
        (max 1 (min 8 (- (host-cpu-count) 1))))))

(define (crew-size) (or *crew-size* (crew-default-size)))

;; More workers start at the next crew-for; workers past n stay idle.
(define (crew-size-set! n)
  (set! *crew-size* n)
  (crew-wake!))

;;; ------------------------------------------------------------------ workers

;; On main: workers up to the size.
(define (crew-ensure!)
  (let loop ([i (length *crew-workers*)])
    (when (< i (crew-size))
      (set! *crew-workers* (append *crew-workers* (list (crew-fork! i))))
      (loop (+ i 1)))))

;; Worker i: its own timer, then turns, each by name, so a reload changes
;; them.
(define (crew-fork! i)
  (fork-thread
   (lambda ()
     (timer-interrupt-handler (lambda () (crew-timer!)))
     (let loop () (crew-worker-step! i) (loop)))))

;; Worker i's turn: an open region's calls, or a wait for one.
(define (crew-worker-step! i)
  (let ([r (and (< i (crew-size)) (crew-open-region))])
    (if r
        (crew-work! r #t)
        (with-mutex *crew-mutex*
          (unless (and (< i (crew-size)) (crew-open-region))
            (condition-wait *crew-wake* *crew-mutex*
                            (make-time 'time-duration crew-idle-ns 0)))))))

;; Wakes the idle workers. The lock is only tried, with interrupts off, so
;; an engine never stops while holding it; if every try fails, a worker
;; wakes at its wait's end, and the caller works meanwhile.
(define (crew-wake!)
  (let try ([n 0])
    (unless (or (with-interrupts-disabled
                 (and (mutex-acquire *crew-mutex* #f)
                      (begin (condition-broadcast *crew-wake*)
                             (mutex-release *crew-mutex*)
                             #t)))
                (= n 1000))
      (try (+ n 1)))))

;; A worker's timer: out of the call when its region is cancelled.
(define (crew-timer!)
  (let ([k (crew-current-escape)])
    (if (and k (crew-cancelled? (crew-current-region)))
        (begin (crew-current-escape #f) (k #f))
        (set-timer crew-fuel))))

;;; ------------------------------------------------------------------ regions

(define (crew-add! b n)
  (let loop ()
    (let ([v (unbox b)]) (unless (box-cas! b v (+ v n)) (loop)))))

;; By its caller, past main's deadline, or its process gone.
(define (crew-cancelled? r)
  (or (unbox (crew-region-cancelled r))
      (let ([d (crew-region-deadline r)]) (and d (> (now-ms) d)))
      (let ([p (crew-region-process r)])
        (and p (not (eq? (proc-state p) 'running))))))

;; Calls left to start.
(define (crew-open? r)
  (and (< (unbox (crew-region-next r)) (crew-region-end r))
       (not (unbox (crew-region-error r)))
       (not (crew-cancelled? r))))

(define (crew-open-region) (find crew-open? (unbox *crew-regions*)))

(define (crew-push! r)
  (let loop ()
    (let ([rs (unbox *crew-regions*)])
      (unless (box-cas! *crew-regions* rs (cons r rs)) (loop)))))

(define (crew-remove! r)
  (let loop ()
    (let ([rs (unbox *crew-regions*)])
      (unless (box-cas! *crew-regions* rs (remq r rs)) (loop)))))

;; Regions whose caller left without removing them (a handler's engine
;; dropped while it waited).
(define (crew-prune!)
  (for-each (lambda (r)
              (unless (or (crew-open? r)
                          (and (> (unbox (crew-region-running r)) 0)
                               (not (crew-cancelled? r))))
                (crew-remove! r)))
            (unbox *crew-regions*)))

;; The next call's start, or #f: counted running before it is claimed,
;; so the caller never sees none running while a claim is under way.
(define (crew-claim! r)
  (crew-add! (crew-region-running r) 1)
  (let loop ()
    (let ([lo (unbox (crew-region-next r))])
      (cond [(not (crew-open? r)) (crew-add! (crew-region-running r) -1) #f]
            [(box-cas! (crew-region-next r) lo (+ lo (crew-region-chunk r)))
             lo]
            [else (loop)]))))

(define (crew-call r lo)
  (guard (c [#t (box-cas! (crew-region-error r) #f c)])
    (apply (crew-region-proc r) lo
           (min (crew-region-end r) (+ lo (crew-region-chunk r)))
           (crew-region-args r))))

;; r's calls until none is left to start. A worker (escape? true) runs
;; each with its timer on; a caller runs them as they are.
(define (crew-work! r escape?)
  (let loop ()
    (let ([lo (crew-claim! r)])
      (when lo
        (if escape?
            (begin
              (call/1cc
               (lambda (k)
                 (crew-current-region r)
                 (crew-current-escape k)
                 (set-timer crew-fuel)
                 (crew-call r lo)))
              (set-timer 0)
              (crew-current-escape #f))
            (crew-call r lo))
        (crew-add! (crew-region-running r) -1)
        (loop)))))

;; Until no call of r runs; on main, at most crew-main-ms past its
;; deadline (for the cancelled calls to escape).
(define (crew-wait! r)
  (let ([give-up (let ([d (crew-region-deadline r)])
                   (and d (+ d crew-main-ms)))])
    (let loop ([spins 0])
      (unless (or (= 0 (unbox (crew-region-running r)))
                  (and give-up (> (now-ms) give-up)))
        (if (< spins 64)
            (loop (+ spins 1))
            (begin (sleep (make-time 'time-duration 20000 0))
                   (loop spins)))))))

;;; ------------------------------------------------------------------ the API

;; The first procedure in x's reach that is no worklet, or #f. A worklet
;; opens only pairs, vectors and boxes (the accessors of records and
;; hashtables are out of its reach), so only those are walked, each once.
(define (crew-stray x)
  (let ([seen (make-eq-hashtable)])
    (let walk ([x x])
      (cond
        [(procedure? x) (and (not (worklet? x)) x)]
        [(not (or (pair? x) (vector? x) (box? x))) #f]
        [(hashtable-contains? seen x) #f]
        [else
         (hashtable-set! seen x #t)
         (cond
           [(pair? x) (or (walk (car x)) (walk (cdr x)))]
           [(box? x) (walk (unbox x))]
           [else (let loop ([i 0])
                   (and (< i (vector-length x))
                        (or (walk (vector-ref x i)) (loop (+ i 1)))))])]))))

;; (crew-for start end worklet arg ...): (worklet lo hi arg ...) for each
;; chunk of [start, end), the range over four times the threads.
(define (crew-for start end proc . args)
  (let ([parts (* 4 (+ (crew-size) 1))])
    (apply crew-for* (max 1 (quotient (+ (- end start) parts -1) parts))
           start end proc args)))

;; crew-for, at least chunk indices a call.
(define (crew-for* chunk start end proc . args)
  (cond
    [(crew-stray (cons proc args))
     => (lambda (p)
          (errorf 'crew-for "the crew runs worklets only (define-worklet): ~s"
                  p))]
    [(<= end start) (void)]
    [(or (= (crew-size) 0) (<= (- end start) chunk))
     (apply proc start end args)]
    [else
     (let* ([p (current-proc)]
            [r (make-crew-region start end chunk proc args (box start) (box 0)
                                 (box #f) (box #f)
                                 (and (not p) (+ (now-ms) crew-main-ms)) p)])
       (crew-prune!)
       (crew-ensure!)
       (crew-push! r)
       (crew-wake!)
       (if p
           (let wait ()   ; a process: no part in it, the desktop runs on
             (unless (and (not (crew-open? r))
                          (= 0 (unbox (crew-region-running r))))
               (sleep-ms 1)
               (wait)))
           (begin (crew-work! r #f) (crew-wait! r)))
       (crew-remove! r)
       (cond [(unbox (crew-region-error r)) => raise]
             [(crew-cancelled? r)
              (raise (make-timeout-condition 'crew-for))]))]))

;; A chunk of crew-map: f of in's items, into out.
(define-worklet (crew-map-chunk lo hi f in out)
  (do ([i lo (fx+ i 1)]) ((fx= i hi))
    (vector-set! out i (f (vector-ref in i)))))

;; (map worklet l), the calls spread over the crew; the results in order.
(define (crew-map f l)
  (let* ([in (list->vector l)] [out (make-vector (vector-length in))])
    (crew-for 0 (vector-length in) crew-map-chunk f in out)
    (vector->list out)))
