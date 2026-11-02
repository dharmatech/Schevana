;;; status.ss — status segments: the right side of the top bar is a list of
;;; (name . thunk); each thunk returns a string (or #f to hide). Add your
;;; own:
;;;   (add-status-segment! 'hello (lambda () "hi!"))

;; the defaults, until you change them (a reload keeps yours)
(defvar *status-segments*
  (list (cons 'procs (lambda () (status-procs)))
        (cons 'frame (lambda () (status-frame)))
        (cons 'memory (lambda () (status-memory)))
        (cons 'clock (lambda () (status-clock)))))

(define (add-status-segment! name thunk)
  (set! *status-segments*
    (append (remp (lambda (s) (eq? (car s) name)) *status-segments*)
            (list (cons name thunk))))
  (request-frame!))

(define (remove-status-segment! name)
  (set! *status-segments*
    (remp (lambda (s) (eq? (car s) name)) *status-segments*))
  (request-frame!))

;; What the top bar shows: ((name . string-or-#f) ...), taken before a
;; frame that draws the top bar, each segment under the watchdog.
(defvar *status-shown* '())

(define (refresh-status!)
  (set! *status-shown*
    (map (lambda (s)
           (cons (car s)
                 (safe-call 'status (cdr s)
                            (lambda (c) (format "⚠ ~a" (car s))))))
         *status-segments*)))

(define (shown-status-strings) (filter string? (map cdr *status-shown*)))

(defvar *last-second* -1)

;; Once a second the bar that shows the status asks for itself, box (the
;; clock). The status shown changes only with that bar: it is taken when
;; the frame this step draws holds the bar.
(define (status-tick box)
  (let ([s (time-second (current-time))])
    (unless (= s *last-second*)
      (set! *last-second* s)
      (call-with-values box request-frame!)))
  (when (and *dirty* (holds? (frame-box *dirty*)
                             (call-with-values box pixel-box)))
    (refresh-status!)))

(define (status-procs)
  (let ([n (length *procs*)])
    (and (> n 0) (format "⚙ ~a running" n))))

(define (status-memory) (format "~,1f MB" (/ (bytes-allocated) 1048576.0)))

(define (status-frame) (format "~,1f ms/frame" *frame-ms*))

(define day-names '#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
(define month-names
  '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define (status-clock)
  (let ([d (current-date)])
    (format "~a ~a ~a   ~2,'0d:~2,'0d:~2,'0d"
            (vector-ref day-names (date-week-day d)) (date-day d)
            (vector-ref month-names (- (date-month d) 1))
            (date-hour d) (date-minute d) (date-second d))))
