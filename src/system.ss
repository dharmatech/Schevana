;;; system.ss — the main loop, timers, boot and reload.

(defvar *running* #f)
(defvar *timers* '())     ; ((due-ms . thunk) ...) sorted by due time
(defvar *injected* '())   ; synthetic events waiting to be handled
(defvar *boot-ms* (now-ms))

;; (after 1000 (lambda () (notify "a second later")))
(define (after ms thunk)
  (let ([t (cons (+ (now-ms) ms) thunk)])
    (set! *timers*
      (let ins ([ts *timers*])
        (cond [(null? ts) (list t)]
              [(< (car t) (caar ts)) (cons t ts)]
              [else (cons (car ts) (ins (cdr ts)))])))
    t))

(define (run-timers!)
  (let loop ()
    (when (and (pair? *timers*) (<= (caar *timers*) (now-ms)))
      (let ([t (car *timers*)])
        (set! *timers* (cdr *timers*))
        (safe-call 'timer (cdr t) (lambda (c) (report-error! 'timer c)))
        (loop)))))

;; Feed a synthetic event into the world, exactly as if the host sent it:
;; (inject! '(mouse-down 100 100 left 1))
(define (inject! . events)
  (set! *injected* (append *injected* events)))

(define (handle-event! ev)
  (case (car ev)
    [(mouse-move) (apply wm-mouse-move (cdr ev))]
    [(mouse-down) (apply wm-mouse-down (cdr ev))]
    [(mouse-up) (apply wm-mouse-up (cdr ev))]
    [(wheel) (apply wm-wheel (cdr ev))]
    [(key-down) (apply wm-key-down (cdr ev))]
    [(text) (wm-text (cadr ev))]
    [(resize) (wm-screen-resized!)]
    [(expose) (invalidate!)]
    [(mouse-leave) (set! *hover* #f) (invalidate!)]
    [(quit) (quit!)]))

;; One turn of the world: events, timers, processes, ticks, and a frame
;; if anything changed.
(define (schevana-step! wait-ms)
  (let ([evs (host-events (if (pair? *injected*) 0 wait-ms))]
        [inj *injected*])
    (set! *injected* '())
    (for-each (lambda (ev) (safely 'event (lambda () (handle-event! ev))))
              (append evs inj)))
  (safely 'timers run-timers!)
  (safely 'procs (lambda () (run-procs! 10)))
  (safely 'tick wm-tick!)
  (when *dirty*
    (set! *dirty* #f)
    (safely 'render render-frame!)
    (host-present!)))

(define (schevana-run!)
  (set! *running* #t)
  (let loop ()
    (when *running*
      (schevana-step! (if (procs-ready?) 0 15))
      (loop)))
  (host-shutdown!))

(define (quit!) (set! *running* #f))

(define (uptime-ms) (- (now-ms) *boot-ms*))

;;; ------------------------------------------------------------------ boot

(define default-global-keys
  `(((f1) . ,(lambda () (toggle-launcher!)))
    ((ctrl tab) . ,(lambda () (cycle-windows!)))
    ((ctrl shift #\w) . ,(lambda () (close-focused-window!)))
    ((ctrl shift #\m) . ,(lambda () (when *focus* (toggle-maximize! *focus*))))
    ((ctrl shift #\n) . ,(lambda () (when *focus* (minimize-window! *focus*))))
    ((ctrl shift #\t) . ,(lambda () (tile-windows!)))
    ((ctrl shift #\r) . ,(lambda () (launch-app 'repl)))
    ((f12) . ,(lambda () (notify (format "Saved ~a" (screenshot (next-screenshot-path))))))))

(define (install-default-keys!)
  (for-each (lambda (b) (unless (assoc (car b) *global-keys*) (bind-key! (car b) (cdr b))))
            default-global-keys))

(define (next-screenshot-path)
  (let loop ([i 1])
    (let ([p (format "~a/schevana-~a.png" (or (getenv "HOME") ".") i)])
      (if (file-exists? p) (loop (+ i 1)) p))))

(define user-init-file
  (format "~a/.schevana.ss" (or (getenv "HOME") ".")))

(define (load-user-init!)
  (when (file-exists? user-init-file)
    (safely 'init (lambda () (load user-init-file)))))

(define (schevana-boot! mode w h)
  (host-init! mode w h)
  (install-default-keys!)
  (install-default-status!)
  (invalidate!))

;; Re-read every source file. Code changes take effect; state declared
;; with defvar (windows, theme, processes...) survives.
(define (reload!)
  (for-each (lambda (f) (load (string-append *schevana-root* "/" f))) *schevana-sources*)
  (notify "Sources reloaded" 'green))
