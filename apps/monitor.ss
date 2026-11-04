;;; apps/monitor.ss — the world, live: where its time and memory go, from
;;; the profiler (desktop/profile.ss), and its windows and processes.
;;;
;;; Steps: a bar a step, all its time (its waits too: a wait delays the
;;; next frame as much as work), stacked by phase, the work's at the
;;; bottom (the one that spent the most over the graph lowest) and the
;;; waits above, against a line at a frame of the screen's, a step a
;;; frame late (profile-step-late?) flagged at the top; hover a bar for
;;; where its time went, click it to pause there. The profiler's API
;;; tells the same (profile-slow-steps, profile-step-phases), for a
;;; reader without eyes. Consumers: each window, surface and process, and the
;;; collector, by ms a second over the last 5 seconds, with the
;;; average and longest a step, calls a second and the last minute; a
;;; window's row opens to its handlers and frame; a row selected shows
;;; its share of every bar; a window's name focuses it, a process's Kill
;;; kills it. Memory: the heap, the bytes allocated and the collector's
;;; time a second, over the seconds kept. At the top: Profiling (on or
;;; off for the whole world), Pause (this view only), Clear, and how long
;;; the history is. With profiling off: the windows,
;;; the processes, the heap, and how to turn it on. All of it is drawn by
;;; the widget kit: its charts, tree, tables and labels.

;; A bar's phases, all the profiler's (profile-phase-names), so that they
;; add up to the step: (phase color . label), each a series of the kit's
;; charts, the two waits in its muted colors.
(define monitor-phases
  '((event chart-1 . "events")
    (timers chart-2 . "timers")
    (procs chart-3 . "processes")
    (tick chart-4 . "ticks")
    (render chart-5 . "render")
    (present chart-6 . "present")
    (gc chart-7 . "collector")
    (other chart-8 . "other")
    (input-wait chart-muted-1 . "waiting for input")
    (vsync-wait chart-muted-2 . "waiting for the screen")))

;; How long the Consumers sum, in seconds.
(define monitor-consumer-seconds 5)

;; A bar's width in the Steps, pixels.
(define monitor-bar-px 3)

;; The history's lengths Monitor offers: (label . seconds of steps), and
;; (label . minutes of seconds).
(define monitor-step-choices
  '(("10 s" . 10) ("30 s" . 30) ("1 min" . 60) ("2 min" . 120)))
(define monitor-minute-choices
  '(("1 min" . 1) ("10 min" . 10) ("30 min" . 30) ("1 h" . 60)))

(define (monitor-ms ms)
  (if (< ms 9.995) (format "~,2f" ms) (format "~,1f" ms)))


;;; ------------------------------------------------------------------ data

;; ns as words: "250 ms", "1.6 ms", "3.2 s", rounded to a tenth, in
;; integers.
(define (monitor-ns-text ns)
  (let* ([s? (>= ns 1000000000)]
         [t (quotient (+ ns (if s? 50000000 50000)) (if s? 100000000 100000))])
    (format "~a~a ~a" (quotient t 10)
            (if (zero? (remainder t 10)) "" (format ".~a" (remainder t 10)))
            (if s? "s" "ms"))))

;;; The Steps: a bar a step kept, by its seq, read in place from the
;;; profiler's ring, in whole ns.

;; Step seq's ns in phase j (of monitor-phases).
(define (monitor-step-phase-ns seq j)
  (profile-step-phase-ns seq (car (list-ref monitor-phases j))))

;; The fixnums from a to z, z included.
(define (monitor-range a z)
  (let loop ([z z] [acc '()]) (if (fx< z a) acc (loop (fx- z 1) (cons z acc)))))

;; The phases in the order the bars stack them, seqs the steps shown: the
;; work's, the one that spent the most over them all first (at the
;; bottom), so that the bands run on from bar to bar; then the waits, as
;; monitor-phases has them, so that a bar past the graph's top loses a
;; wait, never work. One that spent nothing, none (nor in the legend).
;; => indexes into monitor-phases
(define (monitor-bars-order seqs)
  (let* ([totals (filter (lambda (t) (fx> (cdr t) 0))
                         (map (lambda (j)
                                (cons j (fold-left
                                         (lambda (ns seq)
                                           (fx+ ns (monitor-step-phase-ns
                                                    seq j)))
                                         0 seqs)))
                              (iota (length monitor-phases))))]
         [wait? (lambda (t)
                  (memq (car (list-ref monitor-phases (car t)))
                        '(input-wait vsync-wait)))])
    (map car (append (list-sort (lambda (x y) (fx> (cdr x) (cdr y)))
                                (remp wait? totals))
                     (filter wait? totals)))))

;; A time of day from ms since midnight: "12:00:03.400".
(define (monitor-clock-text ms)
  (let ([two (lambda (n) (if (< n 10) (format "0~a" n) (format "~a" n)))]
        [ms (mod ms 86400000)])
    (format "~a:~a:~a.~a" (two (quotient ms 3600000))
            (two (remainder (quotient ms 60000) 60))
            (two (remainder (quotient ms 1000) 60))
            (let ([m (remainder ms 1000)])
              (cond [(< m 10) (format "00~a" m)]
                    [(< m 100) (format "0~a" m)]
                    [else (format "~a" m)])))))

;; The ms of the day at the profiler's clock's 0, by the clock now.
(define (monitor-clock-zero)
  (let ([d (current-date)] [ns (now-ns)])
    (- (+ (* 3600000 (date-hour d)) (* 60000 (date-minute d))
          (* 1000 (date-second d)) (quotient (date-nanosecond d) 1000000))
       (quotient ns 1000000))))

;; A subject is (window . id), (process . id), a surface's name or gc:
;; the subjects Monitor lists.
(define (monitor-subject? s)
  (or (pair? s) (eq? s 'gc) (and (symbol? s) (find-surface s) #t)))

(define (monitor-window? s) (and (pair? s) (eq? (car s) 'window)))

;; A row's node: a subject, or one of its parts, (subject part phase).
(define (monitor-part? node) (and (pair? node) (pair? (cdr node))))

;; A subject's window or process while it lives, found by its id; #f once
;; it is closed or ended (or for a surface or the collector).
(define (monitor-live s)
  (and (pair? s)
       (if (monitor-window? s) (find-window (cdr s)) (find-proc (cdr s)))))

;; A window's title, or the one it had; a process's name.
(define (monitor-title s)
  (let ([live (monitor-live s)])
    (cond [(and live (monitor-window? s)) (window-title live)]
          [live (proc-name live)]
          [else (or (profile-subject-name s)
                    (format "~a ~a" (car s) (cdr s)))])))

(define (monitor-subject-label s)
  (let ([live (monitor-live s)])
    (cond [(not (pair? s)) (if (eq? s 'gc) "Collector" (format "~a" s))]
          [(monitor-window? s)
           (if live (monitor-title s) (format "~a (closed)" (monitor-title s)))]
          [else (format "~a (process ~a~a)" (monitor-title s) (cdr s)
                        (if live "" ", ended"))])))

;; Labels of subjects, ((subject . label) ...): a window's title (the one
;; it had, once closed), with its id where another window among them has
;; the same; a process's name and id; a surface's name.
(define (monitor-labels subjects)
  (let ([titles (map (lambda (s) (and (monitor-window? s) (monitor-title s)))
                     subjects)])
    (map (lambda (s t)
           (cons s
                 (if (and t (> (length (filter (lambda (o) (equal? o t))
                                               titles))
                               1))
                     (format "~a (window ~a~a)" t (cdr s)
                             (if (monitor-live s) "" ", closed"))
                     (monitor-subject-label s))))
         subjects titles)))

;; A part: a handler's kind, or a window's frame, or a surface's or the
;; collector's phase (the collector's own, gc: outside them).
(define (monitor-part-label subject part phase)
  (cond [part (symbol->string part)]
        [(monitor-window? subject) "frame"]
        [(eq? phase 'gc) "outside the phases"]
        [(eq? subject 'gc) (format "during ~a" phase)]
        [(eq? phase 'render) "draw"]
        [else (symbol->string phase)]))

;; Where step seq's time went, a bar's tooltip, a line each: when it
;; ended (zero, the ms of the day at the clock's 0), how long it took
;; and whether it was a frame late (frame, ns), its phases
;; (profile-step-phases) and its three largest consumers, the most first.
(define (monitor-step-tip seq zero frame)
  (let* ([ns (profile-step-ns seq 'ns)]
         [phases (filter (lambda (p) (>= (cdr p) 50000))   ; 0.1 ms shown
                         (profile-step-phases seq))]
         [top (filter (lambda (t) (and (monitor-subject? (profile-total-what t))
                                       (>= (profile-total-ms t) 0.05)))
                      (profile-totals (list seq) #t))]
         [top (list-head top (min 3 (length top)))]
         [labels (monitor-labels (map profile-total-what top))])
    (define (list-of items)   ; (name . words) a line each
      (apply string-append
             (map (lambda (x) (format "\n~a ~a" (car x) (cdr x))) items)))
    (string-append
     (format "step ~a, ended ~a\n" seq
             (monitor-clock-text
              (+ zero (quotient (profile-step-ns seq 'end) 1000000))))
     (format "~a, ~a (a frame is ~a)" (monitor-ns-text ns)
             (if (profile-step-late? seq frame) "a frame late" "in time")
             (monitor-ns-text frame))
     (list-of (map (lambda (p)
                     (cons (cddr (assq (car p) monitor-phases))
                           (monitor-ns-text (cdr p))))
                   phases))
     (list-of (map (lambda (t)
                     (cons (cdr (assoc (profile-total-what t) labels))
                           (format "~a ms" (monitor-ms (profile-total-ms t)))))
                   top)))))

;;; ------------------------------------------------------------------ the app

;; With profiling off: the windows (a click focuses one) and the
;; processes (a click kills one) in the kit's tables, and the world's
;; heap and last frame, from y in a column x w wide. => the y below
(define (monitor-draw-lists ui x y w)
  (let* ([lh (font-line-height 'ui-small)]
         [rh (ui-table-row-height)]
         [table-h (lambda (n most)
                    (+ (ui-table-header-height) (* rh (max 1 (min n most)))))]
         [label (lambda (y flags text) (ui-draw-label x y w lh flags text))]
         [section (lambda (y title words)
                    (label y '(section) title)
                    (label y '(dim right) words)
                    (+ y lh 6))]
         [cell (lambda (text-of)   ; a cell: its row's text for its key
                 (lambda (row key cx cy cw ch flags)
                   (ui-draw-label (+ cx 10) cy (- cw 20) ch
                                  (if (eq? row (focused-window)) '() '(dim))
                                  (text-of row key))))]
         [wins (sort (lambda (a b) (< (window-id a) (window-id b))) (windows))]
         [y (section y "Windows" "a click focuses one")]
         [wh (table-h (length wins) 8)])
    (ui-table ui 'monitor-windows x y w wh
              (let ([w (ui-table-room w)])
                `((id "Id" ,(* w 0.1)) (title "Title" ,(* w 0.38))
                  (app "App" ,(* w 0.2)) (geometry "Geometry" ,(* w 0.32))))
              wins #f focus-window!
              (cell (lambda (win key)
                      (case key
                        [(id) (number->string (window-id win))]
                        [(title) (window-title win)]
                        [(app) (format "~a" (or (window-app win) "–"))]
                        [else (format "~ax~a+~a+~a~a"
                                      (window-w win) (window-h win)
                                      (window-x win) (window-y win)
                                      (cond [(window-error win) " crashed"]
                                            [(eq? (window-state win) 'normal)
                                             ""]
                                            [else (format " ~a"
                                                          (window-state
                                                           win))]))]))))
    (let* ([y (section (+ y wh 12) "Processes" "a click kills one")]
           [y (if (null? (procs))
                  (label y '(hint) "none — try (spawn \"x\" (lambda () \
                                    (let l () (l)))) in a REPL")
                  (let ([ph (table-h (length (procs)) 4)])
                    (ui-table ui 'monitor-procs x y w ph
                              (let ([w (ui-table-room w)])
                                `((id "Id" ,(* w 0.1))
                                  (name "Name" ,(* w 0.38))
                                  (ticks "Ticks" ,(* w 0.2))
                                  (age "Running" ,(* w 0.32))))
                              (procs) #f kill!
                              (cell (lambda (p key)
                                      (case key
                                        [(id) (number->string (proc-id p))]
                                        [(name) (proc-name p)]
                                        [(ticks) (format "~,1fM"
                                                         (/ (proc-ticks p)
                                                            1e6))]
                                        [else (format "~,1f s"
                                                      (/ (- (now-ms)
                                                            (proc-started p))
                                                         1000.0))]))))
                    (+ y ph)))]
           [y (section (+ y 12) "World" "")]
           [y (label y '(dim)
                     (format "heap ~,1f MB, ~a collections; up ~a s; ~a apps"
                             (/ (bytes-allocated) 1048576.0) (collections)
                             (quotient (uptime-ms) 1000) (length (apps))))])
      (label y '(dim) (format "the last frame: drawn in ~,1f ms, ~a of ~a \
                               rows uploaded"
                              *frame-ms* *frame-rows* *canvas-ph*)))))

(define-app (monitor) "Monitor"
  (let ([ui (make-ui)]
        [view #f]          ; the history shown: (newest step . seconds)
        [last 0]           ; when it moved on (now-ms): twice a second
        [paused? #f]
        [held #f]          ; paused: the newest step shown (its seq)
        [pinned #f]        ; the bar clicked (its step's seq)
        [selected #f]      ; the row selected: a subject, or (subject part
                           ; phase)
        [order '(rate . down)]   ; the Consumers' sort: (key . up|down)
        [shares (make-hashtable equal-hash equal?)]   ; (seq . selected): ns
        [tips (make-eqv-hashtable)]                   ; seq: text
        [drawn #f]         ; the Steps as last drawn: (newest hover)
        [memos (make-hashtable equal-hash equal?)]   ; the view's figures
        [memo-view #f])
    (define (move-on!)
      (set! view (cons (profile-newest-step) (profile-seconds)))
      (set! last (now-ms)))
    ;; the newest step kept, or the one held
    (define (newest-step) (or held (profile-newest-step)))
    ;; thunk's value, worked out once a view: Monitor is drawn again with
    ;; whatever frame holds it, far more often than its view moves on.
    (define (remember key thunk)
      (unless (eq? memo-view view)
        (hashtable-clear! memos)
        (set! memo-view view))
      (or (hashtable-ref memos key #f)
          (let ([v (thunk)]) (hashtable-set! memos key v) v)))
    (define (history)
      (unless view (move-on!))
      view)
    (define (pause! seq)
      (set! held (newest-step))
      (set! paused? #t)
      (set! pinned seq))
    (define (resume!)
      (set! paused? #f)
      (set! held #f)
      (set! pinned #f)
      (move-on!))
    (define (toolbar w)
      (let*-values
          ([(pad gap) (values (ui-pad) (ui-gap))]
           [(steps-label) (format "Steps: ~a s" *profile-step-seconds*)]
           [(minutes-label) (format "Seconds: ~a min" *profile-minutes*)]
           [(kw kh) (ui-checkbox-size "Profiling")]
           [(pw ph) (ui-button-size "Resume" #f)]
           [(cw ch) (ui-button-size "Clear" #f)]
           [(dw dh) (ui-dropdown-size (list steps-label minutes-label))]
           [(none?) (not (car (history)))]   ; no step yet
           [(items)
            (list
             (list kw kh
                   (lambda (x y w h)
                     (ui-checkbox ui 'profiling x y w h "Profiling"
                                  (profiling?)
                                  (lambda (on?) (profiling! on?))
                                  'tip "Profile the world: (profiling! #t)")))
             (list pw ph
                   (lambda (x y w h)
                     (ui-button ui 'pause x y w h
                                (if paused? "Resume" "Pause")
                                (lambda () (if paused? (resume!) (pause! #f)))
                                'disabled (and none? (not paused?))
                                'tip "Hold this view still")))
             (list cw ch
                   (lambda (x y w h)
                     (ui-button ui 'clear x y w h "Clear"
                                (lambda ()
                                  (profile-clear!)
                                  (hashtable-clear! shares)
                                  (hashtable-clear! tips)
                                  (resume!))
                                'quiet #t 'disabled none?
                                'tip "Forget the history")))
             (list dw dh
                   (lambda (x y w h)
                     (ui-dropdown ui 'steps x y w h monitor-step-choices
                                  *profile-step-seconds*
                                  (lambda (s)
                                    (profile-history! s *profile-minutes*))
                                  'label steps-label
                                  'tip "How long every step is kept")))
             (list dw dh
                   (lambda (x y w h)
                     (ui-dropdown ui 'minutes x y w h monitor-minute-choices
                                  *profile-minutes*
                                  (lambda (m)
                                    (profile-history! *profile-step-seconds*
                                                      m))
                                  'label minutes-label
                                  'tip "How long each second's sums are \
                                        kept"))))]
           [(at height) (ui-flow pad gap (- w pad pad) gap
                                 (map (lambda (i) (cons (car i) (cadr i)))
                                      items))])
        (ui-draw-surface 0 0 w (+ height gap gap) '(toolbar))
        (for-each (lambda (i p) ((caddr i) (car p) (cdr p) (car i) (cadr i)))
                  items at)
        (+ height gap gap)))
    ;; A section's label at y, and words at its right; the y below.
    (define (section x y w title words)
      (let ([lh (font-line-height 'ui-small)])
        (ui-draw-label x y w lh '(section) title)
        (ui-draw-label x y w lh '(dim right) words)
        (+ y lh 6)))
    ;; The Consumers' rows over the last complete seconds (at most 5 of
    ;; them): the totals, the subjects listed (those spent, then the live
    ;; windows and processes) and their labels, and the seconds' span.
    (define (consumers seconds)
      (remember 'consumers
        (lambda ()
          (let* ([recent (list-head seconds
                                    (min monitor-consumer-seconds
                                         (length seconds)))]
                 [totals (filter (lambda (t)
                                   (monitor-subject? (profile-total-what t)))
                                 (profile-totals recent))]
                 [spent (map profile-total-what totals)]
                 [roots (append spent
                                (remp (lambda (s) (member s spent))
                                      (map profile-subject-of
                                           (append (reverse (windows))
                                                   (procs)))))])
            (list totals roots (monitor-labels roots)
                  (max 1 (length recent)))))))
    (define (label-of s seconds)
      (let ([l (assoc s (caddr (consumers seconds)))])
        (if l (cdr l) (monitor-subject-label s))))
    ;; A bar for each step that fits, the newest at the right, in the
    ;; phases' order by what they spent, up to two frames of the screen's
    ;; (a longer step reaches the top), the frame's line at half, a step a
    ;; frame late flagged at the top; the legend under them in the same
    ;; order; a bar's tooltip says where its time went.
    (define (draw-steps x y w h seconds)
      (let*-values
          ([(frame) (host-frame-ns)]
           [(top) (* 2 frame)]
           [(rules) (list (cons frame (monitor-ns-text frame))
                          (cons top (monitor-ns-text top)))]
           [(newest) (newest-step)]
           ;; as many bars as fit right of the scale
           [(n) (quotient (caddr (cdr (assq 'plot
                                            (ui-bar-chart-areas
                                             x y w h 1 rules))))
                          monitor-bar-px)]
           [(seqs) (if newest
                       (filter profile-step-kept?
                               (monitor-range (- newest n -1) newest))
                       '())]
           [(order) (monitor-bars-order seqs)]
           [(keys) (append (map (lambda (j) (cdr (list-ref monitor-phases j)))
                                order)
                           (if selected
                               (list (cons 'chart-share
                                           (if (monitor-part? selected)
                                               (apply monitor-part-label
                                                      selected)
                                               (label-of selected seconds))))
                               '()))]
           [(kw kh) (ui-legend-size keys)]
           [(gh) (- h kh 4)]
           [(zero) (monitor-clock-zero)]
           [(hover)
            (ui-bar-chart ui 'monitor-steps x y w gh
                          (map (lambda (seq)
                                 (cons (map (lambda (j)
                                              (cons (cadr (list-ref
                                                           monitor-phases j))
                                                    (monitor-step-phase-ns
                                                     seq j)))
                                            order)
                                       (and selected (share-of seq))))
                               seqs)
                          top rules
                          (and pinned (list-index (lambda (s) (eqv? s pinned))
                                                  seqs))
                          (lambda (i) (pause! (list-ref seqs i)))
                          'tip (lambda (i)
                                 (tip-of (list-ref seqs i) zero frame))
                          'flagged (let ([v (list->vector seqs)])
                                     (lambda (i)
                                       (profile-step-late? (vector-ref v i)
                                                           frame))))])
        (set! drawn (list newest hover))
        (ui-draw-legend x (+ y gh 4) w kh '() keys)))
    ;; Step seq's tooltip, worked out once.
    (define (tip-of seq zero frame)
      (or (hashtable-ref tips seq #f)
          (let ([text (monitor-step-tip seq zero frame)])
            (when (> (hashtable-size tips) 64) (hashtable-clear! tips))
            (hashtable-set! tips seq text)
            text)))
    ;; What the row selected spent in step seq, ns, worked out once.
    (define (share-of seq)
      (let ([key (cons seq selected)])
        (or (hashtable-ref shares key #f)
            (let ([ns (profile-step-node-ns seq selected)])
              (when (> (hashtable-size shares) 4096)
                (hashtable-clear! shares))
              (hashtable-set! shares key ns)
              ns))))
    ;; Whether the Steps would look different now: a new step, or a bar
    ;; under the pointer (its tip).
    (define (steps-moved?)
      (or (not drawn)
          (not (eqv? (car drawn) (newest-step)))
          (cadr drawn)))
    (define (draw-consumers x y w h seconds)
      (let* ([c (consumers seconds)]
             [totals (car c)] [roots (cadr c)] [span (cadddr c)]
             [minute (remember 'minute
                       (lambda ()
                         (reverse (list-head seconds
                                             (min 60 (length seconds))))))]
             [cw (font-char-width 'ui)]
             [num (max (* 8 cw) (+ 28 (text-width "calls/s" 'ui-small)))]
             [spark (* 13 cw)]
             ;; each heading's tooltip says what its column means
             [columns
              `((name "Consumer"
                      ,(max 120 (- (ui-table-room w) (* 4 num) spark))
                      "A window, surface or process, or the collector; a \
                       window's row opens to its handlers and frame")
                (rate "ms/s" ,num right
                      ,(format "Milliseconds it spent a second, on average \
                                over the last ~a s: its share of the main \
                                loop's time" monitor-consumer-seconds))
                (avg "avg" ,num right
                     "Milliseconds it took in a step, on average over \
                      the steps it ran in")
                (max "max" ,num right
                     ,(format "The most milliseconds it took in one step: \
                               past a frame (~a), it alone held a frame back"
                              (monitor-ns-text (host-frame-ns))))
                (calls "calls/s" ,num right
                       "How many times a second it ran: a handler called, \
                        a frame drawn, a process's turn")
                (spark "last minute" ,spark
                       "Its milliseconds each second of the last minute, \
                        the newest at the right"))])
        (define (total-of node)
          (let ([t (find (lambda (t)
                           (equal? (profile-total-what t)
                                   (if (monitor-part? node) (car node) node)))
                         totals)])
            (and t (if (monitor-part? node)
                       (find (lambda (p)
                               (equal? (profile-total-what p)
                                       (cons (cadr node) (caddr node))))
                             (profile-total-parts t))
                       t))))
        ;; node's ms in each second of the last minute, the oldest first
        (define (minute-of node)
          (remember (list 'spark node)
                    (lambda ()
                      (map (lambda (s) (profile-second-node-ms s node))
                           minute))))
        ;; what node sorts by in the column key: its label, or a number (0
        ;; for none; the last minute's sum)
        (define (sort-value node key)
          (let ([t (total-of node)])
            (case key
              [(name) (if (monitor-part? node)
                          (apply monitor-part-label node)
                          (label-of node seconds))]
              [(rate) (if t (/ (profile-total-ms t) span) 0)]
              [(avg) (if (and t (> (profile-total-steps t) 0))
                         (/ (profile-total-ms t) (profile-total-steps t))
                         0)]
              [(max) (if t (profile-total-longest t) 0)]
              [(calls) (if t (/ (profile-total-calls t) span) 0)]
              [else (apply + (minute-of node))])))
        ;; siblings in the order order names, ties as they come
        (define (sorted nodes)
          (let ([key (car order)] [down? (eq? (cdr order) 'down)]
                [less? (if (eq? (car order) 'name) string-ci<? <)])
            (list-sort (lambda (a b)
                         (let ([va (sort-value a key)] [vb (sort-value b key)])
                           (if down? (less? vb va) (less? va vb))))
                       nodes)))
        ;; a subject's parts, (subject part phase), when it has more than one
        (define (children node)
          (let ([t (and (not (monitor-part? node)) (total-of node))])
            (if (and t (> (length (profile-total-parts t)) 1))
                (sorted (map (lambda (p)
                               (let ([what (profile-total-what p)])
                                 (list node (car what) (cdr what))))
                             (profile-total-parts t)))
                '())))
        (define (cell node key cx cy cw ch flags)
          (let ([t (total-of node)] [in 10])
            (define (number v)
              (ui-draw-label cx cy (- cw in) ch '(right) v))
            (case key
              [(name)
               (let* ([s (if (monitor-part? node) (car node) node)]
                      [label (if (monitor-part? node)
                                 (monitor-part-label s (cadr node) (caddr node))
                                 (label-of s seconds))]
                      [live (monitor-live s)]
                      [id (and live (cdr s))]
                      [kill? (and live (not (monitor-part? node)) (proc? live))]
                      [bw (if kill? (+ 16 (text-width "Kill" 'ui)) 0)]
                      ;; after the twisty, where the heading's label starts
                      [nx (+ cx in)]
                      [room (- cw in bw 4)])
                 (ui-draw-label nx cy room ch
                                (if (and (pair? s) (not live))
                                    '(dim)
                                    '())
                                label)
                 ;; the live window or process, looked up by its id
                 (when (and live (not (monitor-part? node)) (window? live))
                   (ui-pressable! ui (list 'monitor-focus id)
                                  nx cy (min room (text-width label 'ui)) ch
                                  (lambda ()
                                    (let ([w (find-window id)])
                                      (when w (focus-window! w))))
                                  '()
                                  (list 'focus 'keep 'tip
                                        (format "Focus ~a" label))
                                  '((L . "Focus"))))
                 (when kill?
                   (ui-button ui (list 'monitor-kill id)
                              (+ cx cw (- bw) -4) (+ cy 3) bw (- ch 6) "Kill"
                              (lambda ()
                                (let ([p (find-proc id)]) (when p (kill! p))))
                              'danger #t 'quiet #t)))]
              [(rate) (number (monitor-ms (if t (/ (profile-total-ms t) span)
                                              0.0)))]
              [(avg) (number (if (and t (> (profile-total-steps t) 0))
                                 (monitor-ms (/ (profile-total-ms t)
                                                (profile-total-steps t)))
                                 "–"))]
              [(max) (number (if t (monitor-ms (profile-total-longest t)) "–"))]
              [(calls) (number (if t
                                   (let ([c (/ (profile-total-calls t) span)])
                                     (if (< c 99.95)
                                         (format "~,1f" c)
                                         (number->string (exact (round c)))))
                                   "–"))]
              [(spark)
               (ui-draw-sparkline (+ cx 4) (+ cy 4) (- cw 12) (- ch 8) flags
                                  (minute-of node))])))
        (ui-tree ui 'consumers x y w h columns (sorted roots) children selected
                 (lambda (node) (set! selected node))
                 cell
                 'sort order
                 ;; by a number, the most first; again, the other way
                 'on-sort (lambda (key)
                            (set! order
                                  (cond [(not (eq? key (car order)))
                                         (cons key (if (eq? key 'name)
                                                       'up
                                                       'down))]
                                        [(eq? (cdr order) 'up)
                                         (cons key 'down)]
                                        [else (cons key 'up)]))))))
    (define (draw-memory x y w h seconds)
      (let*-values
          ([(mb) (lambda (b) (/ b 1048576.0))]
           [(at) (lambda (s name) (profile-second-ref s name))]
           [(latest) (and (pair? seconds) (car seconds))]
           [(keys)
            (if latest
                (list (cons 'chart-8
                            (format "heap ~,1f MB" (mb (at latest 'heap))))
                      (cons 'chart-1
                            (format "allocated ~,1f MB/s"
                                    (mb (at latest 'allocated))))
                      (cons 'chart-7
                            (format "collector ~a ms/s, ~a collections/s"
                                    (monitor-ms (at latest 'gc-ms))
                                    (at latest 'gcs))))
                (list (cons 'chart-8 "heap")
                      (cons 'chart-1 "allocated a second")
                      (cons 'chart-7 "collector's time a second")))]
           [(kw kh) (ui-legend-size keys)]
           [(gh) (- h kh 6)])
        (ui-draw-line-chart
         x y w gh '()
         (remember 'memory
           (lambda ()
             (let ([shown (reverse seconds)])
               (list (cons 'chart-8
                           (map (lambda (s) (mb (at s 'heap))) shown))
                     (cons 'chart-1
                           (map (lambda (s) (mb (at s 'allocated))) shown))
                     (cons 'chart-7
                           (map (lambda (s) (at s 'gc-ms)) shown)))))))
        (ui-draw-legend x (+ y gh 6) w kh '() keys)))
    (define (draw win w h)
      (ui-begin! ui)
      (ui-draw-surface 0 0 w h '(page))
      (let ([y (toolbar w)] [pad (ui-pad)])
        (if (profiling?)
            (let* ([h* (history)] [seconds (cdr h*)]
                   [x pad] [cw (- w pad pad)]
                   [lh (font-line-height 'ui-small)]
                   [steps-h (round (* 9 lh))]
                   [memory-h (round (* 5 lh))]
                   [rate (if (pair? seconds)
                             (profile-second-ref (car seconds) 'steps)
                             0)]
                   [y (section x (+ y 4) cw "Steps"
                               (format "a bar a step, flagged a frame \
                                        late; ~a steps/s~a"
                                       rate (if paused? ", paused" "")))]
                   [y (begin (draw-steps x y cw steps-h seconds)
                             (+ y steps-h 8))]
                   [y (section x y cw "Consumers"
                               (format "a second's average over the \
                                        last ~a s"
                                       monitor-consumer-seconds))]
                   [consumers-h (max (* 3 (ui-table-row-height))
                                     (- h y memory-h lh 6 8 pad))]
                   [y (begin (draw-consumers x y cw consumers-h seconds)
                             (+ y consumers-h 8))]
                   [y (section x y cw "Memory"
                               (let ([n (length seconds)])
                                 (if (< n 120)
                                     (format "over the last ~a s" n)
                                     (format "over the last ~a min"
                                             (quotient n 60)))))])
              (draw-memory x y cw (- h y pad) seconds))
            (let ([y (+ y 8)])
              (ui-draw-label pad y (- w pad pad) 0 '(hint wrap)
                             "Profiling is off: check Profiling, or type \
                              (profiling! #t) in a REPL, to see where the \
                              world's time and memory go.")
              (monitor-draw-lists ui pad
                                  (+ y (* 3 (font-line-height 'ui-small)) 8)
                                  (- w pad pad)))))
      (ui-end! ui w h))
    (apply open-window "Monitor"
           'size (cons (max 640 (+ 160 (* 58 (font-char-width 'ui))))
                       (max 600 (* 21 (ui-table-row-height))))
           'min-size '(420 . 360)
           'draw (lambda (win w h) (draw win w h))
           (append
            (ui-handlers ui)
            ;; the Steps at every new step, the rest twice a second
            (list 'tick (lambda (win)
                          (ui-tick ui win)
                          (unless paused?
                            (when (> (- (now-ms) last) 500)
                              (move-on!)
                              (invalidate! win))
                            (when (and (profiling?) (steps-moved?))
                              (invalidate! win)))))))))
