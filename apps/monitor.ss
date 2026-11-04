;;; apps/monitor.ss — the world, live: where its time and memory go, from
;;; the profiler (desktop/profile.ss), and its windows and processes.
;;;
;;; Steps: the last 10 seconds in columns of equal time, each the slowest
;;; step in it, its time but idle stacked by phase, and a line at a frame's
;;; 16.7 ms; hover a column for what its step spent, click it to pause
;;; there. Consumers: each window, surface and process,
;;; and the collector, by ms a second over the last 5 seconds, with the
;;; average and longest a step, calls a second and the last minute; a
;;; window's row opens to its handlers and frame; a row selected shows
;;; its share of every bar; a window's name focuses it, a process's Kill
;;; kills it. Memory: the heap, the bytes allocated and the collector's
;;; time a second, over the seconds kept. At the top: Profiling (on or
;;; off for the whole world), Pause (this view only), Clear, and how long
;;; the history is. With profiling off: the windows, the processes, the
;;; heap, and how to turn it on. All of it is drawn by the widget kit:
;;; its charts, tree, tables and labels.

;; A bar's phases, bottom up: (phase color . label), each a series of
;; the kit's charts.
(define monitor-phases
  '((event chart-1 . "events")
    (timers chart-2 . "timers")
    (procs chart-3 . "processes")
    (tick chart-4 . "ticks")
    (render chart-5 . "render")
    (present chart-6 . "present")
    (gc chart-7 . "collector")))

;; How long the Steps show, and the Consumers sum, in seconds; a frame's
;; time at 60 Hz, in ms; the Steps' columns' width, in pixels.
(define monitor-steps-seconds 10)
(define monitor-consumer-seconds 5)
(define monitor-frame-ms (/ 50.0 3))
(define monitor-column-px 3)

;; The history's lengths Monitor offers: (label . seconds of steps), and
;; (label . minutes of seconds).
(define monitor-step-choices
  '(("10 s" . 10) ("30 s" . 30) ("1 min" . 60) ("2 min" . 120)))
(define monitor-minute-choices
  '(("1 min" . 1) ("10 min" . 10) ("30 min" . 30) ("1 h" . 60)))

(define (monitor-ms ms)
  (if (< ms 9.995) (format "~,2f" ms) (format "~,1f" ms)))


;;; ------------------------------------------------------------------ data

;; The steps kept, from the seq newest back (the newest first), that
;; ended after since, at most n of them: what Monitor looks at is bounded.
(define (monitor-steps-back newest since n)
  (let loop ([seq newest] [acc '()] [k 0])
    (if (and (< k n) (profile-step-kept? seq)
             (> (profile-step-end seq) since))
        (loop (- seq 1) (cons seq acc) (+ k 1))
        (reverse acc))))

;; A step's segments, bottom up: ((color . ms) ...).
(define (monitor-segments step)
  (map (lambda (p) (cons (cadr p) (profile-step-phase-ms step (car p))))
       monitor-phases))

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

;; What a step spent, in a line: its time, its phases, its three largest
;; consumers.
(define (monitor-step-words step newest)
  (let* ([phases (filter (lambda (p) (>= (cdr p) 0.005))
                         (map (lambda (p)
                                (cons (cddr p)
                                      (profile-step-phase-ms step (car p))))
                              monitor-phases))]
         [top (filter (lambda (t) (monitor-subject? (profile-total-what t)))
                      (profile-totals (list step) #t))]
         [labels (monitor-labels (map profile-total-what top))])
    (format "~,1f s ago: ~a of ~a ms~a~a"
            (/ (- newest (profile-step-end step)) 1000)
            (monitor-ms (profile-busy step)) (monitor-ms (profile-step-ms step))
            (apply string-append
                   (map (lambda (p) (format ", ~a ~a" (car p)
                                            (monitor-ms (cdr p))))
                        phases))
            (apply string-append
                   (map (lambda (t)
                          (format "; ~a ~a"
                                  (cdr (assoc (profile-total-what t) labels))
                                  (monitor-ms (profile-total-ms t))))
                        (list-head top (min 3 (length top))))))))

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
              `((id "Id" ,(* w 0.1)) (title "Title" ,(* w 0.38))
                (app "App" ,(* w 0.2)) (geometry "Geometry" ,(* w 0.32)))
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
                              `((id "Id" ,(* w 0.1)) (name "Name" ,(* w 0.38))
                                (ticks "Ticks" ,(* w 0.2))
                                (age "Running" ,(* w 0.32)))
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
        [pinned #f]        ; the step clicked
        [selected #f]      ; the row selected: a subject, or (subject part
                           ; phase)
        [memos (make-hashtable equal-hash equal?)]   ; the view's figures
        [memo-view #f])
    (define (move-on!)
      (set! view (cons (profile-newest-step) (profile-seconds)))
      (set! last (now-ms)))
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
    (define (pause! step)
      (set! paused? #t)
      (set! pinned step))
    (define (resume!)
      (set! paused? #f)
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
                                (lambda () (profile-clear!) (resume!))
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
    ;; The last seconds' steps in columns, the legend under them, and the
    ;; breakdown of the step under the pointer, or paused on, or the
    ;; newest.
    (define (draw-steps x y w h step0 seconds newest)
      (let*-values
          ([(lh) (font-line-height 'ui-small)]
           [(keys) (append (map cdr monitor-phases)
                           (if selected
                               (list (cons 'chart-share
                                           (if (monitor-part? selected)
                                               (apply monitor-part-label
                                                      selected)
                                               (label-of selected seconds))))
                               '()))]
           [(kw kh) (ui-legend-size keys)]
           [(gh) (- h kh lh 12)]
           [(shown bars top rules)
            (apply values
                   (remember (list 'steps w gh selected)
                             (lambda () (steps-shown x y w gh step0 newest))))]
           [(hover)
            (ui-bar-chart ui 'monitor-steps x y w gh bars top rules
                          (and pinned
                               (list-index (lambda (s) (eqv? s pinned))
                                           shown))
                          (lambda (i)
                            (let ([s (list-ref shown i)])
                              (when s (pause! s)))))]
           [(step) (or (and hover (list-ref shown hover))
                       (and pinned (profile-step-kept? pinned) pinned)
                       step0)])
        (ui-draw-legend x (+ y gh 4) w kh '() keys)
        (ui-draw-label x (+ y gh kh 8) w lh '(hint)
                       (if step
                           (monitor-step-words step newest)
                           "Collecting…"))))
    ;; (shown bars top rules): the last monitor-steps-seconds in columns of
    ;; equal time, the oldest first, each the slowest step that ended in it
    ;; (#f: none), so that the chart moves at one speed and keeps its shape
    ;; whatever the steps' rate; its top a frame's time, doubled until the
    ;; slowest fits; its rules, a frame's line and the top.
    (define (steps-shown x y w gh step0 newest)
      (let* ([span (* 1000 monitor-steps-seconds)]
             [recent (if step0
                         (monitor-steps-back step0 (- newest span) 100000)
                         '())]
             [slowest (apply max 0.0 (map profile-busy recent))]
             [top (let double ([t (* 2 monitor-frame-ms)])
                    (if (< t slowest) (double (* 2 t)) t))]
             [rules (list (cons monitor-frame-ms
                                (format "~a ms" (monitor-ms monitor-frame-ms)))
                          (cons top (format "~a ms" (monitor-ms top))))]
             [plot (cdr (assq 'plot (ui-bar-chart-areas x y w gh 1 rules)))]
             [n (max 1 (exact (floor (/ (caddr plot) monitor-column-px))))]
             [slot (/ span n)]
             [columns (make-vector n #f)])
        (for-each (lambda (s)
                    ;; columns are fixed slices of time, so that a step
                    ;; stays in its column as the view moves on
                    (let ([j (- n 1 (- (exact (floor (/ newest slot)))
                                       (exact (floor (/ (profile-step-end s)
                                                        slot)))))])
                      (when (and (<= 0 j) (< j n)
                                 (let ([c (vector-ref columns j)])
                                   (or (not c)
                                       (> (profile-busy s)
                                          (profile-busy c)))))
                        (vector-set! columns j s))))
                  recent)
        (let ([shown (vector->list columns)])
          (list shown
                (map (lambda (s)
                       (if s
                           (cons (monitor-segments s)
                                 (and selected
                                      (profile-step-node-ms s selected)))
                           (cons '() #f)))
                     shown)
                top rules))))
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
             [columns `((name "Consumer" ,(max 120 (- w (* 4 num) spark)))
                        (rate "ms/s" ,num) (avg "avg" ,num) (max "max" ,num)
                        (calls "calls/s" ,num) (spark "last minute" ,spark))])
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
        ;; a subject's parts, (subject part phase), when it has more than one
        (define (children node)
          (let ([t (and (not (monitor-part? node)) (total-of node))])
            (if (and t (> (length (profile-total-parts t)) 1))
                (map (lambda (p)
                       (let ([what (profile-total-what p)])
                         (list node (car what) (cdr what))))
                     (profile-total-parts t))
                '())))
        (define (cell node key cx cy cw ch flags)
          (let ([t (total-of node)] [in 8])
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
                      [room (- cw bw 4)])
                 (ui-draw-label cx cy room ch
                                (if (and (pair? s) (not live))
                                    '(dim)
                                    '())
                                label)
                 ;; the live window or process, looked up by its id
                 (when (and live (not (monitor-part? node)) (window? live))
                   (ui-pressable! ui (list 'monitor-focus id)
                                  cx cy (min room (text-width label 'ui)) ch
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
                                  (remember
                                   (list 'spark node)
                                   (lambda ()
                                     (map (lambda (s)
                                            (profile-second-node-ms s node))
                                          minute))))])))
        (ui-tree ui 'consumers x y w h columns roots children selected
                 (lambda (node) (set! selected node))
                 cell)))
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
            (let* ([h* (history)] [step0 (car h*)] [seconds (cdr h*)]
                   [newest (if step0 (profile-step-end step0) 0)]
                   [x pad] [cw (- w pad pad)]
                   [lh (font-line-height 'ui-small)]
                   [steps-h (round (* 9 lh))]
                   [memory-h (round (* 5 lh))]
                   [rate (if (pair? seconds)
                             (profile-second-ref (car seconds) 'steps)
                             0)]
                   [y (section x (+ y 4) cw "Steps"
                               (format "the last ~a s, ~a steps/s~a"
                                       monitor-steps-seconds rate
                                       (if paused? ", paused" "")))]
                   [y (begin (draw-steps x y cw steps-h step0 seconds newest)
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
            (list 'tick (lambda (win)
                          (ui-tick ui win)
                          (when (and (not paused?)
                                     (> (- (now-ms) last) 500))
                            (move-on!)
                            (invalidate! win))))))))
