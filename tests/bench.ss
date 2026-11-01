;;; tests/bench.ss — what a frame costs on the render ladder's four scenes.
;;;
;;;   scheme --script tests/bench.ss [output-dir [frames]]
;;;
;;; Headless, 1280 × 800 logical pixels, at 100% and 200%:
;;;   S30T  30 Life windows, tiled: no two overlap
;;;   S5    a typical desktop: a REPL with a screenful of text, Settings,
;;;         Clock, Life and Monitor, overlapping
;;;   S30   30 Life windows, cascaded
;;;   S30M  S30 under a maximized REPL
;;; A fake clock moves 81 ms a step, so every Life window steps, and asks
;;; for a frame, at every step. Everything runs on the main thread. The
;;; host's SDL upload runs against a stand-in whose texture takes the
;;; changed rows with memcpy, as SDL_UpdateTexture does. Each scene: 10
;;; steps to warm up, then frames (default 100) measured, each one step:
;;;   main    the main thread's CPU time (median): events, ticks, drawing
;;;           the frame, comparing its rows, the upload
;;;   wall    the step's time, steps back to back (median, and 90th
;;;           percentile)
;;;   upload  bytes given to the texture (mean)
;;;   GCs     collections (mean)
;;; Also a keystroke in S5's REPL (the clock still: the frame shows the
;;; new character), an idle step (30 paused Life windows, the clock
;;; standing still), a theme change on S30 (it paints the wallpaper
;;; again), the clock's second with no window and a keystroke in a REPL
;;; alone (each step one of them, at 100% and 200%, and at 200% on a
;;; 1920 × 1080 screen, a 4K one), Tasks maximized on that screen (an idle
;;; step, a keystroke in a title, two in one step, a card dragged), and a
;;; busy process's share of the main thread while S30T animates at 200%
;;; (printed, not bounded). Fails (exit 1) unless every measured step drew
;;; a frame, an idle step takes under 0.2 ms and uploads nothing (with
;;; Tasks open too), and the busy process and the animation both go on.

(define *schevana-root*
  (let ([dir (path-parent (path-parent (car (command-line))))])
    (if (string=? dir "") "." dir)))

(load (string-append *schevana-root* "/load.ss"))

(define args (command-line-arguments))
(define out-dir (if (pair? args) (car args) "."))
(define frames (if (> (length args) 1) (string->number (cadr args)) 100))
(set! *theme-file* (string-append out-dir "/theme.ss"))
(when (file-exists? *theme-file*) (delete-file *theme-file*))

(define fake-ms 1000000)
(set! now-ms (lambda () fake-ms))
(random-seed 4242)
(schevana-boot! 'headless 1280 800)

(define (ms-of t) (+ (* 1000.0 (time-second t)) (/ (time-nanosecond t) 1e6)))
(define (wall-ms) (ms-of (current-time 'time-monotonic)))
(define (main-ms) (ms-of (current-time 'time-thread)))

(define (median xs)
  (if (null? xs) 0.0 (list-ref (sort < xs) (quotient (length xs) 2))))
(define (percentile xs p)
  (if (null? xs)
      0.0
      (list-ref (sort < xs)
                (min (- (length xs) 1) (exact (floor (* p (length xs))))))))

;;; ------------------------------------------------------------------
;;; the upload

;; The host's SDL branch, with *host* 'sdl only around the present, and
;; the texture a buffer the canvas's size.
(define texture 0)
(define texture-bytes 0)
(define (texture-upload! t rect pixels pitch)
  (let ([n (* pitch *canvas-ph*)])
    (unless (= n texture-bytes)
      (unless (eqv? texture 0) (foreign-free texture))
      (set! texture (foreign-alloc n))
      (set! texture-bytes n))
    (if (eqv? rect 0)
        (sdl-memcpy texture pixels n)
        (let ([x (foreign-ref 'integer-32 rect 0)]
              [y (foreign-ref 'integer-32 rect 4)]
              [w (foreign-ref 'integer-32 rect 8)]
              [h (foreign-ref 'integer-32 rect 12)])
          (if (and (= x 0) (= (* 4 w) pitch))
              (sdl-memcpy (+ texture (* y pitch)) pixels (* h pitch))
              (do ([r 0 (+ r 1)]) ((= r h))
                (sdl-memcpy (+ texture (* (+ y r) pitch) (* 4 x))
                            (+ pixels (* r pitch))
                            (* 4 w))))))
    #t))
(set! sdl-update-texture texture-upload!)
(set! sdl-render-texture (lambda (r t src dst) #t))
(set! sdl-render-present (lambda (r) #t))
(define plain-present host-present-damage!)
(set! host-present-damage!
      (lambda () (fluid-let ([*host* 'sdl]) (plain-present))))

;;; ------------------------------------------------------------------ scenes

(define (steps n ms)
  (do ([i 0 (+ i 1)]) ((= i n))
    (set! fake-ms (+ fake-ms ms))
    (schevana-step! 0)))

;; Status segments that never change.
(define (fixed-status!)
  (set! *status-segments* '())
  (add-status-segment! 'a (lambda () "12.3 ms/frame"))
  (add-status-segment! 'b (lambda () "Thu 29 Oct   12:00:00")))

(define (clear-world!)
  (for-each close-window! (windows))
  (set! *toasts* '())
  (set! *cascade* 0)
  (set! *hover* #f)
  (set! *drag* #f)
  (when (menu-open?) (close-menu!)))

(define (lifes! layout)
  (do ([i 0 (+ i 1)]) ((= i 30)) (life))
  (case layout
    [(tiled) (tile-windows!)]
    [else (cascade-windows!)]))

(define repl-text-lines
  '((input . "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))")
    (result . "fib")
    (input . "(map fib (iota 20))")
    (result . "(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 \
               4181)")
    (input . "(windows)")
    (result . "(#<window 4 \"Life\" 720x496+520+290> \
               #<window 3 \"Clock\" 230x290+1020+50>")
    (result . " #<window 2 \"Settings\" 780x560+380+140> \
               #<window 1 \"REPL\" 692x470+30+45>)")
    (input . "(for-each (lambda (w) (printf \"~a ~a\\n\" (window-id w) \
              (window-title w))) (windows))")
    (output . "4 Life")
    (output . "3 Clock")
    (output . "2 Settings")
    (output . "1 REPL")
    (input . "(set-theme! 'accent #xff9e64)")
    (input . "(apply-theme-preset! 'tokyo-night)")
    (input . "(define-app (counter) \"Counter\" (let ([n 0]) \
              (open-window \"Counter\" 'size '(240 . 100))))")
    (result . "counter")
    (input . "(list (screen-w) (screen-h) *scale* (length (windows)) \
              (length (procs)) (apps))")
    (result . "(1280 800 1.0 5 0 (repl private-repl pure-repl clock life \
               sketch monitor settings))")
    (input . "(string-append \"The quick brown fox \" \
              \"jumps over the lazy dog, \" \"again and again.\")")
    (result . "\"The quick brown fox jumps over the lazy dog, again and \
               again.\"")
    (input . "(vector-map (lambda (x) (* x x)) '#(1 2 3 4 5 6 7 8 9 10 \
              11 12 13 14 15 16))")
    (result . "#(1 4 9 16 25 36 49 64 81 100 121 144 169 196 225 256)")
    (input . "(bytes-allocated)")
    (result . "48233472")))

(define (texted-repl!)
  (let* ([r (repl)] [rs (window-ref r 'state)])
    (for-each (lambda (l) (repl-add! rs (car l) (cdr l))) repl-text-lines)
    r))

(define (scene! name)
  (clear-world!)
  (random-seed 4242)
  (case name
    [(S30T) (lifes! 'tiled)]
    [(S30) (lifes! 'cascaded)]
    [(S30M) (lifes! 'cascaded) (steps 3 17) (toggle-maximize! (texted-repl!))]
    [(S5)
     (let* ([r (texted-repl!)]
            [s (settings)]
            [c (clock)]
            [l (life)]
            [m (monitor)])
       (move-window! r 30 45)
       (move-window! s 380 120)
       (move-window! c 1020 50)
       (move-window! l 520 290)
       (move-window! m 60 300)
       (toggle-maximize! s)
       (steps 1 17)
       (toggle-maximize! s)
       (for-each focus-window! (list s m l c r)))])   ; the REPL on top, focused
  (steps 3 17)
  (fixed-status!)
  (steps 2 17))

;;; ------------------------------------------------------------------
;;; the ladder

;; One scene at one scale: a property list of what it measured.
(define (run-scene name pct)
  (set-theme! 'scale pct)
  (steps 2 17)
  (scene! name)
  (steps 10 81)
  (let ([u0 *upload-bytes*] [g0 (collections)] [d0 *frames-drawn*])
    (let loop ([i 0] [walls '()] [mains '()])
      (if (< i frames)
          (let ([w0 (wall-ms)] [m0 (main-ms)])
            (steps 1 81)
            (let ([m (- (main-ms) m0)] [w (- (wall-ms) w0)])
              (loop (+ i 1) (cons w walls) (cons m mains))))
          (list 'scene name 'scale pct 'windows (length (windows))
                'drawn (- *frames-drawn* d0)
                'main (median mains) 'wall (median walls)
                'wall-p90 (percentile walls 0.9)
                'upload (/ (- *upload-bytes* u0) frames 1.0)
                'gcs (/ (- (collections) g0) frames 1.0))))))

(define (row r)
  (printf "~5a ~4a ~7a ~7,2f ~7,2f (~6,2f) ~9,2f ~5,2f\n"
          (plist-ref r 'scene #f) (plist-ref r 'scale #f)
          (plist-ref r 'windows #f) (plist-ref r 'main #f)
          (plist-ref r 'wall #f) (plist-ref r 'wall-p90 #f)
          (/ (plist-ref r 'upload #f) 1048576.0) (plist-ref r 'gcs #f)))

(printf "Per frame, ~a frames a scene; ms unless said.\n" frames)
(printf "~5a ~4a ~7a ~7a ~7a (~6a) ~9a ~5a\n"
        "scene" "%" "windows" "main" "wall" "p90" "upload MiB" "GCs")
(define results '())
(for-each (lambda (name)
            (for-each (lambda (pct)
                        (let ([r (run-scene name pct)])
                          (row r)
                          (set! results (cons r results))))
                      '(100 200)))
          '(S30T S5 S30 S30M))

(define (result name pct)
  (find (lambda (r)
          (and (eq? (plist-ref r 'scene #f) name)
               (eqv? (plist-ref r 'scale #f) pct)))
        results))

;;; ------------------------------------------------------------------
;;; idle steps and theme changes

;; Steps with 30 paused Life windows and the clock standing still: the
;; ones that draw no frame (all but the once-a-second one).
(define (idle-steps pct)
  (set-theme! 'scale pct)
  (scene! 'S30)
  (for-each (lambda (w) (window-call w 'key #\space '())) (windows))
  (quiet-steps))

;; 3000 steps with nothing happening: the median of those that draw no
;; frame, and the bytes they uploaded.
(define (quiet-steps)
  (steps 3 0)
  (let loop ([i 0] [times '()] [uploaded 0])
    (if (< i 3000)
        (let ([u *upload-bytes*] [n *frames-drawn*] [t0 (wall-ms)])
          (schevana-step! 0)
          (let ([t (- (wall-ms) t0)])
            (if (> *frames-drawn* n)
                (loop (+ i 1) times uploaded)
                (loop (+ i 1) (cons t times)
                      (+ uploaded (- *upload-bytes* u))))))
        (values (median times) uploaded))))

;; A theme change and the step after it, which paints the wallpaper's new
;; picture and draws the frame: (values wall main), medians.
(define (theme-change-ms pct)
  (set-theme! 'scale pct)
  (scene! 'S30)
  (for-each (lambda (w) (window-call w 'key #\space '())) (windows))
  (steps 3 17)
  (let loop ([colors
              '(#xff9e64 #x7aa2f7 #xff9e64 #x7aa2f7 #xff9e64 #x7aa2f7 #xff9e64)]
             [walls '()]
             [mains '()])
    (if (null? colors)
        (values (median walls) (median mains))
        (let ([w0 (wall-ms)] [m0 (main-ms)])
          (set-theme! 'accent (car colors))
          (steps 1 17)
          (loop (cdr colors)
                (cons (- (wall-ms) w0) walls)
                (cons (- (main-ms) m0) mains))))))

;; A key typed into S5's REPL, the clock standing still: the step that
;; handles it draws the frame that shows it. (values wall main) medians.
(define (keystroke-ms pct)
  (set-theme! 'scale pct)
  (scene! 'S5)
  (steps 3 0)
  (let loop ([i 0] [walls '()] [mains '()])
    (if (< i 40)
        (let ([w0 (wall-ms)] [m0 (main-ms)] [n *frames-drawn*])
          (inject! (list 'text (if (even? i) "a" "b")))
          (schevana-step! 0)
          (unless (= *frames-drawn* (+ n 1))
            (errorf 'keystroke-ms "a keystroke drew no frame"))
          (loop (+ i 1)
                (cons (- (wall-ms) w0) walls)
                (cons (- (main-ms) m0) mains)))
        (values (median walls) (median mains)))))

(newline)
(for-each (lambda (pct)
            (let-values ([(wall main) (keystroke-ms pct)])
              (printf "a keystroke in S5's REPL at ~a%: ~,2f ms until its \
                       frame is drawn (main thread ~,2f ms)\n"
                      pct wall main)))
          '(100 200))
(define-values (idle-100 idle-up-100) (idle-steps 100))
(define-values (idle-200 idle-up-200) (idle-steps 200))
(printf "idle step with 30 paused Life windows: ~,3f ms at 100%, \
         ~,3f ms at 200%\n"
        idle-100 idle-200)
(for-each (lambda (pct)
            (let-values ([(wall main) (theme-change-ms pct)])
              (printf "a theme change at ~a%: ~,1f ms until its frame is \
                       drawn, the wallpaper painted again \
                       (main thread ~,1f ms)\n"
                      pct wall main)))
          '(100 200))

;;; ------------------------------------------------------------------
;;; the clock's second and a keystroke, on a quiet desktop

;; The headless screen, w × h logical pixels at pct.
(define (screen! w h pct)
  (set! *headless-size* (cons w h))
  (set-theme! 'scale pct)
  (unless (and (= *canvas-w* w) (= *canvas-h* h))
    (canvas-create! w h *host-scale*)
    (wm-screen-resized!))
  (steps 2 17))

;; Steps that each draw the frame of one thing that changed (change!
;; makes it change): (values main wall upload), the medians of the main
;; thread's and the step's ms and the mean bytes uploaded.
(define (frame-steps change!)
  (steps 3 0)
  (let ([u0 *upload-bytes*])
    (let loop ([i 0] [walls '()] [mains '()])
      (if (< i frames)
          (let ([w0 (wall-ms)] [m0 (main-ms)] [n *frames-drawn*])
            (change! i)
            (schevana-step! 0)
            (unless (= *frames-drawn* (+ n 1))
              (errorf 'frame-steps "a change drew no frame"))
            (loop (+ i 1)
                  (cons (- (wall-ms) w0) walls)
                  (cons (- (main-ms) m0) mains)))
          (values (median mains) (median walls)
                  (/ (- *upload-bytes* u0) frames 1.0))))))

;; No window; the top bar's clock shows a new second at every step.
(define (idle-ticks)
  (clear-world!)
  (let ([second 0])
    (set! *status-segments* '())
    (add-status-segment! 'clock
      (lambda () (format "Thu 29 Oct   12:00:~2,'0d" (mod second 60))))
    (frame-steps (lambda (i)
                   (set! second (+ second 1))
                   (set! *last-second* -1)))))

;; A REPL alone, where it opens, typed into.
(define (lone-keystrokes)
  (clear-world!)
  (fixed-status!)
  (texted-repl!)
  (frame-steps (lambda (i) (inject! (list 'text (if (even? i) "a" "b"))))))

(for-each
 (lambda (s)
   (apply screen! s)
   (let*-values ([(tick-main tick-wall tick-up) (idle-ticks)]
                 [(key-main key-wall key-up) (lone-keystrokes)])
     (printf "~ax~a at ~a%: the clock's second, no window: main ~,2f ms, \
              wall ~,2f, ~,3f MiB uploaded; a keystroke in a lone REPL: \
              main ~,2f ms, wall ~,2f, ~,3f MiB uploaded\n"
             (car s) (cadr s) (caddr s) tick-main tick-wall
             (/ tick-up 1048576.0) key-main key-wall (/ key-up 1048576.0))))
 '((1280 800 100) (1280 800 200) (1920 1080 200)))
(screen! 1280 800 100)

;;; ------------------------------------------------------------------
;;; Tasks, maximized on a 1920 × 1080 screen at 200%
;;; With nothing focused it asks for no frame. A keystroke in a task's
;;; title, two in one step (the second lays the window out again first)
;;; and a card dragged a step a move each draw the whole window.

(define (tasks-bench)
  (screen! 1920 1080 200)
  (clear-world!)
  (fixed-status!)
  (tasks-reset!)
  (let* ([win (tasks)] [ui (window-ref win 'ui)])
    (define (at id)   ; region id's middle on screen
      (let ([r (ui-find ui id)])
        (let-values ([(cx cy cw ch) (content-rect win)])
          (list (exact (floor (+ cx (ui-region-x r) (/ (ui-region-w r) 2))))
                (exact (floor (+ cy (ui-region-y r) (/ (ui-region-h r) 2))))))))
    (maximize-window! win)
    (steps 2 17)
    (apply inject! (let ([p (at '(card 9))])
                     `((mouse-move ,@p) (mouse-down ,@p left 1)
                       (mouse-up ,@p left))))
    (let*-values ([(idle idle-up) (quiet-steps)]
                  [(key-main key-wall key-up)
                   (begin
                     (inject! '(key-down return ()))
                     (frame-steps (lambda (i)
                                    (inject! (if (even? i)
                                                 '(text "a")
                                                 '(key-down backspace
                                                   ()))))))]
                  [(two-main two-wall two-up)
                   (frame-steps (lambda (i)
                                  (if (even? i)
                                      (inject! '(text "a") '(text "b"))
                                      (inject! '(key-down backspace ())
                                               '(key-down backspace ())))))]
                  [(drag-main drag-wall drag-up)
                   (let ([p (at '(card 10))] [a (at '(column review))]
                         [b (at '(column done))])
                     (inject! '(key-down escape ()) `(mouse-move ,@p)
                              `(mouse-down ,@p left 1))
                     (steps 1 0)
                     (frame-steps
                      (lambda (i)
                        (inject! `(mouse-move ,@(if (even? i) a b))))))])
      (inject! `(mouse-up ,@(at '(column todo)) left))
      (steps 2 0)
      (close-window! win)
      (printf "Tasks maximized on 1920x1080 at 200%: an idle step ~,3f ms, \
               ~a bytes uploaded; a keystroke in a title: main ~,2f ms, wall \
               ~,2f; two in one step: main ~,2f ms, wall ~,2f; a card \
               dragged, a move: main ~,2f ms, wall ~,2f\n"
              idle idle-up key-main key-wall two-main two-wall drag-main
              drag-wall)
      (values idle idle-up))))

(define-values (tasks-idle tasks-idle-up) (tasks-bench))
(screen! 1280 800 100)

;;; ------------------------------------------------------------------
;;; a busy process

;; A REPL evaluation that computes (a loop counting) while S30T animates,
;; steps back to back as with a window: its loops a second, as a share of
;; what it gets with the Life windows paused, and the frames a second
;; meanwhile. The frames are drawn on the main thread, between the
;; process's slices. run-procs!' 10 ms budget needs a clock that moves:
;; the fake one goes on with real time, plus 81 ms a step, so every Life
;; window steps at every step.
(define (busy-run pct animate? ms)   ; (values loops a second, frames a second)
  (set-theme! 'scale pct)
  (scene! 'S30T)
  (unless animate?
    (for-each (lambda (w) (window-call w 'key #\space '())) (windows)))
  (steps 2 81)
  (let* ([count 0] [k 0] [n0 *frames-drawn*] [t0 (wall-ms)] [ms0 fake-ms]
         [p (spawn "busy"
                   (lambda ()
                     (let loop () (set! count (fx+ count 1)) (loop))))])
    (fluid-let ([now-ms (lambda ()
                          (+ ms0 (exact (floor (- (wall-ms) t0))) (* 81 k)))])
      (let loop ()
        (when (< (- (wall-ms) t0) ms)
          (set! k (+ k 1))
          (schevana-step! 0)
          (loop)))
      (let ([s (/ (- (wall-ms) t0) 1000.0)] [frames (- *frames-drawn* n0)])
        (kill! p)
        (set! fake-ms (now-ms))
        (values (/ count s) (/ frames s))))))

(define-values (busy-alone busy-idle-fps) (busy-run 200 #f 2000))
(define-values (busy-loops busy-fps) (busy-run 200 #t 2000))
(printf "a busy process while S30T animates at 200%: ~a% of its loops with \
         Life paused, ~,1f frames a second\n"
        (exact (round (* 100.0 (/ busy-loops busy-alone)))) busy-fps)

(define failures 0)
(define (assert! name ok)
  (unless ok (set! failures (+ failures 1)))
  (printf "~a ~a\n" (if ok "ok  " "FAIL") name))

(newline)
(assert! "every measured step drew a frame"
         (for-all (lambda (r) (= frames (plist-ref r 'drawn #f))) results))
(assert! (format
          "an idle step takes under 0.2 ms (~,3f, ~,3f) and uploads nothing"
          idle-100 idle-200)
         (and (< (max idle-100 idle-200) 0.2) (= 0 idle-up-100 idle-up-200)))
(assert! (format "with Tasks open and nothing focused, an idle step takes \
                  under 0.2 ms (~,3f) and uploads nothing" tasks-idle)
         (and (< tasks-idle 0.2) (= 0 tasks-idle-up)))
(assert! "a busy process and S30T's animation both go on"
         (and (> busy-loops 0) (> busy-fps 1)))
(printf "S30T main per frame: ~,2f ms at 100%, ~,2f ms at 200%\n"
        (plist-ref (result 'S30T 100) 'main #f)
        (plist-ref (result 'S30T 200) 'main #f))
(printf "~a\n" (if (zero? failures)
                   "all assertions hold"
                   (format "~a assertion(s) FAILED" failures)))
(exit (if (zero? failures) 0 1))
