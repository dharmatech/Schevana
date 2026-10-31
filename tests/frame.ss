;;; tests/frame.ss — the renderer: every frame drawn on the main thread
;;; into the second screen buffer, compared row by row with the screen;
;;; the rows that changed are copied onto it and uploaded.
;;;
;;;   scheme --script tests/frame.ss [output-dir]
;;;
;;; The reference for a frame is render-full-into! drawn directly into an
;;; image: the very procedure the renderer runs.
;;;   identity      once the frames asked for are drawn, the canvas is the
;;;                 reference, bit for bit: S30T, S30, S5, a translucent
;;;                 window, every style, a menu and a toast, at 100, 125,
;;;                 150, 200 and 300% (Monitor's content masked: it shows
;;;                 the heap)
;;;   damage        a texture fed only the uploaded rows is the canvas after
;;;                 every step of this file; a frame that changes nothing
;;;                 uploads and presents nothing; the screen stays one
;;;                 surface
;;;   the window    a fake window: boot, a resize, a drag, a rescale and a
;;;                 lost texture each show their own frame at once, never
;;;                 an empty new canvas
;;;   processes     a process, switched out anywhere, frees tile caches and
;;;                 owned surfaces, rescales and calls (render-frame!) while
;;;                 the main loop draws: frames stay whole
;;;   raising       a window's draw that raises crashes the window; a frame
;;;                 whose drawing raises shows nothing
;;;   redefinition  draw-window, draw-taskbar and a style, redefined: the
;;;                 next frame shows them, is the reference, costs the same
;;;   wallpaper     a theme change (raw or by set-theme!), a redefined
;;;                 draw-wallpaper or the wallpaper in use defined again
;;;                 paints it again for the next frame; a clock-reading
;;;                 draw-wallpaper is a still picture; the picture is
;;;                 draw-wallpaper's drawing
;;;   Life          each generation of 30 Life windows shows on the next frame
;;;
;;; output-dir receives a scratch theme file and a screenshot of a failure.

(define *schevana-root*
  (let ([dir (path-parent (path-parent (car (command-line))))])
    (if (string=? dir "") "." dir)))

(load (string-append *schevana-root* "/load.ss"))

(define out-dir
  (if (pair? (command-line-arguments)) (car (command-line-arguments)) "."))
(set! *theme-file* (string-append out-dir "/theme.ss"))
(when (file-exists? *theme-file*) (delete-file *theme-file*))

(define failures 0)

(define-syntax check
  (syntax-rules ()
    [(_ name expr)
     (let ([ok (guard (c [#t (printf "  error: ~a\n" (condition->string c)) #f])
                 expr)])
       (unless ok (set! failures (+ failures 1)))
       (printf "~a ~a\n" (if ok "ok  " "FAIL") name))]))

;; A clock that moves only when told, so that Life steps when a check says.
(define fake-ms 1000000)
(define (fake-clock!) (set! now-ms (lambda () fake-ms)))
(fake-clock!)
(random-seed 4242)
(schevana-boot! 'headless 1280 800)

(define (steps n . ms)
  (do ([i 0 (+ i 1)]) ((= i n))
    (set! fake-ms (+ fake-ms (if (pair? ms) (car ms) 0)))
    (schevana-step! 0)))

;; Steps while a frame is asked for (at most 20): then the canvas shows
;; the world as it is.
(define (settle)
  (let loop ([n 0])
    (when (and (< n 20) *dirty*)
      (schevana-step! 0)
      (loop (+ n 1)))))

;;; ------------------------------------------------------------------ images

(define (canvas-image)
  (cairo-image-surface-create CAIRO_FORMAT_ARGB32 *canvas-pw* *canvas-ph*))

;; The frame render-full-into! draws now, straight into an image.
(define (reference)
  (let ([ref (canvas-image)])
    (render-full-into! ref)
    ref))

;; A copy of the canvas.
(define (canvas-copy)
  (let ([c (canvas-image)])
    (cairo-surface-flush *canvas*)
    (sdl-memcpy (cairo-image-surface-get-data c)
                (cairo-image-surface-get-data *canvas*)
                (* (cairo-image-surface-get-stride c) *canvas-ph*))
    (cairo-surface-mark-dirty c)
    c))

;; Monitor's content in device pixels, (x0 y0 x1 y1), or #f.
(define (monitor-mask)
  (let ([m (find-window 'monitor)])
    (and m (visible? m)
         (let-values ([(cx cy cw ch) (content-rect m)])
           (map (lambda (v) (exact (round (* *scale* v))))
                (list cx cy (+ cx cw) (+ cy ch)))))))

;; How many pixels of a and b (the same size) differ outside the mask.
(define (differing a b mask)
  (cairo-surface-flush a)
  (cairo-surface-flush b)
  (let ([pa (cairo-image-surface-get-data a)]
        [pb (cairo-image-surface-get-data b)]
        [st (cairo-image-surface-get-stride a)]
        [w (cairo-image-surface-get-width a)]
        [h (cairo-image-surface-get-height a)])
    (let row ([y 0] [n 0])
      (cond
        [(= y h) n]
        [(zero? (sdl-memcmp (+ pa (* y st)) (+ pb (* y st)) (* 4 w)))
         (row (+ y 1) n)]
        [else
         (row (+ y 1)
              (let col ([x 0] [n n])
                (cond
                  [(= x w) n]
                  [(and mask (<= (car mask) x) (< x (caddr mask))
                        (<= (cadr mask) y) (< y (cadddr mask)))
                   (col (+ x 1) n)]
                  [(= (foreign-ref 'unsigned-32 pa (+ (* y st) (* 4 x)))
                      (foreign-ref 'unsigned-32 pb (+ (* y st) (* 4 x))))
                   (col (+ x 1) n)]
                  [else (col (+ x 1) (+ n 1))])))]))))

(define (same-image? a b)
  (and (= (cairo-image-surface-get-width a) (cairo-image-surface-get-width b))
       (= (cairo-image-surface-get-height a) (cairo-image-surface-get-height b))
       (zero? (differing a b #f))))

(define shots 0)

;; Does the canvas, once the frames asked for are drawn, equal the
;; reference? label is printed with a count when it does not.
(define (matches-reference? label)
  (settle)
  (let* ([ref (reference)] [n (differing *canvas* ref (monitor-mask))])
    (unless (= n 0)
      (set! shots (+ shots 1))
      (printf "  ~a: ~a pixels differ from the reference\n" label n)
      (when (<= shots 3)
        (screenshot (format "~a/differs-~a.png" out-dir shots))))
    (cairo-surface-destroy ref)
    (= n 0)))

(define (pixel-of surface x y)   ; logical point
  (cairo-surface-flush surface)
  (foreign-ref 'unsigned-32 (cairo-image-surface-get-data surface)
               (+ (* (exact (floor (* *scale* y)))
                     (cairo-image-surface-get-stride surface))
                  (* 4 (exact (floor (* *scale* x)))))))

;;; ------------------------------------------------------------------
;;; the texture
;;; The host's SDL upload, with *host* 'sdl only around the present and the
;;; texture an image that uploads copy rows into, as SDL_UpdateTexture does.
;;; After every step it must equal the canvas, and a step presents if and
;;; only if it uploaded or was exposed.

(define texture #f)
(define texture-steps 0)
(define texture-mismatches 0)
(define bad-presents 0)
(define presents 0)
(define plain-present #f)

(define (texture-upload! t rect pixels pitch)
  (unless (and texture (= (cairo-image-surface-get-width texture) *canvas-pw*)
               (= (cairo-image-surface-get-height texture) *canvas-ph*))
    (when texture (cairo-surface-destroy texture))
    ;; a new texture: its pixels are undefined (here clear)
    (set! texture (canvas-image)))
  (let ([td (cairo-image-surface-get-data texture)]
        [ts (cairo-image-surface-get-stride texture)])
    (let-values ([(x y w h)
                  (if (eqv? rect 0)
                      (values 0 0 *canvas-pw* *canvas-ph*)
                      (apply values
                             (map (lambda (i) (foreign-ref 'integer-32 rect i))
                                  '(0 4 8 12))))])
      (unless (and (= x 0) (= w *canvas-pw*) (<= 0 y) (< 0 h)
                   (<= (+ y h) *canvas-ph*) (= pitch ts))
        (errorf 'sdl-update-texture "not a run of whole rows: ~s"
                (list x y w h pitch)))
      (sdl-memcpy (+ td (* y ts)) pixels (* h ts))
      (cairo-surface-mark-dirty texture)
      0)))

(set! sdl-update-texture texture-upload!)
(set! sdl-render-copy (lambda (r t src dst) 0))
(set! sdl-render-present (lambda (r) (set! presents (+ presents 1))))
(set! plain-present host-present-damage!)
(set! host-present-damage!
  (lambda ()
    (let ([p0 presents] [u0 *upload-bytes*] [exposed? *present-only?*])
      (fluid-let ([*host* 'sdl]) (plain-present))
      (set! texture-steps (+ texture-steps 1))
      (unless (and texture (same-image? texture *canvas*))
        (set! texture-mismatches (+ texture-mismatches 1)))
      (unless (= (- presents p0) (if (or exposed? (> *upload-bytes* u0)) 1 0))
        (set! bad-presents (+ bad-presents 1))))))

(set! *full-upload?* #t)
(steps 2)

;;; ------------------------------------------------------------------ scenes

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

(define (texted-repl!)
  (let* ([r (repl)] [rs (window-ref r 'state)])
    (for-each (lambda (l) (repl-add! rs (car l) (cdr l)))
              '((input . "(define (fib n) (if (< n 2) n \
                          (+ (fib (- n 1)) (fib (- n 2)))))")
                (result . "fib")
                (input . "(map fib (iota 20))")
                (result . "(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 \
                           1597 2584 4181)")
                (input . "(set-theme! 'accent #xff9e64)")))
    r))

;; S30T: 30 Life windows tiled; S30: cascaded; S5: a REPL, Settings, Clock,
;; Life and Monitor, overlapping, the REPL on top.
(define (scene! name)
  (clear-world!)
  (random-seed 4242)
  (case name
    [(S30T S30)
     (do ([i 0 (+ i 1)]) ((= i 30)) (life))
     (if (eq? name 'S30T) (tile-windows!) (cascade-windows!))]
    [(S5)
     (let* ([r (texted-repl!)] [s (settings)] [c (clock)] [l (life)]
            [m (monitor)])
       (move-window! r 30 45)
       (move-window! s 380 120)
       (move-window! c 1020 50)
       (move-window! l 520 290)
       (move-window! m 60 300)
       (for-each focus-window! (list s m l c r)))])
  (fixed-status!)
  (steps 3 81))

(define (pause-lifes!)
  (for-each (lambda (w)
              (when (eq? (window-app w) 'life)
                (window-call w 'key #\space '())))
            (windows)))

;;; ------------------------------------------------------------------ identity

(define styles '(mac windows win98 interlisp genera))

(for-each
 (lambda (pct)
   (set-theme! 'scale pct)
   (steps 1)
   (let ([bad '()])
     (define (try! label)
       (unless (matches-reference? (list pct label))
         (set! bad (cons label bad))))
     (scene! 'S30T) (try! 'S30T)
     (steps 1 81) (try! 'S30T-stepped)
     (scene! 'S30) (try! 'S30)
     (scene! 'S5) (try! 'S5)
     (let ([g (open-window "Glass"
                'size '(320 . 200)
                'pos '(560 . 330)
                'bg (alpha 'bg-1 0.45)
                'draw (lambda (win w h)
                        (draw-text 14 14 "translucent" 'text 'big)))])
       (try! 'translucent)
       (for-each (lambda (style) (set-theme! 'window-style style) (try! style))
                 styles)
       (set-theme! 'window-style 'mac)
       (inject! `(mouse-down ,(+ (window-x g) 120) ,(+ (window-y g) 8) right 1)
                `(mouse-up ,(+ (window-x g) 120) ,(+ (window-y g) 8) right))
       (steps 1)
       (unless (menu-open?) (set! bad (cons 'no-window-menu bad)))
       (try! 'window-menu)
       (close-menu!)
       (notify "A toast")
       (try! 'toast)
       (close-window! g))
     (check (format "once the frames asked for are drawn, the canvas is \
                     render-full-into!'s frame, bit for bit, at ~a% (~a)" pct
                    (if (null? bad)
                        "S30T, S30, S5, translucent, every style, menu, toast"
                        (reverse bad)))
            (null? bad))))
 '(100 125 150 200 300))

;;; ------------------------------------------------------------------ damage

(set-theme! 'scale 100)
(scene! 'S5)
(pause-lifes!)
;; they change by themselves
(for-each (lambda (app) (close-window! (find-window app))) '(clock monitor))
(steps 2)
(invalidate!)
(steps 1)
(let ([u0 *upload-bytes*] [p0 presents] [n0 *frames-drawn*] [screen *canvas*])
  (invalidate!)
  (steps 1)
  (let ([none (list (- *frames-drawn* n0) (- *upload-bytes* u0)
                    (- presents p0))]
        [kept (list (eqv? screen *canvas*))])   ; after each frame
    (inject! '(text "x"))   ; into the REPL: a few rows change
    (steps 1)
    (let ([typed (list *frame-rows* (- *upload-bytes* u0) (- presents p0))]
          [kept (append kept (list (eqv? screen *canvas*)))])
      (check (format
              "a frame that changes nothing uploads and presents nothing ~s; \
               a keystroke uploads only its rows ~s; \
               the rows are copied onto the screen, which stays one surface ~s"
              none typed kept)
             (and (equal? none '(1 0 0))
                  (< 0 (car typed) (quotient *canvas-ph* 4))
                  (= (cadr typed) (* 4 *canvas-pw* (car typed)))
                  (= (caddr typed) 1)
                  (equal? kept '(#t #t)))))))

;;; ------------------------------------------------------------------
;;; the window

;; Does the texture hold a frame? render-full-into! paints every pixel, so
;; none is clear (one pixel in 7 looked at).
(define (texture-holds-frame?)
  (and texture
       (let ([td (cairo-image-surface-get-data texture)]
             [ts (cairo-image-surface-get-stride texture)]
             [w (cairo-image-surface-get-width texture)]
             [h (cairo-image-surface-get-height texture)])
         (let row ([y 0])
           (or (= y h)
               (and (let col ([x 0])
                      (or (>= x w)
                          (and (not (fx= 0 (fxsrl (foreign-ref 'unsigned-32 td
                                                               (+ (* y ts)
                                                                  (* 4 x)))
                                                  24)))
                               (col (+ x 7)))))
                    (row (+ y 1))))))))

;; With a window, a step that uploads the whole canvas (at boot, after a
;; resize (full screen, a drag) or a rescale, or when the texture was lost)
;; shows its own frame: the canvas is new and holds no frame yet, and none
;; may be shown. A fake window answers the SDL calls; the host's own
;; events, resizing and upload run as with a real one.
(let ([size (cons *canvas-pw* *canvas-ph*)] [queue 0] [label #f] [blank '()]
      [log '()] [present sdl-render-present])
  (define (size! a b)
    (foreign-set! 'integer-32 a 0 (car size))
    (foreign-set! 'integer-32 b 0 (cdr size)))
  ;; A turn of the main loop with a window: its events, then the step.
  (define (turn! l)
    (set! label l)
    (let ([p0 presents])
      (apply inject! (fluid-let ([*host* 'sdl]) (host-events 0)))
      (schevana-step! 0)
      (set! log (cons (cons l (- presents p0)) log))))
  (define (resize! l w h)
    (set! size (cons w h))
    (set! queue 1)
    (turn! l))
  (fluid-let ([sdl-get-window-size (lambda (win a b) (size! a b))]
              [sdl-get-renderer-output-size (lambda (r a b) (size! a b) 0)]
              [sdl-set-window-minimum-size (lambda (win w h) (void))]
              [sdl-get-window-flags (lambda (win) 0)]
              [sdl-poll-event   ; the queued size-changed events
               (lambda (buf)
                 (if (= queue 0)
                     0
                     (begin (foreign-set! 'unsigned-32 buf 0 SDL_WINDOWEVENT)
                            (foreign-set! 'unsigned-8 buf 12
                                          SDL_WINDOWEVENT_SIZE_CHANGED)
                            (set! queue (- queue 1))
                            1)))]
              [sdl-create-texture   ; a new texture: its pixels are undefined
               (lambda (r f a w h)
                 (when texture
                   (cairo-surface-destroy texture)
                   (set! texture #f))
                 1)]
              [sdl-destroy-texture (lambda (t) (void))]
              [sdl-render-present
               (lambda (r)
                 (unless (texture-holds-frame?) (set! blank (cons label blank)))
                 (present r))]
              [*sdl-texture* 0] [*pixel-ratio* *pixel-ratio*]
              [*host-min-size* #f])
    ;; as host-init! ends, then schevana-boot!
    (fluid-let ([*host* 'sdl]) (host-sync-size!))
    (invalidate!)
    (turn! 'boot)
    (resize! 'resize 1100 700)
    (resize! 'full-screen 1920 1080)
    (resize! 'back 1280 800)
    (do ([i 0 (+ i 1)]) ((= i 4))   ; a drag: a frame, then a resize
      (invalidate!)
      (turn! 'frame)
      (resize! 'drag (- 1240 (* 40 i)) (- 780 (* 20 i))))
    (resize! 'drag-end 1280 800)
    (fluid-let ([*host* 'sdl]) (set-theme! 'scale 150))
    (turn! 'rescale)
    (fluid-let ([*host* 'sdl]) (set-theme! 'scale 100))
    (turn! 'rescale-back)
    (inject! '(render-reset))
    (turn! 'render-reset)
    (fluid-let ([*host* 'sdl]) (handle-event! '(device-reset)))
    (turn! 'device-reset))
  (let ([log (reverse log)])
    (check (format
            "with a window, a step that uploads the whole canvas shows its \
             own frame, never an empty canvas: presents per step ~s; empty ~s"
            log (reverse blank))
           (and (null? blank)
                (for-all (lambda (e) (or (eq? (car e) 'frame) (= (cdr e) 1)))
                         log)
                (= (length (filter (lambda (e) (eq? (car e) 'drag)) log)) 4)
                (= *canvas-pw* 1280) (= *canvas-ph* 800) (= *scale* 1.0)
                (matches-reference? 'window-end)))))

;;; ------------------------------------------------------------------ processes

;; A process (a REPL evaluation is one) is an engine, switched out at any
;; call where interrupts are on, while the main loop draws on, here on the
;; real clock. At 200% it flushes the tile caches, frees owned surfaces,
;; rescales and calls (render-frame!), which in a process only asks for a
;; frame. Besides its time running out, it is switched out for a step now
;; and then where a timer could do it: before a surface is freed or
;; destroyed. No error may come, every frame is the main loop's (one a
;; step at most), and the frame is the reference.
(set-theme! 'scale 200)
(scene! 'S5)
(let* ([acts (make-vector 5 0)] [switches 0] [errors 0] [n0 *frames-drawn*]
       [destroy cairo-surface-destroy] [free free-owned-surface!]
       [report report-error!] [steps-run 300])
  (define (switch-point!)
    (when (and (current-proc) (= 0 (#%$tc-field 'disable-count (#%$tc)))
               (zero? (random 3)))
      (set! switches (+ switches 1))
      (sleep-ms 1)))
  (fluid-let ([now-ms (lambda () (real-time))]
              [report-error! (lambda (who c)
                               (set! errors (+ errors 1)) (report who c))]
              [cairo-surface-destroy (lambda (s) (switch-point!) (destroy s))]
              [free-owned-surface! (lambda (s) (switch-point!) (free s))])
    (let ([p (spawn "repl"
               (lambda ()
                 (let loop ()
                   (do ([k (random 2000)] [j 0 (fx+ j 1)]) ((fx= j k)))
                   (let ([a (random 5)])
                     (vector-set! acts a (+ 1 (vector-ref acts a)))
                     (case a
                       [(0) (flush-shadow-tiles!)]   ; shadow
                       [(1) (free-owned-surfaces!)]
                       [(2) (when (zero? (random 40))
                              (set-theme! 'scale (if (= *scale* 2.0) 150 200)))]
                       [(3) (render-frame!)]
                       [else (free-layer! (make-layer* 40 40))]))
                   (loop))))])
      (do ([n 0 (+ n 1)]) ((= n steps-run)) (schevana-step! 0))
      (kill! p)))
  (set-theme! 'scale 200)
  (let ([frames (- *frames-drawn* n0)])
    (check (format
            "a process, switched out anywhere, frees caches and surfaces, \
             rescales and calls (render-frame!) while the main loop draws \
             (~a calls by kind ~s, ~a switches): \
             ~a frames in ~a steps, ~a errors"
            (apply + (vector->list acts)) acts switches frames steps-run errors)
           (and (= errors 0) (> switches 50) (< 0 frames (+ steps-run 1))
                (> (vector-ref acts 3) 100)
                (matches-reference? 'process-calls)))))

;;; ------------------------------------------------------------------ raising

;; Draws that raise: a window's (it crashes) and the whole frame's (nothing
;; is drawn or uploaded, and the next frame is whole).
(let ([w (open-window "Crashy" 'size '(300 . 160) 'pos '(400 . 300)
                      'draw (lambda (win cw ch) (car '())))])
  (let ([crashed (matches-reference? 'crashed-window)])
    (close-window! w)
    (let ([plain refresh-status!] [frames *frames-drawn*] [u0 *upload-bytes*])
      (set! refresh-status! (lambda () (error 'refresh-status! "on purpose")))
      (parameterize ([console-error-port (open-output-string)])
        (invalidate!)
        (steps 1))
      (let ([shown? (or (not (= frames *frames-drawn*))
                        (not (= u0 *upload-bytes*)))])
        (set! refresh-status! plain)
        (set! *toasts* '())
        (check (format
                "a draw that raises crashes its window, and the frame is the \
                 reference; a frame whose drawing raises shows nothing (~a)"
                shown?)
               (and crashed (window-error w) (not shown?)
                    (matches-reference? 'after-raise)))))))

;;; ------------------------------------------------------------------
;;; redefinition

(define (frame-ms n)   ; median time of n forced frames
  (let loop ([i 0] [ms '()])
    (if (= i n)
        (list-ref (sort < ms) (quotient n 2))
        (begin (invalidate!) (steps 1) (loop (+ i 1) (cons *frame-ms* ms))))))

(set-theme! 'scale 100)
(scene! 'S5)
(pause-lifes!)
(let* ([before (frame-ms 9)]
       [plain-draw-window draw-window] [plain-taskbar draw-taskbar]
       [plain-styles *window-styles*]
       [mac (find-window-style 'mac)])
  (set! draw-window (lambda (win)
                      (draw-window* win #t)
                      (fill-rect (window-x win) (window-y win) 40 6 #xff0000)))
  (set! draw-taskbar (lambda ()
                       (plain-taskbar)
                       (fill-rect 0 (- (screen-h) 6) 60 6 #x0000ff)))
  (define-window-style mac "Mac, striped"
    'insets (window-style-insets mac) 'radius (window-style-radius mac)
    'buttons (window-style-buttons mac)
    'draw-under (window-style-draw-under mac)
    'draw-over (lambda (win x y w h focused? hover pressed)
                 ((window-style-draw-over mac) win x y w h
                                               focused? hover pressed)
                 (fill-rect x (+ y h -5) w 5 #x00ff00)))
  (let* ([after (frame-ms 9)]
         [same (matches-reference? 'redefined)]
         [r (find-window 'repl)]
         [shown (list (pixel-of *canvas* (+ (window-x r) 2) (+ (window-y r) 2))
                      (pixel-of *canvas* 2 (- (screen-h) 2))
                      (pixel-of *canvas* (+ (window-x r) 100)
                                (+ (window-y r) (window-h r) -2)))])
    (set! draw-window plain-draw-window)
    (set! draw-taskbar plain-taskbar)
    (set! *window-styles* plain-styles)
    (request-frame!)
    (check (format
            "draw-window, draw-taskbar and a style, redefined: the next frame \
             shows them ~s, is the reference, and takes ~,2f ms (~,2f before)"
            (map (lambda (p) (format "#x~x" p)) shown) after before)
           (and same (equal? shown '(#xffff0000 #xff0000ff #xff00ff00))
                (< after (+ (* 2 before) 0.5))
                (matches-reference? 'restored)))))

;;; ------------------------------------------------------------------ wallpaper

;; Is the wallpaper's picture draw-wallpaper's drawing as things are now,
;; drawn straight into a layer?
(define (wallpaper-is-drawn?)
  (let ([l (make-layer* *canvas-w* *canvas-h*)])
    (draw-into-layer! l (lambda () (draw-wallpaper *canvas-w* *canvas-h*)))
    (let ([ok (same-image? (layer-surface (car *wallpaper*))
                           (layer-surface l))])
      (free-layer! l)
      ok)))

;; no window there
(define (desktop-pixel) (pixel-of *canvas* 20 (quotient (screen-h) 2)))

;; A theme change, through set-theme! or behind its back, paints the
;; wallpaper again, whatever key changed, and the next frame shows it. A
;; frame with the theme as it was keeps the picture.
(set-theme! 'scale 200)
(clear-world!)
(steps 2)
(let ([p0 (car *wallpaper*)] [px0 (desktop-pixel)] [top (theme 'desktop-top)]
      [bottom (theme 'desktop-bottom)] [red (theme 'red)])
  (invalidate!)
  (steps 1)
  (let ([kept (eq? (car *wallpaper*) p0)])
    (hashtable-set! *theme* 'desktop-top #x204060)
    (request-frame!)
    (steps 1)
    (let* ([p1 (car *wallpaper*)] [px1 (desktop-pixel)]
           [raw (and (wallpaper-is-drawn?) (matches-reference? 'wallpaper-raw))]
           [t0 (current-time 'time-monotonic)])
      (set-theme! 'desktop-bottom #x603020)
      (steps 1)
      (let* ([ms (ms-since t0)] [p2 (car *wallpaper*)] [px2 (desktop-pixel)]
             [set (and (wallpaper-is-drawn?)
                       (matches-reference? 'wallpaper-set))])
        (set-theme! 'red #x00ff00)   ; a key the wallpaper does not use
        (steps 1)
        (let ([p3 (car *wallpaper*)])
          (hashtable-set! *theme* 'desktop-top top)
          (set-theme! 'desktop-bottom bottom)
          (set-theme! 'red red)
          (steps 1)
          (check (format
                  "a theme change, raw (#x~x -> #x~x) or by set-theme! \
                   (-> #x~x), paints the wallpaper again for the next frame, \
                   whatever key changed \
                   (the change and its frame take ~,1f ms at 200%)"
                  px0 px1 px2 ms)
                 (and kept raw set (not (= px0 px1)) (not (= px1 px2))
                      (not (memq p1 (list p0))) (not (memq p2 (list p0 p1)))
                      (not (memq p3 (list p0 p1 p2)))
                      (= (desktop-pixel) px0) (wallpaper-is-drawn?))))))))

;; A redefined draw-wallpaper shows on the next frame. One that reads the
;; clock shows a still picture (the wallpaper is a picture of its key).
(let ([plain draw-wallpaper])
  (set! draw-wallpaper (lambda (w h) (fill-rect 0 0 w h #x336699)))
  (invalidate!)
  (steps 1)
  (let ([shown (desktop-pixel)]
        [same (and (wallpaper-is-drawn?)
                   (matches-reference? 'wallpaper-redefined))])
    (set! draw-wallpaper (lambda (w h)
                           (fill-rect 0 0 w h
                                      (if (even? (quotient fake-ms 1000))
                                          #x112233
                                          #x445566))))
    (invalidate!)
    (steps 1)
    (let ([p (car *wallpaper*)] [px (desktop-pixel)])
      (steps 3 1000)
      (invalidate!)
      (steps 1)
      (let ([still (and (eq? (car *wallpaper*) p) (= px (desktop-pixel)))])
        (set! draw-wallpaper plain)
        (invalidate!)
        (steps 1)
        (check (format
                "a redefined draw-wallpaper shows on the next frame (#x~x); \
                 one that reads the clock shows a still picture (~a); \
                 the old one is back the frame after"
                shown still)
               (and (= shown #xff336699) same still (wallpaper-is-drawn?)
                    (matches-reference? 'wallpaper-back)))))))

;; The wallpaper in use, defined again, shows on the next frame.
(let ([pattern (theme 'desktop-pattern)] [plain *wallpapers*])
  (set-theme! 'desktop-pattern 'flat)
  (steps 1)
  (let ([px0 (desktop-pixel)])
    (define-wallpaper flat "Flat"
      (lambda (x y w h top bottom) (fill-rect x y w h #x336699)))
    (steps 1)
    (let ([shown (desktop-pixel)]
          [same (and (wallpaper-is-drawn?)
                     (matches-reference? 'wallpaper-defined-again))])
      (set! *wallpapers* plain)
      (set-theme! 'desktop-pattern pattern)
      (steps 1)
      (check (format "the wallpaper in use, defined again, shows on the next \
                      frame (#x~x -> #x~x)" px0 shown)
             (and (= shown #xff336699) (not (= px0 shown)) same
                  (wallpaper-is-drawn?)
                  (matches-reference? 'wallpaper-restored))))))

;;; ------------------------------------------------------------------ Life

;; Each Life window steps a generation (its pixmap's buffers swap), twice:
;; each next frame shows it.
(set-theme! 'scale 200)
(scene! 'S30T)
(pause-lifes!)
(steps 2)
(let ([lifes (filter (lambda (w) (eq? (window-app w) 'life)) (windows))])
  (define (generation!)
    (let ([before (canvas-copy)])
      (for-each (lambda (w) (window-call w 'key #\n '())) lifes)
      (steps 1)
      (let ([changed (> (differing *canvas* before #f) 0)])
        (cairo-surface-destroy before)
        (and changed (matches-reference? 'life)))))
  (let* ([first (generation!)] [second (generation!)])
    (check (format "30 Life windows step a generation, twice: each next \
                    frame shows it and is the reference (~a ~a)" first second)
           (and first second))))

(check (format "the texture, fed only the uploaded rows, was the canvas after \
                every step (~a of ~a); \
                presents only after an upload or an expose (~a bad)"
               (- texture-steps texture-mismatches) texture-steps bad-presents)
       (and (= texture-mismatches 0) (= bad-presents 0) (> texture-steps 400)))

(if (zero? failures)
    (printf "all checks passed\n")
    (printf "~a check(s) FAILED\n" failures))
(exit (if (zero? failures) 0 1))
