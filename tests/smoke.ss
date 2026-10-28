;;; tests/smoke.ss — drive the whole world headlessly with synthetic events.
;;;
;;;   scheme --script tests/smoke.ss [output-dir]
;;;
;;; Writes screenshots into output-dir (default: current directory).

(define *schevana-root*
  (let ([dir (path-parent (path-parent (car (command-line))))])
    (if (string=? dir "") "." dir)))

(load (string-append *schevana-root* "/load.ss"))

(define out-dir
  (if (pair? (command-line-arguments)) (car (command-line-arguments)) "."))

;; Never touch the user's real theme file.
(set! *theme-file* (string-append out-dir "/theme.ss"))
(when (file-exists? *theme-file*) (delete-file *theme-file*))

(define failures 0)

(define-syntax check
  (syntax-rules ()
    [(_ name expr)
     (let ([ok (guard (c [#t (printf "  error: ~a\n" (condition->string c)) #f]) expr)])
       (unless ok (set! failures (+ failures 1)))
       (printf "~a ~a\n" (if ok "ok  " "FAIL") name))]))

(define (steps n) (do ([i 0 (+ i 1)]) ((= i n)) (schevana-step! 0)))

(define (type! str)
  (for-each (lambda (c) (inject! (list 'text (string c)))) (string->list str)))

(define (press! key . mods) (inject! (list 'key-down key mods)))

(define (repl-of win) (window-ref win 'state))

(define (transcript win)
  (map cdr (reverse (repl-lines (repl-of win)))))

(define (wait-for pred)
  (let ([deadline (+ (now-ms) 3000)])
    (let loop ()
      (cond [(pred) #t]
            [(> (now-ms) deadline) #f]
            [else (schevana-step! 5) (loop)]))))

;; Click (or drag across) a ui widget by its id, with real mouse events.
(define (ui-point win id frac)
  (let ([r (ui-find (window-ref win 'ui) id)])
    (unless r (errorf 'ui-point "no widget ~s" id))
    (let-values ([(cx cy cw ch) (content-rect win)])
      (values (exact (round (+ cx (ui-region-x r) 8 (* frac (- (ui-region-w r) 16)))))
              (exact (round (+ cy (ui-region-y r) (/ (ui-region-h r) 2))))))))

(define (click-ui! win id)
  (let-values ([(x y) (ui-point win id 0.5)])
    (inject! `(mouse-move ,x ,y) `(mouse-down ,x ,y left 1) `(mouse-up ,x ,y left))
    (steps 2)))

(define (drag-ui! win id frac)
  (let-values ([(x0 y) (ui-point win id 0.5)] [(x1 y1) (ui-point win id frac)])
    (inject! `(mouse-down ,x0 ,y left 1) `(mouse-move ,x1 ,y) `(mouse-up ,x1 ,y left))
    (steps 2)))

(schevana-boot! 'headless 1280 800)
(clock)
(move-window! (find-window 'clock) 1000 60)
(define r (repl))
(steps 2)

(check "boot opened two windows" (= 2 (length (windows))))
(check "repl is focused" (eq? r (focused-window)))

;; Evaluate an expression by typing it.
(type! "(map (lambda (x) (* x x)) '(1 2 3 4))")
(press! 'return)
(check "repl evaluates" (wait-for (lambda () (member "(1 4 9 16)" (transcript r)))))

;; Multi-line input: an incomplete form gets a newline, not evaluation.
(type! "(define (fact n)")
(press! 'return)
(type! "(if (= n 0) 1 (* n (fact (- n 1)))))")
(press! 'return)
(type! "(fact 20)")
(press! 'return)
(check "multi-line define" (wait-for (lambda () (member "2432902008176640000" (transcript r)))))

;; Output from display goes into the transcript.
(type! "(begin (display \"hello\") (newline) 42)")
(press! 'return)
(check "display output" (wait-for (lambda () (and (member "hello" (transcript r)) (member "42" (transcript r))))))

;; Errors are shown, not fatal.
(type! "(car 5)")
(press! 'return)
(check "errors are reported"
       (wait-for (lambda () (exists (lambda (l) (string-contains-ci? l "car")) (transcript r)))))

;; An infinite loop runs as a process; Ctrl+C kills it, the world stays alive.
(type! "(let loop () (loop))")
(press! 'return)
(steps 5)
(check "infinite loop becomes a process" (= 1 (length (procs))))
(press! #\c 'ctrl)
(steps 3)
(check "ctrl+c kills it" (null? (procs)))

;; Define a brand-new app from the REPL and launch it.
(type! "(define-app (hello) \"Hello\" (open-window \"Hello\" 'size '(260 . 90) 'draw (lambda (win w h) (draw-text 20 30 \"Hello from the REPL!\" 'accent 'ui-bold))))")
(press! 'return)
(type! "(hello)")
(press! 'return)
(check "app defined at the REPL opens" (wait-for (lambda () (find-window "Hello"))))
(check "and is in the launcher" (memq 'hello (apps)))

;; A crashing app only crashes its own window.
(define bad (open-window "Broken" 'size '(300 . 120) 'pos '(80 . 480)
                         'draw (lambda (win w h) (vector-ref (vector) 3))))
(steps 2)
(check "crashing draw marks the window" (window-error bad))

;; A runaway handler is stopped by the watchdog.
(define stuck (open-window "Stuck" 'size '(300 . 80) 'pos '(420 . 560)
                           'key (lambda (win k m) (let loop () (loop)))))
(press! #\x)
(steps 2)
(check "watchdog stops runaway handler" (window-error stuck))
(close-window! stuck)

;; Window management through synthetic mouse events.
(define c (find-window 'clock))
(let ([x0 (window-x c)])
  (inject! `(mouse-down ,(+ (window-x c) 120) ,(+ (window-y c) 10) left 1)
           `(mouse-move ,(+ (window-x c) 20) ,(+ (window-y c) 30))
           `(mouse-up ,(+ (window-x c) 20) ,(+ (window-y c) 30) left))
  (steps 2)
  (check "dragging the title bar moves a window" (= (window-x c) (- x0 100))))

(let ([w0 (window-w c)])
  (inject! `(mouse-down ,(+ (window-x c) (window-w c) -2) ,(+ (window-y c) (window-h c) -2) left 1)
           `(mouse-move ,(+ (window-x c) (window-w c) 38) ,(+ (window-y c) (window-h c) 18))
           `(mouse-up ,(+ (window-x c) (window-w c) 38) ,(+ (window-y c) (window-h c) 18) left))
  (steps 2)
  (check "dragging the corner resizes" (= (window-w c) (+ w0 40))))

(screenshot (string-append out-dir "/desktop.png"))

;; The launcher menu.
(press! 'f1)
(steps 1)
(check "F1 opens the launcher" (launcher-open?))
(type! "lif")
(steps 1)
(screenshot (string-append out-dir "/launcher.png"))
(press! 'return)
(steps 2)
(check "launcher starts Life" (find-window 'life))

;; Theme changes are live.
(set-theme! 'accent #xff9e64)
(sketch)
(monitor)
(tile-windows!)
(steps 3)
(screenshot (string-append out-dir "/tiled.png"))
(reset-theme!)

;; Settings: every control is a theme key; changes apply live and persist.
(for-each close-window! (windows))
(define s (settings))
(steps 2)
(check "settings opens" (and s (not (window-error s))))
(check "settings is single-instance" (eq? s (settings)))
(click-ui! s '(preset nord))
(check "clicking a preset applies it" (and (= (theme 'accent) #x88c0d0) (eq? (theme 'preset) 'nord)))
(screenshot (string-append out-dir "/settings-theme.png"))
(click-ui! s '(accent orange))
(check "accent swatch sets the accent" (= (theme 'accent) #xd08770))
(click-ui! s '(tab colours))
(click-ui! s '(colour accent))
(click-ui! s 'hex)
(type! "e06c75")
(press! 'return)
(steps 2)
(check "typing a hex colour sets it" (= (theme 'accent) #xe06c75))
(check "accent edits update derived keys" (= (theme 'window-border-focus) (mix (theme 'bg-3) #xe06c75 0.45)))
(drag-ui! s 'lightness 0.25)
(check "lightness slider darkens it"
       (let-values ([(h sat l) (color->hsl (theme 'accent))]) (< (abs (- l 0.25)) 0.02)))
(screenshot (string-append out-dir "/settings-colours.png"))
(click-ui! s '(tab layout))
(drag-ui! s '(metric corner-radius) 0)
(check "layout slider sets a metric" (= 0 (theme 'corner-radius)))
(drag-ui! s '(metric title-height) 1)
(check "title bar height follows" (= 44 (theme 'title-height)))
(screenshot (string-append out-dir "/settings-layout.png"))
(click-ui! s '(tab fonts))
(click-ui! s '(ui-font "serif"))
(drag-ui! s '(metric mono-font-size) 1)
(check "font settings rebuild fonts"
       (and (equal? (font-family (get-font 'ui)) "serif") (= 22 (font-size (get-font 'mono)))))
(screenshot (string-append out-dir "/settings-fonts.png"))
(check "settings never crashed" (not (window-error s)))
(check "changes are saved shortly after" (wait-for (lambda () (file-exists? *theme-file*))))
(let ([saved-accent (theme 'accent)])
  (reset-theme!)
  (check "reset restores defaults" (= (theme 'corner-radius) (theme-default 'corner-radius)))
  (load-saved-theme!)
  (check "saved theme loads back"
         (and (= (theme 'accent) saved-accent) (= 0 (theme 'corner-radius)) (eq? (theme 'preset) 'nord)
              (equal? (theme 'ui-font) "serif"))))
(apply-theme-preset! 'paper)
(set-theme! 'title-height 30)
(set-theme! 'mono-font-size 14)
(set-theme! 'ui-font "sans-serif")
(repl)
(clock)
(move-window! (find-window 'clock) 900 300)
(click-ui! s '(tab theme))
(steps 2)
(screenshot (string-append out-dir "/paper.png"))
(reset-theme!)
(close-window! s)

;; Reloading the sources keeps the world's state.
(let ([n (length (windows))])
  (reload!)
  (steps 2)
  (check "reload keeps windows" (= n (length (windows))))
  (check "windows still draw after reload" (not (window-error (find-window 'clock)))))

(check "frame time is sane" (< *frame-ms* 200))
(printf "frame: ~,1f ms, heap: ~,1f MB\n" *frame-ms* (/ (bytes-allocated) 1048576.0))
(printf "~a\n" (if (zero? failures) "all checks passed" (format "~a check(s) FAILED" failures)))
(exit (if (zero? failures) 0 1))
