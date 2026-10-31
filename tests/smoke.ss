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
     (let ([ok (guard (c [#t (printf "  error: ~a\n" (condition->string c)) #f])
                 expr)])
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
      (values (exact (round (+ cx (ui-region-x r) 8
                               (* frac (- (ui-region-w r) 16)))))
              (exact (round (+ cy (ui-region-y r) (/ (ui-region-h r) 2))))))))

;; Wheels a scrolling pane (Settings') until the area id is in view.
(define (scroll-to-ui! win id)
  (let loop ([n 0])
    (let-values ([(x y) (ui-point win id 0.5)]
                 [(cx cy cw ch) (content-rect win)])
      (let ([dy (cond [(> y (+ cy ch -10)) -1] [(< y (+ cy 10)) 1] [else 0])])
        (unless (or (zero? dy) (= n 40))
          (inject! `(mouse-move ,(+ cx cw -20) ,(+ cy 100)) `(wheel 0 ,dy))
          (steps 2)
          (loop (+ n 1)))))))

(define (click-ui! win id)
  (scroll-to-ui! win id)
  (let-values ([(x y) (ui-point win id 0.5)])
    (inject! `(mouse-move ,x ,y) `(mouse-down ,x ,y left 1)
             `(mouse-up ,x ,y left))
    (steps 2)))

(define (drag-ui! win id frac)
  (let-values ([(x0 y) (ui-point win id 0.5)] [(x1 y1) (ui-point win id frac)])
    (inject! `(mouse-down ,x0 ,y left 1) `(mouse-move ,x1 ,y)
             `(mouse-up ,x1 ,y left))
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
(check "repl evaluates"
       (wait-for (lambda () (member "(1 4 9 16)" (transcript r)))))

;; Multi-line input: an incomplete form gets a newline, not evaluation.
(type! "(define (fact n)")
(press! 'return)
(type! "(if (= n 0) 1 (* n (fact (- n 1)))))")
(press! 'return)
(type! "(fact 20)")
(press! 'return)
(check "multi-line define"
       (wait-for (lambda () (member "2432902008176640000" (transcript r)))))

;; Output from display goes into the transcript.
(type! "(begin (display \"hello\") (newline) 42)")
(press! 'return)
(check "display output"
       (wait-for (lambda () (and (member "hello" (transcript r))
                                 (member "42" (transcript r))))))

;; Errors are shown, not fatal.
(type! "(car 5)")
(press! 'return)
(check "errors are reported"
       (wait-for (lambda () (exists (lambda (l) (string-contains-ci? l "car"))
                                    (transcript r)))))

;; An infinite loop runs as a process; Ctrl+C kills it, the world stays alive.
(type! "(let loop () (loop))")
(press! 'return)
(steps 5)
(check "infinite loop becomes a process" (= 1 (length (procs))))
(press! #\c 'ctrl)
(steps 3)
(check "ctrl+c kills it" (null? (procs)))

;; Define a brand-new app from the REPL and launch it.
(type! "(define-app (hello) \"Hello\" (open-window \"Hello\" 'size '(260 . 90) \
        'draw (lambda (win w h) (draw-text 20 30 \"Hello from the REPL!\" \
        'accent 'ui-bold))))")
(press! 'return)
(type! "(hello)")
(press! 'return)
(check "app defined at the REPL opens"
       (wait-for (lambda () (find-window "Hello"))))
(check "and is in the launcher" (memq 'hello (apps)))

;; Private and pure REPLs: experiment without touching the world.
(define world-draw-window draw-window)
(define p (private-repl))
(steps 2)
(check "private repl opens focused" (eq? p (focused-window)))
(type! "(define (draw-window win) (error 'oops \"broken\"))")
(press! 'return)
(type! "(define private-secret 42)")
(press! 'return)
(type! "(set! *windows* '())")
(press! 'return)
(type! "(list private-secret (length (windows)))")
(press! 'return)
(check "private definitions work inside, world procedures see the world"
       (wait-for (lambda () (member (format "(42 ~a)" (length (windows)))
                                    (transcript p)))))
(check "but nothing leaks into the world"
       (and (eq? draw-window world-draw-window)
            (not (top-level-bound? 'private-secret))
            (memq p (windows))))
(type! "private-sec")
(press! 'tab)
(steps 1)
(check "tab completes from the private environment"
       (string=? "private-secret" (repl-input (repl-of p))))

;; The completion list: narrowing, choosing, contexts.
(let* ([w (repl)] [r (repl-of w)])
  (define (fresh! text)
    (repl-input-set! r "")
    (repl-point-set! r 0)
    (close-completion! r)
    (type! text)
    (steps 1))
  (define (offered)
    (let ([c (repl-completion r)])
      (and c (map car (vector->list (completion-items c))))))
  (fresh! "(set-th")
  (press! 'tab)
  (steps 1)
  (check "tab extends to the shared prefix and lists the choices"
         (and (string=? "(set-theme" (repl-input r))
              (equal? (list-head (offered) 2)
                      '("set-theme!" "set-theme-plist!"))))
  (screenshot (string-append out-dir "/completion.png"))
  (press! 'down)
  (press! 'tab)
  (steps 1)
  (check "down and tab take the second one"
         (and (string=? "(set-theme-plist!" (repl-input r)) (not (offered))))
  (fresh! "(stp")
  (press! 'tab)
  (steps 1)
  (check "initials find set-theme-plist! first"
         (equal? "set-theme-plist!" (car (offered))))
  (type! "l")
  (steps 1)
  (check "typing narrows the list"
         (for-all (lambda (n) (string-contains-ci? n "l")) (offered)))
  (let ([lines (length (repl-lines r))])
    (press! 'return)
    (steps 1)
    (check "enter takes the choice instead of evaluating"
           (and (string=? "(set-theme-plist!" (repl-input r))
                (= lines (length (repl-lines r))))))
  (fresh! "(theme 'acc")
  (press! 'tab)
  (steps 1)
  (check "after (theme ' it offers theme keys only"
         (equal? (offered) '("accent" "accent-2")))
  (press! 'escape)
  (steps 1)
  (check "escape closes the list" (not (offered)))
  (call-with-output-file (string-append out-dir "/zz-complete-me.txt")
    (lambda (p) (display "x" p))
    'replace)
  (fresh! (string-append "(load \"" out-dir "/zz-comp"))
  (press! 'tab)
  (steps 1)
  (check "inside a string it completes file names"
         (string-suffix? "/zz-complete-me.txt" (repl-input r)))
  (let-values ([(sig doc) (name-description "after" (interaction-environment))])
    (check "a name shows its argument list and the comment above its definition"
           (and (string=? sig "(after ms thunk)")
                (string-contains-ci? doc "a second later"))))
  (check "other procedures show their arity"
         (string=? "Takes 1 argument."
                   (arity-description (procedure-arity-mask car))))
  ;; Libraries: a folder of our own, searched by import and offered by Tab.
  (let ([lib (string-append out-dir "/lib")])
    (for-each (lambda (d) (unless (file-exists? d) (mkdir d)))
              (list lib (string-append lib "/acme")))
    (call-with-output-file (string-append lib "/acme/math.sls")
      (lambda (p)
        (write '(library (acme math) (export square (rename (twice double)))
                  (import (rnrs))
                  (define (square x) (* x x)) (define (twice x) (+ x x)))
               p))
      'replace)
    (add-library-directory! lib)
    (check "the tree's lib folder is searched by import, the user's not"
           (let ([on-path? (lambda (dir)
                             (exists (lambda (e)
                                       (equal? (if (pair? e) (car e) e) dir))
                                     (library-directories)))])
             (and (on-path? (string-append *schevana-root* "/lib"))
                  (not (on-path? user-library-directory)))))
    (check "import loads a library from an added folder"
           (equal? '(25 6)
                   (eval '(let () (import (acme math))
                            (list (square 5) (double 3))))))
    (fresh! "(import (ac")
    (press! 'tab)
    (steps 1)
    (check "tab in an import completes the first part and offers the next"
           (and (string=? "(import (acme " (repl-input r))
                (equal? (offered) '("math"))))
    (press! 'tab)
    (steps 1)
    (check "and tab takes the library"
           (and (string=? "(import (acme math" (repl-input r)) (not (offered))))
    (fresh! "(import (only (acme m")
    (press! 'tab)
    (steps 1)
    (check "library names complete inside only"
           (string=? "(import (only (acme math" (repl-input r)))
    (fresh! "(import (")
    (press! 'tab)
    (steps 1)
    (check "an empty import set offers libraries and import forms"
           (for-all (lambda (n) (member n (offered)))
                    '("acme" "rnrs" "chezscheme" "only")))
    (fresh! "(list (acm")
    (press! 'tab)
    (steps 1)
    (check "outside an import there are no library names"
           (not (member "acme" (or (offered) '()))))
    (fresh! "(library (acm")
    (press! 'tab)
    (steps 1)
    (check "a library's own name is not completed"
           (not (member "acme" (or (offered) '()))))
    (let-values ([(sig doc) (library-description '(acme) "math" "library")])
      (check "a library shows what it exports and where it is"
             (and (string=? sig "(acme math)")
                  (string-contains-ci? doc "double square")
                  (string-contains-ci? doc "acme/math.sls")))))
  (close-window! w))
(press! #\u 'ctrl)
(type! "(exit)")
(press! 'return)
(check "(exit) closes only the private repl"
       (wait-for (lambda () (not (memq p (windows))))))
(define q (pure-repl))
(steps 2)
(type! "(windows)")
(press! 'return)
(check "pure repl has none of Schevana's names"
       (wait-for (lambda ()
                   (exists (lambda (l) (string-contains-ci? l "not bound"))
                           (transcript q)))))
(type! "(define x (* 6 7))")
(press! 'return)
(type! "x")
(press! 'return)
(check "but is a working Scheme"
       (wait-for (lambda () (member "42" (transcript q)))))
(check "whose definitions stay put" (not (top-level-bound? 'x)))
(press! #\p 'ctrl 'shift)
(steps 2)
(check "ctrl+shift+p opens a private repl"
       (equal? "Private REPL" (window-title (focused-window))))
(type! "(define note \"only here\")")
(press! 'return)
(steps 20)
(tile-windows!)
(steps 2)
(screenshot (string-append out-dir "/repl-modes.png"))
(close-window! q)
(close-window! (focused-window))
(focus-window! r)

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
  (inject! `(mouse-down ,(+ (window-x c) (window-w c) -2)
                        ,(+ (window-y c) (window-h c) -2) left 1)
           `(mouse-move ,(+ (window-x c) (window-w c) 38)
                        ,(+ (window-y c) (window-h c) 18))
           `(mouse-up ,(+ (window-x c) (window-w c) 38)
                      ,(+ (window-y c) (window-h c) 18) left))
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

;; Apps picked from the desktop's right-click menu open where you clicked.
(define (launch-from-desktop! x y title)
  (inject! `(mouse-move ,x ,y) `(mouse-down ,x ,y right 1)
           `(mouse-up ,x ,y right))
  (steps 2)
  (when (launcher-open?)
    (menu-activate!
     (list-index (lambda (e) (and (pair? e) (string=? (car e) title)))
                 (menu-entries *menu*)))
    (steps 2)))
(close-window! (find-window 'life))
(launch-from-desktop! 700 400 "Clock")
(check "an app picked from the desktop menu opens where you clicked"
       (let ([c (find-window 'clock)])
         (and c (= (window-x c) 700) (= (window-y c) 400))))
(close-window! (find-window 'clock))
(launch-from-desktop! 1250 740 "Clock")
(check "and stays on screen near the edge"
       (let ([c (find-window 'clock)])
         (and c (= (+ (window-x c) (window-w c)) (screen-w))
              (= (+ (window-y c) (window-h c))
                 (- (screen-h) (taskbar-height))))))
(close-window! (find-window 'clock))

;; Full screen, and the host window's icon.
(press! 'f11)
(steps 1)
(check "F11 switches to full screen" (fullscreen?))
(press! 'f11)
(steps 1)
(check "and back" (not (fullscreen?)))
(check "the app icon draws"
       (let ([l (make-layer* 64 64)])
         (draw-into-layer! l (lambda () (draw-app-icon 64)))
         (free-layer! l)
         #t))

;; Theme changes are live.
(set-theme! 'accent #xff9e64)
(sketch)
(monitor)
(tile-windows!)
(steps 3)
(screenshot (string-append out-dir "/tiled.png"))
(reset-theme!)

;; Window styles: the frame, its buttons and their actions come from the style.
(define (click-button! win kind)
  (let ([b (assq kind (window-buttons win))])
    (unless b (errorf 'click-button! "~s has no ~s button" win kind))
    (let ([x (+ (window-x win) (list-ref b 1) (quotient (list-ref b 3) 2))]
          [y (+ (window-y win) (list-ref b 2) (quotient (list-ref b 4) 2))])
      (inject! `(mouse-move ,x ,y) `(mouse-down ,x ,y left 1)
               `(mouse-up ,x ,y left))
      (steps 2))))

(define (style-scene! style)
  (for-each close-window! (windows))
  (set-theme! 'window-style style)
  (repl)
  (clock)
  (move-window! (find-window 'clock) 900 60)
  (life)
  (move-window! (find-window 'life) 560 330)
  (focus-window! (find-window 'repl))
  (steps 2)
  (screenshot (string-append out-dir "/style-" (symbol->string style) ".png"))
  (for-all (lambda (w) (not (window-error w))) (windows)))

(check "mac style draws" (style-scene! 'mac))
(check "windows style draws" (style-scene! 'windows))
(let ([w (open-window "Alpha" 'size '(400 . 200) 'pos '(100 . 100))])
  (let-values ([(cx cy cw ch) (content-rect w)])
    (check "windows frame is just a title bar"
           (and (= cx 100) (= cw 400) (= (window-w w) 400))))
  (click-button! w 'maximize)
  (check "windows maximize button" (maximized? w))
  (click-button! w 'minimize)
  (check "windows minimize button" (not (visible? w)))
  (focus-window! w)
  (click-button! w 'close)
  (check "windows close button" (not (memq w (windows)))))

(check "interlisp style draws" (style-scene! 'interlisp))
(let ([w (open-window "Gamma" 'size '(300 . 150) 'pos '(200 . 140))])
  (let-values ([(l t r b) (frame-insets w)])
    (check "interlisp has a 2-pixel border, a 15-pixel title bar and no buttons"
           (and (= l 2) (= r 2) (= b 2) (= t 15) (null? (window-buttons w)))))
  (inject! '(mouse-move 300 147) '(mouse-down 300 147 right 1)
           '(mouse-up 300 147 right))
  (steps 2)
  (check "so the right-button menu on its title closes it"
         (and (menu-open?)
              (find (lambda (e) (and (pair? e) (string=? (car e) "Close")))
                    (menu-entries *menu*))))
  (close-menu!))

(check "genera style draws" (style-scene! 'genera))
(let ([w (open-window "Delta" 'size '(300 . 150) 'pos '(200 . 140))])
  (let-values ([(l t r b) (frame-insets w)])
    (check "genera has a 2-pixel border, a 4-pixel margin and the label at \
            the bottom"
           (and (= l 6) (= t 6) (= r 6) (= b 23))))
  (let ([x 260] [y (+ 140 (window-h w) -10)])
    (inject! `(mouse-move ,x ,y) `(mouse-down ,x ,y left 1)
             `(mouse-move ,(+ x 40) ,(- y 30))
             `(mouse-up ,(+ x 40) ,(- y 30) left))
    (steps 2))
  (check "dragging the label at the bottom moves the window"
         (and (= (window-x w) 240) (= (window-y w) 110)))
  (let* ([w0 (window-w w)]
         [x (+ (window-x w) w0 -3)]
         [y (+ (window-y w) (window-h w) -3)])
    (inject! `(mouse-move ,x ,y) `(mouse-down ,x ,y left 1)
             `(mouse-move ,(+ x 20) ,y) `(mouse-up ,(+ x 20) ,y left))
    (steps 2)
    (check "and its corner still resizes it" (= (window-w w) (+ w0 20)))))
(apply-theme-preset! 'genera)
(style-scene! 'genera)
(screenshot (string-append out-dir "/style-genera-palette.png"))
(check "genera's palette keeps its own panel text"
       (= (theme 'panel-text) #xffffff))
(reset-theme!)

;; Depth and size gadgets, which no built-in style uses now, still work in
;; a style defined at run time.
(define-window-style gadgets "Gadgets"
  'insets (lambda (win) (values 4 20 4 12))
  'buttons (lambda (win)
             (let ([w (window-w win)] [h (window-h win)])
               (list (list 'maximize (- w 48) 0 24 20)
                     (list 'depth (- w 24) 0 24 20)
                     (list 'resize-br (- w 16) (- h 12) 16 12)))))
(for-each close-window! (windows))
(set-theme! 'window-style 'gadgets)
(let* ([a (open-window "One" 'size '(300 . 150) 'pos '(80 . 80))]
       [b (open-window "Two" 'size '(300 . 150) 'pos '(200 . 140))])
  (click-button! b 'depth)
  (check "depth gadget sends the front window back"
         (and (eq? (car (windows)) a) (eq? (car (last-pair (windows))) b)))
  (click-button! b 'depth)
  (check "and brings it to the front again" (eq? (car (windows)) b))
  (let ([w0 (window-w b)])
    (let ([g (assq 'resize-br (window-buttons b))])
      (let ([x (+ (window-x b) (list-ref g 1) 5)]
            [y (+ (window-y b) (list-ref g 2) 5)])
        (inject! `(mouse-down ,x ,y left 1) `(mouse-move ,(+ x 30) ,(+ y 10))
                 `(mouse-up ,(+ x 30) ,(+ y 10) left))
        (steps 2)))
    (check "size gadget resizes" (= (window-w b) (+ w0 30))))
  (click-button! b 'maximize)
  (check "zoom gadget maximizes" (maximized? b))
  (let ([m (open-window "Mac island" 'size '(200 . 100) 'window-style 'mac)])
    (check "a window can override the style"
           (and (= (window-w m) 200) (assq 'minimize (window-buttons m))))))
(set! *window-styles*
  (remp (lambda (s) (eq? (window-style-name s) 'gadgets)) *window-styles*))
(set-theme! 'window-style 'mac)

;; A style, theme, wallpaper or app defined again keeps its place and takes
;; the new definition.
(let ([styles *window-styles*] [themes0 *themes*] [papers *wallpapers*]
      [apps0 *apps*])
  (define-window-style genera "Genera, again")
  (define-theme nord "Nord, again")
  (define-wallpaper flat "Flat, again" (lambda (x y w h top bottom) (void)))
  (register-app! 'clock "Clock, again" (lambda () #f))
  (let ([named (lambda (registry name-of title-of)
                 (map (lambda (e) (cons (name-of e) (title-of e))) registry))])
    (check "a style, theme, wallpaper or app defined again keeps its place"
           (and (equal? (window-styles) (map window-style-name styles))
                (equal? (themes) (map car themes0))
                (equal? (wallpapers) (map car papers))
                (equal? (apps) (map app-name apps0))
                (member '(genera . "Genera, again")
                        (named *window-styles* window-style-name
                               window-style-title))
                (equal? (preset-colors 'nord) default-colors)
                (equal? (preset-title 'nord) "Nord, again")
                (member '(flat . "Flat, again") (named *wallpapers* car cadr))
                (member '(clock . "Clock, again")
                        (named *apps* app-name app-title)))))
  (set! *window-styles* styles)
  (set! *themes* themes0)
  (set! *wallpapers* papers)
  (set! *apps* apps0)
  (wm-chrome-changed!))

(apply-theme-preset! 'interlisp)
(style-scene! 'interlisp)
(screenshot (string-append out-dir "/style-interlisp-palette.png"))
(check "the interlisp palette shades the desktop"
       (eq? (theme 'desktop-pattern) 'shade))
(apply-theme-preset! 'nord)
(check "and other palettes go back to the gradient"
       (eq? (theme 'desktop-pattern) 'gradient))
;; A theme saved before Amiga and Workbench were removed still loads.
(reset-theme!)
(call-with-output-file *theme-file*
  (lambda (p)
    (write '((window-style . amiga) (preset . workbench) (corner-radius . 4))
           p)))
(load-saved-theme!)
(check
 "a saved theme naming a removed style or palette falls back to the defaults"
 (and (eq? (theme 'window-style) 'mac) (eq? (theme 'preset) 'tokyo-night)
      (= 4 (theme 'corner-radius))))
(delete-file *theme-file*)
(reset-theme!)
(for-each close-window! (windows))

;; Settings: every control is a theme key; changes apply live and persist.
(for-each close-window! (windows))
(define s (settings))
(steps 2)
;; The Layout tab's sliders sit below the style cards, out of view once
;; there are enough styles: scroll the pane to its end.
(define (scroll-settings-down!)
  (let-values ([(cx cy cw ch) (content-rect s)])
    (apply inject! `(mouse-move ,(+ cx cw -20) ,(+ cy 100))
           (make-list 30 '(wheel 0 -1)))
    (steps 2)))
(check "settings opens" (and s (not (window-error s))))
(check "settings is single-instance" (eq? s (settings)))
(click-ui! s '(preset nord))
(check "clicking a preset applies it"
       (and (= (theme 'accent) #x88c0d0) (eq? (theme 'preset) 'nord)))
(screenshot (string-append out-dir "/settings-theme.png"))
(click-ui! s '(accent orange))
(check "accent swatch sets the accent" (= (theme 'accent) #xd08770))
(click-ui! s '(tab colors))
(click-ui! s '(color accent))
(click-ui! s 'hex)
(type! "e06c75")
(press! 'return)
(steps 2)
(check "typing a hex color sets it" (= (theme 'accent) #xe06c75))
(check "accent edits update derived keys"
       (= (theme 'window-border-focus) (mix (theme 'bg-3) #xe06c75 0.45)))
(drag-ui! s 'lightness 0.25)
(check "lightness slider darkens it"
       (let-values ([(h sat l) (color->hsl (theme 'accent))])
         (< (abs (- l 0.25)) 0.02)))
(screenshot (string-append out-dir "/settings-colors.png"))
(click-ui! s '(tab layout))
(click-ui! s '(style windows))
(check "style card sets the window style" (eq? (theme 'window-style) 'windows))
(screenshot (string-append out-dir "/settings-layout-windows.png"))
(click-ui! s '(style genera))
(check "and Genera" (eq? (theme 'window-style) 'genera))
(click-ui! s '(style mac))
(scroll-settings-down!)
(drag-ui! s '(metric corner-radius) 0)
(check "layout slider sets a metric" (= 0 (theme 'corner-radius)))
(drag-ui! s '(metric title-height) 1)
(check "title bar height follows" (= 44 (theme 'title-height)))
(screenshot (string-append out-dir "/settings-layout.png"))
(click-ui! s '(tab fonts))
(click-ui! s '(ui-font "serif"))
(drag-ui! s '(metric mono-font-size) 1)
(check "font settings rebuild fonts"
       (and (equal? (font-family (get-font 'ui)) "serif")
            (= 44 (font-size (get-font 'mono)))))
(drag-ui! s '(metric ui-font-size) 1)
(check "at the biggest fonts the sidebar button fits its label"
       (let ([r (ui-find (window-ref s 'ui) 'reset-all)])
         (and (= 36 (theme 'ui-font-size))
              (<= (+ 12 (text-width "Reset to defaults" 'ui))
                  (ui-region-w r)))))
(check "title bar, top bar and taskbar grow to fit the biggest fonts"
       (and (> (title-height) (theme 'title-height))
            (> (topbar-height) (theme 'topbar-height))
            (> (taskbar-height) (theme 'taskbar-height))
            (>= (title-height) (font-line-height 'title))))
(let-values ([(cx cy cw ch) (content-rect s)])
  (inject! `(mouse-move ,(+ cx cw -20) ,(+ cy 100)) '(wheel 0 -1) '(wheel 0 -1))
  (steps 2)
  (check "the settings pane scrolls when its content doesn't fit"
         (and (> (settings-st-pane-scroll (window-ref s 'state)) 0)
              (ui-find (window-ref s 'ui) 'pane-scrollbar))))
(screenshot (string-append out-dir "/settings-fonts.png"))
(click-ui! s '(tab layout))
(scroll-settings-down!)
(drag-ui! s '(metric title-height) 0.5)
(check "chrome height sliders start at what the font needs"
       (and (> (theme 'title-height) (chrome-min-height 'title-height))
            (= (theme 'title-height) (title-height))))
(click-ui! s '(tab fonts))
;; 80 columns of 44 px would not fit this screen
(set-theme! 'mono-font-size 22)
(let ([r (repl)])
  (steps 1)
  (check "a new REPL opens 80 columns wide at any code size"
         (= 80 (repl-cols (window-ref r 'state))))
  (close-window! r))
(check "settings never crashed" (not (window-error s)))
(check "changes are saved shortly after"
       (wait-for (lambda () (and (not *theme-save-pending*)
                                 (file-exists? *theme-file*)))))
(let ([saved-accent (theme 'accent)])
  (reset-theme!)
  (check "reset restores defaults"
         (= (theme 'corner-radius) (theme-default 'corner-radius)))
  (load-saved-theme!)
  (check "saved theme loads back"
         (and (= (theme 'accent) saved-accent) (= 0 (theme 'corner-radius))
              (eq? (theme 'preset) 'nord)
              (equal? (theme 'ui-font) "serif"))))
(apply-theme-preset! 'paper)
(set-theme! 'title-height 30)
(set-theme! 'mono-font-size 14)
(set-theme! 'ui-font-size 13)
(set-theme! 'ui-font "sans-serif")
(repl)
(clock)
(move-window! (find-window 'clock) 900 300)
(click-ui! s '(tab theme))
(steps 2)
(screenshot (string-append out-dir "/paper.png"))
(focus-window! s)   ; the REPL above opened on top of it
(click-ui! s '(tab display))
(click-ui! s '(scale 200))
(check "the scale setting draws the same world twice as big"
       (and (= 200 (theme 'scale)) (= *scale* 2.0)
            (= (screen-w) 1280) (= (canvas-pixel-width) 2560)))
(steps 1)
(screenshot (string-append out-dir "/scale-200.png"))
(click-ui! s '(scale 0))
(check "and Auto follows the desktop again"
       (and (= 0 (theme 'scale)) (= *scale* 1.0) (= (canvas-pixel-width) 1280)))
(reset-theme!)
(close-window! s)

;; The desktop entry that gives GNOME and co. the app icon (into a scratch
;; folder).
(putenv "XDG_DATA_HOME" (string-append out-dir "/xdg"))
(let ([files (install-desktop-entry!)])
  (check "the desktop entry names the window class and the icon"
         (let ([text (call-with-input-file (car files) get-string-all)])
           (and (string-contains-ci? text "StartupWMClass=Schevana")
                (string-contains-ci? text "Icon=schevana")
                (file-exists? (cadr files)))))
  (uninstall-desktop-entry!)
  (check "and uninstalling removes it" (not (exists file-exists? files))))

;; Reloading the sources keeps the world's state.
(let ([n (length (windows))])
  (reload!)
  (check "reload asks for the whole screen" (eq? *dirty* #t))
  (steps 2)
  (check "reload keeps windows" (= n (length (windows))))
  (check "windows still draw after reload"
         (not (window-error (find-window 'clock)))))
(let ([s (settings)])
  (steps 2)
  (click-ui! s '(tab layout))
  (click-ui! s '(style interlisp))
  (check "settings still work after reload"
         (and (eq? (theme 'window-style) 'interlisp) (not (window-error s))))
  (set-theme! 'window-style 'mac)
  (close-window! s))

;;; Frames: whenever something asks for one, everything is drawn again.
(for-each close-window! (windows))
(set! *toasts* '())
(steps 2)

;; Whether one step drew a frame.
(define (step-drew?)
  (let ([n *frames-drawn*])
    (schevana-step! 0)
    (> *frames-drawn* n)))

;; Steps until the frame the clock forces once a second has just been
;; drawn, so that the next steps (well under a second of them) get none.
(define (after-clock-frame!)
  (let ([s (time-second (current-time))])
    (let loop ()
      (when (= s (time-second (current-time))) (schevana-step! 0) (loop)))
    (steps 1)))

;; thunk with status segments that never change, so the top bar stays put.
(define (with-quiet-status thunk)
  (let ([segs *status-segments*])
    (set! *status-segments* (list (cons 'quiet (lambda () "quiet"))))
    (steps 2)
    (let ([v (thunk)])
      (set! *status-segments* segs)
      (steps 1)
      v)))

;; A counter of the window's draws.
(define (count-draws! win)
  (let ([n 0]
        [draw (hashtable-ref (window-handlers win) 'draw
                             (lambda args (void)))])
    (window-on! win 'draw (lambda (w cw ch) (set! n (+ n 1)) (draw w cw ch)))
    (lambda () n)))

;; (open-window takes the first 'size)
(define (plain-window title x y . opts)
  (apply open-window title 'size (plist-ref opts 'size '(260 . 120))
         'pos (cons x y)
         'draw (lambda (win w h) (draw-text 14 14 title 'text 'ui))
         opts))

;; The device pixels the logical rect (x y w h) covers completely:
;; (x0 y0 x1 y1).
(define (device-box x y w h)
  (let ([s *scale*])
    (list (exact (ceiling (* s x))) (exact (ceiling (* s y)))
          (exact (floor (* s (+ x w)))) (exact (floor (* s (+ y h)))))))

;; How many pixels of the canvas inside the device box b are px.
(define (canvas-pixels b px)
  (cairo-surface-flush *canvas*)
  (let ([data (cairo-image-surface-get-data *canvas*)]
        [stride (cairo-image-surface-get-stride *canvas*)])
    (do ([y (cadr b) (+ y 1)]
         [n 0 (do ([x (car b) (+ x 1)]
                   [n n (if (= px (foreign-ref 'unsigned-32 data
                                               (+ (* y stride) (* 4 x))))
                            (+ n 1) n)])
                  ((= x (caddr b)) n))])
        ((= y (cadddr b)) n))))

;; Do two images of the same size hold the same pixels?
(define (same-image? a b)
  (cairo-surface-flush a)
  (cairo-surface-flush b)
  (let ([pa (cairo-image-surface-get-data a)]
        [pb (cairo-image-surface-get-data b)]
        [sa (cairo-image-surface-get-stride a)]
        [sb (cairo-image-surface-get-stride b)]
        [w (cairo-image-surface-get-width a)]
        [h (cairo-image-surface-get-height a)])
    (and (= w (cairo-image-surface-get-width b))
         (= h (cairo-image-surface-get-height b))
         (let loop ([y 0])
           (or (= y h)
               (and (zero? (sdl-memcmp (+ pa (* y sa)) (+ pb (* y sb))
                                       (* 4 w)))
                    (loop (+ y 1))))))))

;; Input asks for a frame by itself, so a handler that only changes its
;; state is seen.
(let* ([seen #f] [state 0]
       [bump (lambda (win . args) (set! state (+ state 1)))]
       [w (plain-window "Quiet" 100 100
                        'draw (lambda (win cw ch)
                                (set! seen state)
                                (draw-text 14 14 (number->string state)
                                           'text 'big))
                        'key bump 'text bump 'wheel bump 'mouse-up bump)])
  (steps 2)
  (let-values ([(cx cy cw ch) (content-rect w)])
    (define (shown-after kind . evs)
      (apply inject! evs)
      (steps 2)
      (cons kind (eqv? seen state)))
    (let* ([x (+ cx 50)] [y (+ cy 50)]
           [key (shown-after 'key '(key-down #\a ()))]
           [text (shown-after 'text '(text "x"))]
           [wheel (shown-after 'wheel `(mouse-move ,x ,y) '(wheel 0 1))]
           ;; (no 'mouse-down handler)
           [up (shown-after 'mouse-up `(mouse-down ,x ,y left 1)
                            `(mouse-up ,x ,y left))]
           [shown (list key text wheel up)])
      (check (format "input reaches windows that never invalidate ~s" shown)
             (and (= state 4) (for-all cdr shown)))))
  (close-window! w))

;; A window that changes by itself says so, and the next frame shows it
;; (the frame the clock asks for once a second is the top bar's).
(let* ([count 0]
       [w (open-window "Ticker" 'size '(260 . 120) 'pos '(400 . 100)
                       'tick (lambda (win)
                               (set! count (+ count 1))
                               (invalidate! win))
                       'draw (lambda (win cw ch)
                               (fill-rect 0 0 cw ch (if (even? count)
                                                        #xff0000
                                                        #x0000ff))))])
  (let-values ([(x y cw ch) (content-rect w)])
    (define (red?) (> (canvas-pixels (device-box x y cw ch) #xffff0000) 0))
    (steps 2)
    (let* ([a (red?)] [b (begin (steps 1) (red?))]
           [c (begin (steps 1) (red?))])
      (check (format "a window that invalidates itself as it ticks shows \
                      each tick on the next frame ~s" (list a b c))
             (and (not (eq? a b)) (not (eq? b c))))))
  (close-window! w))

;; Steps that nothing asked a frame of draw and upload nothing. The frame
;; the clock asks for once a second (the top bar's) calls every window's
;; draw once, clipped away, and uploads the rows it changed (the top
;; bar's clock); (invalidate!) draws every window once too, and when
;; nothing on screen changed it uploads nothing.
(let* ([a (plain-window "One" 100 100)] [b (plain-window "Two" 420 160)]
       [na (count-draws! a)] [nb (count-draws! b)] [row (* 4 *canvas-pw*)])
  (steps 2)
  (let ([a0 (na)] [b0 (nb)] [t0 (now-ms)])
    (let loop ([n 0] [forced 0] [ok #t])
      (if (< (- (now-ms) t0) 1100)
          (let* ([u *upload-bytes*] [d (step-drew?)] [up (- *upload-bytes* u)])
            (loop (+ n 1) (if d (+ forced 1) forced)
                  (and ok (= up (if d (* row *frame-rows*) 0)))))
          (check (format "idle steps draw and upload nothing; a forced frame \
                          draws each window once and uploads the rows it \
                          changed (~a steps, ~a forced)" n forced)
                 (and ok (> n 3) (<= 1 forced 2) (= (- (na) a0) forced)
                      (= (- (nb) b0) forced))))))
  (with-quiet-status
   (lambda ()
     (invalidate!)
     (steps 1)   ; the quiet top bar drawn
     (let ([a0 (na)] [b0 (nb)] [u0 *upload-bytes*])
       (invalidate!)
       (steps 1)
       (check "(invalidate!) draws every window again, and uploads nothing \
               when nothing on screen changed"
              (and (= (na) (+ a0 1)) (= (nb) (+ b0 1))
                   (= *upload-bytes* u0))))))
  (close-window! a)
  (close-window! b))

;; The host's texture: an expose only shows it again, and a lost one is
;; uploaded whole.
(with-quiet-status
 (lambda ()
   (let ([w (plain-window "Plain" 300 200)]
         [whole (* 4 *canvas-pw* *canvas-ph*)])
     (after-clock-frame!)
     (let ([u0 *upload-bytes*])
       (inject! '(expose))
       (steps 2)
       (let ([exposed (- *upload-bytes* u0)])
         (inject! '(render-reset))
         (steps 2)
         (let ([reset (- *upload-bytes* u0 exposed)])
           (inject! '(device-reset))
           (steps 2)
           (let ([device (- *upload-bytes* u0 exposed reset)]
                 [u1 *upload-bytes*])
             (invalidate! w 10 10 30 20)   ; part of w's content
             (steps 1)
             (check "an expose uploads nothing; a lost texture goes up whole; \
                     a frame that changes nothing uploads nothing"
                    (and (= exposed 0) (= reset whole) (= device whole)
                         (= (- *upload-bytes* u1) 0)))))))
     (close-window! w)
     (steps 1))))

;;; The host's SDL code, run against a stand-in for SDL: every SDL call
;;; src/ffi.ss binds is replaced (one with no stand-in raises), so nothing
;;; here reaches the real SDL or opens a window. The fake window is
;;; fake-units window units big with fake-ratio physical pixels per unit,
;;; on an X11 desktop at fake-dpi; its texture is an image that uploads
;;; copy rows into, as SDL_UpdateTexture does. *host* is 'sdl only around
;;; single host calls, so steps stay headless.

(define fake-sdl-names
  (call-with-input-file (string-append *schevana-root* "/src/ffi.ss")
    (lambda (p)
      (let loop ([acc '(x-resource-string)])
        (let ([x (read p)])
          (cond
            [(eof-object? x) acc]
            [(and (pair? x) (eq? (car x) 'define-foreign)
                  (string-prefix? "SDL_" (caddr x))
                  ;; plain memory: kept
                  (not (member (caddr x) '("SDL_memcmp" "SDL_memcpy"))))
             (loop (cons (cadr x) acc))]
            [else (loop acc)]))))))

(define fake-units '(1280 . 800))
(define fake-ratio 1)
(define fake-dpi 96)
(define fake-flags 0)
(define fake-queue '())        ; the window events SDL_PollEvent hands out next
(define fake-log '())          ; what was asked of the window, newest first
(define fake-texture #f)
(define fake-bytes 0)
(define fake-copies 0)
(define fake-presents 0)
(define fake-steps 0)
;; steps after which the texture was not the canvas
(define fake-mismatches 0)
;; steps that presented though nothing was uploaded or exposed, or the reverse
(define fake-bad-presents 0)
(define fake-unfaked '())      ; SDL calls made that have no stand-in

(define (fake-log! . entry) (set! fake-log (cons entry fake-log)))

(define (fake-sdl name)
  (define (pair! a b x y)
    (foreign-set! 'integer-32 a 0 x)
    (foreign-set! 'integer-32 b 0 y))
  (define (poll buf)
    (if (null? fake-queue)
        0
        (begin
          (foreign-set! 'unsigned-32 buf 0 SDL_WINDOWEVENT)
          (foreign-set! 'unsigned-8 buf 12 (car fake-queue))
          (set! fake-queue (cdr fake-queue))
          1)))
  (case name
    [(sdl-init) (lambda (flags) 0)]
    [(sdl-set-hint) (lambda (k v) #t)]
    [(sdl-start-text-input) (lambda () (void))]
    [(sdl-get-current-video-driver) (lambda () "x11")]
    [(x-resource-string)
     (lambda () (format "Xft.antialias:\t1\nXft.dpi:\t~a\n" fake-dpi))]
    [(sdl-create-window)
     (lambda (title x y w h flags)
       (set! fake-units (cons w h))
       (set! fake-flags flags)
       (fake-log! 'create w h
                  (if (fxzero? (fxand flags SDL_WINDOW_HIDDEN)) 'shown 'hidden))
       1)]
    [(sdl-create-renderer) (lambda (win index flags) 2)]
    [(sdl-set-window-size)
     (lambda (win w h) (set! fake-units (cons w h)) (fake-log! 'size w h))]
    [(sdl-set-window-position)
     (lambda (win x y)
       (fake-log! 'position
                  (if (= x y SDL_WINDOWPOS_CENTERED) 'centered (list x y))))]
    [(sdl-show-window) (lambda (win) (fake-log! 'show))]
    [(sdl-set-window-minimum-size) (lambda (win w h) (fake-log! 'minimum w h))]
    [(sdl-get-window-flags) (lambda (win) fake-flags)]
    [(sdl-get-window-size)
     (lambda (win w h) (pair! w h (car fake-units) (cdr fake-units)))]
    [(sdl-get-renderer-output-size)
     (lambda (r w h)
       (pair! w h (* fake-ratio (car fake-units))
              (* fake-ratio (cdr fake-units)))
       0)]
    [(sdl-poll-event) poll]
    [(sdl-wait-event-timeout) (lambda (buf ms) (poll buf))]
    [(sdl-create-texture)   ; its pixels are undefined: clear here
     (lambda (r format access w h)
       (when fake-texture (cairo-surface-destroy fake-texture))
       (set! fake-texture (cairo-image-surface-create CAIRO_FORMAT_ARGB32 w h))
       3)]
    [(sdl-destroy-texture) (lambda (t) (void))]
    [(sdl-update-texture)
     (lambda (t rect pixels pitch)
       (let ([td (cairo-image-surface-get-data fake-texture)]
             [ts (cairo-image-surface-get-stride fake-texture)]
             [tw (cairo-image-surface-get-width fake-texture)]
             [th (cairo-image-surface-get-height fake-texture)])
         (let-values ([(x y w h)
                       (if (= rect 0)
                           (values 0 0 tw th)
                           (apply values
                                  (map (lambda (i)
                                         (foreign-ref 'integer-32 rect i))
                                       '(0 4 8 12))))])
           (unless (and (= t 3) (<= 0 x) (<= 0 y) (< 0 w) (< 0 h)
                        (<= (+ x w) tw) (<= (+ y h) th))
             (errorf 'sdl-update-texture "bad upload ~s" (list t x y w h)))
           (do ([r 0 (+ r 1)]) ((= r h))
             (sdl-memcpy (+ td (* (+ y r) ts) (* 4 x)) (+ pixels (* r pitch))
                         (* 4 w)))
           (cairo-surface-mark-dirty fake-texture)
           (set! fake-bytes (+ fake-bytes (* 4 w h)))
           0)))]
    [(sdl-render-copy)
     (lambda (r t src dst)
       (unless (and (= t 3) (= src 0) (= dst 0))
         (errorf 'sdl-render-copy "not the whole texture"))
       (set! fake-copies (+ fake-copies 1))
       0)]
    [(sdl-render-present) (lambda (r) (set! fake-presents (+ fake-presents 1)))]
    [else (lambda args
            (set! fake-unfaked (cons name fake-unfaked))
            (errorf name "the fake SDL has no stand-in for this"))]))

;; Runs thunk with the fake SDL in place of the real one. Each step then
;; presents through the host's SDL branch, and after it the texture must
;; be the canvas, and it presented if and only if it uploaded or was exposed.
(define (with-fake-sdl thunk)
  (let ([saved (map top-level-value fake-sdl-names)]
        [present host-present-damage!])
    (dynamic-wind
      (lambda ()
        (for-each (lambda (n) (set-top-level-value! n (fake-sdl n)))
                  fake-sdl-names)
        (set! host-present-damage!
          (lambda ()
            (let ([b0 fake-bytes] [p0 fake-presents] [exposed? *present-only?*])
              (set! fake-steps (+ fake-steps 1))
              (unless (guard (c [#t (printf "  error: ~a\n"
                                            (condition->string c))
                                    #f])
                        (fluid-let ([*host* 'sdl]) (present))
                        (and (= (cairo-image-surface-get-width fake-texture)
                                *canvas-pw*)
                             (= (cairo-image-surface-get-height fake-texture)
                                *canvas-ph*)
                             (same-image? fake-texture *canvas*)))
                (set! fake-mismatches (+ fake-mismatches 1)))
              (unless (= (- fake-presents p0)
                         (if (or exposed? (> fake-bytes b0)) 1 0))
                (set! fake-bad-presents (+ fake-bad-presents 1)))))))
      thunk
      (lambda ()
        (for-each set-top-level-value! fake-sdl-names saved)
        (set! host-present-damage! present)))))

;; SDL reports a window event (through the host's real event loop); the
;; world handles what the host makes of it.
(define (fake-event! id)
  (set! fake-queue (list id))
  (apply inject! (fluid-let ([*host* 'sdl]) (host-events 0)))
  (steps 2))

(define (fake-resize! units ratio)
  (set! fake-units units)
  (set! fake-ratio ratio)
  (fake-event! SDL_WINDOWEVENT_SIZE_CHANGED))

;; A font's metrics measured afresh (no cache) at the current scale.
(define (fresh-metrics name)
  (let ([f (get-font name)])
    (font-metrics (make-font (font-family f) (font-slant f) (font-weight f)
                             (font-size f)
                             (cairo-toy-font-face-create (font-family f)
                                                         (font-slant f)
                                                         (font-weight f))
                             #f))))

(with-quiet-status
 (lambda ()
   (fluid-let ([*sdl-texture* 0] [*host-min-size* #f] [*pixel-ratio* 1]
               [*host-scale* *host-scale*])
     (with-fake-sdl
      (lambda ()
        (set! fake-log '())
        (fake-resize! '(1280 . 800) 1)   ; the first one makes the texture
        ;; uploads: a drag, a small invalidation, Life running
        (let ([w (plain-window "Uploaded" 300 200)] [l (life)])
          (move-window! l 640 260)
          (steps 2)
          ;; on w's title
          (inject! '(mouse-move 420 210) '(mouse-down 420 210 left 1))
          (do ([i 1 (+ i 1)]) ((> i 6))
            (inject! `(mouse-move ,(+ 420 (* 17 i)) ,(+ 210 (* 5 i))))
            (steps 1))
          (inject! '(mouse-up 522 240 left))
          (steps 1)
          (invalidate! w 10 10 30 20)
          (steps 1)
          (let ([t0 (now-ms)])
            (let loop () (when (< (- (now-ms) t0) 400) (steps 1) (loop))))
          (window-call l 'key #\space '())   ; paused
          (after-clock-frame!)
          (let ([p0 fake-presents] [b0 fake-bytes])
            (fake-event! SDL_WINDOWEVENT_EXPOSED)
            (let ([exposed (list (- fake-presents p0) (- fake-bytes b0))]
                  [p1 fake-presents]
                  [b1 fake-bytes])
              (steps 5)
              (let ([idle (list (- fake-presents p1) (- fake-bytes b1))])
                (check (format "the host's SDL upload leaves its texture equal \
                                to the screen after every step (~a of ~a)"
                               (- fake-steps fake-mismatches) fake-steps)
                       (and (= fake-mismatches 0) (> fake-steps 20)
                            (null? fake-unfaked)))
                (check (format "an expose presents once and uploads nothing \
                                (~s); idle steps present nothing (~s)"
                               exposed idle)
                       (and (equal? exposed '(1 0)) (equal? idle '(0 0))
                            (= fake-bad-presents 0)
                            (= fake-copies fake-presents))))))
          (close-window! w)
          (close-window! l)
          (steps 1))
        ;; fonts: the desktop's scale changes, as a resize event tells
        (let ([before (font-metrics 'mono)])
          ;; a window moved to a 2x display (macOS, Wayland)
          (fake-resize! '(1280 . 800) 2)
          (check (format "a scale change by a resize event measures the fonts \
                          again (mono ~s at 100%, ~s at 200%)"
                         before (font-metrics 'mono))
                 (and (= *scale* 2.0) (= *canvas-w* 1280) (= *canvas-h* 800)
                      (for-all (lambda (n)
                                 (equal? (font-metrics n) (fresh-metrics n)))
                               '(mono ui ui-bold title)))))
        (fake-resize! '(1280 . 800) 1)
        ;; the minimum size: plain resizes do not repeat it; in full screen SDL
        ;; only stores it, so it is passed on again after
        (do ([i 1 (+ i 1)]) ((> i 8))
          (fake-resize! (cons (+ 1280 (* 20 i)) (+ 800 (* 12 i))) 1))
        (set! fake-dpi 144)
        (set! fake-flags SDL_WINDOW_FULLSCREEN_DESKTOP)
        (fake-resize! '(1920 . 1200) 1)
        (set! fake-flags 0)
        (fake-resize! '(1300 . 820) 1)
        (fake-resize! '(1320 . 830) 1)
        (set! fake-dpi 96)
        (fake-resize! '(1280 . 800) 1)
        (let ([minimums
               (reverse
                (filter-map (lambda (e) (and (eq? (car e) 'minimum) (cdr e)))
                            fake-log))])
          (check (format "the minimum size goes to SDL only when it changes, \
                          and again after full screen ~s" minimums)
                 (and (equal? minimums
                              '((640 400) (960 600) (960 600) (640 400)))
                      (= *scale* 1.0) (= *canvas-w* 1280)
                      (= *canvas-h* 800)))))))))

;; The host window opens hidden, is sized in window units once the pixel
;; ratio is known (a 2x display here), and only then is shown.
(with-quiet-status
 (lambda ()
   (fluid-let ([*host* 'headless] [*sdl-window* 0] [*sdl-renderer* 0]
               [*sdl-texture* 0] [*host-fullscreen* #f] [*host-min-size* #f]
               [*pixel-ratio* 1] [*host-scale* *host-scale*])
     (with-fake-sdl
      (lambda ()
        (define (open scale . fullscreen?)
          (hashtable-set! *theme* 'scale scale)
          (set! fake-ratio 2)
          (set! fake-log '())
          (set! *sdl-texture* 0)   ; as in a new session
          (apply host-init! 'sdl 1280 800 fullscreen?)
          (set! *host* 'headless)   ; steps stay headless
          (list (reverse fake-log) *canvas-w* *canvas-h*))
        (let* ([explicit (open 200)] [auto (open 0)] [full (open 200 #t)])
          (hashtable-set! *theme* 'scale 0)
          (set! fake-flags 0)
          (set! *host-fullscreen* #f)
          (fake-resize! '(1280 . 800) 1)   ; the world as it was
          (check (format "the host window opens hidden, sized for the pixel \
                          ratio, then shown ~s" (list explicit auto full))
                 (and (equal? explicit '(((create 2560 1600 hidden)
                                          (size 1280 800) (position centered)
                                          (show) (minimum 640 400))
                                         1280 800))
                      (equal? auto '(((create 1280 800 hidden) (show)
                                      (minimum 640 400))
                                     1280 800))
                      (equal? (car full) '((create 2560 1600 hidden) (show)
                                           (minimum 640 400)))
                      (= *scale* 1.0) (= *canvas-w* 1280) (= *canvas-h* 800)
                      (null? fake-unfaked)))))))))


;; Settings: input and the full-screen switch reach it.
(let* ([s (settings)] [st (window-ref s 'state)])
  (steps 2)
  (click-ui! s '(tab colors))
  (click-ui! s 'hex)
  (let ([n (count-draws! s)])
    (steps 2)
    (let ([n0 (n)] [editing? (and (settings-st-hex st) #t)])
      (let-values ([(x y) (ui-point s '(color accent) 0.5)])
        (inject! `(mouse-move ,x ,y) `(mouse-down ,x ,y right 1)
                 `(mouse-up ,x ,y right)))
      (steps 2)
      (check
       "right-clicking Settings while editing a hex color closes the editor"
       (and editing? (not (settings-st-hex st)) (> (n) n0))))
    (click-ui! s '(tab display))
    (let ([n0 (n)])
      (press! 'f11)
      (steps 2)
      (check "F11 updates Settings' Full screen control"
             (and (fullscreen?) (> (n) n0)))
      (press! 'f11)
      (steps 2)))
  (close-window! s))

;; A drawing procedure redefined at the REPL draws the next frame; so does
;; the original, put back.
(let ([w (plain-window "Plain" 300 200)] [orig draw-window] [calls 0])
  (steps 2)
  (set! draw-window (lambda (win) (set! calls (+ calls 1)) (orig win)))
  (request-frame!)
  (steps 1)
  (let ([n calls])
    (set! draw-window orig)
    (request-frame!)
    (steps 1)
    (check (format "a redefined draw-window draws the next frame \
                    (~a windows), the one put back the frame after" n)
           (and (= n 1) (= calls 1))))
  (let ([orig draw-taskbar] [calls 0])
    (set! draw-taskbar (lambda ()
                         (orig)
                         (set! calls (+ calls 1))
                         (draw-text 700 (- (screen-h) 28) (number->string calls)
                                    'panel-text 'ui)))
    (do ([i 0 (+ i 1)]) ((= i 3)) (request-frame!) (steps 1))
    (let ([n calls])
      (set! draw-taskbar orig)
      (steps 1)
      (check (format
              "a redefined draw-taskbar draws every forced frame (~a of 3)" n)
             (= n 3))))
  (close-window! w))

;; Hover and press-and-hold on the buttons of the styles that draw them.
(for-each
 (lambda (style)
   (let ([w (plain-window "Buttons" 200 200 'window-style style)])
     (steps 2)
     (for-each (lambda (kind)
                 (let* ([b (assq kind (window-buttons w))]
                        [x (+ (window-x w) (list-ref b 1)
                              (quotient (list-ref b 3) 2))]
                        [y (+ (window-y w) (list-ref b 2)
                              (quotient (list-ref b 4) 2))])
                   (inject! `(mouse-move ,x ,y))
                   (steps 1)
                   (inject! `(mouse-down ,x ,y left 1))
                   (steps 1)
                   ;; held, then released elsewhere
                   (inject! `(mouse-move ,(+ x 300) ,(+ y 200)))
                   (steps 1)
                   (inject! `(mouse-up ,(+ x 300) ,(+ y 200) left))
                   (steps 1)))
               '(close minimize maximize))
     (check (format "~a buttons hover and press without acting" style)
            (and (memq w (windows)) (visible? w) (not (maximized? w))))
     (close-window! w)))
 '(mac windows))

(let* ([broken #t]
       [w (plain-window "Fragile" 300 200
                        'draw (lambda (win cw ch)
                                (when broken (error 'draw "broken on purpose"))
                                (draw-text 14 14 "fixed" 'text 'ui)))])
  (parameterize ([console-error-port (open-output-string)]) (steps 2))
  (let ([crashed? (and (window-error w) #t)])
    (set! broken #f)
    (let-values ([(cx cy cw ch) (content-rect w)])
      (inject! `(mouse-move ,(+ cx 50) ,(+ cy 50))
               `(mouse-down ,(+ cx 50) ,(+ cy 50) left 1)
               `(mouse-up ,(+ cx 50) ,(+ cy 50) left)))
    (steps 2)
    (check "a click on a crashed window retries it"
           (and crashed? (not (window-error w)))))
  (close-window! w)
  (set! *toasts* '())
  (steps 1))

;; Minimized from its taskbar button's window menu while another window
;; keeps the focus.
(let* ([a (plain-window "Aaa" 100 100)] [b (plain-window "Bbb" 500 100)])
  (steps 2)
  (let* ([it (find (lambda (it) (eq? (caddr it) a)) *taskbar-items*)]
         [x (exact (round (/ (+ (car it) (cadr it)) 2)))]
         [y (- (screen-h) (quotient (taskbar-height) 2))])
    (inject! `(mouse-move ,x ,y) `(mouse-down ,x ,y right 1)
             `(mouse-up ,x ,y right))
    (steps 2)
    (press! 'down)   ; Bind, Maximize, Minimize
    (press! 'down)
    (steps 1)
    (press! 'return)
    (steps 2)
    (check
     "a window minimized from its taskbar menu leaves the focus where it was"
     (and (not (visible? a)) (eq? *focus* b))))
  (for-each close-window! (list a b))
  (steps 1))

;; A status segment's change shows on the next frame.
(let ([v "probe-a"])
  (add-status-segment! 'probe (lambda () v))
  (steps 2)
  (set! v "probe-b")
  (request-frame!)
  (steps 1)
  (check (format "a status segment's change shows on the next frame ~s"
                 (shown-status-strings))
         (and (member "probe-b" (shown-status-strings)) #t))
  (remove-status-segment! 'probe)
  (steps 1))

;; A style whose drawing fails: nothing of what was under the window
;; stays on screen.
(define-window-style broken "Broken"
  'radius (lambda (w) 8)
  'draw-under (lambda (win x y w h focused? hover pressed)
                (error 'broken "broken on purpose")))
(with-quiet-status
 (lambda ()
   (let ([x (open-window "Red" 'size '(300 . 200) 'pos '(300 . 200)
                         'draw (lambda (win cw ch)
                                 (fill-rect 0 0 cw ch #xff0000)))])
     (steps 2)
     (minimize-window! x)
     (let ([y (open-window "Broken" 'size '(300 . 200) 'pos '(300 . 200)
                           'window-style 'broken)])
       (set-geometry! y (window-x x) (window-y x) (window-w x) (window-h x))
       (parameterize ([console-error-port (open-output-string)]) (steps 3))
       (check "a window whose style fails to draw hides what was under it"
              (= 0 (canvas-pixels (device-box (window-x y) (window-y y)
                                              (window-w y) (window-h y))
                                  #xffff0000)))
       (for-each close-window! (list x y))
       (set! *toasts* '())
       (steps 1)))))
(set! *window-styles*
  (remp (lambda (s) (eq? (window-style-name s) 'broken)) *window-styles*))

;; Life's cells take a theme change made behind set-theme!'s back.
(let ([l (life)] [green (theme 'green)] [cyan (theme 'cyan)])
  (window-call l 'key #\space '())   ; paused
  (move-window! l 100 60)
  (steps 2)
  (let*-values ([(cx cy cw ch) (content-rect l)]
                [(grid) (device-box cx cy cw (- ch (life-bar-height)))]
                [(old) (canvas-pixels grid (color->pixel green))])
    (hashtable-set! *theme* 'green #xff00ff)
    (hashtable-set! *theme* 'cyan #xff00ff)
    (request-frame!)
    (steps 2)
    (let ([stale (canvas-pixels grid (color->pixel green))]
          [new (canvas-pixels grid (color->pixel #xff00ff))])
      (hashtable-set! *theme* 'green green)
      (hashtable-set! *theme* 'cyan cyan)
      (steps 1)
      (check (format "a raw change to the theme recolors Life's cells on the \
                      next frame (~a, ~a stale)" new stale)
             (and (> old 0) (= stale 0) (> new 0)))))
  (close-window! l)
  (steps 1))

;;; Live changes: reloading and redefining while the world runs.

;; Reloading as a process (the launcher's "Reload sources", a REPL) holds
;; the world still until it ends, but stays a process. These reload a copy
;; of the tree whose apps/sketch.ss ends in form.
(define (tree-copy-with name form)
  (let ([dir (string-append out-dir "/" name)])
    (system (format "rm -rf '~a' && mkdir -p '~a' && cp -r~{ '~a'~} '~a'"
                    dir dir
                    (map (lambda (f) (string-append *schevana-root* "/" f))
                         (cons* "load.ss" "src" source-folders))
                    dir))
    (let ([p (open-output-file (string-append dir "/apps/sketch.ss") 'append)])
      (write form p)
      (newline p)
      (close-port p))
    dir))

;; A long load: until it ends no frame, timer, tick, handler or other
;; process runs, and events that come meanwhile are handled after it, in
;; order. The slow form notes what had run before and after its 0.6 s;
;; a process that comes after the reload in each round counts its slices
;; during it.
(define probe-counts (lambda () '()))
(define probe-seen '())
(let* ([root *schevana-root*] [ticks 0] [draws 0] [timers 0] [slices 0]
       [keys '()] [sent '()] [n 0] [done? #f] [late-held 0]
       [probe (open-window "Probe" 'size '(200 . 100)
                           'tick (lambda (win) (set! ticks (+ ticks 1)))
                           'draw (lambda (win w h) (set! draws (+ draws 1)))
                           'key (lambda (win key mods)
                                  (set! keys (cons key keys))))]
       [bystander (spawn "bystander"
                         (lambda ()
                           (let loop ()
                             (set! slices (+ slices 1))
                             (yield)
                             (loop))))])
  (set! probe-counts (lambda () (list ticks draws timers slices (length keys))))
  (let again ()
    (after 5 (lambda () (set! timers (+ timers 1)) (unless done? (again)))))
  (steps 2)
  (set! *schevana-root*
    (tree-copy-with "slow-tree"
      '(begin (set! probe-seen (list (probe-counts)))
              (let ([t (+ (real-time) 600)])
                (let loop () (when (< (real-time) t) (loop))))
              (set! probe-seen (cons (probe-counts) probe-seen)))))
  (let* ([p (spawn "reload" reload!)]
         [late (spawn "late"
                      (lambda ()
                        (let loop ()
                          (when *reloading* (set! late-held (+ late-held 1)))
                          (yield)
                          (loop))))])
    (let loop ()
      (when (and (memq p (procs)) (< n 5000))
        (let ([k (integer->char (+ 97 (mod n 26)))])
          (inject! (list 'key-down k '()))
          (set! sent (cons k sent)))
        (set! n (+ n 1))
        (schevana-step! 0)
        (loop)))
    (kill! late))
  (set! *schevana-root* root)
  (steps 3)
  (check (format "a long reload run as a process holds the world still (~a \
                  steps; ticks, draws, timers, other process, keys before and \
                  after: ~s; slices of a later process during it: ~a)"
                 n (reverse probe-seen) late-held)
         (and (> n 10) (= (length probe-seen) 2)
              (equal? (car probe-seen) (cadr probe-seen)) (= late-held 0)
              (> (length keys) 0) (equal? keys sent)
              (> ticks (car (car probe-seen)))))
  (set! done? #t)
  (kill! bystander)
  (close-window! probe)
  (set! *toasts* '())
  (steps 1))

;; A source whose top level loops: the world does not freeze, Ctrl+C kills
;; the reload, a toast names the file, and the world goes on.
(define reload-reached #f)
(let ([root *schevana-root*] [worst 0] [n 0] [ticks 0])
  (set! *schevana-root*
    (tree-copy-with "loop-tree"
      '(begin (set! reload-reached #t) (let loop () (loop)))))
  (let ([p (spawn "reload" reload!)]
        [w (open-window "Ticks" 'tick (lambda (win) (set! ticks (+ ticks 1))))])
    (define (step!)
      (let ([t0 (real-time)])
        (schevana-step! 0)
        (set! n (+ n 1))
        (set! worst (max worst (- (real-time) t0)))))
    (let loop () (unless (or reload-reached (> n 3000)) (step!) (loop)))
    (do ([i 0 (+ i 1)]) ((= i 20)) (step!))
    (let ([alive? (and (memq p (procs)) #t)] [t0 ticks])
      (press! #\c 'ctrl)
      (steps 3)
      (set! *schevana-root* root)
      (check (format "a reload whose source loops does not freeze the world \
                      (~a steps, the longest ~a ms), Ctrl+C kills it and a \
                      toast names the file"
                     n worst)
             (and reload-reached alive? (< worst 500) (not (memq p (procs)))
                  (not *reloading*) (> ticks t0)
                  (exists (lambda (t)
                            (string-contains-ci? (vector-ref t 1)
                                                 "apps/sketch.ss"))
                          *toasts*))))
    (close-window! w)
    (set! *toasts* '())
    (steps 1)))

;; A source that raises: the reload ends there and a toast names the file,
;; also when the process catches the error and carries on.
(let ([root *schevana-root*] [caught? #f])
  (set! *schevana-root*
    (tree-copy-with "error-tree" '(error 'smoke "on purpose")))
  (let ([p (spawn "reload"
                  (lambda ()
                    (guard (c [#t (set! caught? #t)]) (reload!))
                    (let loop () (yield) (loop))))])
    (let loop ([n 0])
      (unless (or caught? (> n 3000)) (schevana-step! 0) (loop (+ n 1))))
    (steps 2)
    (set! *schevana-root* root)
    (check "a reload whose source raises ends there, and a toast names the file"
           (and caught? (memq p (procs)) (not (reload-holding?))
                (exists (lambda (t)
                          (string-contains-ci? (vector-ref t 1)
                                               "failed in apps/sketch.ss"))
                        *toasts*)))
    (kill! p)
    (set! *toasts* '())
    (steps 1)))

;; The hold ends when the loading does, not when the process does: a
;; process that goes on after (reload!) lets the world run.
(let ([state 'unset] [ticks 0])
  (let ([p (spawn "reload+work"
                  (lambda ()
                    (reload!)
                    (set! state *reloading*)
                    (let loop () (yield) (loop))))]
        [w (open-window "Ticks" 'tick (lambda (win) (set! ticks (+ ticks 1))))])
    (let loop ([n 0])
      (when (and (eq? state 'unset) (< n 3000))
        (schevana-step! 0)
        (loop (+ n 1))))
    (let ([t0 ticks])
      (steps 3)
      (let ([alive? (and (memq p (procs)) #t)] [ran (- ticks t0)])
        (kill! p)
        (steps 2)
        (check (format "a process that goes on after (reload!) does not hold \
                        the world (*reloading* ~s, ~a ticks, toasts ~s)"
                       state ran (map (lambda (t) (vector-ref t 1)) *toasts*))
               (and (not state) alive? (> ran 0) (not *reloading*)
                    (exists (lambda (t)
                              (equal? (vector-ref t 1) "Sources reloaded"))
                            *toasts*)
                    (not (exists (lambda (t)
                                   (string-contains-ci? (vector-ref t 1)
                                                        "stopped"))
                                 *toasts*))))))
    (close-window! w)
    (set! *toasts* '())
    (steps 1)))

;; (reload!) in a private REPL reloads the world, not the REPL's copy.
(let ([p (private-repl)] [old render-frame!])
  (steps 2)
  (type! "(reload!)")
  (press! 'return)
  (let ([reloaded? (wait-for (lambda () (not (eq? render-frame! old))))])
    (wait-for (lambda () (not (repl-proc (repl-of p)))))
    ;; private: the world's styles must not see it
    (type! "(define (title-height) 99)")
    (press! 'return)
    (wait-for (lambda () (not (repl-proc (repl-of p)))))
    (let ([w (plain-window "Plain" 300 200)])
      (let-values ([(l t r b) (frame-insets w)])
        (check "(reload!) in a private REPL reloads the world, whose styles \
                keep the world's names"
               (and reloaded? (= t (title-height)) (not (= t 99)))))
      (close-window! w)))
  (close-window! p)
  (set! *toasts* '())
  (steps 1))

;; A draw-wallpaper that raises: its cairo context is let go, it is tried
;; once per change, and the old picture stays.
(let ([refs (foreign-procedure "cairo_surface_get_reference_count" (uptr)
                               unsigned-int)]
      [l (make-layer* 10 10)])
  (guard (c [#t #f])
    (draw-into-layer! l (lambda () (error 'smoke "on purpose"))))
  (check "draw-into-layer! lets go of its cairo context when the drawing raises"
         (= 1 (refs (layer-surface l))))
  (free-layer! l))
(let ([orig draw-wallpaper] [calls 0] [old (car *wallpaper*)])
  ;; an argument short
  (set! draw-wallpaper (lambda (w h)
                         (set! calls (+ calls 1))
                         (fill-gradient 0 0 w h #x102030)))
  (parameterize ([console-error-port (open-output-string)])
    (invalidate!)
    (steps 3)
    (invalidate!)
    (steps 3))
  (let ([kept? (eq? (car *wallpaper*) old)] [n calls])
    (set! draw-wallpaper orig)
    (invalidate!)
    (steps 1)
    (check (format "a draw-wallpaper that raises is tried once (~a) and keeps \
                    the old picture" n)
           (and (= n 1) kept? (not (eq? (car *wallpaper*) old)))))
  (set! *toasts* '())
  (steps 1))

(check "frame time is sane" (< *frame-ms* 200))
(printf "frame: ~,1f ms, heap: ~,1f MB\n" *frame-ms*
        (/ (bytes-allocated) 1048576.0))
(printf "~a\n" (if (zero? failures)
                   "all checks passed"
                   (format "~a check(s) FAILED" failures)))
(exit (if (zero? failures) 0 1))
