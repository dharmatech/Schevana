;;; wm.ss — the window manager.
;;;
;;; A window is a record with geometry, a table of handlers and a table of
;;; properties. Apps are procedures that call open-window:
;;;
;;;   (open-window "Hello"
;;;     'size '(300 . 120)                        ; content size
;;;     'draw (lambda (win w h) (draw-text 20 20 "Hi!" 'text 'big))
;;;     'key  (lambda (win key mods) ...))
;;;
;;; Options other than handlers (and size, min-size, pos, app) become
;;; window properties: (window-ref win 'bg), (window-ref win 'cursor).
;;;
;;; Handlers: open draw tick key text mouse-down mouse-up mouse-move wheel
;;;           resize focus blur close.
;;;
;;; The frame around the content (title bar, borders, buttons) belongs to
;;; the window style, see styles.ss. Geometry here is the outer frame;
;;; 'size and the w h passed to handlers are always the content size.
;;; Every handler runs under safe-call: errors and runaway loops crash only
;;; that window, which then shows the error and can be retried.

(define-record-type window
  (nongenerative schevana-window)
  (fields id app
          (mutable title) (mutable x) (mutable y) (mutable w) (mutable h)
          (mutable state)       ; normal | minimized | maximized
          (mutable restore)     ; (x y w h) before maximizing
          handlers              ; eq-hashtable: kind -> procedure
          props                 ; eq-hashtable: app-defined state
          (mutable error)       ; (handler-kind . condition) once crashed
          (mutable min-w) (mutable min-h)))

(record-writer (record-type-descriptor window)
  (lambda (w p wr)
    (fprintf p "#<window ~a ~s ~ax~a+~a+~a~a>" (window-id w) (window-title w)
             (window-w w) (window-h w) (window-x w) (window-y w)
             (if (eq? (window-state w) 'normal) "" (format " ~a" (window-state w))))))

(define handler-kinds
  '(open draw tick key text mouse-down mouse-up mouse-move wheel resize focus blur close))

(defvar *windows* '())        ; z-order, topmost first
(defvar *focus* #f)
(defvar *next-window-id* 1)
(defvar *dirty* #t)
(defvar *drag* #f)            ; the current mouse interaction, see drag record
(defvar *hover* #f)
(defvar *mouse-x* 0)
(defvar *mouse-y* 0)
(defvar *cascade* 0)

(define-record-type drag
  (nongenerative schevana-drag)
  (fields kind win button mx my x y w h))

;;; ------------------------------------------------------------------ invalidation
;;; *dirty* asks for a forced frame: everything on screen is drawn again
;;; from scratch (render-full-into!), so every change only has to say that
;;; something changed.
;;;
;;;   (invalidate!)       anything may have changed
;;;   (invalidate! win)   win's content changed (input to a window says so
;;;                       by itself)
;;;   (invalidate! win x y w h)   the same as (invalidate! win), for apps
;;;                       written when only that part was drawn again

(defvar current-window (make-thread-parameter #f))   ; the window whose handler is running

(define invalidate!
  (case-lambda
    [() (set! *dirty* #t)]
    [(win) (set! *dirty* #t)]
    [(win x y w h) (invalidate! win)]))

;; The window manager's or the shell's state changed.
(define (request-frame!) (set! *dirty* #t))

(define (screen-w) *canvas-w*)
(define (screen-h) *canvas-h*)
;; The chrome grows with the fonts: each height is the theme's setting or
;; the room its text needs, whichever is more. At the default fonts the
;; text fits every setting the Settings app offers.
(define (chrome-min-height key)
  (exact (round (case key
                  [(title-height) (+ (font-line-height 'title) 7)]
                  [(topbar-height) (+ (font-line-height 'ui) 7)]
                  [(taskbar-height) (+ (font-line-height 'ui) 13)]
                  [else 0]))))
(define (title-height) (max (theme 'title-height) (chrome-min-height 'title-height)))
(define (topbar-height) (max (theme 'topbar-height) (chrome-min-height 'topbar-height)))
(define (taskbar-height) (max (theme 'taskbar-height) (chrome-min-height 'taskbar-height)))

;; The area between the top bar and the taskbar.
(define (desktop-rect)
  (let ([top (topbar-height)] [bottom (taskbar-height)])
    (values 0 top (screen-w) (- (screen-h) top bottom))))

(define (point-in? px py x y w h)
  (and (>= px x) (< px (+ x w)) (>= py y) (< py (+ y h))))

;;; ------------------------------------------------------------------ apps

(define-record-type app
  (nongenerative schevana-app)
  (fields name title launch))

(defvar *apps* '())
(defvar current-app (make-parameter #f))

(define (register-app! name title launch)
  (set! *apps* (append (remp (lambda (a) (eq? (app-name a) name)) *apps*)
                       (list (make-app name title launch))))
  (request-frame!))

;; (define-app (name) "Title" body ...) defines an ordinary procedure that
;; opens the app, and registers it with the launcher.
(define-syntax define-app
  (syntax-rules ()
    [(_ (name . args) title body ...)
     (begin
       (define (name . args) (parameterize ([current-app 'name]) body ...))
       (register-app! 'name title (lambda () (name))))]))

(define (launch-app name)
  (let ([a (find (lambda (a) (eq? (app-name a) name)) *apps*)])
    (if a ((app-launch a)) (errorf 'launch-app "no app named ~s" name))))

(define (apps) (map app-name *apps*))

(define app-colors '(accent accent-2 green orange cyan red yellow teal magenta))

(define (app-color name)
  (let ([s (symbol->string (or name 'unknown))])
    (list-ref app-colors
              (mod (fold-left (lambda (h c) (+ (* h 31) (char->integer c))) 7 (string->list s))
                   (length app-colors)))))

;;; ------------------------------------------------------------------ windows

(define (open-window title . opts)
  (let* ([whole (lambda (p) (cons (exact (round (car p))) (exact (round (cdr p)))))]
         [size (whole (plist-ref opts 'size '(480 . 320)))]
         [min-size (whole (plist-ref opts 'min-size '(160 . 80)))]
         [win (make-window *next-window-id* (plist-ref opts 'app (current-app)) title
                           0 0 (car size) (cdr size) 'normal #f
                           (make-eq-hashtable) (make-eq-hashtable) #f
                           (car min-size) (cdr min-size))])
    (set! *next-window-id* (+ *next-window-id* 1))
    ;; Handler options become handlers; any other option becomes a property.
    (let loop ([o opts])
      (when (and (pair? o) (pair? (cdr o)))
        (cond
          [(memq (car o) handler-kinds) (hashtable-set! (window-handlers win) (car o) (cadr o))]
          [(memq (car o) '(size min-size pos app)) (void)]
          [else (hashtable-set! (window-props win) (car o) (cadr o))])
        (loop (cddr o))))
    ;; The frame depends on the style, which may come from the options.
    (let-values ([(l t r b) (frame-insets win)])
      (window-w-set! win (+ l r (car size)))
      (window-h-set! win (+ t b (cdr size))))
    (place-window! win (plist-ref opts 'pos #f))
    (set! *windows* (cons win *windows*))
    (window-call win 'open)
    (focus-window! win)
    win))

;; Where a window opens when open-window gets no 'pos: #f to cascade, or a
;; point for its top-left corner, moved as little as needed to fit on the
;; desktop. (parameterize ([new-window-position '(400 . 300)]) (clock))
(defvar new-window-position (make-parameter #f))

(define (place-window! win pos)
  (let-values ([(dx dy dw dh) (desktop-rect)])
    (window-w-set! win (min (window-w win) dw))
    (window-h-set! win (min (window-h win) dh))
    (cond
      [pos (window-x-set! win (car pos)) (window-y-set! win (cdr pos))]
      [(new-window-position)
       => (lambda (at)
            (window-x-set! win (clamp (car at) dx (max dx (- (+ dx dw) (window-w win)))))
            (window-y-set! win (clamp (cdr at) dy (max dy (- (+ dy dh) (window-h win))))))]
      [else
        (let ([k (mod *cascade* 8)])
          (set! *cascade* (+ *cascade* 1))
          (window-x-set! win (min (+ 60 (* 34 k)) (max 0 (- dw (window-w win)))))
          (window-y-set! win (min (+ dy 24 (* 30 k)) (max dy (- (+ dy dh) (window-h win))))))])))

(define (windows) *windows*)
(define (focused-window) *focus*)
(define (visible? win) (not (eq? (window-state win) 'minimized)))
(define (maximized? win) (eq? (window-state win) 'maximized))

(define (find-window x)
  (find (lambda (w)
          (cond [(number? x) (= (window-id w) x)]
                [(string? x) (string-contains-ci? (window-title w) x)]
                [(symbol? x) (eq? (window-app w) x)]
                [else (eq? w x)]))
        *windows*))

(define (window-ref win key . default)
  (hashtable-ref (window-props win) key (if (null? default) #f (car default))))

(define (window-set! win key val)
  (hashtable-set! (window-props win) key val)
  (invalidate! win))

(define (window-on! win kind proc)
  (hashtable-set! (window-handlers win) kind proc)
  (window-error-set! win #f)
  (invalidate! win))

(define (window-bg win) (window-ref win 'bg 'window-bg))

;; The window's style: its own 'window-style property, else the theme's.
(define (window-style-of win)
  (or (find-window-style (or (window-ref win 'window-style) (theme 'window-style)))
      (find-window-style 'mac)))

;; Frame thickness around the content: (values left top right bottom).
(define (frame-insets win)
  ((window-style-insets (window-style-of win)) win))

;; Content area in screen coordinates.
(define (content-rect win)
  (let-values ([(l t r b) (frame-insets win)])
    (values (+ (window-x win) l) (+ (window-y win) t)
            (- (window-w win) l r) (- (window-h win) t b))))

(define (window-content-size win)
  (let-values ([(x y w h) (content-rect win)]) (values w h)))

(define (window-local win x y)
  (let-values ([(l t r b) (frame-insets win)])
    (values (- x (window-x win) l) (- y (window-y win) t))))

;; Calls one of the window's handlers under the watchdog, with
;; (current-window) set to win. Input invalidates the window by itself, so
;; a handler that only changes its state is still seen.
(define (window-call win kind . args)
  (let ([h (hashtable-ref (window-handlers win) kind #f)])
    (if (and h (not (window-error win)))
        (begin
          (when (memq kind '(key text mouse-down mouse-up wheel)) (invalidate! win))
          (safe-call kind (lambda () (parameterize ([current-window win]) (apply h win args)))
                     (lambda (c) (window-crash! win kind c) #f)))
        #f)))

(define (window-crash! win kind c)
  (window-error-set! win (cons kind c))
  (invalidate! win))

(define (focus-window! win)
  (unless (eq? win *focus*)
    (let ([old *focus*])
      (set! *focus* win)
      (when (and old (memq old *windows*)) (window-call old 'blur))
      (when win (window-call win 'focus))))
  (when win
    (when (eq? (window-state win) 'minimized)
      (window-state-set! win (if (window-restore win) 'maximized 'normal)))
    (raise-window! win))
  (request-frame!))

(define (raise-window! win)
  (set! *windows* (cons win (remq win *windows*)))
  (request-frame!))

(define (focus-topmost!)
  (let ([w (find visible? *windows*)])
    (if w
        (focus-window! w)
        (begin
          (when (and *focus* (memq *focus* *windows*)) (window-call *focus* 'blur))
          (set! *focus* #f)))))

(define (close-window! win)
  (when (memq win *windows*)
    (window-call win 'close)
    (set! *windows* (remq win *windows*))
    (when (and *drag* (eq? (drag-win *drag*) win)) (set! *drag* #f))
    (when (eq? *focus* win) (focus-topmost!))
    (request-frame!)))

(define (minimize-window! win)
  (window-state-set! win 'minimized)
  (when (eq? win *focus*) (focus-topmost!))
  (request-frame!))

;; Sets the outer geometry, enforcing minimum sizes, and tells the app.
(define (set-geometry! win x y w h)
  (let*-values ([(l t r b) (frame-insets win)]
                [(w) (max (+ l r (window-min-w win)) (exact (round w)))]
                [(h) (max (+ t b (window-min-h win)) (exact (round h)))]
                [(resized?) (not (and (= w (window-w win)) (= h (window-h win))))])
    (window-x-set! win (exact (round x)))
    (window-y-set! win (exact (round y)))
    (window-w-set! win w)
    (window-h-set! win h)
    (when resized? (window-call win 'resize (- w l r) (- h t b)))
    (request-frame!)))

(define (move-window! win x y)
  (set-geometry! win x y (window-w win) (window-h win)))

;; Resizes the content area to w x h.
(define (resize-window! win w h)
  (let-values ([(l t r b) (frame-insets win)])
    (set-geometry! win (window-x win) (window-y win) (+ w l r) (+ h t b))))

;; What a 'depth button does (the Amiga's depth gadget): send the front
;; window to the back, or bring a window that is behind others to the front.
(define (depth-window! win)
  (if (eq? win (find visible? *windows*))
      (begin
        (set! *windows* (append (remq win *windows*) (list win)))
        (focus-topmost!))
      (focus-window! win))
  (request-frame!))

(define (maximize-window! win)
  (unless (eq? (window-state win) 'maximized)
    (window-restore-set! win (list (window-x win) (window-y win) (window-w win) (window-h win)))
    (window-state-set! win 'maximized)
    (fit-maximized! win)))

(define (fit-maximized! win)
  (let-values ([(x y w h) (desktop-rect)])
    (set-geometry! win x y w h)))

(define (restore-window! win)
  (let ([r (window-restore win)])
    (window-state-set! win 'normal)
    (window-restore-set! win #f)
    (when r (apply set-geometry! win r))
    (request-frame!)))

(define (toggle-maximize! win)
  (if (eq? (window-state win) 'maximized) (restore-window! win) (maximize-window! win)))

(define (cycle-windows!)
  (let ([ws (reverse *windows*)])
    (unless (null? ws)
      (focus-window! (car ws)))))

(define (close-focused-window!) (when *focus* (close-window! *focus*)))

(define (tile-windows!)
  (let ([ws (reverse (filter visible? *windows*))])
    (unless (null? ws)
      (let-values ([(dx dy dw dh) (desktop-rect)])
        (let* ([n (length ws)]
               [cols (exact (ceiling (sqrt n)))]
               [rows (exact (ceiling (/ n cols)))]
               [gap 8]
               [cw (/ (- dw gap) cols)]
               [ch (/ (- dh gap) rows)])
          (let loop ([ws ws] [i 0])
            (unless (null? ws)
              (let ([win (car ws)])
                (window-state-set! win 'normal)
                (window-restore-set! win #f)
                (set-geometry! win (+ dx gap (* cw (mod i cols))) (+ dy gap (* ch (quotient i cols)))
                               (- cw gap) (- ch gap))
                (loop (cdr ws) (+ i 1))))))))))

(define (cascade-windows!)
  (set! *cascade* 0)
  (for-each (lambda (win)
              (when (eq? (window-state win) 'maximized) (restore-window! win))
              (place-window! win #f))
            (reverse *windows*))
  (request-frame!))

;; The host window was resized.
;; The screen changed size (or scale): refit maximized windows, and pull
;; others back until their title bar can be reached.
(define (wm-screen-resized!)
  (unless (null? *windows*)   ; (also called while loading, before there is a canvas to measure)
    (let-values ([(dx dy dw dh) (desktop-rect)])
      (for-each (lambda (w)
                  (if (eq? (window-state w) 'maximized)
                      (fit-maximized! w)
                      (begin
                        (window-x-set! w (clamp (window-x w) (- (+ dx 80) (window-w w)) (max dx (- (+ dx dw) 80))))
                        (window-y-set! w (clamp (window-y w) dy (max dy (- (+ dy dh) 30)))))))
                *windows*)))
  (invalidate!))

;; The window style or title height changed: frames keep their outer size,
;; so tell every app its new content size.
(define (wm-chrome-changed!)
  (wm-screen-resized!)
  (for-each (lambda (w)
              (let-values ([(cw ch) (window-content-size w)])
                (window-call w 'resize cw ch)))
            *windows*))

;;; ------------------------------------------------------------------ hit testing

(define (window-at x y)
  (find (lambda (w)
          (and (visible? w)
               (point-in? x y (window-x w) (window-y w) (window-w w) (window-h w))))
        *windows*))

;; Button kinds that act on release. A style may also list a 'resize-br
;; button (a classic size gadget), which starts a resize instead.
(define window-button-kinds '(close minimize maximize depth))

;; The style's buttons: ((kind x y w h) ...) in window coordinates.
(define (window-buttons win)
  ((window-style-buttons (window-style-of win)) win))

(define (window-button-at win lx ly)
  (let ([b (find (lambda (b) (point-in? lx ly (list-ref b 1) (list-ref b 2) (list-ref b 3) (list-ref b 4)))
                 (window-buttons win))])
    (and b (car b))))

;; Which part of the window is at screen point (x, y)?
(define (window-region win x y)
  (let*-values ([(lx) (- x (window-x win))] [(ly) (- y (window-y win))]
                [(w) (window-w win)] [(h) (window-h win)]
                [(l t r b) (frame-insets win)]
                [(edge corner) (values 5 14)]
                [(resizable?) (not (eq? (window-state win) 'maximized))])
    (cond
      [(window-button-at win lx ly)]
      [(and resizable? (>= ly (- h corner)) (>= lx (- w corner))) 'resize-br]
      [(and resizable? (>= ly (- h corner)) (< lx corner)) 'resize-bl]
      [(and resizable? (>= lx (- w edge)) (> ly edge)) 'resize-r]
      [(and resizable? (< lx edge) (> ly t)) 'resize-l]
      [(and resizable? (>= ly (- h edge))) 'resize-b]
      [(< ly t) 'title]
      [(and (>= lx l) (< lx (- w r)) (< ly (- h b))) 'content]
      [else 'frame])))

(define (region-cursor win region)
  (case region
    [(resize-br) 'resize-nwse]
    [(resize-bl) 'resize-nesw]
    [(resize-r resize-l) 'resize-ew]
    [(resize-b) 'resize-ns]
    [(content) (if (window-error win) 'hand (window-ref win 'cursor 'arrow))]
    [else 'arrow]))

;;; ------------------------------------------------------------------ mouse

(define (update-hover! x y)
  (let ([h (cond
             [(menu-open?) '(menu)]
             [(< y (topbar-height)) (list 'topbar (topbar-item-at x y))]
             [(>= y (- (screen-h) (taskbar-height))) (list 'taskbar (taskbar-item-at x y))]
             [(window-at x y) => (lambda (w) (list 'window w (window-region w x y)))]
             [else '(desktop)])])
    (unless (equal? h *hover*)
      (set! *hover* h)
      (request-frame!))
    (host-set-cursor!
     (if (eq? (car h) 'window) (region-cursor (cadr h) (caddr h)) 'arrow))))

(define (wm-mouse-move x y)
  (set! *mouse-x* x)
  (set! *mouse-y* y)
  (let ([d *drag*])
    (cond
      [(not d)
       (update-hover! x y)
       (cond
         [(menu-open?) (menu-mouse-move x y)]
         [(and (eq? (car *hover*) 'window) (eq? (caddr *hover*) 'content))
          (let ([win (cadr *hover*)])
            (let-values ([(lx ly) (window-local win x y)])
              (window-call win 'mouse-move lx ly)))])]
      [(eq? (drag-kind d) 'move) (drag-move! d x y)]
      [(eq? (drag-kind d) 'capture)
       (let ([win (drag-win d)])
         (let-values ([(lx ly) (window-local win x y)])
           (window-call win 'mouse-move lx ly)))]
      [(memq (drag-kind d) '(resize-r resize-l resize-b resize-br resize-bl)) (drag-resize! d x y)]
      [else (update-hover! x y)])))

(define (drag-move! d x y)
  (let ([win (drag-win d)])
    (if (eq? (window-state win) 'maximized)
        ;; Tearing a maximized window off: restore it under the cursor.
        (when (> (+ (abs (- x (drag-mx d))) (abs (- y (drag-my d)))) 6)
          (let* ([frac (/ (- (drag-mx d) (window-x win)) (max 1 (window-w win)))])
            (restore-window! win)
            (let-values ([(l t r b) (frame-insets win)])
             (let ([nx (- x (* frac (window-w win)))] [ny (- y (/ t 2))])
              (move-window! win nx ny)
              (set! *drag* (make-drag 'move win 'left x y (window-x win) (window-y win) 0 0))))))
        (let-values ([(dx dy dw dh) (desktop-rect)])
          (move-window! win
                        (clamp (+ (drag-x d) (- x (drag-mx d))) (- 80 (window-w win)) (- dw 80))
                        (clamp (+ (drag-y d) (- y (drag-my d))) dy (- (+ dy dh) 20)))))))

(define (drag-resize! d x y)
  (let* ([k (drag-kind d)] [win (drag-win d)]
         [dx (- x (drag-mx d))] [dy (- y (drag-my d))]
         [min-w (let-values ([(l t r b) (frame-insets win)]) (+ l r (window-min-w win)))]
         [nw (case k
               [(resize-r resize-br) (+ (drag-w d) dx)]
               [(resize-l resize-bl) (- (drag-w d) dx)]
               [else (drag-w d)])]
         [nw (max nw min-w)]
         [nh (if (memq k '(resize-b resize-br resize-bl)) (+ (drag-h d) dy) (drag-h d))]
         [nx (if (memq k '(resize-l resize-bl)) (- (+ (drag-x d) (drag-w d)) nw) (drag-x d))])
    (set-geometry! win nx (drag-y d) nw nh)))

(define (wm-mouse-down x y button clicks)
  (set! *mouse-x* x)
  (set! *mouse-y* y)
  (cond
    [(menu-open?)
     (if (point-in-menu? x y) (menu-mouse-down x y) (close-menu!))]
    [(< y (topbar-height)) (topbar-click x y button)]
    [(>= y (- (screen-h) (taskbar-height))) (taskbar-click x y button)]
    [(window-at x y) => (lambda (win) (window-mouse-down win x y button clicks))]
    [else (desktop-click x y button)]))

(define (window-mouse-down win x y button clicks)
  (let ([region (window-region win x y)])
    ;; The depth gadget decides about raising on release.
    (unless (eq? region 'depth) (focus-window! win))
    (case region
      [(close minimize maximize depth)
       (when (eq? button 'left) (set! *drag* (make-drag region win button x y 0 0 0 0)))]
      [(frame) (void)]
      [(title)
       (cond
         [(eq? button 'right) (open-window-menu! win x y)]
         [(and (eq? button 'left) (>= clicks 2)) (toggle-maximize! win)]
         [(eq? button 'left)
          (set! *drag* (make-drag 'move win button x y (window-x win) (window-y win) 0 0))
          (host-set-cursor! 'move)])]
      [(content)
       (cond
         [(window-error win)
          (when (eq? button 'left) (window-error-set! win #f) (invalidate! win))]
         [else
          (set! *drag* (make-drag 'capture win button x y 0 0 0 0))
          (let-values ([(lx ly) (window-local win x y)])
            (window-call win 'mouse-down lx ly button clicks))])]
      [else
       (when (eq? button 'left)
         (set! *drag* (make-drag region win button x y (window-x win) (window-y win)
                                 (window-w win) (window-h win))))])))

(define (wm-mouse-up x y button)
  (let ([d *drag*])
    (when d
      (let ([win (drag-win d)] [kind (drag-kind d)])
        (case kind
          [(close minimize maximize depth)
           (set! *drag* #f)
           (when (and (memq win *windows*)
                      (eq? (window-at x y) win)
                      (eq? (window-region win x y) kind))
             (case kind
               [(close) (close-window! win)]
               [(minimize) (minimize-window! win)]
               [(maximize) (toggle-maximize! win)]
               [(depth) (depth-window! win)]))]
          [(capture)
           (when (eq? button (drag-button d)) (set! *drag* #f))
           (let-values ([(lx ly) (window-local win x y)])
             (window-call win 'mouse-up lx ly button))]
          [else (set! *drag* #f)])))
    (update-hover! x y)
    (request-frame!)))

(define (wm-wheel dx dy)
  (cond
    [(menu-open?) (void)]
    [(window-at *mouse-x* *mouse-y*)
     => (lambda (win)
          (when (eq? (window-region win *mouse-x* *mouse-y*) 'content)
            (window-call win 'wheel dx dy)))]))

;;; ------------------------------------------------------------------ keyboard

(defvar *global-keys* '())

;; (bind-key! '(ctrl shift #\w) close-focused-window!)
(define (bind-key! spec proc)
  (set! *global-keys*
    (cons (cons spec proc) (remp (lambda (b) (equal? (car b) spec)) *global-keys*))))

(define (unbind-key! spec)
  (set! *global-keys* (remp (lambda (b) (equal? (car b) spec)) *global-keys*)))

(define (key-matches? spec key mods)
  (let ([rev (reverse spec)])
    (and (equal? (car rev) key)
         (let ([want (cdr rev)])
           (and (= (length want) (length mods))
                (for-all (lambda (m) (memq m mods)) want))))))

(define (wm-key-down key mods)
  (cond
    [(memq key '(ctrl shift alt super)) (void)]
    [(find (lambda (b) (key-matches? (car b) key mods)) *global-keys*)
     => (lambda (b) (safe-call 'key-binding (cdr b) (lambda (c) (report-error! 'key-binding c))))]
    [(menu-open?) (menu-key key mods)]
    [*focus* (window-call *focus* 'key key mods)]))

(define (wm-text str)
  (cond
    [(menu-open?) (menu-text str)]
    [*focus* (window-call *focus* 'text str)]))

;;; ------------------------------------------------------------------ tick

(defvar *last-second* -1)

(define (wm-tick!)
  (for-each (lambda (w) (window-call w 'tick)) *windows*)
  (let ([s (time-second (current-time))])
    (unless (= s *last-second*)   ; a forced frame at least once a second
      (set! *last-second* s)
      (request-frame!)))
  (expire-toasts!))

;;; ------------------------------------------------------------------ drawing

;; The style draws the frame in two passes around the content.
(define (draw-window win) (draw-window* win (eq? win *focus*)))

;; The part of win under the mouse, and the drag kind holding it, as the
;; style sees them (#f when none).
(define (window-hover win)
  (and *hover* (eq? (car *hover*) 'window) (eq? (cadr *hover*) win) (caddr *hover*)))

(define (window-pressed win)
  (and *drag* (eq? (drag-win *drag*) win) (drag-kind *drag*)))

(define (draw-window* win focused?)
  (let* ([style (window-style-of win)]
         [x (window-x win)] [y (window-y win)] [w (window-w win)] [h (window-h win)]
         [hover (window-hover win)]
         [pressed (window-pressed win)]
         [r ((window-style-radius style) win)])
    ((window-style-draw-under style) win x y w h focused? hover pressed)
    (let-values ([(cx cy cw ch) (content-rect win)])
      (call-with-round-clip x y w h r
        (lambda ()
          (call-with-clip cx cy cw ch
            (lambda ()
              (call-with-translation cx cy
                (lambda ()
                  (fill-rect 0 0 cw ch (window-bg win))
                  (if (window-error win)
                      (draw-crash-panel win cw ch)
                      (window-call win 'draw cw ch)))))))))
    ((window-style-draw-over style) win x y w h focused? hover pressed)))

(define (draw-crash-panel win w h)
  (let ([err (window-error win)])
    (fill-rect 0 0 w h (mix 'bg-1 'red 0.10))
    (let* ([y1 (round (+ 14 (font-line-height 'ui-bold) 5))]
           [y2 (round (+ y1 (font-line-height 'ui) 9))]
           [hint-h (round (+ (font-line-height 'ui-small) 13))]
           [hint? (> h (+ y2 hint-h 66))])
      (draw-text 16 14 "This app crashed" 'red 'ui-bold)
      (draw-text 16 y1 (format "in its '~a handler:" (car err)) 'text-dim 'ui)
      (draw-wrapped-text 16 y2 (- w 32) (- h y2 (if hint? (+ hint-h 4) 4)) (condition->string (cdr err)) 'text 'mono-small)
      (when hint?
        (draw-text 16 (- h hint-h) "Click to retry.  Fix it live: (window-on! win 'draw ...)" 'text-faint 'ui-small)))))

;; Character-wrapped text for monospace fonts; returns the y after the text.
(define (draw-wrapped-text x y w h str color font)
  (let* ([cw (font-char-width font)] [lh (+ 2 (font-line-height font))]
         [cols (max 1 (exact (floor (/ w cw))))]
         [bottom (+ y h)])
    (let loop ([rows (wrap-lines str cols)] [y y])
      (if (or (null? rows) (> (+ y lh) bottom))
          y
          (begin
            (draw-text x y (car rows) color font)
            (loop (cdr rows) (+ y lh)))))))

(define (wrap-lines str cols)
  (apply append
         (map (lambda (line)
                (let chunk ([s line] [acc '()])
                  (if (<= (string-length s) cols)
                      (reverse (cons s acc))
                      (chunk (substring s cols (string-length s)) (cons (substring s 0 cols) acc)))))
              (string-split str #\newline))))

(defvar *frame-ms* 0.0)      ; the last frame's time: drawing it, comparing its rows
(defvar *frames-drawn* 0)

(define (ms-since t0)
  (let ([dt (time-difference (current-time 'time-monotonic) t0)])
    (+ (* 1000.0 (time-second dt)) (/ (time-nanosecond dt) 1e6))))

;; The whole screen drawn from scratch onto surface, an image the
;; canvas's size.
(define (render-full-into! surface)
  ;; A fresh cairo context, so a crashed draw can't leave the graphics
  ;; state unbalanced. Outside it *cr* only measures.
  (let ([cr (cairo-create surface)])
    (cairo-set-operator cr CAIRO_OPERATOR_SOURCE)   ; every pixel, the last column at 150% too
    (cairo-set-source-rgba cr 0 0 0 1)
    (cairo-paint cr)
    (cairo-set-operator cr CAIRO_OPERATOR_OVER)
    (cairo-scale cr *scale* *scale*)
    (fluid-let ([*cr* cr])
      (safely 'desktop draw-desktop)
      (for-each (lambda (w) (when (visible? w) (safely 'window (lambda () (draw-window w)))))
                (reverse *windows*))
      (safely 'taskbar draw-taskbar)
      (safely 'topbar draw-topbar)
      (safely 'menu draw-menu)
      (safely 'toasts draw-toasts))
    (cairo-destroy cr)
    (cairo-surface-flush surface)))

;; One step's frame. If something asked for one (forced?, from *dirty*),
;; the whole screen is drawn again from scratch into the second buffer,
;; and the rows that changed are copied onto the screen (gfx.ss), where
;; they wait for the host's upload. A process may be switched out at any
;; call while the main loop draws on, so typed in a REPL (a process) or
;; called by a handler, (render-frame!) only asks for a frame.
(define render-frame!
  (case-lambda
    [() (render-frame! #t)]
    [(forced?)
     (cond
       [(or (not forced?) (eqv? *canvas* 0)) (void)]
       [(in-engine?) (request-frame!)]
       [else
        (let ([t0 (current-time 'time-monotonic)])
          (refresh-status!)
          (render-full-into! *back*)
          (set! *upload-rows* (append (copy-changed-rows!) *upload-rows*))
          (set! *frame-ms* (ms-since t0))
          (set! *frames-drawn* (+ *frames-drawn* 1)))])]))
