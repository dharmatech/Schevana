;;; shell.ss — the desktop around the windows: wallpaper, top bar, taskbar,
;;; menus, notifications and status segments.

;;; ------------------------------------------------------------------ desktop

(defvar *wallpaper* #f)   ; (layer . cache-key)

;; Theme keys the wallpaper depends on. If your own draw-wallpaper uses
;; other keys, add them here.
(define wallpaper-theme-keys
  '(desktop-top desktop-bottom accent accent-2 watermark ui-font ui-font-size))

;; The wallpaper is drawn once into a layer and re-blitted every frame.
;; Redefining draw-wallpaper (or changing its theme keys) repaints it.
(define (draw-desktop)
  (let ([key (list *canvas-w* *canvas-h* *scale* (map theme wallpaper-theme-keys) draw-wallpaper)])
    (unless (and *wallpaper* (equal? (cdr *wallpaper*) key))
      (when *wallpaper* (free-layer! (car *wallpaper*)))
      (let ([l (make-layer* *canvas-w* *canvas-h*)])
        (draw-into-layer! l (lambda () (draw-wallpaper *canvas-w* *canvas-h*)))
        (set! *wallpaper* (cons l key))))
    (blit-layer! (car *wallpaper*) 0 0)))

(define (draw-wallpaper w h)
  (fill-gradient 0 0 w h 'desktop-top 'desktop-bottom)
  (fill-glow (* w 0.80) (* h 0.22) (* 0.55 (max w h)) (alpha 'accent-2 0.10))
  (fill-glow (* w 0.12) (* h 0.90) (* 0.50 (max w h)) (alpha 'accent 0.08))
  (draw-text-centered 0 0 w (- h 60) "λ" (alpha 'watermark 0.05) 'huge)
  (draw-text-centered 0 (+ (/ h 2) 150) w 30 "S C H E V A N A" (alpha 'watermark 0.10) 'ui-bold))

(define (desktop-click x y button)
  (case button
    [(right) (open-launcher! x y)]
    [(left) (let ([old *focus*])
              (set! *focus* #f)
              (when old (window-call old 'blur))
              (invalidate!))]))

;;; ------------------------------------------------------------------ status segments
;;; The right side of the top bar is a list of (name . thunk); each thunk
;;; returns a string (or #f to hide). Add your own:
;;;   (add-status-segment! 'hello (lambda () "hi!"))

(defvar *status-segments* '())

(define (add-status-segment! name thunk)
  (set! *status-segments*
    (append (remp (lambda (s) (eq? (car s) name)) *status-segments*) (list (cons name thunk))))
  (invalidate!))

(define (remove-status-segment! name)
  (set! *status-segments* (remp (lambda (s) (eq? (car s) name)) *status-segments*))
  (invalidate!))

(define (status-strings)
  (filter string?
          (map (lambda (s) (safe-call 'status (cdr s) (lambda (c) (format "⚠ ~a" (car s)))))
               *status-segments*)))

(define (status-procs)
  (let ([n (length *procs*)])
    (and (> n 0) (format "⚙ ~a running" n))))

(define (status-memory) (format "~,1f MB" (/ (bytes-allocated) 1048576.0)))

(define (status-frame) (format "~,1f ms/frame" *frame-ms*))

(define day-names '#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
(define month-names '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define (status-clock)
  (let ([d (current-date)])
    (format "~a ~a ~a   ~2,'0d:~2,'0d:~2,'0d"
            (vector-ref day-names (date-week-day d)) (date-day d)
            (vector-ref month-names (- (date-month d) 1))
            (date-hour d) (date-minute d) (date-second d))))

(define (install-default-status!)
  (when (null? *status-segments*)
    (add-status-segment! 'procs (lambda () (status-procs)))
    (add-status-segment! 'frame (lambda () (status-frame)))
    (add-status-segment! 'memory (lambda () (status-memory)))
    (add-status-segment! 'clock (lambda () (status-clock)))))

;;; ------------------------------------------------------------------ top bar

(defvar *topbar-launcher-w* 0)

(define (draw-topbar)
  (let* ([w (screen-w)] [h (theme 'topbar-height)]
         [ty (/ (- h (font-line-height 'ui)) 2)])
    (fill-rect 0 0 w h (alpha 'panel 0.92))
    (draw-line 0 (- h 0.5) w (- h 0.5) (alpha 'panel-text 0.12))
    (let* ([label "λ  Schevana"]
           [bw (+ 24 (text-width label 'ui-bold))])
      (set! *topbar-launcher-w* (+ 6 bw))
      (when (or (equal? *hover* '(topbar launcher)) (launcher-open?))
        (fill-round-rect 6 4 bw (- h 8) 6 'panel-hover))
      (draw-text 18 ty label 'accent 'ui-bold)
      (draw-text (+ bw 20) ty (if *focus* (window-title *focus*) "Desktop") 'panel-text 'ui-bold))
    (let loop ([segs (reverse (status-strings))] [x (- w 14)])
      (unless (null? segs)
        (let* ([s (car segs)] [x0 (- x (text-width s 'ui))])
          (draw-text x0 ty s 'panel-text 'ui)
          (unless (null? (cdr segs)) (draw-text (- x0 15) ty "·" 'panel-dim 'ui))
          (loop (cdr segs) (- x0 26)))))))

(define (topbar-item-at x y)
  (and (< x *topbar-launcher-w*) 'launcher))

(define (topbar-click x y button)
  (when (eq? (topbar-item-at x y) 'launcher) (toggle-launcher!)))

;;; ------------------------------------------------------------------ taskbar

(defvar *taskbar-items* '())   ; ((x0 x1 window) ...) from the last frame

(define (draw-taskbar)
  (let* ([w (screen-w)] [h (theme 'taskbar-height)] [y0 (- (screen-h) h)]
         [lh (font-line-height 'ui)]
         [ws (sort (lambda (a b) (< (window-id a) (window-id b))) *windows*)]
         [n (length ws)]
         [hint "F1 apps  ·  Ctrl+Tab switch  ·  right-click desktop"]
         [hint-w (+ 24 (text-width hint 'ui-small))]
         [bw (if (zero? n) 0 (min 210 (/ (- w 16 hint-w (* 6 (- n 1))) n)))])
    (fill-rect 0 y0 w h (alpha 'panel 0.94))
    (draw-line 0 (+ y0 0.5) w (+ y0 0.5) (alpha 'panel-text 0.12))
    (draw-text (- w hint-w) (+ y0 (/ (- h (font-line-height 'ui-small)) 2)) hint 'panel-dim 'ui-small)
    (let loop ([ws ws] [x 8] [items '()])
      (if (null? ws)
          (set! *taskbar-items* items)
          (let* ([win (car ws)]
                 [focused? (eq? win *focus*)]
                 [hover? (equal? *hover* (list 'taskbar win))]
                 [min? (not (visible? win))]
                 [color (app-color (window-app win))])
            (when (or focused? hover?)
              (fill-round-rect x (+ y0 5) bw (- h 10) 6 (if focused? 'panel-hover (alpha 'panel-hover 0.6))))
            (when focused?
              (fill-round-rect (+ x 12) (- (+ y0 h) 6) (- bw 24) 2 1 'accent))
            (fill-circle (+ x 15) (+ y0 (/ h 2)) 4 (if min? (alpha color 0.35) color))
            (draw-text (+ x 27) (+ y0 (/ (- h lh) 2))
                       (fit-text (window-title win) (- bw 36) 'ui)
                       (if min? 'panel-dim 'panel-text) 'ui)
            (loop (cdr ws) (+ x bw 6) (cons (list x (+ x bw) win) items)))))))

(define (taskbar-item-at x y)
  (let ([it (find (lambda (it) (and (>= x (car it)) (< x (cadr it)))) *taskbar-items*)])
    (and it (memq (caddr it) *windows*) (caddr it))))

(define (taskbar-click x y button)
  (let ([win (taskbar-item-at x y)])
    (when win
      (case button
        [(left) (if (and (eq? win *focus*) (visible? win))
                    (minimize-window! win)
                    (focus-window! win))]
        [(middle) (close-window! win)]
        [(right) (open-window-menu! win x (- (screen-h) (theme 'taskbar-height)))]))))

;;; ------------------------------------------------------------------ menus
;;; A menu is a list of entries: (label . thunk) or 'separator.

(define-record-type menu
  (nongenerative schevana-menu)
  (fields x y items (mutable sel) (mutable filter) filter? tag))

(defvar *menu* #f)

(define menu-item-height 26)

(define (menu-open?) (and *menu* #t))

(define (open-menu! x y items . opts)
  (set! *menu* (make-menu x y items 0 "" (plist-ref opts 'filter #f) (plist-ref opts 'tag #f)))
  (menu-select-next! 0)
  (invalidate!))

(define (close-menu!)
  (set! *menu* #f)
  (invalidate!))

(define (menu-entries m)
  (let ([f (menu-filter m)])
    (if (string=? f "")
        (menu-items m)
        (filter (lambda (e) (and (pair? e) (string-contains-ci? (car e) f))) (menu-items m)))))

(define (menu-header-height m) (if (menu-filter? m) 34 0))

(define (menu-geometry m)
  (let* ([es (menu-entries m)]
         [w (fold-left (lambda (acc e) (if (pair? e) (max acc (+ 44 (text-width (car e) 'ui))) acc))
                       230 es)]
         [h (fold-left (lambda (acc e) (+ acc (if (pair? e) menu-item-height 9)))
                       (+ 10 (menu-header-height m)) es)]
         [x (clamp (menu-x m) 4 (- (screen-w) w 4))]
         [y (clamp (menu-y m) (+ 2 (theme 'topbar-height)) (- (screen-h) h 4))])
    (values x y w h)))

(define (point-in-menu? px py)
  (let-values ([(x y w h) (menu-geometry *menu*)])
    (point-in? px py x y w h)))

;; Index of the entry under (px, py), or #f.
(define (menu-entry-at px py)
  (let-values ([(x y w h) (menu-geometry *menu*)])
    (let loop ([es (menu-entries *menu*)] [i 0] [ey (+ y 5 (menu-header-height *menu*))])
      (cond
        [(null? es) #f]
        [(pair? (car es))
         (if (and (>= py ey) (< py (+ ey menu-item-height)) (>= px x) (< px (+ x w)))
             i
             (loop (cdr es) (+ i 1) (+ ey menu-item-height)))]
        [else (loop (cdr es) (+ i 1) (+ ey 9))]))))

(define (menu-select-next! dir)
  (let* ([es (list->vector (menu-entries *menu*))] [n (vector-length es)])
    (unless (zero? n)
      (let loop ([i (if (zero? dir) 0 (+ (menu-sel *menu*) dir))] [tries 0])
        (when (< tries n)
          (let ([i (mod i n)])
            (if (pair? (vector-ref es i))
                (menu-sel-set! *menu* i)
                (loop (+ i (if (zero? dir) 1 dir)) (+ tries 1)))))))
    (invalidate!)))

(define (menu-activate! i)
  (let ([es (menu-entries *menu*)])
    (when (and i (< i (length es)) (pair? (list-ref es i)))
      (let ([thunk (cdr (list-ref es i))])
        (close-menu!)
        (safe-call 'menu thunk (lambda (c) (report-error! 'menu c)))))))

(define (menu-mouse-move x y)
  (let ([i (menu-entry-at x y)])
    (when (and i (not (= i (menu-sel *menu*))))
      (menu-sel-set! *menu* i)
      (invalidate!))))

(define (menu-mouse-down x y)
  (menu-activate! (menu-entry-at x y)))

(define (menu-key key mods)
  (case key
    [(escape) (close-menu!)]
    [(up) (menu-select-next! -1)]
    [(down tab) (menu-select-next! 1)]
    [(return) (menu-activate! (menu-sel *menu*))]
    [(backspace)
     (let ([f (menu-filter *menu*)])
       (when (> (string-length f) 0)
         (menu-filter-set! *menu* (substring f 0 (- (string-length f) 1)))
         (menu-select-next! 0)))]))

(define (menu-text str)
  (when (menu-filter? *menu*)
    (menu-filter-set! *menu* (string-append (menu-filter *menu*) str))
    (menu-select-next! 0)))

(define (draw-menu)
  (when *menu*
    (let-values ([(x y w h) (menu-geometry *menu*)])
      (let ([lh (font-line-height 'ui)] [m *menu*])
        (draw-shadow x y w h 8 12)
        (fill-round-rect x y w h 8 'bg-2)
        (stroke-round-rect x y w h 8 'window-border-focus)
        (when (menu-filter? m)
          (fill-round-rect (+ x 6) (+ y 6) (- w 12) 26 6 'bg-0)
          (let ([f (menu-filter m)])
            (if (string=? f "")
                (draw-text (+ x 16) (+ y 6 (/ (- 26 lh) 2)) "Type to filter…" 'text-faint 'ui)
                (let ([ex (draw-text (+ x 16) (+ y 6 (/ (- 26 lh) 2)) f 'text 'ui)])
                  (fill-rect (+ ex 1) (+ y 11) 1.5 16 'accent)))))
        (let loop ([es (menu-entries m)] [i 0] [ey (+ y 5 (menu-header-height m))])
          (unless (null? es)
            (let ([e (car es)])
              (if (pair? e)
                  (begin
                    (when (= i (menu-sel m))
                      (fill-round-rect (+ x 5) ey (- w 10) menu-item-height 5 (alpha 'accent 0.22)))
                    (draw-text (+ x 18) (+ ey (/ (- menu-item-height lh) 2)) (car e)
                               (if (= i (menu-sel m)) 'text 'text-dim) 'ui)
                    (loop (cdr es) (+ i 1) (+ ey menu-item-height)))
                  (begin
                    (draw-line (+ x 12) (+ ey 4.5) (- (+ x w) 12) (+ ey 4.5) 'window-border)
                    (loop (cdr es) (+ i 1) (+ ey 9)))))))))))

;;; The launcher: every registered app, plus a few system actions.

(define (launcher-items)
  (append
   (map (lambda (a) (cons (app-title a) (app-launch a)))
        (sort (lambda (a b) (string<? (app-title a) (app-title b))) *apps*))
   (list 'separator
         (cons "Tile windows" (lambda () (tile-windows!)))
         (cons "Cascade windows" (lambda () (cascade-windows!)))
         (cons "Reload sources" (lambda () (spawn "reload" reload!)))
         (cons "Quit Schevana" (lambda () (quit!))))))

(define (launcher-open?) (and *menu* (eq? (menu-tag *menu*) 'launcher)))

(define (open-launcher! x y)
  (open-menu! x y (launcher-items) 'filter #t 'tag 'launcher))

(define (toggle-launcher!)
  (if (launcher-open?)
      (close-menu!)
      (open-launcher! 6 (+ 2 (theme 'topbar-height)))))

(define (open-window-menu! win x y)
  (open-menu! x y
              (list (cons "Bind to $w for the REPL"
                          (lambda ()
                            (define-top-level-value '$w win)
                            (notify (format "$w = ~s" win))))
                    (cons (if (eq? (window-state win) 'maximized) "Restore" "Maximize")
                          (lambda () (toggle-maximize! win)))
                    (cons "Minimize" (lambda () (minimize-window! win)))
                    'separator
                    (cons "Close" (lambda () (close-window! win))))
              'tag 'window))

;;; ------------------------------------------------------------------ notifications

(defvar *toasts* '())   ; (#(expires-at text colour) ...)

(define notify
  (case-lambda
    [(msg) (notify msg 'accent)]
    [(msg color)
     (let ([text (list->string (map (lambda (c) (if (char=? c #\newline) #\space c))
                                    (string->list (if (string? msg) msg (format "~a" msg)))))])
       (set! *toasts* (append *toasts* (list (vector (+ (now-ms) 5000) text color))))
       (when (> (length *toasts*) 5) (set! *toasts* (cdr *toasts*)))
       (invalidate!))]))

(define (expire-toasts!)
  (let ([now (now-ms)])
    (when (exists (lambda (t) (< (vector-ref t 0) now)) *toasts*)
      (set! *toasts* (filter (lambda (t) (>= (vector-ref t 0) now)) *toasts*))
      (invalidate!))))

(define (draw-toasts)
  (let ([lh (font-line-height 'ui)] [h 34])
    (let loop ([ts *toasts*] [y (+ (theme 'topbar-height) 10)])
      (unless (null? ts)
        (let* ([t (car ts)]
               [text (fit-text (vector-ref t 1) (- (screen-w) 120) 'ui)]
               [tw (+ 40 (text-width text 'ui))]
               [x (- (screen-w) tw 12)])
          (draw-shadow x y tw h 8 10)
          (fill-round-rect x y tw h 8 'bg-2)
          (stroke-round-rect x y tw h 8 'window-border)
          (fill-round-rect (+ x 8) (+ y 9) 4 (- h 18) 2 (vector-ref t 2))
          (draw-text (+ x 22) (+ y (/ (- h lh) 2)) text 'text 'ui)
          (loop (cdr ts) (+ y h 8)))))))

(defvar *last-error* (cons "" 0))

;; System-level errors become a red notification (and a line on stderr).
(define (report-error! who c)
  (let ([msg (format "~a: ~a" who (condition->string c))])
    (unless (and (string=? msg (car *last-error*)) (< (- (now-ms) (cdr *last-error*)) 5000))
      (set! *last-error* (cons msg (now-ms)))
      (notify msg 'red)
      (fprintf (console-error-port) "schevana: ~a\n" msg))))
