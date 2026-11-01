;;; shell.ss — the desktop around the windows: wallpaper, top bar, taskbar,
;;; menus, notifications and status segments.

;;; ------------------------------------------------------------------ desktop

;;; A wallpaper fills the desktop from the theme's two desktop colors. Each
;;; is a file in wallpapers/, and the theme key 'desktop-pattern names the
;;; one in use:
;;;
;;;   (define-wallpaper flat "Flat"
;;;     (lambda (x y w h top bottom) (fill-rect x y w h top)))

;; ((name title fill) ...) in registration order
(defvar *wallpapers* '())

(define (register-wallpaper! name title fill)
  (set! *wallpapers* (registry-add *wallpapers* (list name title fill) car))
  (request-frame!))

;; (define-wallpaper name "Title" fill) adds a wallpaper, or replaces the
;; one of that name. (fill x y w h top bottom) fills the rectangle.
(define-syntax define-wallpaper
  (syntax-rules ()
    [(_ name title fill) (register-wallpaper! 'name title fill)]))

(define (wallpapers) (map car *wallpapers*))

;; The wallpaper is a picture, painted once and blitted every frame. It is
;; a picture of its key: the screen's size and scale, draw-wallpaper, the
;; wallpapers registered and the whole theme, however it changed. When the
;; key changes, the next frame paints it again first, and is the whole
;; screen. Nothing else does, so a draw-wallpaper that reads the clock
;; shows a still picture.

(defvar *wallpaper* #f)   ; (layer key . theme): the picture, and what it is of

(define (wallpaper-key)
  (list *canvas-w* *canvas-h* *scale* draw-wallpaper *wallpapers*
        *theme-version*))

;; Has the key changed since the picture was painted?
(define (wallpaper-stale?)
  (let ([old *wallpaper*])
    (not (and old (equal? (cadr old) (wallpaper-key))
              (theme-same? (cddr old))))))

;; The picture of the wallpaper as it is now, painted again if its key
;; changed. If draw-wallpaper raises, the error is reported once and the
;; old picture stays (none: a blank one) until the key changes again.
(define (wallpaper-picture)
  (let ([key (wallpaper-key)] [old *wallpaper*])
    (when (wallpaper-stale?)
      (let ([new (make-layer* *canvas-w* *canvas-h*)] [theme (theme-snapshot)])
        (if (guard (c [#t (report-error! 'desktop c) #f])
              (draw-into-layer! new
                (lambda () (draw-wallpaper *canvas-w* *canvas-h*)))
              #t)
            (begin (when old (free-layer! (car old)))
                   (set! *wallpaper* (cons* new key theme)))
            (begin (when old (free-layer! new))
                   (set! *wallpaper*
                     (cons* (if old (car old) new) key theme))))))
    (car *wallpaper*)))

;; Interrupts off: a process switched out between the lookup and the blit
;; would find the picture freed by main.
(define (draw-desktop)
  (with-interrupts-disabled (blit-layer! (wallpaper-picture) 0 0)))

;; The desktop's background in a rectangle: the wallpaper named pattern (a
;; value of the theme key 'desktop-pattern) from the colors top and
;; bottom, or a plain fill when no wallpaper has that name.
(define (fill-desktop-pattern x y w h pattern top bottom)
  (let ([wp (assq pattern *wallpapers*)])
    (if wp
        ((caddr wp) x y w h top bottom)
        (fill-rect x y w h top))))

(define (draw-wallpaper w h)
  (fill-desktop-pattern 0 0 w h (theme 'desktop-pattern)
                        'desktop-top 'desktop-bottom)
  (when (eq? (theme 'desktop-pattern) 'gradient)
    (fill-glow (* w 0.80) (* h 0.22) (* 0.55 (max w h)) (alpha 'accent-2 0.10))
    (fill-glow (* w 0.12) (* h 0.90) (* 0.50 (max w h)) (alpha 'accent 0.08)))
  (draw-text-centered 0 0 w (- h 60) "λ" (alpha 'watermark 0.05) 'huge)
  (draw-text-centered 0 (+ (/ h 2) 150) w 30 "S C H E V A N A"
                      (alpha 'watermark 0.10) 'ui-bold))

(define (desktop-click x y button)
  (case button
    [(right) (open-launcher! x y (cons x y))]   ; apps open where you clicked
    [(left) (let ([old *focus*])
              (set! *focus* #f)
              (when old (window-call old 'blur))
              (request-frame!))]))

;;; ------------------------------------------------------------------
;;; status segments
;;; The right side of the top bar is a list of (name . thunk); each thunk
;;; returns a string (or #f to hide). Add your own:
;;;   (add-status-segment! 'hello (lambda () "hi!"))

(defvar *status-segments* '())

(define (add-status-segment! name thunk)
  (set! *status-segments*
    (append (remp (lambda (s) (eq? (car s) name)) *status-segments*)
            (list (cons name thunk))))
  (request-frame!))

(define (remove-status-segment! name)
  (set! *status-segments*
    (remp (lambda (s) (eq? (car s) name)) *status-segments*))
  (request-frame!))

;; What the top bar shows: ((name . string-or-#f) ...), taken once per
;; frame before anything is drawn, each segment under the watchdog.
(defvar *status-shown* '())

(define (refresh-status!)
  (set! *status-shown*
    (map (lambda (s)
           (cons (car s)
                 (safe-call 'status (cdr s)
                            (lambda (c) (format "⚠ ~a" (car s))))))
         *status-segments*)))

(define (shown-status-strings) (filter string? (map cdr *status-shown*)))

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

(define (install-default-status!)
  (when (null? *status-segments*)
    (add-status-segment! 'procs (lambda () (status-procs)))
    (add-status-segment! 'frame (lambda () (status-frame)))
    (add-status-segment! 'memory (lambda () (status-memory)))
    (add-status-segment! 'clock (lambda () (status-clock)))))

;;; ------------------------------------------------------------------ top bar

;; The accent on the panels, or the panels' text color where the accent
;; would vanish (Genera's black accent on its black panels).
(define (panel-accent)
  (let-values ([(ar ag ab aa) (color-rgba 'accent)]
               [(pr pg pb pa) (color-rgba 'panel)])
    (define (lum r g b) (+ (* 0.299 r) (* 0.587 g) (* 0.114 b)))
    (if (< (abs (- (lum ar ag ab) (lum pr pg pb))) 0.2) 'panel-text 'accent)))

(defvar *topbar-launcher-w* 0)

(define (draw-topbar)
  (let* ([w (screen-w)] [h (topbar-height)]
         [ty (/ (- h (font-line-height 'ui)) 2)]
         [gap (max 11 (round (* 0.6 (font-line-height 'ui))))]
         [dot-w (text-width "·" 'ui)])
    (fill-rect 0 0 w h (alpha 'panel 0.92))
    (draw-line 0 (- h 0.5) w (- h 0.5) (alpha 'panel-text 0.12))
    (let* ([label "λ  Schevana"]
           [bw (+ 24 (text-width label 'ui-bold))])
      (set! *topbar-launcher-w* (+ 6 bw))
      (when (or (equal? *hover* '(topbar launcher)) (launcher-open?))
        (fill-round-rect 6 4 bw (- h 8) 6 'panel-hover))
      (draw-text 18 ty label (panel-accent) 'ui-bold)
      (let ([title-end (draw-text (+ bw 20) ty
                                  (if *focus* (window-title *focus*) "Desktop")
                                  'panel-text 'ui-bold)])
        ;; status segments from the right, while they fit beside the title
        (let loop ([segs (reverse (shown-status-strings))]
                   [x (- w 14)]
                   [first? #t])
          (unless (null? segs)
            (let* ([s (car segs)]
                   ;; room for the separator
                   [right (if first? x (- x gap dot-w gap))]
                   [x0 (- right (text-width s 'ui))])
              (when (> x0 (+ title-end gap gap))
                (unless first?
                  (draw-text (- x gap dot-w) ty "·" 'panel-dim 'ui))
                (draw-text x0 ty s 'panel-text 'ui)
                (loop (cdr segs) x0 #f)))))))))

(define (topbar-item-at x y)
  (and (< x *topbar-launcher-w*) 'launcher))

(define (topbar-click x y button)
  (when (eq? (topbar-item-at x y) 'launcher) (toggle-launcher!)))

;;; ------------------------------------------------------------------ taskbar

(defvar *taskbar-items* '())   ; ((x0 x1 window) ...) from the last frame

(define (draw-taskbar)
  (let* ([w (screen-w)] [h (taskbar-height)] [y0 (- (screen-h) h)]
         [lh (font-line-height 'ui)]
         [ws (sort (lambda (a b) (< (window-id a) (window-id b))) *windows*)]
         [n (length ws)]
         [hint "F1 apps  ·  Ctrl+Tab switch  ·  right-click desktop"]
         [button-w (lambda (hint-w)
                     (if (zero? n)
                         0
                         (min 210 (/ (- w 16 hint-w (* 6 (- n 1))) n))))]
         ;; the hint gives way when the buttons would be too narrow for their
         ;; titles
         [hint-w (let ([hw (+ 24 (text-width hint 'ui-small))]
                       [want (min 210 (+ 48 (widest-text (map window-title ws)
                                                         'ui)))])
                   (if (>= (button-w hw) want) hw 0))]
         [bw (button-w hint-w)])
    (fill-rect 0 y0 w h (alpha 'panel 0.94))
    (draw-line 0 (+ y0 0.5) w (+ y0 0.5) (alpha 'panel-text 0.12))
    (when (> hint-w 0)
      (draw-text (- w hint-w) (+ y0 (/ (- h (font-line-height 'ui-small)) 2))
                 hint 'panel-dim 'ui-small))
    (let loop ([ws ws] [x 8] [items '()])
      (if (null? ws)
          (set! *taskbar-items* items)
          (let* ([win (car ws)]
                 [focused? (eq? win *focus*)]
                 [hover? (equal? *hover* (list 'taskbar win))]
                 [min? (not (visible? win))]
                 [color (app-color (window-app win))])
            (when (or focused? hover?)
              (fill-round-rect x (+ y0 5) bw (- h 10) 6
                               (if focused?
                                   'panel-hover
                                   (alpha 'panel-hover 0.6))))
            (when focused?
              (fill-round-rect (+ x 12) (- (+ y0 h) 6) (- bw 24) 2 1
                               (panel-accent)))
            (fill-circle (+ x 15) (+ y0 (/ h 2)) 4
                         (if min? (alpha color 0.35) color))
            (draw-text (+ x 27) (+ y0 (/ (- h lh) 2))
                       (fit-text (window-title win) (- bw 36) 'ui)
                       (if min? 'panel-dim 'panel-text) 'ui)
            (loop (cdr ws) (+ x bw 6) (cons (list x (+ x bw) win) items)))))))

(define (taskbar-item-at x y)
  (let ([it (find (lambda (it) (and (>= x (car it)) (< x (cadr it))))
                  *taskbar-items*)])
    (and it (memq (caddr it) *windows*) (caddr it))))

(define (taskbar-click x y button)
  (let ([win (taskbar-item-at x y)])
    (when win
      (case button
        [(left) (if (and (eq? win *focus*) (visible? win))
                    (minimize-window! win)
                    (focus-window! win))]
        [(middle) (close-window! win)]
        [(right) (open-window-menu! win x (- (screen-h) (taskbar-height)))]))))

;;; ------------------------------------------------------------------ menus
;;; A menu is a list of entries: (label . thunk) or 'separator.

(define-record-type menu
  (nongenerative schevana-menu)
  (fields x y items (mutable sel) (mutable filter) filter? tag))

(defvar *menu* #f)

(define (menu-item-height) (max 26 (round (+ (font-line-height 'ui) 11))))
(define (menu-field-height) (menu-item-height))

(define (menu-open?) (and *menu* #t))

(define (open-menu! x y items . opts)
  (set! *menu* (make-menu x y items 0 "" (plist-ref opts 'filter #f)
                          (plist-ref opts 'tag #f)))
  (menu-select-next! 0)
  (let ([i (plist-ref opts 'selected #f)])   ; a dropdown's current entry
    (when i (menu-sel-set! *menu* i)))
  (request-frame!))

(define (close-menu!)
  (set! *menu* #f)
  (request-frame!))

(define (menu-entries m)
  (let ([f (menu-filter m)])
    (if (string=? f "")
        (menu-items m)
        (filter (lambda (e) (and (pair? e) (string-contains-ci? (car e) f)))
                (menu-items m)))))

(define (menu-header-height m)
  (if (menu-filter? m) (+ (menu-field-height) 8) 0))

(define (menu-geometry m)
  (let* ([es (menu-entries m)]
         [w (fold-left (lambda (acc e)
                         (if (pair? e)
                             (max acc (+ 44 (text-width (car e) 'ui)))
                             acc))
                       230 es)]
         [h (fold-left (lambda (acc e)
                         (+ acc (if (pair? e) (menu-item-height) 9)))
                       (+ 10 (menu-header-height m)) es)]
         [x (clamp (menu-x m) 4 (- (screen-w) w 4))]
         [y (clamp (menu-y m) (+ 2 (topbar-height)) (- (screen-h) h 4))])
    (values x y w h)))

(define (point-in-menu? px py)
  (let-values ([(x y w h) (menu-geometry *menu*)])
    (point-in? px py x y w h)))

;; Index of the entry under (px, py), or #f.
(define (menu-entry-at px py)
  (let-values ([(x y w h) (menu-geometry *menu*)])
    (let loop ([es (menu-entries *menu*)]
               [i 0]
               [ey (+ y 5 (menu-header-height *menu*))])
      (cond
        [(null? es) #f]
        [(pair? (car es))
         (if (and (>= py ey) (< py (+ ey (menu-item-height)))
                  (>= px x) (< px (+ x w)))
             i
             (loop (cdr es) (+ i 1) (+ ey (menu-item-height))))]
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
    (request-frame!)))

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
      (request-frame!))))

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
      (let ([lh (font-line-height 'ui)] [m *menu*]
            [ih (menu-item-height)] [fh (menu-field-height)])
        (draw-shadow x y w h 8 12)   ; shadow
        (fill-round-rect x y w h 8 'bg-2)
        (stroke-round-rect x y w h 8 'window-border-focus)
        (when (menu-filter? m)
          (fill-round-rect (+ x 6) (+ y 6) (- w 12) fh 6 'bg-0)
          (let ([f (menu-filter m)] [ty (+ y 6 (/ (- fh lh) 2))])
            (if (string=? f "")
                (draw-text (+ x 16) ty "Type to filter…" 'text-faint 'ui)
                ;; inside the field; a longer filter scrolls, so its end and
                ;; the caret stay in view
                (call-with-clip (+ x 6) (+ y 6) (- w 12) fh
                  (lambda ()
                    (let ([ex (draw-text (min (+ x 16)
                                              (- (+ x w) 19 (text-width f 'ui)))
                                         ty f 'text 'ui)])
                      (fill-rect (+ ex 1) (+ y 6 (round (/ (- fh lh 1) 2)))
                                 1.5 (round (+ lh 1)) 'accent)))))))
        (let loop ([es (menu-entries m)]
                   [i 0]
                   [ey (+ y 5 (menu-header-height m))])
          (unless (null? es)
            (let ([e (car es)])
              (if (pair? e)
                  (begin
                    (when (= i (menu-sel m))
                      (fill-round-rect (+ x 5) ey (- w 10) ih 5
                                       (alpha 'accent 0.22)))
                    (draw-text (+ x 18) (+ ey (/ (- ih lh) 2)) (car e)
                               (if (= i (menu-sel m)) 'text 'text-dim) 'ui)
                    (loop (cdr es) (+ i 1) (+ ey ih)))
                  (begin
                    (draw-line (+ x 12) (+ ey 4.5) (- (+ x w) 12) (+ ey 4.5)
                               'window-border)
                    (loop (cdr es) (+ i 1) (+ ey 9)))))))))))

;;; The launcher: every registered app, plus a few system actions.

;; at: where the apps' windows open (see new-window-position), or #f.
(define (launcher-items at)
  (append
   (map (lambda (a)
          (cons (app-title a)
                (lambda ()
                  (parameterize ([new-window-position at]) ((app-launch a))))))
        (sort (lambda (a b) (string<? (app-title a) (app-title b))) *apps*))
   (list 'separator
         (cons "Tile windows" (lambda () (tile-windows!)))
         (cons "Cascade windows" (lambda () (cascade-windows!)))
         (cons (if (fullscreen?) "Leave full screen" "Full screen")
               (lambda () (toggle-fullscreen!)))
         (cons "Reload sources" (lambda () (spawn "reload" reload!)))
         (cons "Quit Schevana" (lambda () (quit!))))))

(define (launcher-open?) (and *menu* (eq? (menu-tag *menu*) 'launcher)))

(define (open-launcher! x y . at)
  (open-menu! x y (launcher-items (and (pair? at) (car at)))
              'filter #t 'tag 'launcher))

(define (toggle-launcher!)
  (if (launcher-open?)
      (close-menu!)
      (open-launcher! 6 (+ 2 (topbar-height)))))

(define (open-window-menu! win x y)
  (open-menu! x y
              (list (cons "Bind to $w for the REPL"
                          (lambda ()
                            (define-top-level-value '$w win)
                            (notify (format "$w = ~s" win))))
                    (cons (if (eq? (window-state win) 'maximized)
                              "Restore"
                              "Maximize")
                          (lambda () (toggle-maximize! win)))
                    (cons "Minimize" (lambda () (minimize-window! win)))
                    'separator
                    (cons "Close" (lambda () (close-window! win))))
              'tag 'window))

;;; ------------------------------------------------------------------ app icon
;;; The host window's icon in the taskbar or dock: the wallpaper's λ on the
;;; desktop gradient, in the current theme. Redefine draw-app-icon and call
;;; (update-host-icon!) to change it.

(define (draw-app-icon size)
  (let* ([m (* size 0.06)] [s (- size m m)] [r (* s 0.22)])
    (call-with-round-clip m m s s r
      (lambda ()
        (fill-gradient m m s s 'desktop-top 'desktop-bottom)
        (fill-glow (+ m (* s 0.8)) (+ m (* s 0.2)) (* s 0.9)
                   (alpha 'accent-2 0.30))
        (fill-glow (+ m (* s 0.15)) (+ m (* s 0.9)) (* s 0.8)
                   (alpha 'accent 0.25))))
    (stroke-round-rect (+ m 0.5) (+ m 0.5) (- s 1) (- s 1) r
                       (alpha 'text 0.18) 1)
    (draw-text-centered m m s s "λ" 'text
                        (make-font* "serif" (* s 0.72) 'bold))))

(define (update-host-icon!) (host-set-icon! 256 draw-app-icon))

;;; Desktops like GNOME don't use the icon a window sets: they show the
;;; icon of the app's .desktop file, which they match to the window by its
;;; app id (host-app-id; the class under X11). (install-desktop-entry!)
;;; writes that file and the icon under ~/.local/share; ./schevana
;;; --install-desktop does the same.

(define (desktop-data-home)
  (or (getenv "XDG_DATA_HOME")
      (string-append (or (getenv "HOME") ".") "/.local/share")))

(define (desktop-entry-files)
  (let ([data (desktop-data-home)])
    ;; named after the app id, which Wayland desktops look it up by
    (values (string-append data "/applications/" host-app-id ".desktop")
            (string-append data "/icons/hicolor/256x256/apps/schevana.png"))))

(define (make-directories! path)
  (unless (or (string=? path "") (file-exists? path))
    (make-directories! (path-parent path))
    (mkdir path)))

;; The app icon as a PNG file, size pixels square.
(define (write-app-icon! path size)
  (let* ([surface (cairo-image-surface-create CAIRO_FORMAT_ARGB32 size size)]
         [cr (cairo-create surface)])
    (fluid-let ([*cr* cr]) (draw-app-icon size))
    (cairo-destroy cr)
    (cairo-surface-write-to-png surface path)
    (cairo-surface-destroy surface)))

;; Returns the files it wrote.
(define (install-desktop-entry!)
  (let-values ([(entry icon) (desktop-entry-files)])
    (let* ([root (cond [(path-absolute? *schevana-root*) *schevana-root*]
                       [(member *schevana-root* '("." "")) (current-directory)]
                       [else (string-append (current-directory) "/"
                                            *schevana-root*)])]
           [exec (string-append root "/schevana")])
      (make-directories! (path-parent entry))
      (make-directories! (path-parent icon))
      (write-app-icon! icon 256)
      (call-with-output-file entry
        (lambda (p)
          (fprintf p "[Desktop Entry]~%Type=Application~%Name=Schevana~%")
          (fprintf p
            "Comment=A Lisp Machine-style desktop written in Chez Scheme~%")
          (fprintf p "Exec=\"~a\"~%Icon=schevana~%Terminal=false~%\
                      Categories=Development;~%" exec)
          (fprintf p "StartupWMClass=~a~%" host-app-id))
        'replace)
      ;; let the desktop notice; harmless where these tools are missing
      (system (format
               "update-desktop-database -q '~a' 2>/dev/null; \
                gtk-update-icon-cache -q -t '~a/icons/hicolor' 2>/dev/null"
               (path-parent entry) (desktop-data-home)))
      (list entry icon))))

(define (uninstall-desktop-entry!)
  (let-values ([(entry icon) (desktop-entry-files)])
    (for-each (lambda (f) (when (file-exists? f) (delete-file f)))
              (list entry icon))
    (system (format "update-desktop-database -q '~a' 2>/dev/null"
                    (path-parent entry)))
    (list entry icon)))

(defvar *icon-update-pending* #f)

;; Follow theme changes, once things settle.
(define (update-host-icon-soon!)
  (unless *icon-update-pending*
    (set! *icon-update-pending* #t)
    (after 500 (lambda ()
                 (set! *icon-update-pending* #f)
                 (update-host-icon!)))))

;;; ------------------------------------------------------------------
;;; notifications

(defvar *toasts* '())   ; (#(expires-at text color) ...)

(define notify
  (case-lambda
    [(msg) (notify msg 'accent)]
    [(msg color)
     (let ([text (list->string
                  (map (lambda (c) (if (char=? c #\newline) #\space c))
                       (string->list
                        (if (string? msg) msg (format "~a" msg)))))])
       (set! *toasts*
         (append *toasts* (list (vector (+ (now-ms) 5000) text color))))
       (when (> (length *toasts*) 5) (set! *toasts* (cdr *toasts*)))
       (request-frame!))]))

(define (expire-toasts!)
  (let ([now (now-ms)])
    (when (exists (lambda (t) (< (vector-ref t 0) now)) *toasts*)
      (set! *toasts* (filter (lambda (t) (>= (vector-ref t 0) now)) *toasts*))
      (request-frame!))))

;; Where each toast goes: (#(x y w h text color) ...), top to bottom.
(define (toast-boxes)
  (let ([h (max 34 (round (+ (font-line-height 'ui) 19)))])
    (let loop ([ts *toasts*] [y (+ (topbar-height) 10)] [acc '()])
      (if (null? ts)
          (reverse acc)
          (let* ([t (car ts)]
                 [text (fit-text (vector-ref t 1) (- (screen-w) 120) 'ui)]
                 [tw (+ 40 (text-width text 'ui))])
            (loop (cdr ts) (+ y h 8)
                  (cons (vector (- (screen-w) tw 12) y tw h text
                                (vector-ref t 2))
                        acc)))))))

(define (draw-toasts)
  (let ([lh (font-line-height 'ui)])
    (for-each (lambda (b)
                (let ([x (vector-ref b 0)] [y (vector-ref b 1)]
                      [tw (vector-ref b 2)] [h (vector-ref b 3)])
                  (draw-shadow x y tw h 8 10)   ; shadow
                  (fill-round-rect x y tw h 8 'bg-2)
                  (stroke-round-rect x y tw h 8 'window-border)
                  (fill-round-rect (+ x 8) (+ y 9) 4 (- h 18) 2
                                   (vector-ref b 5))
                  (draw-text (+ x 22) (+ y (/ (- h lh) 2)) (vector-ref b 4)
                             'text 'ui)))
              (toast-boxes))))

(defvar *last-error* (cons "" 0))

;; System-level errors become a red notification (and a line on stderr).
(define (report-error! who c)
  (let ([msg (format "~a: ~a" who (condition->string c))])
    (unless (and (string=? msg (car *last-error*))
                 (< (- (now-ms) (cdr *last-error*)) 5000))
      (set! *last-error* (cons msg (now-ms)))
      (notify msg 'red)
      (fprintf (console-error-port) "schevana: ~a\n" msg))))
