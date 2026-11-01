;;; menu.ss — the desktop's one menu at a time: the launcher, a window's
;;; menu, a dropdown's (the widget kit opens it by name, open-menu!). A
;;; menu is a list of entries: (label . thunk) or 'separator. While one
;;; is open it takes every press, key and text (its surface's box is the
;;; whole screen); a press outside it closes it.

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
  (let ([f (menu-filter m)] [items (menu-items m)])
    (cond [(procedure? items) (items f)]   ; (lambda (filter) entries)
          [(string=? f "") items]
          [else (filter (lambda (e)
                          (and (pair? e) (string-contains-ci? (car e) f)))
                        items)])))

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

;;; The surface: over the bars, under the toasts.

(define (menu-box)
  (if *menu* (values 0 0 (screen-w) (screen-h)) (values 0 0 0 0)))

(define (menu-at x y) (and *menu* 'menu))

(define (menu-mouse kind what x y button clicks)
  (case kind
    [(down) (if (point-in-menu? x y) (menu-mouse-down x y) (close-menu!))]
    [(move) (menu-mouse-move x y)]))

(define (menu-takes kind value mods)
  (and *menu*
       (begin (if (eq? kind 'key) (menu-key value mods) (menu-text value))
              #t)))

(define-surface menu 30
  'draw 'draw-menu 'box 'menu-box 'at 'menu-at 'mouse 'menu-mouse
  'key 'menu-takes)

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

;; A window's menu: a right click on its title (the window manager calls
;; it) or on its taskbar button.
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
