;;; wallpaper.ss — the desktop under the windows. A wallpaper fills it
;;; from the theme's two desktop colors. Each is a file in wallpapers/,
;;; and the theme key 'wallpaper names the one in use:
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

;; The theme as it is now; theme-same? tells whether it still is, however
;; it changed meanwhile (set-theme!, or hashtable-set! behind its back).
(define (theme-snapshot)
  (let-values ([(keys vals) (hashtable-entries *theme*)]) (cons keys vals)))

(define (theme-same? snapshot)
  (let ([keys (car snapshot)] [vals (cdr snapshot)])
    (and (= (vector-length keys) (hashtable-size *theme*))
         (let loop ([i 0])
           (or (= i (vector-length keys))
               (and (equal? (hashtable-ref *theme* (vector-ref keys i) keys)
                            (vector-ref vals i))
                    (loop (+ i 1))))))))

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

;; The name of the wallpaper drawn for name: the default for one that is
;; gone, #f when that is gone too.
(define (wallpaper-shown name)
  (let ([wp (or (assq name *wallpapers*)
                (assq (theme-default 'wallpaper) *wallpapers*))])
    (and wp (car wp))))

;; The desktop's background in a rectangle: the wallpaper named pattern (a
;; value of the theme key 'wallpaper) from the colors top and bottom, or a
;; plain fill when none is there.
(define (fill-desktop-pattern x y w h pattern top bottom)
  (let ([wp (assq (wallpaper-shown pattern) *wallpapers*)])
    (if wp
        ((caddr wp) x y w h top bottom)
        (fill-rect x y w h top))))

(define-part (draw-wallpaper w h)
  (fill-desktop-pattern 0 0 w h (theme 'wallpaper) 'desktop-top 'desktop-bottom)
  (when (eq? (wallpaper-shown (theme 'wallpaper)) 'gradient)
    (fill-glow (* w 0.80) (* h 0.22) (* 0.55 (max w h)) (alpha 'accent-2 0.10))
    (fill-glow (* w 0.12) (* h 0.90) (* 0.50 (max w h)) (alpha 'accent 0.08)))
  (draw-text-centered 0 0 w (- h 60) "λ" (alpha 'watermark 0.05) 'huge)
  (draw-text-centered 0 (+ (/ h 2) 150) w 30 "S C H E V A N A"
                      (alpha 'watermark 0.10) 'ui-bold))

(define (desktop-mouse kind what x y button clicks)
  (when (eq? kind 'down)
    (case button
      [(right) (open-launcher! x y (cons x y))]   ; apps open where clicked
      [(left) (let ([old *focus*])
                (set! *focus* #f)
                (when old (window-call old 'blur))
                (request-frame!))])))

;; Before a frame: a stale picture makes it the whole screen. A new theme
;; repaints the app icon too (icon.ss), once things settle.
(define (desktop-tick)
  (when (and *dirty* (wallpaper-stale?)) (request-frame!))
  (unless (eqv? *icon-version* *theme-version*)
    (set! *icon-version* *theme-version*)
    (update-host-icon-soon!)))

(define-surface desktop -10
  'draw 'draw-desktop 'at (lambda (x y) 'desktop) 'mouse 'desktop-mouse
  'tick 'desktop-tick 'doc (lambda (what) '((L . "Unfocus") (R . "Apps"))))
