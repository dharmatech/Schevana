;;; theme.ss — colors, metrics and fonts of the world, as live data.
;;;
;;; (theme 'accent)                 => #x7aa2f7
;;; (set-theme! 'accent #xff9e64)   recolors everything on the next frame
;;; (set-theme! 'ui-font-size 15)   fonts are theme keys too
;;; (apply-theme-preset! 'nord)     switch the whole palette
;;; (save-theme!)                   persist to *theme-file*, loaded at boot
;;;
;;; Anywhere a color is expected you may pass a theme key symbol instead.

(defvar *theme* (make-eq-hashtable))
(defvar *theme-version* 0)
(defvar *theme-file* (format "~a/.schevana-theme.ss" (or (getenv "HOME") ".")))

;; The built-in palette (Tokyo Night).
(define default-colors
  '(bg-0 #x13141c   bg-1 #x1a1b26   bg-2 #x1f2335   bg-3 #x292e42
    text #xc0caf5   text-dim #x9aa5ce  text-faint #x565f89
    accent #x7aa2f7 accent-2 #xbb9af7
    red #xf7768e    orange #xff9e64  yellow #xe0af68  green #x9ece6a
    cyan #x7dcfff   teal #x73daca    magenta #xc678dd
    selection #x2e3c64
    desktop-top #x1b1d2e  desktop-bottom #x2b2140  watermark #xffffff
    panel #x111219  panel-text #xc0caf5  panel-dim #x6b7394
    panel-hover #x24283b
    window-bg #x1a1b26  window-border #x30364f  window-border-focus #x4a5687
    title-bg #x1f2233   title-bg-focus #x262b40  title-text #xc0caf5
    title-text-dim #x6b7394
    button-close #xf7768e  button-min #xe0af68  button-max #x9ece6a
    button-idle #x3b4261))

(define default-metrics
  '(title-height 30  corner-radius 9  topbar-height 28  taskbar-height 38
    shadow 14   ; shadow
    ui-font "sans-serif"  ui-font-size 13  mono-font "monospace"
    mono-font-size 14
    window-style mac  preset tokyo-night
    ;; a wallpaper's name: gradient, flat or shade
    desktop-pattern gradient
    scale 0))   ; percent; 0 follows the desktop

(define default-theme (append default-colors default-metrics))
(define color-keys (plist-keys default-colors))
(define font-theme-keys '(ui-font ui-font-size mono-font mono-font-size))

(define (theme key)
  (let ([v (hashtable-ref *theme* key #f)])
    (or v (errorf 'theme "unknown theme key ~s" key))))

(define (theme-default key) (plist-ref default-theme key #f))

(define (theme-keys)
  (vector->list
   (vector-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                (hashtable-keys *theme*))))

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

(define (theme-changed!)
  (set! *theme-version* (+ *theme-version* 1))
  (update-host-icon-soon!)   ; the window icon uses the theme's colors
  (invalidate!))

(define (set-theme! key value)
  (hashtable-set! *theme* key value)
  (when (memq key font-theme-keys) (install-default-fonts!))
  (cond
    ;; the fonts set how tall the title bar and the panels must be
    [(or (memq key font-theme-keys) (memq key '(window-style title-height)))
     (wm-chrome-changed!)]
    [(memq key '(topbar-height taskbar-height)) (wm-screen-resized!)]
    [(eq? key 'scale) (host-rescale!)])
  (theme-changed!))

(define (set-theme-plist! plist)
  (let loop ([p plist])
    (unless (null? p)
      (hashtable-set! *theme* (car p) (cadr p))
      (loop (cddr p)))))

(define (reset-theme!)
  (set-theme-plist! default-theme)
  (install-default-fonts!)
  (host-rescale!)
  (wm-chrome-changed!)
  (theme-changed!))

;; Fill in keys that are missing, e.g. after (reload!) added new ones.
(define (ensure-theme-defaults!)
  (let loop ([p default-theme])
    (unless (null? p)
      (unless (hashtable-contains? *theme* (car p))
        (hashtable-set! *theme* (car p) (cadr p)))
      (loop (cddr p)))))

(ensure-theme-defaults!)

;;; ------------------------------------------------------------------ themes
;;; A theme (a palette) gives the base colors; the remaining keys are
;;; derived from them. Each is a file in themes/:
;;;
;;;   (define-theme nord "Nord" '(bg-0 #x242933 ... panel #x1f232b))
;;;
;;; A theme with no palette is default-colors. The theme key 'preset names
;;; the one applied last.

;; ((name title palette-or-#f) ...) in registration order
(defvar *themes* '())

(define (register-theme! name title palette)
  (set! *themes* (registry-add *themes* (list name title palette) car))
  (request-frame!))

;; (define-theme name "Title" palette) adds a theme, or replaces the one of
;; that name. The palette is a property list of theme keys.
(define-syntax define-theme
  (syntax-rules ()
    [(_ name title) (register-theme! 'name title #f)]
    [(_ name title palette) (register-theme! 'name title palette)]))

(define (themes) (map car *themes*))

(define (preset-title name)
  (let ([p (assq name *themes*)]) (if p (cadr p) (symbol->string name))))

;; A preset may also give any of the derived keys itself.
(define (derive-colors base)
  (define (ref k) (plist-ref base k #f))
  (define (unless-given p)
    (cond [(null? p) '()]
          [(ref (car p)) (unless-given (cddr p))]
          [else (cons* (car p) (cadr p) (unless-given (cddr p)))]))
  (append base
          (unless-given
           (list 'selection (mix (ref 'accent) (ref 'bg-1) 0.65)
                'panel-text (ref 'text) 'panel-dim (ref 'text-faint)
                'panel-hover (ref 'bg-3)
                'window-bg (ref 'bg-1)
                'window-border (mix (ref 'bg-3) (ref 'text) 0.08)
                'window-border-focus (mix (ref 'bg-3) (ref 'accent) 0.45)
                'title-bg (ref 'bg-2) 'title-bg-focus (ref 'bg-3)
                'title-text (ref 'text) 'title-text-dim (ref 'text-faint)
                'button-close (ref 'red) 'button-min (ref 'yellow)
                'button-max (ref 'green)
                'button-idle (mix (ref 'bg-3) (ref 'text) 0.15)))))

;; The full color plist of a theme. One with no palette, or one not
;; registered (there may be no theme files at all), is default-colors.
(define (preset-colors name)
  (let ([p (assq name *themes*)])
    (if (and p (caddr p)) (derive-colors (caddr p)) default-colors)))

(define (apply-theme-preset! name)
  (unless (assq name *themes*)
    (errorf 'apply-theme-preset! "no theme ~s" name))
  ;; unless the preset sets it
  (hashtable-set! *theme* 'desktop-pattern (theme-default 'desktop-pattern))
  (set-theme-plist! (preset-colors name))
  (hashtable-set! *theme* 'preset name)
  (theme-changed!))

;; Change the accent along with the keys derived from it.
(define (set-accent! color)
  (hashtable-set! *theme* 'accent color)
  (hashtable-set! *theme* 'window-border-focus (mix (theme 'bg-3) color 0.45))
  (hashtable-set! *theme* 'selection (mix color (theme 'bg-1) 0.65))
  (theme-changed!))

;;; ------------------------------------------------------------------
;;; persistence
;;; The theme file holds only the keys that differ from the defaults, as an
;;; association list. It is plain data: loading it never evaluates code.

(define (theme-overrides)
  (let loop ([p default-theme] [acc '()])
    (if (null? p)
        (reverse acc)
        (let ([v (hashtable-ref *theme* (car p) (cadr p))])
          (loop (cddr p)
                (if (equal? v (cadr p)) acc (cons (cons (car p) v) acc)))))))

(define (hex-color v)
  (let ([s (string-downcase (number->string v 16))])
    (string-append "#x" (make-string (max 0 (- 6 (string-length s))) #\0) s)))

(define (save-theme!)
  (let ([tmp (string-append *theme-file* ".tmp")])
    (when (file-exists? tmp) (delete-file tmp))
    (call-with-output-file tmp
      (lambda (p)
        (fprintf p ";; Schevana theme, written by the Settings app. \
                    Plain data; safe to edit.\n(")
        (let loop ([os (theme-overrides)] [first? #t])
          (unless (null? os)
            (let ([k (caar os)] [v (cdar os)])
              (unless first? (fprintf p "\n "))
              (if (and (memq k color-keys) (fixnum? v))
                  (fprintf p "(~s . ~a)" k (hex-color v))
                  (fprintf p "(~s . ~s)" k v)))
            (loop (cdr os) #f)))
        (fprintf p ")\n")))
    (rename-file tmp *theme-file*)
    *theme-file*))

(defvar *theme-save-pending* #f)

;; Save shortly after the last change, so dragging a slider writes once.
(define (save-theme-soon!)
  (unless *theme-save-pending*
    (set! *theme-save-pending* #t)
    (after 600 (lambda ()
                 (set! *theme-save-pending* #f)
                 (save-theme!)))))

;; Names must be registered: a theme file naming a palette, window style
;; or wallpaper that is gone falls back to the default for that key.
(define (valid-theme-value? key v)
  (let ([d (theme-default key)])
    (cond
      [(memq key color-keys) (or (fixnum? v) (pair? v))]
      [(eq? key 'preset) (and (assq v *themes*) #t)]
      [(eq? key 'window-style) (and (symbol? v) (find-window-style v) #t)]
      [(eq? key 'desktop-pattern) (and (assq v *wallpapers*) #t)]
      [(fixnum? d) (fixnum? v)]
      [(string? d) (string? v)]
      [(symbol? d) (symbol? v)]
      [else #f])))

(define (load-saved-theme!)
  (when (file-exists? *theme-file*)
    (safely 'theme
      (lambda ()
        (let ([data (call-with-input-file *theme-file* read)])
          (when (list? data)
            (for-each (lambda (kv)
                        (when (and (pair? kv)
                                   (memq (car kv) (plist-keys default-theme))
                                   (valid-theme-value? (car kv) (cdr kv)))
                          (hashtable-set! *theme* (car kv) (cdr kv))))
                      data))
          (install-default-fonts!)
          (host-rescale!)
          (wm-chrome-changed!)
          (theme-changed!))))))
