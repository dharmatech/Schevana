;;; theme.ss — colours, metrics and fonts of the world, as live data.
;;;
;;; (theme 'accent)                 => #x7aa2f7
;;; (set-theme! 'accent #xff9e64)   recolours everything on the next frame
;;; (set-theme! 'ui-font-size 15)   fonts are theme keys too
;;; (apply-theme-preset! 'nord)     switch the whole palette
;;; (save-theme!)                   persist to *theme-file*, loaded at boot
;;;
;;; Anywhere a colour is expected you may pass a theme key symbol instead.

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
    panel #x111219  panel-text #xc0caf5  panel-dim #x6b7394  panel-hover #x24283b
    window-bg #x1a1b26  window-border #x30364f  window-border-focus #x4a5687
    title-bg #x1f2233   title-bg-focus #x262b40  title-text #xc0caf5  title-text-dim #x6b7394
    button-close #xf7768e  button-min #xe0af68  button-max #x9ece6a  button-idle #x3b4261))

(define default-metrics
  '(title-height 30  corner-radius 9  topbar-height 28  taskbar-height 38  shadow 14
    ui-font "sans-serif"  ui-font-size 13  mono-font "monospace"  mono-font-size 14
    window-style mac  preset tokyo-night))

(define default-theme (append default-colors default-metrics))
(define color-keys (plist-keys default-colors))
(define font-theme-keys '(ui-font ui-font-size mono-font mono-font-size))

(define (theme key)
  (let ([v (hashtable-ref *theme* key #f)])
    (or v (errorf 'theme "unknown theme key ~s" key))))

(define (theme-default key) (plist-ref default-theme key #f))

(define (theme-keys)
  (vector->list (vector-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                             (hashtable-keys *theme*))))

(define (theme-changed!)
  (set! *theme-version* (+ *theme-version* 1))
  (invalidate!))

(define (set-theme! key value)
  (hashtable-set! *theme* key value)
  (when (memq key font-theme-keys) (install-default-fonts!))
  (when (memq key '(topbar-height taskbar-height)) (wm-screen-resized!))
  (when (memq key '(window-style title-height)) (wm-chrome-changed!))
  (theme-changed!))

(define (set-theme-plist! plist)
  (let loop ([p plist])
    (unless (null? p)
      (hashtable-set! *theme* (car p) (cadr p))
      (loop (cddr p)))))

(define (reset-theme!)
  (set-theme-plist! default-theme)
  (install-default-fonts!)
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

;;; ------------------------------------------------------------------ presets
;;; A preset gives a base palette; the remaining keys are derived from it.

(define theme-presets
  '((tokyo-night "Tokyo Night" #f)
    (catppuccin "Catppuccin"
     (bg-0 #x181825 bg-1 #x1e1e2e bg-2 #x252537 bg-3 #x313244
      text #xcdd6f4 text-dim #xbac2de text-faint #x6c7086
      accent #x89b4fa accent-2 #xcba6f7 red #xf38ba8 orange #xfab387 yellow #xf9e2af
      green #xa6e3a1 cyan #x89dceb teal #x94e2d5 magenta #xf5c2e7
      desktop-top #x1e1e2e desktop-bottom #x302845 watermark #xffffff panel #x11111b))
    (nord "Nord"
     (bg-0 #x242933 bg-1 #x2e3440 bg-2 #x353c4a bg-3 #x3b4252
      text #xeceff4 text-dim #xd8dee9 text-faint #x6d7a96
      accent #x88c0d0 accent-2 #xb48ead red #xbf616a orange #xd08770 yellow #xebcb8b
      green #xa3be8c cyan #x8fbcbb teal #x8fbcbb magenta #xb48ead
      desktop-top #x252a34 desktop-bottom #x3a4660 watermark #xffffff panel #x1f232b))
    (gruvbox "Gruvbox"
     (bg-0 #x1d2021 bg-1 #x282828 bg-2 #x32302f bg-3 #x3c3836
      text #xebdbb2 text-dim #xd5c4a1 text-faint #x928374
      accent #x83a598 accent-2 #xd3869b red #xfb4934 orange #xfe8019 yellow #xfabd2f
      green #xb8bb26 cyan #x8ec07c teal #x8ec07c magenta #xd3869b
      desktop-top #x1d2021 desktop-bottom #x473c2e watermark #xfbf1c7 panel #x161819))
    (paper "Paper"
     (bg-0 #xfbfaf7 bg-1 #xf4f2ee bg-2 #xe9e6df bg-3 #xdcd7ce
      text #x2b2a33 text-dim #x4f4d5a text-faint #x8b8794
      accent #x3b6fd8 accent-2 #x8a4fd0 red #xd43f4d orange #xc96a12 yellow #xa87b00
      green #x3d8a3a cyan #x1f88a8 teal #x238a7a magenta #xb0399a
      desktop-top #xe9e5dd desktop-bottom #xc9d3e6 watermark #x000000 panel #xece9e3))
    ;; Amiga Workbench 3.1's four pens: grey, black, white and blue.
    (workbench "Workbench"
     (bg-0 #xaaaaaa bg-1 #xaaaaaa bg-2 #xa0a0a0 bg-3 #x959595
      text #x000000 text-dim #x1c1c1c text-faint #x4a4a4a
      accent #x6688bb accent-2 #x5a3e8e red #x9a1c1c orange #x9a4400 yellow #x6a5400
      green #x1c5a1c cyan #x0e4c6a teal #x0e564c magenta #x741c6a
      desktop-top #xaaaaaa desktop-bottom #xaaaaaa watermark #x000000 panel #xffffff))))

(define (preset-title name)
  (let ([p (assq name theme-presets)]) (if p (cadr p) (symbol->string name))))

(define (derive-colors base)
  (define (ref k) (plist-ref base k #f))
  (append base
          (list 'selection (mix (ref 'accent) (ref 'bg-1) 0.65)
                'panel-text (ref 'text) 'panel-dim (ref 'text-faint) 'panel-hover (ref 'bg-3)
                'window-bg (ref 'bg-1)
                'window-border (mix (ref 'bg-3) (ref 'text) 0.08)
                'window-border-focus (mix (ref 'bg-3) (ref 'accent) 0.45)
                'title-bg (ref 'bg-2) 'title-bg-focus (ref 'bg-3)
                'title-text (ref 'text) 'title-text-dim (ref 'text-faint)
                'button-close (ref 'red) 'button-min (ref 'yellow) 'button-max (ref 'green)
                'button-idle (mix (ref 'bg-3) (ref 'text) 0.15))))

;; The full colour plist of a preset.
(define (preset-colors name)
  (let ([p (assq name theme-presets)])
    (unless p (errorf 'preset-colors "no theme preset ~s" name))
    (if (caddr p) (derive-colors (caddr p)) default-colors)))

(define (apply-theme-preset! name)
  (set-theme-plist! (preset-colors name))
  (hashtable-set! *theme* 'preset name)
  (theme-changed!))

;; Change the accent along with the keys derived from it.
(define (set-accent! color)
  (hashtable-set! *theme* 'accent color)
  (hashtable-set! *theme* 'window-border-focus (mix (theme 'bg-3) color 0.45))
  (hashtable-set! *theme* 'selection (mix color (theme 'bg-1) 0.65))
  (theme-changed!))

;;; ------------------------------------------------------------------ persistence
;;; The theme file holds only the keys that differ from the defaults, as an
;;; association list. It is plain data: loading it never evaluates code.

(define (theme-overrides)
  (let loop ([p default-theme] [acc '()])
    (if (null? p)
        (reverse acc)
        (let ([v (hashtable-ref *theme* (car p) (cadr p))])
          (loop (cddr p) (if (equal? v (cadr p)) acc (cons (cons (car p) v) acc)))))))

(define (hex-color v)
  (let ([s (string-downcase (number->string v 16))])
    (string-append "#x" (make-string (max 0 (- 6 (string-length s))) #\0) s)))

(define (save-theme!)
  (let ([tmp (string-append *theme-file* ".tmp")])
    (when (file-exists? tmp) (delete-file tmp))
    (call-with-output-file tmp
      (lambda (p)
        (fprintf p ";; Schevana theme, written by the Settings app. Plain data; safe to edit.\n(")
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

(define (valid-theme-value? key v)
  (let ([d (theme-default key)])
    (cond
      [(memq key color-keys) (or (fixnum? v) (pair? v))]
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
                        (when (and (pair? kv) (memq (car kv) (plist-keys default-theme))
                                   (valid-theme-value? (car kv) (cdr kv)))
                          (hashtable-set! *theme* (car kv) (cdr kv))))
                      data))
          (install-default-fonts!)
          (wm-chrome-changed!)
          (theme-changed!))))))
