;;; theme.ss — how everything looks, as live data: each setting from the
;;; first layer that has it (your Overrides, your changes to the chosen
;;; style, the style, its palette, the built-in defaults), resolved into
;;; one table that (theme key) reads.
;;;
;;; (theme 'accent)                 => #x7aa2f7
;;; (set-theme! 'accent #xff9e64)   your change, remembered for this style
;;; (override! 'ui-font-size 15)    on top of every style that honors it
;;; (theme-source 'accent)          which layer gives it
;;;
;;; A rule is a setting computed from others when the table is built:
;;; 'selection (rule (mix 'accent 'bg-1 0.65)). Anywhere a color is
;;; expected you may pass a theme key symbol instead.

(defvar *theme* (make-eq-hashtable))   ; every setting in effect
(defvar *theme-version* 0)
(defvar *theme-file* (format "~a/.schevana-theme.ss" (or (getenv "HOME") ".")))

(defvar *style-name* 'mac)   ; the chosen style (a window style's name)
(defvar *changes* '())       ; ((style key value ...) ...): yours, per style
(defvar *overrides* '())     ; (key value ...): on top of every style
(defvar *scale-setting* 0)   ; the display's, in percent; 0 follows it

;; A setting computed from others when the theme is built.
(define-record-type (rule-box make-rule rule?)
  (nongenerative schevana-rule-1)
  (fields thunk))
(define-syntax rule
  (syntax-rules () [(_ e) (make-rule (lambda () e))]))

(define theme-unset (list 'unset))
(define theme-busy (list 'busy))

;; The built-in defaults, the bottom layer: Tokyo Night's colors, the
;; rest derived from them as every palette's are, and the metrics.
(define base-colors
  '(bg-0 #x13141c   bg-1 #x1a1b26   bg-2 #x1f2335   bg-3 #x292e42
    text #xc0caf5   text-dim #x9aa5ce  text-faint #x565f89
    accent #x7aa2f7 accent-2 #xbb9af7
    red #xf7768e    orange #xff9e64  yellow #xe0af68  green #x9ece6a
    cyan #x7dcfff   teal #x73daca    magenta #xc678dd
    desktop-top #x1b1d2e  desktop-bottom #x2b2140  watermark #xffffff
    panel #x111219))

;; (a palette may give any of these itself)
(define derived-colors
  (list 'selection (rule (mix 'accent 'bg-1 0.65))
        'panel-text (rule (theme 'text))  'panel-dim (rule (theme 'text-faint))
        'panel-hover (rule (theme 'bg-3))  'window-bg (rule (theme 'bg-1))
        'window-border (rule (mix 'bg-3 'text 0.08))
        'window-border-focus (rule (mix 'bg-3 'accent 0.45))
        'title-bg (rule (theme 'bg-2))  'title-bg-focus (rule (theme 'bg-3))
        'title-text (rule (theme 'text))
        'title-text-dim (rule (theme 'text-faint))
        'button-close (rule (theme 'red))  'button-min (rule (theme 'yellow))
        'button-max (rule (theme 'green))
        'button-idle (rule (mix 'bg-3 'text 0.15))))

(define default-theme
  (append base-colors derived-colors
          '(title-height 30  corner-radius 9  topbar-height 28
            taskbar-height 38
            shadow 14   ; shadow
            ui-font "sans-serif"  ui-font-size 13  mono-font "monospace"
            mono-font-size 14
            palette tokyo-night
            ;; a wallpaper's name: gradient, flat or shade
            wallpaper gradient)))

(define color-keys
  (append (plist-keys base-colors) (plist-keys derived-colors)))
(define font-theme-keys '(ui-font ui-font-size mono-font mono-font-size))

;; The look's knobs: a style honors those its 'options list (none: all);
;; one it does not list holds its own value whatever you change.
(define common-options
  '((palette "Palette" palette)  (accent "Accent" color)
    (wallpaper "Wallpaper" wallpaper)  (ui-font "Typeface" typeface)
    (ui-font-size "Text size" 9 36)  (mono-font "Code typeface" typeface)
    (mono-font-size "Code size" 9 36)  (corner-radius "Corner radius" 0 20)
    (title-height "Title bar" 22 44)
    (shadow "Window shadow" 0 32)   ; shadow
    (topbar-height "Top bar" 22 40)  (taskbar-height "Taskbar" 28 56)))

;; A rule still in the table is computed the first time it is read.
(define (theme key)
  (let ([v (hashtable-ref *theme* key theme-unset)])
    (cond [(eq? v theme-unset) (errorf 'theme "unknown theme key ~s" key)]
          [(rule? v) (compute-rule! key v)]
          [(eq? v theme-busy) (errorf 'theme "~s is computed from itself" key)]
          [else v])))

(define (compute-rule! key r)
  (hashtable-set! *theme* key theme-busy)
  (let ([v (guard (c [#t (hashtable-set! *theme* key r) (raise c)])
             ((rule-box-thunk r)))])
    (hashtable-set! *theme* key v)
    v))

;; The built-in value of key (a rule as it is).
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

;;; ------------------------------------------------------------------
;;; palettes
;;; A palette is named colors, a layer under the styles. Each is a file in
;;; themes/; the setting 'palette names the one in use:
;;;
;;;   (define-theme nord "Nord" '(bg-0 #x242933 ... panel #x1f232b))

;; ((name title plist) ...) in registration order
(defvar *themes* '())

(define (register-theme! name title plist)
  (set! *themes* (registry-add *themes* (list name title plist) car))
  (theme-changed!))

;; (define-theme name "Title" plist) adds a palette, or replaces the one
;; of that name. Its plist gives colors (and may give derived ones).
(define-syntax define-theme
  (syntax-rules ()
    [(_ name title) (register-theme! 'name title '())]
    [(_ name title plist) (register-theme! 'name title plist)]))

(define (themes) (map car *themes*))

(define (preset-title name)
  (let ([p (assq name *themes*)]) (if p (cadr p) (symbol->string name))))

;; (a world from before keeps #f for a palette of no colors until its
;; files register again)
(define (palette-colors name)
  (let ([p (assq name *themes*)])
    (let loop ([l (or (and p (caddr p)) '())])   ; old keys renamed
      (if (and (pair? l) (pair? (cdr l)))
          (cons* (new-key (car l)) (cadr l) (loop (cddr l)))
          '()))))

;; The colors and wallpaper the chosen style shows in palette name.
(define (preset-colors name)
  (let ([t (resolve-theme *style-name* '() (list 'palette name))])
    (fluid-let ([*theme* t])
      (apply append (map (lambda (k) (list k (theme k)))
                         (append color-keys '(wallpaper)))))))

;;; ------------------------------------------------------------------
;;; the layers

(define (changes-of name)
  (let ([p (assq name *changes*)]) (if p (cdr p) '())))

(define (plist-has? p key)
  (and (pair? p) (pair? (cdr p))
       (or (eq? (car p) key) (plist-has? (cddr p) key))))

(define (plist-remove p key)
  (cond [(or (null? p) (null? (cdr p))) '()]
        [(eq? (car p) key) (plist-remove (cddr p) key)]
        [else (cons* (car p) (cadr p) (plist-remove (cddr p) key))]))

;; The style and its parents, nearest first, each (name . settings).
;; Today's window styles have no settings of their own: each is its name,
;; which your changes are kept under.
(define (style-chain name) (list (cons name '())))

;; The nearest style setting of key in the chain.
(define (chain-ref chain key default)
  (let ([s (find (lambda (s) (plist-has? (cdr s) key)) chain)])
    (if s (plist-ref (cdr s) key default) default)))

(define meta-keys '(parent options places))

(define (honored? chain key)
  (or (not (assq key common-options))
      (let ([os (chain-ref chain 'options #f)])
        (or (not os)
            (exists (lambda (o) (eq? key (if (pair? o) (car o) o))) os)))))

;; Sets each key of p that ok? takes and t does not have yet.
(define (plist-into! t p ok?)
  (let loop ([p p])
    (when (and (pair? p) (pair? (cdr p)))
      (unless (or (hashtable-contains? t (car p)) (not (ok? (car p))))
        (hashtable-set! t (car p) (cadr p)))
      (loop (cddr p)))))

;; The theme of style name with these Overrides and changes: each setting
;; from the first layer that has it, the palette's colors under the
;; styles. Its rules are computed when read (theme-changed! reads them
;; all, so that a broken one fails at the change that broke it).
(define (resolve-theme name overrides changes)
  (let ([chain (style-chain name)] [t (make-eq-hashtable)])
    (define (all k) #t)
    (hashtable-set! t 'window-style name)
    (hashtable-set! t 'scale *scale-setting*)
    (plist-into! t overrides (lambda (k) (honored? chain k)))
    (plist-into! t changes (lambda (k) (honored? chain k)))
    (for-each (lambda (s) (plist-into! t (cdr s)
                                       (lambda (k) (not (memq k meta-keys)))))
              chain)
    (plist-into! t (list 'palette (theme-default 'palette)) all)
    (plist-into! t (palette-colors (hashtable-ref t 'palette #f)) all)
    (plist-into! t default-theme all)
    t))

(define (compute-rules! t)
  (fluid-let ([*theme* t]) (vector-for-each theme (hashtable-keys t))))

;; Whatever changed: build the theme again, then tell who cares. Fonts
;; follow their keys; a change not only of colors changes the chrome.
(define (theme-changed!)
  (let* ([old *theme*]
         [new (resolve-theme *style-name* *overrides*
                             (changes-of *style-name*))]
         [changed? (lambda (k)
                     (not (equal? (hashtable-ref old k theme-unset)
                                  (hashtable-ref new k theme-unset))))])
    (compute-rules! new)
    (set! *theme* new)
    (set! *theme-version* (+ *theme-version* 1))
    (when (exists changed? font-theme-keys) (install-default-fonts!))
    (when (changed? 'scale) (host-rescale!))
    (unless (for-all (lambda (k) (or (memq k color-keys) (not (changed? k))))
                     (append (vector->list (hashtable-keys old))
                             (vector->list (hashtable-keys new))))
      (wm-chrome-changed!))
    (invalidate!)))

;; Which layer gives key: (overrides v), (changes mac v), (style genera
;; v), (palette nord v) or (built-in v); #f if none.
(define (theme-source key)
  (let* ([name *style-name*] [chain (style-chain name)]
         [yours? (lambda (p) (and (honored? chain key) (plist-has? p key)))]
         [style (find (lambda (s) (plist-has? (cdr s) key)) chain)]
         [palette (theme 'palette)])
    (cond [(yours? *overrides*) (list 'overrides (theme key))]
          [(yours? (changes-of name)) (list 'changes name (theme key))]
          [style (list 'style (car style) (theme key))]
          [(plist-has? (palette-colors palette) key)
           (list 'palette palette (theme key))]
          [(plist-has? default-theme key) (list 'built-in (theme key))]
          [else #f])))

;;; ------------------------------------------------------------------
;;; changing settings

;; Changes the layers by thunk and builds the theme again; if that fails
;; (a broken rule, a bad value), the layers are as they were, and so is
;; the theme (a failure after the new table took over, in fonts, say).
(define (change-layers! thunk)
  (let ([s *style-name*] [c *changes*] [o *overrides*] [sc *scale-setting*])
    (guard (e [#t (set! *style-name* s) (set! *changes* c)
                  (set! *overrides* o) (set! *scale-setting* sc)
                  (guard (again [#t #f]) (theme-changed!))
                  (raise e)])
      (thunk)
      (theme-changed!))))

;; Old keys still work: preset is the palette, desktop-pattern the
;; wallpaper.
(define (new-key key)
  (case key [(preset) 'palette] [(desktop-pattern) 'wallpaper] [else key]))

(define (set-changes! name plist)
  (set! *changes* (let ([rest (remp (lambda (c) (eq? (car c) name))
                                    *changes*)])
                    (if (null? plist) rest (cons (cons name plist) rest)))))

;; The one switch: frames, palette, everything you changed for it.
(define (set-style! name)
  (unless (find-window-style name) (errorf 'set-style! "no style ~s" name))
  (change-layers! (lambda () (set! *style-name* name)))
  (save-theme-soon!))

;; Your change to the chosen style, remembered for it. window-style
;; still chooses a style, scale the display's scale.
(define (set-theme! key value)
  (case key
    [(window-style) (set-style! value)]
    [(scale) (set-scale! value)]
    [else
     (let ([key (new-key key)])
       (change-layers!
        (lambda ()
          (set-changes! *style-name*
                        (cons* key value
                               (plist-remove (changes-of *style-name*) key)))))
       (save-theme-soon!))]))

;; Forgets your changes to the chosen style: those keys, or all.
(define (reset-style! . keys)
  (change-layers!
   (lambda ()
     (set-changes! *style-name*
                   (if (null? keys)
                       '()
                       (fold-left plist-remove (changes-of *style-name*)
                                  (map new-key keys))))))
  (save-theme-soon!))

;; (override! key value) holds in every style that honors key;
;; (override! key) clears it. The style and the scale are not a style's.
(define (override! key . value)
  (when (memq key '(window-style scale))
    (errorf 'override! "~s is not a style's setting" key))
  (let ([key (new-key key)])
    (change-layers!
     (lambda ()
       (set! *overrides*
         (let ([rest (plist-remove *overrides* key)])
           (if (pair? value) (cons* key (car value) rest) rest))))))
  (save-theme-soon!))

(define (overrides) *overrides*)

;; The display's scale in percent, 0 to follow the desktop: not a style's.
(define (set-scale! pct)
  (change-layers! (lambda () (set! *scale-setting* pct)))
  (save-theme-soon!))

;; Old names, kept.
(define (set-theme-plist! plist)
  (for-each (lambda (k) (set-theme! k (plist-ref plist k #f)))
            (plist-keys plist)))

(define (apply-theme-preset! name)
  (unless (assq name *themes*)
    (errorf 'apply-theme-preset! "no theme ~s" name))
  (set-theme! 'palette name))

(define (set-accent! color) (set-theme! 'accent color))

;; Every style's changes and the Overrides go.
(define (reset-theme!)
  (change-layers! (lambda () (set! *changes* '()) (set! *overrides* '()))))

;;; ------------------------------------------------------------------
;;; persistence
;;; The file holds the chosen style, the scale, the Overrides and your
;;; changes per style. It is plain data: loading it never evaluates code.
;;; A rule or a procedure typed at the REPL lasts the session.

(define (hex-color v)
  (let ([s (string-downcase (number->string v 16))])
    (string-append "#x" (make-string (max 0 (- 6 (string-length s))) #\0) s)))

(define (plain-data? v)
  (or (number? v) (string? v) (symbol? v) (boolean? v) (null? v)
      (and (pair? v) (plain-data? (car v)) (plain-data? (cdr v)))))

(define (save-theme!)
  (let ([tmp (string-append *theme-file* ".tmp")])
    (define (pairs p plist)   ; (key . value) a line, colors in hex
      (for-each (lambda (k)
                  (let ([v (plist-ref plist k #f)])
                    (when (plain-data? v)
                      (if (and (memq k color-keys) (fixnum? v))
                          (fprintf p "\n   (~s . ~a)" k (hex-color v))
                          (fprintf p "\n   (~s . ~s)" k v)))))
                (plist-keys plist)))
    (when (file-exists? tmp) (delete-file tmp))
    (call-with-output-file tmp
      (lambda (p)
        (fprintf p ";; Schevana appearance, written by Settings. Plain data; \
                    safe to edit.\n((version . 2)\n (style . ~s)\n \
                    (scale . ~s)\n (overrides" *style-name* *scale-setting*)
        (pairs p *overrides*)
        (fprintf p ")\n (changes")
        (for-each (lambda (c)
                    (fprintf p "\n  (~s" (car c))
                    (pairs p (cdr c))
                    (fprintf p ")"))
                  *changes*)
        (fprintf p "))\n")))
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

;; A palette or style that is gone falls back, a wallpaper that is gone
;; draws as the default; a value that does not fit its key is dropped.
(define (valid-theme-value? key v)
  (let ([d (theme-default key)])
    (cond
      [(memq key color-keys)
       (or (fixnum? v)
           (and (pair? v) (or (fixnum? (car v)) (symbol? (car v)))
                (real? (cdr v))))]
      [(eq? key 'palette) (and (assq v *themes*) #t)]
      [(eq? key 'wallpaper) (symbol? v)]
      [(fixnum? d) (fixnum? v)]
      [else (and (string? d) (string? v))])))

(define (registered-style s) (if (find-window-style s) s 'mac))

;; The settings of ((key . value) ...) that fit their keys, as a plist.
(define (valid-plist alist)
  (if (list? alist)
      (apply append
             (filter-map (lambda (kv)
                           (and (pair? kv)
                                (valid-theme-value? (car kv) (cdr kv))
                                (list (car kv) (cdr kv))))
                         alist))
      '()))

;; An old file, one flat table: its style chosen (mac for one that is
;; gone), its preset that style's palette, and its keys that differ from
;; what the layers under them give, your changes to it; a color a rule
;; derives is one only where it differs from what the rule gives from
;; them. Its wallpaper counts even when left out (it saved no default),
;; or a palette's own would replace it. => (style key value ...)
(define (old-theme->changes alist)
  (let* ([ref (lambda (k d) (let ([p (assq k alist)]) (if p (cdr p) d)))]
         [style (registered-style (ref 'window-style 'mac))]
         [palette (if (assq (ref 'preset #f) *themes*)
                      (ref 'preset #f)
                      'tokyo-night)]
         [kvs (valid-plist
               (cons (cons 'wallpaper (ref 'desktop-pattern 'gradient))
                     (remp (lambda (kv) (eq? (car kv) 'desktop-pattern))
                           alist)))]
         [derived? (lambda (k) (rule? (theme-default k)))]
         ;; the keys of kvs that keep? takes and that differ from the
         ;; theme these changes give
         [differing
          (lambda (changes keep?)
            (fluid-let ([*theme* (resolve-theme style '() changes)])
              (let loop ([p kvs])
                (cond [(null? p) '()]
                      [(and (keep? (car p))
                            (not (equal? (cadr p) (theme (car p)))))
                       (cons* (car p) (cadr p) (loop (cddr p)))]
                      [else (loop (cddr p))]))))]
         [chosen (cons* 'palette palette
                        (differing (list 'palette palette)
                                   (lambda (k) (not (derived? k)))))])
    (cons style (append chosen (differing chosen derived?)))))

;; data: the file's, a list of pairs. Nothing changes until all of it is
;; read.
(define (load-theme-data! data)
  (let* ([ref (lambda (k d) (let ([p (assq k data)]) (if p (cdr p) d)))]
         [scale (let ([s (ref 'scale 0)]) (if (fixnum? s) s 0))])
    (define (set-layers! style overrides changes)
      (set! *style-name* style)
      (set! *overrides* overrides)
      (set! *changes* changes)
      (set! *scale-setting* scale))
    (if (eqv? (ref 'version 1) 2)
        (let ([cs (ref 'changes '())])
          (set-layers!
           (registered-style (ref 'style 'mac))
           (valid-plist (ref 'overrides '()))
           ;; a style that is gone keeps your changes for when it returns
           (if (list? cs)
               (filter-map (lambda (c)
                             (and (pair? c) (symbol? (car c))
                                  (cons (car c) (valid-plist (cdr c)))))
                           cs)
               '())))
        (let ([c (old-theme->changes data)])
          (set-layers! (car c) '() (list c))))))

(define (load-saved-theme!)
  (when (file-exists? *theme-file*)
    (safely 'theme
      (lambda ()
        (let ([data (call-with-input-file *theme-file* read)])
          (when (list? data)
            (change-layers!
             (lambda () (load-theme-data! (filter pair? data))))))))))

;; A world reloaded from before this file keeps its look: its one flat
;; table, less its old defaults (what it would have saved), loads as an
;; old file does. Then the theme until its first change, its rules
;; computed when read (no color can be mixed before gfx.ss loads).
(when (and (> (hashtable-size *theme*) 0)
           (not (hashtable-contains? *theme* 'palette)))
  (let ([old (if (top-level-bound? 'default-metrics)
                 (append (top-level-value 'default-colors)
                         (top-level-value 'default-metrics))
                 '())])
    (let-values ([(keys vals) (hashtable-entries *theme*)])
      (load-theme-data!
       (filter (lambda (kv)
                 (not (equal? (cdr kv) (plist-ref old (car kv) theme-unset))))
               (map cons (vector->list keys) (vector->list vals)))))))
(set! *theme*
  (resolve-theme *style-name* *overrides* (changes-of *style-name*)))
