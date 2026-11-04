;;; desktop/theme-file.ss — the saved look (src/theme.ss's layers): the file
;;; holds the chosen style, the scale, the Overrides and your changes per
;;; style. It is plain data: loading it never evaluates code. A rule or a
;;; procedure typed at the REPL lasts the session. Every change to the
;;; layers saves it soon after, through the core's *theme-saver*; main.ss
;;; loads it before boot (load-saved-theme!).

(defvar *theme-file* (format "~a/.schevana-theme.ss" (or (getenv "HOME") ".")))

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
        (pairs p *theme-overrides*)
        (fprintf p ")\n (changes")
        (for-each (lambda (c)
                    (fprintf p "\n  (~s" (car c))
                    (pairs p (cdr c))
                    (fprintf p ")"))
                  *style-changes*)
        (fprintf p "))\n")))
    (rename-file tmp *theme-file*)
    *theme-file*))

(defvar *theme-save-pending* #f)

;; Saves 600 ms after a change, with the changes made meanwhile.
(define (save-theme-soon!)
  (unless *theme-save-pending*
    (set! *theme-save-pending* #t)
    (after 600 (lambda ()
                 (set! *theme-save-pending* #f)
                 (save-theme!)))))

;; A palette or style that is gone falls back, a wallpaper that is gone
;; draws as the default; a value of no kind its defaults have is dropped
;; (the built-in one, or a style's for its own options).
(define (valid-theme-value? key v)
  (let ([ds (map (lambda (p) (plist-ref p key #f))
                 (filter (lambda (p) (plist-has? p key))
                         (cons default-theme (map style-settings *styles*))))])
    (cond
      [(memq key color-keys)
       (or (fixnum? v) (stipple? v)
           (and (pair? v) (or (fixnum? (car v)) (symbol? (car v)))
                (real? (cdr v))))]
      [(eq? key 'palette) (and (assq v *palettes*) #t)]
      [else (exists (lambda (kind?) (and (kind? v) (exists kind? ds)))
                    (list fixnum? string? symbol? boolean? list?))])))

(define (registered-style s) (if (find-style s) s 'mac))

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
         [palette (if (assq (ref 'preset #f) *palettes*)
                      (ref 'preset #f)
                      'tokyo-night)]
         [kvs (valid-plist
               (cons (cons 'wallpaper (ref 'desktop-pattern 'gradient))
                     (remp (lambda (kv) (eq? (car kv) 'desktop-pattern))
                           alist)))]
         [derived? (lambda (k) (theme-rule? (theme-default k)))]
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
      (set! *theme-overrides* overrides)
      (set! *style-changes* changes)
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

(set! *theme-saver* (lambda () (save-theme-soon!)))
