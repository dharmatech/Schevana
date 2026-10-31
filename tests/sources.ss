;;; tests/sources.ss — guards against the one global world's collisions.
;;;
;;;   scheme --script tests/sources.ss
;;;
;;; Every line of every source file fits in 80 columns; no top-level name
;;; is defined in two files; src/ stays within its size budget; and every
;;; file in a folder registers what it defines. Fast: it loads the world
;;; but does not boot it.

(define *schevana-root*
  (let ([dir (path-parent (path-parent (car (command-line))))])
    (if (string=? dir "") "." dir)))

(load (string-append *schevana-root* "/load.ss"))

(define failures 0)

(define-syntax check
  (syntax-rules ()
    [(_ name expr)
     (let ([ok (guard (c [#t (printf "  error: ~a\n" (condition->string c))
                             #f])
                 expr)])
       (unless ok (set! failures (+ failures 1)))
       (printf "~a ~a\n" (if ok "ok  " "FAIL") name))]))

;; Prints the problems found, one per line; true when there are none.
(define (none? problems)
  (for-each (lambda (p) (printf "  ~a\n" p)) problems)
  (null? problems))

;; Its lines, as wc -l counts them.
(define (file-lines f)
  (let ([ls (reverse
             (string-split (call-with-input-file (source-path f) get-string-all)
                           #\newline))])
    (reverse (if (and (pair? ls) (string=? (car ls) "")) (cdr ls) ls))))

(define (file-forms f)
  (call-with-input-file (source-path f)
    (lambda (p)
      (let loop ([acc '()])
        (let ([x (read p)])
          (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))

;; "styles/mac.ss" -> "styles", "mac"
(define (file-folder f) (path-first f))
(define (file-stem f) (path-root (path-last f)))

;;; ------------------------------------------------------------------
;;; 80 columns

(define line-files
  (append (source-files)
          (map (lambda (f) (string-append "tests/" f))
               (sort string<? (directory-list (source-path "tests"))))
          '("load.ss" "main.ss" "schevana")))

(define (long-lines f)
  (let loop ([ls (file-lines f)] [n 1] [acc '()])
    (cond
      [(null? ls) (reverse acc)]
      [(> (string-length (car ls)) 80)
       (loop (cdr ls) (+ n 1)
             (cons (format "~a:~a has ~a characters"
                           f n (string-length (car ls)))
                   acc))]
      [else (loop (cdr ls) (+ n 1) acc)])))

(check (format "every line of the ~a source files is at most 80 characters"
               (length line-files))
       (none? (apply append (map long-lines line-files))))

;;; ------------------------------------------------------------------
;;; one name, one file

;; What a top-level form defines: ((kind . name) ...). Plain definitions
;; are of kind name; each definer's registry is a kind of its own, so a
;; style and a theme may share a name (Genera's do).
(define (defined form)
  (define (record-names spec fields)
    (let* ([type (if (pair? spec) (car spec) spec)]
           [s (symbol->string type)]
           [sym (lambda parts (string->symbol (apply string-append parts)))])
      (append (if (pair? spec)
                  spec
                  (list type (sym "make-" s) (sym s "?")))
              (apply append
                     (map (lambda (fd)
                            (let ([f (symbol->string
                                      (if (pair? fd) (cadr fd) fd))])
                              (if (and (pair? fd) (eq? (car fd) 'mutable))
                                  (list (sym s "-" f) (sym s "-" f "-set!"))
                                  (list (sym s "-" f)))))
                          fields)))))
  (define (names kind xs) (map (lambda (x) (cons kind x)) xs))
  (if (and (pair? form) (pair? (cdr form)))
      (let ([head (cadr form)])
        (case (car form)
          [(begin) (apply append (map defined (cdr form)))]
          [(define defvar define-syntax define-foreign)
           (names 'name (list (if (pair? head) (car head) head)))]
          [(define-app)
           (list (cons 'name (car head)) (cons 'app (car head)))]
          [(define-record-type)
           (let ([fields (find (lambda (c)
                                 (and (pair? c) (eq? (car c) 'fields)))
                               (cddr form))])
             (names 'name (record-names head (if fields (cdr fields) '()))))]
          [(define-window-style) (names 'style (list head))]
          [(define-theme) (names 'theme (list head))]
          [(define-wallpaper) (names 'wallpaper (list head))]
          [else '()]))
      '()))

(define (definitions f) (apply append (map defined (file-forms f))))

;; Names defined in two files on purpose: ((kind . name) reason) ...
(define defined-twice '())

(define world-files (append (source-files) '("load.ss")))

(check (format "no top-level name is defined in two of the world's ~a files"
               (length world-files))
       (let ([where (make-hashtable equal-hash equal?)])
         (for-each (lambda (f)
                     (for-each (lambda (d)
                                 (hashtable-update! where d
                                   (lambda (fs) (if (member f fs) fs
                                                    (cons f fs)))
                                   '()))
                               (definitions f)))
                   world-files)
         (none? (filter-map
                 (lambda (d)
                   (let ([fs (hashtable-ref where d '())])
                     (and (> (length fs) 1) (not (assoc d defined-twice))
                          (format "~a ~a: ~a" (car d) (cdr d)
                                  (reverse fs)))))
                 (vector->list (hashtable-keys where))))))

;;; ------------------------------------------------------------------
;;; the core's size

;; Raise it deliberately: the core is what the desktop needs to boot and
;; run; everything else belongs in a folder.
(define src-budget 4100)

(let ([n (apply + (map (lambda (f) (length (file-lines f)))
                       (map (lambda (f) (string-append "src/" f))
                            (directory-list (source-path "src")))))])
  (check (format "src/ is ~a lines, within its budget of ~a (to grow the \
                  core, raise src-budget in tests/sources.ss on purpose)"
                 n src-budget)
         (<= n src-budget)))

;;; ------------------------------------------------------------------
;;; folders

;; Each file in styles/, themes/ and wallpapers/ defines one of its kind,
;; named after the file; each in apps/ at least the app named after it.
(define folder-kinds
  '(("styles" . style) ("themes" . theme) ("wallpapers" . wallpaper)
    ("apps" . app)))

(define world-symbols (environment-symbols (interaction-environment)))

(define (registered? d)
  (case (car d)
    [(name) (and (memq (cdr d) world-symbols) #t)]
    [(app) (and (memq (cdr d) (apps)) #t)]
    [(style) (and (find-window-style (cdr d)) #t)]
    [(theme) (and (assq (cdr d) *themes*) #t)]
    [(wallpaper) (and (assq (cdr d) *wallpapers*) #t)]))

(define (folder-problems f)
  (let* ([ds (definitions f)]
         [stem (string->symbol (file-stem f))]
         [kind (cdr (or (assoc (file-folder f) folder-kinds) '(#f . #f)))]
         [own (filter (lambda (d) (eq? (car d) kind)) ds)])
    (append
     (map (lambda (d)
            (format "~a: ~a ~a is not registered" f (car d) (cdr d)))
          (remp registered? ds))
     (cond
       [(not kind)
        ;; a kit: its names start with its own (ui-, make-ui-state)
        (filter-map
         (lambda (d)
           (let* ([s (symbol->string (cdr d))]
                  [s (if (string-prefix? "make-" s)
                         (substring s 5 (string-length s))
                         s)]
                  [p (symbol->string stem)])
             (and (not (or (string=? s p)
                           (string-prefix? (string-append p "-") s)))
                  (format "~a: ~a lacks the kit's prefix ~a-" f (cdr d) p))))
         ds)]
       [(and (not (eq? kind 'app)) (not (= (length own) 1)))
        (list (format "~a defines ~a of kind ~a, not one" f (length own)
                      kind))]
       [(not (member (cons kind stem) own))
        (list (format "~a does not define the ~a ~a" f kind stem))]
       [else '()]))))

(check (format "every file in ~a registers what it defines"
               (string-join source-folders ", "))
       (none? (apply append (map folder-problems (folder-files)))))

;; What the files define, in load order, of one kind.
(define (defined-in-files kind)
  (map cdr (filter (lambda (d) (eq? (car d) kind))
                   (apply append (map definitions (folder-files))))))

(check "styles, themes, wallpapers and apps are listed in file-name order"
       (and (equal? (window-styles) (defined-in-files 'style))
            (equal? (themes) (defined-in-files 'theme))
            (equal? (wallpapers) (defined-in-files 'wallpaper))
            (equal? (apps) (defined-in-files 'app))))

(check "the REPL's docs read the folders too"
       (for-all (lambda (name) (hashtable-contains? (name-docs) name))
                '("ui-button" "mac-draw-under" "fill-shade" "settings")))

(check "the tests never load ~/.schevana.d"
       (and (not *user-folders?*) (not (exists path-absolute? (source-files)))))

(if (zero? failures)
    (printf "all checks passed\n")
    (printf "~a check(s) FAILED\n" failures))
(exit (if (zero? failures) 0 1))
