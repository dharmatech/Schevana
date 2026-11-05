;;; apps/envs.ss — Environments: the tree of environments (services/env.ss),
;;; what each holds, and what can be done with it.
;;;
;;; Left, the tree (the line beside it drags, and a double click puts it
;;; back): each environment's icon and name, its kind, how many names it
;;; was shared and holds of its own, and its thread's jobs.
;;; Right, the one selected: what it is (its kind, base, parent and
;;; children, what it forks, its thread), then its names: those shared
;;; with it, each with the parent's name it forwards to, whether its set!
;;; goes through a setter and where it is bound; or its own. The toolbar
;;; acts on the one selected, as the menu of a right press on a row does:
;;;   Fork…    a child of a kind its fork makes, named, shared names
;;;   REPL     a REPL in it (Return, a double click)
;;;   Share…   more of its parent's names
;;;   Load…    a file evaluated in it (env-load), picked in the desktop's
;;;            file dialog, which Environments waits on
;;;   Drop…    it, its children and their threads
;;; The pool's threads, /worklets', are set here too. Keys, with no field
;;; focused: arrows move in the tree, Return opens a REPL, F forks, /
;;; filters, Delete drops.

(define-record-type envs-view
  (nongenerative schevana-envs-view-2)
  (fields ui
          (mutable selected)     ; the environment shown
          (mutable query)        ; the tree's filter
          (mutable names)        ; shared or own: what the names show
          (mutable names-query)  ; their filter
          (mutable sheet)        ; #f, or fork, share, load or drop
          (mutable kind)         ; the fork's kind
          (mutable name)         ; its name, as typed
          (mutable shares)       ; the fork's or share's chips
          (mutable draft)        ; the chip being typed
          (mutable folder)       ; where Load… last picked a file, or #f
          (mutable repl?)        ; a REPL in the child, once forked (off
                                 ; each time the sheet opens)
          (mutable error)        ; what the sheet's last try raised, or #f
          (mutable memo)         ; (key . value): the last suggestions
          facts))                ; env -> (ms shared own), a second's

;;; ------------------------------------------------------------------
;;; what an environment is

(define (envs-label e)
  (if (env-parent e) (symbol->string (env-name e)) "/"))

;; What the kind means, under its title.
(define (envs-kind-doc e)
  (case (env-kind e)
    [(root) "The world itself: every name of Schevana's and of Chez. What \
             is defined here changes the running world."]
    [(worklets) "Where the worklets live (define-worklet); the pool's \
                 threads run them."]
    [(#f) (format "Made by code (env-child): ~a"
                  (if (eq? (env-base e) 'worklet)
                      "Scheme's pure core, and what is shared with it"
                      "all of Chez, and what is shared with it"))]
    [else (env-kind-ref (env-kind e) 'doc "")]))

(define (envs-base-words e)
  (case (env-base e)
    [(chez) "chez: all of Chez Scheme"]
    [(worklet) "worklet: Scheme's pure core, no files, C or threads"]
    [else "The world's own"]))

;; The names e was shared and holds of its own, and those of the world's
;; its next sync brings, kept for a second: (values shared own new), own #f
;; for a vat's sandbox, its thread's to read.
(define (envs-facts v e)
  (let ([f (hashtable-ref (envs-view-facts v) e #f)])
    (if (and f (< (- (now-ms) (car f)) 1000))
        (apply values (cdr f))
        (let* ([vat? (and (vat-of e) #t)]
               [shared (env-shared-names e (not vat?))]
               [own (and (not vat?) (env-own-names e))]
               [new (env-unsynced e)])
          (hashtable-set! (envs-view-facts v) e
                          (list (now-ms) shared own new))
          (values shared own new)))))

;; Where a name e was shared is bound: the path, as env-holder finds it,
;; but in a vat's sandbox from its parent on.
(define (envs-holder e name target)
  (let-values ([(h n) (if (vat-of e)
                          (env-holder (env-parent e) target)
                          (env-holder e name))])
    (env-path h)))

;; Every environment, depth first.
(define (envs-all)
  (let walk ([e (env-root)])
    (cons e (apply append (map walk (env-children e))))))

;;; ------------------------------------------------------------------
;;; what can be done with it

;; Why what (fork, repl, share, load, drop) is not for e, or #f when it
;; is.
(define (envs-why-not e what)
  (let ([vat? (and (vat-of e) #t)] [kind (env-kind e)])
    (case what
      [(fork) (and (null? (env-forks e))
                   (format "~a forks nothing: the world, world namespaces \
                            and sandboxes fork" (env-path e)))]
      [(repl) (and (eq? kind 'worklets)
                   "No REPL in /worklets: the pool's threads run there")]
      [(share load)
       (cond [(eq? kind 'worklets)
              "/worklets holds worklets only: define-worklet puts them there"]
             [vat? "Its sandbox is its vat's: share and load before its \
                    thread starts"]
             [(and (eq? what 'share) (not (env-parent e)))
              "The world has no parent to share with it"]
             [else #f])]
      [(drop) (cond [(not (env-parent e)) "The world is not dropped"]
                    [(eq? kind 'worklets)
                     "/worklets is not dropped: the worklets live there"]
                    [else #f])])))

;; The actions on e: ((what label glyph tip) ...), in the toolbar's order.
(define envs-actions
  '((fork "Fork…" plus "A child of a kind this environment forks  F")
    (repl "REPL" prompt "A REPL in this environment  Return")
    (share "Share…" #f "More of its parent's names, shared with it")
    (load "Load…" #f "A file's forms, evaluated in it")
    (drop "Drop…" #f "It, its children and their threads  Delete")))

(define (envs-act! v what)
  (let ([e (envs-view-selected v)])
    (unless (envs-why-not e what)
      (case what
        [(repl) (repl e)]
        [(fork) (envs-open-fork! v (car (env-forks e)))]
        [(load) (envs-open-load! v)]
        [else (envs-open-sheet! v what)]))))

;; The file dialog, over Environments, for a file to load into the one
;; selected, where the last was picked.
(define (envs-open-load! v)
  (let ([e (envs-view-selected v)])
    (open-file-dialog (current-window) (format "Load into ~a" (env-path e))
                      "Load"
                      (lambda (path)
                        (envs-view-folder-set! v (path-parent path))
                        (envs-load! v e path))
                      'folder (or (envs-view-folder v) (current-directory))
                      'types '(("Scheme files" "ss" "sls" "scm")
                               ("All files")))))

(define (envs-open-sheet! v what)
  (envs-view-sheet-set! v what)
  (envs-view-shares-set! v '())
  (envs-view-draft-set! v "")
  (envs-view-error-set! v #f)
  (ui-focus! (envs-view-ui v)
             (case what
               [(share) (list '(envs-sheet shares) 'field)]
               [else #f])))

(define (envs-open-fork! v kind)
  (envs-open-sheet! v 'fork)
  (envs-view-kind-set! v kind)
  (envs-view-name-set! v "")
  (envs-view-repl?-set! v #f)
  (ui-focus! (envs-view-ui v) '(envs-sheet name)))

;; value-of's value, kept while key stays the same: what a field suggests,
;; asked at every draw.
(define (envs-memo v key value-of)
  (let ([m (envs-view-memo v)])
    (if (equal? (car m) key)
        (cdr m)
        (let ([value (value-of)])
          (envs-view-memo-set! v (cons key value))
          value))))

(define (envs-select! v e)
  (envs-view-selected-set! v e)
  (envs-view-names-query-set! v "")
  (ui-forget! (envs-view-ui v) 'inspector))

;; A share as typed: "name", "name as alias", either then "via setter".
;; => (values share setter): share a name or (alias . name), setter #f or
;; a name; #f when it reads as none of these.
(define (envs-share-words text)
  (let ([words (remp (lambda (w) (string=? w ""))
                     (string-split text #\space))])
    (define (sym s) (string->symbol s))
    (cond
      [(= (length words) 1) (values (sym (car words)) #f)]
      [(and (= (length words) 3) (string=? (cadr words) "as"))
       (values (cons (sym (caddr words)) (sym (car words))) #f)]
      [(and (= (length words) 3) (string=? (cadr words) "via"))
       (values (sym (car words)) (sym (caddr words)))]
      [(and (= (length words) 5) (string=? (cadr words) "as")
            (string=? (cadddr words) "via"))
       (values (cons (sym (caddr words)) (sym (car words)))
               (sym (list-ref words 4)))]
      [else (values #f #f)])))

;; The chips, and the draft being typed, as fork and env-share! take them:
;; (values shares setters).
(define (envs-sheet-shares v)
  (envs-shares (append (envs-view-shares v)
                       (let ([d (envs-trim (envs-view-draft v))])
                         (if (string=? d "") '() (list d))))))

(define (envs-shares chips)
  (let loop ([cs chips] [shares '()] [setters '()])
    (if (null? cs)
        (values (reverse shares) (reverse setters))
        (let-values ([(share setter) (envs-share-words (car cs))])
          (cond
            [(not share)
             (errorf 'share "~s reads as no share: a name, “name as alias”, \
                             either then “via setter”" (car cs))]
            [setter (loop (cdr cs) shares (cons (cons share setter) setters))]
            [else (loop (cdr cs) (cons share shares) setters)])))))

;; The fork sheet's Fork: a child of the selected environment, through
;; the same rules its own fork keeps.
(define (envs-fork! v)
  (let ([e (envs-view-selected v)] [kind (envs-view-kind v)]
        [typed (envs-trim (envs-view-name v))])
    (guard (c [#t (envs-view-error-set! v (condition->string c))])
      (let-values ([(shares setters) (envs-sheet-shares v)])
        (let ([c (apply env-fork e kind
                        (append (if (string=? typed "")
                                    '()
                                    (list 'name (string->symbol typed)))
                                (list 'share shares 'setters setters)))])
          (envs-view-sheet-set! v #f)
          (envs-select! v c)
          (when (and (envs-view-repl? v) (not (envs-why-not c 'repl)))
            (repl c)))))))

(define (envs-share! v)
  (let ([e (envs-view-selected v)])
    (guard (c [#t (envs-view-error-set! v (condition->string c))])
      (let-values ([(shares setters) (envs-sheet-shares v)])
        (env-shares-check 'share (env-parent e) shares setters)
        (env-share! e shares)
        (for-each (lambda (s) (env-share-setter! e (car s) (cdr s))) setters)
        (hashtable-delete! (envs-view-facts v) e)
        (envs-view-sheet-set! v #f)))))

;; The file's forms evaluated in e, in a process; a toast says how it
;; went.
(define (envs-load! v e path)
  (spawn "load"
         (lambda () (env-load e path))
         (lambda (status value)
           (hashtable-delete! (envs-view-facts v) e)
           (case status
             [(error) (report-error! 'env-load value)]
             [(killed) (notify (format "Loading ~a stopped" path))]
             [else (notify (format "Loaded ~a into ~a" path
                                   (env-path e)))]))))

(define (envs-drop! v)
  (let ([e (envs-view-selected v)])
    (envs-view-sheet-set! v #f)
    (envs-select! v (env-parent e))
    (vat-drop-env! e)))

(define (envs-trim s)
  (let loop ([a 0] [b (string-length s)])
    (cond [(and (< a b) (char-whitespace? (string-ref s a))) (loop (+ a 1) b)]
          [(and (< a b) (char-whitespace? (string-ref s (- b 1))))
           (loop a (- b 1))]
          [else (substring s a b)])))

;;; ------------------------------------------------------------------
;;; drawing: the icons and the tree's cells

(define (envs-draw-icon e x y s flags)
  (ui-draw-env-icon x y s flags (vat-env-kind e)))

;; The tree's first cell: the icon under the heading's first letter, then
;; the name.
(define (envs-draw-name-cell e x y w h flags)
  (let* ([gs (ui-glyph-size)] [in (ui-table-cell-inset)] [gap 6])
    (envs-draw-icon e (+ x in) (+ y (/ (- h gs) 2)) gs '())
    (ui-draw-label (+ x in gs gap) y (max 0 (- w in gs gap)) h flags
                   (envs-label e))))

;; The inspector's head: a big icon, the name, the path and kind.
(define (envs-draw-head e x y w)
  (let* ([s (* 2 (ui-glyph-size))] [hh (font-line-height 'heading)]
         [tx (+ x s 12)])
    (envs-draw-icon e x (+ y 2) s '())
    (ui-draw-label tx y (- w s 12) hh '(heading) (envs-label e))
    (ui-draw-label tx (+ y hh 2) (- w s 12) (font-line-height 'ui-small)
                   '(hint)
                   (if (env-parent e)
                       (format "~a  ·  ~a" (env-path e) (env-title e))
                       (env-title e)))
    (+ y (max s (+ hh 2 (font-line-height 'ui-small))))))

;;; ------------------------------------------------------------------
;;; the tree

;; The tree's columns in room: the kind as wide as the widest title, the
;; counts as their headings, the environment the rest; as the room runs
;; out, Jobs, Own, Shared and Kind leave, in that order.
(define (envs-columns room)
  (let* ([in (* 2 (ui-table-cell-inset))]
         [kw (exact (ceiling (+ in 4 (text-widest
                                      (map env-kind-title
                                           (cons* 'root 'worklets (env-kinds)))
                                      'ui))))]
         ;; a heading keeps room for a sort mark
         [nw (exact (ceiling (+ in (ui-small-glyph-size) 4
                                (text-widest '("Shared" "Own" "Jobs")
                                             'ui-small))))])
    (ui-table-fit room
                  `((env "Environment" #f)
                    (kind "Kind" ,kw)
                    (shared "Shared" ,nw right
                            "Names its parent shared with it")
                    (own "Own" ,nw right "Names it defined itself")
                    (jobs "Jobs" ,nw right
                          "Its thread's jobs, waiting or running"))
                  '(jobs own shared kind)
                  (* 9 (font-line-height 'ui)))))

(define (envs-matches? v e)
  (let ([q (envs-trim (envs-view-query v))])
    (or (string=? q "")
        (string-contains-ci? (env-path e) q)
        (string-contains-ci? (env-title e) q))))

(define (envs-tree v x y w h)
  (let ([ui (envs-view-ui v)] [pad (ui-pad)])
    (ui-draw-surface (+ x pad) (+ y pad) (- w pad pad) (- h pad pad) '(well))
    (ui-tree ui 'tree (+ x pad) (+ y pad 4) (- w pad pad) (- h pad pad 8)
             (envs-columns (ui-table-room (- w pad pad)))
             (list (env-root)) env-children (envs-view-selected v)
             (lambda (e) (envs-select! v e))
             (lambda (e key cx cy cw ch flags)
               (if (eq? key 'env)
                   (envs-draw-name-cell e cx cy cw ch flags)
                   (ui-draw-cell cx cy cw ch flags (envs-cell-text v e key))))
             'expanded #t 'generation (env-generation)
             'dim (lambda (e) (not (envs-matches? v e)))
             'on-open (lambda (e) (envs-act! v 'repl))
             'on-menu (lambda (e mx my)
                        (envs-select! v e)
                        (envs-menu! v mx my))
             'tip (lambda (e cw) (env-path e)))))

(define (envs-cell-text v e key)
  (case key
    [(kind) (env-title e)]
    [(shared own)
     (let-values ([(shared own new) (envs-facts v e)])
       (let ([l (if (eq? key 'shared) shared own)])
         (cond [(and (eq? key 'shared) (not (env-parent e))) ""]
               [l (number->string (length l))]
               [else "—"])))]
    [(jobs) (let ([vat (vat-of e)])
              (if vat (number->string (length (vat-jobs-of vat))) ""))]
    [else ""]))

;; The menu of a right press: what can be done with the one selected.
(define (envs-menu! v x y)
  (let ([e (envs-view-selected v)])
    (ui-open-menu! (envs-view-ui v) x y
                   (filter-map (lambda (a)
                                 (and (not (envs-why-not e (car a)))
                                      (cons (cadr a)
                                            (lambda () (envs-act! v (car a))))))
                               envs-actions))))

;;; ------------------------------------------------------------------
;;; the inspector

;; The selected environment's facts, as form rows.
(define (envs-fact-rows v e)
  (let* ([ui (envs-view-ui v)] [ch (ui-control-height)]
         [vat (vat-of e)]
         [chips-row
          (lambda (label key labels press . opts)
            (list label
                  (lambda (w) (ui-chips-height w labels #f #f))
                  (lambda (x y w)
                    (apply ui-chips ui (list 'envs key) x y w labels press
                           opts))))]
         [top (/ (- ch (font-line-height 'ui)) 2)]   ; a line in a control
         [words-row (lambda (label words . opts)
                      (cons* label
                             (lambda (w)
                               (let-values ([(lw lh) (ui-label-size
                                                      '(wrap) words w)])
                                 (max ch (+ top lh))))
                             (lambda (x y w)
                               (ui-draw-label x (+ y top) w 0 '(wrap) words))
                             opts))]
         [children (env-children e)] [forks (env-forks e)])
    (append
     (list (words-row "Kind" (env-title e) 'hint (envs-kind-doc e))
           (words-row "Base" (envs-base-words e)))
     (if (env-parent e)
         (list (chips-row "Parent" 'parent (list (envs-label (env-parent e)))
                          (lambda (i) (envs-select! v (env-parent e)))
                          'tip (lambda (i) (env-path (env-parent e)))))
         '())
     (list (chips-row "Children" 'children (map envs-label children)
                      (lambda (i) (envs-select! v (list-ref children i)))
                      'tip (lambda (i) (env-path (list-ref children i))))
           (chips-row "Forks" 'forks (map env-kind-title forks)
                      (lambda (i) (envs-open-fork! v (list-ref forks i)))
                      'empty "Nothing"
                      'tip (lambda (i) (format "Fork a ~a"
                                               (env-kind-title
                                                (list-ref forks i))))))
     (if (env-world? e) (list (envs-follows-row v e)) '())
     (cond
       [(eq? (env-kind e) 'worklets) (envs-pool-rows v)]
       [vat (envs-vat-rows v vat)]
       [else '()]))))

;; A world namespace's: Sync, and whether the world gained names since
;; its last, which come at its next evaluation, or now.
(define (envs-follows-row v e)
  (let*-values ([(ui) (envs-view-ui v)] [(ch) (ui-control-height)]
                [(shared own new) (envs-facts v e)] [(n) (length new)]
                [(bw bh) (ui-button-size "Sync" #f)] [(gap) (ui-gap)])
    (list "Follows" ch
          (lambda (x y w)
            (ui-button ui '(envs sync) x y bw ch "Sync"
                       (lambda ()
                         (env-sync! e)
                         (hashtable-delete! (envs-view-facts v) e))
                       'disabled (= n 0)
                       'tip "Share it the names the world gained since: \
                             (env-sync! e)")
            (ui-draw-label (+ x bw gap) y (max 0 (- w bw gap)) ch
                           (if (= n 0) '(dim) '())
                           (if (= n 0)
                               "Up to date with the world"
                               (format "~a name~a of the world's not here yet"
                                       n (if (= n 1) "" "s")))))
          'hint "Every name of the world's, those it gains later too: at \
                 its next evaluation, or now with Sync")))

;; /worklets: the pool's threads, and its jobs.
(define (envs-pool-rows v)
  (let ([ui (envs-view-ui v)] [ch (ui-control-height)])
    (list (list "Pool" ch
                (lambda (x y w)
                  (ui-stepper ui '(envs pool) x y (vat-pool-size) 1 64
                              (lambda (n) (vat-pool-size-set! n))
                              'tip "How many threads run the pool's \
                                    worklets: (vat-pool-size-set! n)"))
                'hint (let ([v (vat-of (env-worklets))])
                        (if v
                            (format "~a running, ~a jobs"
                                    (vat-threads-of v)
                                    (length (vat-jobs-of v)))
                            "Not started: it starts with its first job"))))))

;; A thread's vat: its threads, and its jobs, each a chip whose × kills
;; it.
(define (envs-vat-rows v vat)
  (let* ([ui (envs-view-ui v)] [jobs (vat-jobs-of vat)]
         [labels (map (lambda (j)
                        (let ([w (vat-job-window j)])
                          (format "~a~a" (vat-job-kind j)
                                  (if w (format " · ~a" (window-title w)) ""))))
                      jobs)])
    (list (list "Thread" (ui-control-height)
                (lambda (x y w)
                  (ui-draw-label x y w (ui-control-height) '()
                                 (format "~a thread, its vat's"
                                         (vat-threads-of vat)))))
          (list "Jobs" (lambda (w) (ui-chips-height w labels #t #f))
                (lambda (x y w)
                  (ui-chips ui '(envs jobs) x y w labels #f
                            'on-remove (lambda (i)
                                         (vat-job-kill! (list-ref jobs i)))
                            'empty "None waiting"))))))

(define (envs-names-columns which room)
  (let ([third (round (/ room 3))] [quarter (round (/ room 4))])
    (if (eq? which 'shared)
        `((name "Name" ,(- room third quarter quarter))
          (from "Parent's" ,quarter
                "The parent's name it forwards to, where it is renamed")
          (set "Set!" ,quarter "Refused, or through the parent's setter")
          (holder "Bound in" ,third
                  "Where its binding is: double click to go there"))
        `((name "Name" ,(- room third quarter))
          (what "Kind" ,quarter)
          (value "Value" ,third)))))

(define (envs-names-text e which row key)
  (if (eq? which 'shared)
      (case key
        [(name) (symbol->string (car row))]
        [(from) (if (eq? (car row) (cadr row)) "" (symbol->string (cadr row)))]
        [(set) (if (caddr row) (format "via ~a" (caddr row)) "read-only")]
        [else (envs-holder e (car row) (cadr row))])
      (let ([env (env-environment e)])
        (case key
          [(name) (symbol->string row)]
          [(what) (cond [(not (top-level-bound? row env)) "syntax"]
                        [(worklet? (top-level-value row env)) "worklet"]
                        [(procedure? (top-level-value row env)) "procedure"]
                        [else "value"])]
          [else (if (and (top-level-bound? row env)
                         (not (procedure? (top-level-value row env))))
                    (parameterize ([print-length 6] [print-level 3])
                      (let ([s (format "~s" (top-level-value row env))])
                        (substring s 0 (min 120 (string-length s)))))
                    "")]))))

;; The names: Shared or Own, a filter, and their table, at least rows
;; high. => the y below
(define (envs-names v e x y w rows)
  (let*-values ([(ui) (envs-view-ui v)] [(gap) (ui-gap)]
                [(shared own new) (envs-facts v e)]
                [(which) (if (and (eq? (envs-view-names v) 'own) own)
                             'own
                             'shared)]
                [(all) (if (eq? which 'own) own shared)]
                [(q) (envs-trim (envs-view-names-query v))]
                [(shown) (if (string=? q "")
                             all
                             (filter (lambda (r)
                                       (string-contains-ci?
                                        (symbol->string
                                         (if (pair? r) (car r) r))
                                        q))
                                     all))]
                [(sw sh) (ui-label-size '(section) "Names" w)]
                [(ch) (ui-control-height)]
                [(segs) (list (cons (format "Shared ~a" (length shared))
                                    'shared)
                              (cons (if own
                                        (format "Own ~a" (length own))
                                        "Own")
                                    'own))]
                [(segw) (* 2 (+ 34 (text-widest (map car segs) 'ui)))]
                [(fw) (max 120 (- w segw gap))]
                [(th) (+ (ui-table-header-height)
                         (* rows (ui-table-row-height)))])
    (ui-draw-label x y w sh '(section) "Names")
    (let ([y (+ y sh 6)])
      (ui-segmented ui '(envs names) x y segw ch segs which
                    (lambda (n)
                      (envs-view-names-set! v n)
                      (ui-forget! ui (list '(envs names-table) 'body))))
      (ui-text-field ui '(envs names-query) (+ x segw gap) y (- w segw gap) ch
                     (envs-view-names-query v)
                     (lambda (s) (envs-view-names-query-set! v s))
                     'search #t 'placeholder "Filter names"
                     'on-enter (lambda () (ui-focus! ui #f)))
      (let ([ty (+ y ch gap)])
        (if (and (eq? (envs-view-names v) 'own) (not own))
            (ui-draw-label x ty w 0 '(hint wrap)
                           "Its own names are its thread's: its vat's \
                            sandbox is read there, not here.")
            (let ([room (ui-table-room w)])
              (ui-draw-surface x ty w th '(well))
              (ui-table ui '(envs names-table) x (+ ty 4) w (- th 8)
                        (envs-names-columns which room) shown #f
                        (lambda (row) #f)
                        (lambda (row key cx cy cw ch flags)
                          (ui-draw-cell cx cy cw ch flags
                                        (envs-names-text e which row key)))
                        'on-open
                        (lambda (row)
                          (when (pair? row)
                            (let ([h (env-find (envs-holder e (car row)
                                                            (cadr row)))])
                              (when h (envs-select! v h))))))))
        (+ ty th)))))

(define (envs-inspector v x y w h)
  (let* ([ui (envs-view-ui v)] [e (envs-view-selected v)] [p (ui-pad)]
         [l (max p (ui-scrollbar-inset 'left))]
         [r (max p (ui-scrollbar-inset 'right))])
    (ui-draw-surface x y w h '(side))
    (ui-scroll ui 'inspector x y w h
      (lambda (top)
        (let* ([cx (+ x l)] [cw (- w l r)]
               [y (envs-draw-head e cx (+ top p) cw)]
               [y (ui-form ui 'envs-facts cx (+ y (ui-gap) 8) cw
                           (envs-fact-rows v e))])
          ;; the names fill what the view leaves, scrolled or not
          (+ p (envs-names v e cx (+ y 20) cw
                           (max 6 (exact
                                   (floor (/ (- h (- y top) 120)
                                             (ui-table-row-height))))))))))))

;;; ------------------------------------------------------------------
;;; the sheets

(define (envs-sheet-width) (round (* 30 (font-line-height 'ui))))

;; A row of the sheet's error, when its last try raised one.
(define (envs-error-rows v)
  (let ([err (envs-view-error v)])
    (if err
        (list (list #f
                    (lambda (w)
                      (let-values ([(ew eh) (ui-label-size '(wrap) err w)])
                        eh))
                    (lambda (x y w) (ui-draw-label x y w 0 '(error wrap) err))))
        '())))

;; The share field: the chips, and what is typed, suggested from the names
;; holder holds.
(define (envs-share-row v holder hint)
  (let ([ui (envs-view-ui v)])
    (list "Share"
          (lambda (w) (ui-chip-input-size w (envs-view-shares v)))
          (lambda (x y w)
            (ui-chip-input ui '(envs-sheet shares) x y w (envs-view-shares v)
                           (envs-view-draft v)
                           (lambda (s) (envs-view-draft-set! v s))
                           (lambda (ss)
                             (envs-view-shares-set!
                              v (append (envs-view-shares v)
                                        (filter (lambda (s)
                                                  (not (string=?
                                                        (envs-trim s) "")))
                                                (map envs-trim ss)))))
                           (lambda (i)
                             (envs-view-shares-set!
                              v (let ([l (envs-view-shares v)])
                                  (append (list-head l i)
                                          (list-tail l (+ i 1))))))
                           'placeholder "A name…"
                           'suggest
                           (lambda (typed)   ; the first word's
                             (if (memv #\space (string->list typed))
                                 '()
                                 (envs-memo v (list 'share holder typed)
                                   (lambda ()
                                     (ui-suggestions
                                      typed
                                      (map symbol->string
                                           (remp gensym?
                                                 (environment-symbols
                                                  (env-environment holder))))
                                      8)))))))
          'hint hint)))

(define (envs-close-sheet! v) (envs-view-sheet-set! v #f))

;; Fork: the kinds, those its fork does not make off; its name, and the
;; path it will have; what it is shared; a REPL in it.
(define (envs-fork-sheet v)
  (let* ([ui (envs-view-ui v)] [e (envs-view-selected v)]
         [kind (envs-view-kind v)] [forks (env-forks e)] [kinds (env-kinds)]
         [bw (envs-sheet-width)] [ch (ui-control-height)]
         [radio-h (lambda (w k)   ; and 2 below
                    (let-values ([(rw rh) (ui-radio-size
                                           (env-kind-title k)
                                           (env-kind-ref k 'doc #f) #t w)])
                      (+ rh 2)))]
         [typed (envs-trim (envs-view-name v))]
         [child (env-child-name e (if (string=? typed "")
                                      kind
                                      (string->symbol typed)))]
         [path (if (env-parent e)
                   (format "~a/~a" (env-path e) child)
                   (format "/~a" child))]
         [rows
          (append
           (list
            (list #f (lambda (w) (apply + (map (lambda (k) (radio-h w k))
                                               kinds)))
                  (lambda (x y w)
                    (fold-left
                     (lambda (y k)
                       (let ([h (radio-h w k)] [ok? (memq k forks)])
                         (ui-radio ui (list 'envs-sheet 'kind k) x y w (- h 2)
                                   (env-kind-title k) (eq? k kind)
                                   (lambda () (envs-view-kind-set! v k))
                                   'hint (env-kind-ref k 'doc #f)
                                   'icon (lambda (ix iy s flags)
                                           (ui-draw-env-icon ix iy s flags
                                                             k))
                                   'disabled (not ok?)
                                   'tip (and (not ok?)
                                             (format "~a forks ~a only"
                                                     (env-path e)
                                                     (string-join
                                                      (map env-kind-title
                                                           forks)
                                                      ", "))))
                         (+ y h)))
                     y kinds)))
            (list "Name" ch
                  (lambda (x y w)
                    (ui-text-field ui '(envs-sheet name) x y w ch
                                   (envs-view-name v)
                                   (lambda (s) (envs-view-name-set! v s))
                                   'placeholder (symbol->string kind)))
                  'hint (format "It will be ~a" path))
            (envs-share-row
             v e
             (string-append
              (format "Names of ~a's. “now-ms as clock” renames one; \
                       “size via size-set!” sets it through the setter."
                      (env-path e))
              (if (eq? kind 'thread)
                  " What a thread is shared runs on its thread: share \
                   worklets, or what is safe there."
                  "")))
            (list #f ch
                  (lambda (x y w)
                    (ui-checkbox ui '(envs-sheet repl) x y w ch
                                 "Open a REPL in it" (envs-view-repl? v)
                                 (lambda (on?) (envs-view-repl?-set! v on?))))))
           (envs-error-rows v))])
    (ui-sheet ui 'envs-sheet (format "Fork ~a" (env-path e)) bw
              (ui-form-height rows bw)
              (lambda (x y w) (ui-form ui '(envs-sheet form) x y w rows))
              (list (list "Cancel" (lambda () (envs-close-sheet! v)))
                    (list "Fork" (lambda () (envs-fork! v)) 'primary))
              (lambda () (envs-close-sheet! v)))))

;; Share: more of the parent's names.
(define (envs-share-sheet v)
  (let* ([ui (envs-view-ui v)] [e (envs-view-selected v)] [p (env-parent e)]
         [bw (envs-sheet-width)]
         [rows (cons (envs-share-row
                      v p
                      (format "Names of ~a's, read-only and live in ~a. \
                               “now-ms as clock” renames one; “size via \
                               size-set!” sets it through the setter."
                              (env-path p) (env-path e)))
                     (envs-error-rows v))])
    (ui-sheet ui 'envs-sheet (format "Share with ~a" (env-path e)) bw
              (ui-form-height rows bw)
              (lambda (x y w) (ui-form ui '(envs-sheet form) x y w rows))
              (list (list "Cancel" (lambda () (envs-close-sheet! v)))
                    (cons* "Share" (lambda () (envs-share! v))
                           'primary
                           (if (and (null? (envs-view-shares v))
                                    (string=? (envs-trim (envs-view-draft v))
                                              ""))
                               '(disabled)
                               '())))
              (lambda () (envs-close-sheet! v)))))

(define (envs-drop-dialog v)
  (let* ([e (envs-view-selected v)]
         [under (let walk ([e e])
                  (apply append (map (lambda (c) (cons c (walk c)))
                                     (env-children e))))]
         [threads (filter vat-of (cons e under))])
    (ui-dialog (envs-view-ui v) 'envs-sheet (format "Drop ~a?" (env-path e))
               (format "It goes~a~a. REPLs in them evaluate nowhere after."
                       (case (length under)
                         [(0) ""]
                         [(1) ", with the environment under it"]
                         [else (format ", with the ~a environments under it"
                                       (length under))])
                       (if (null? threads)
                           ""
                           ", and its threads' jobs stop"))
               (list (list "Cancel" (lambda () (envs-close-sheet! v)))
                     (list "Drop" (lambda () (envs-drop! v)) 'danger))
               (lambda () (envs-close-sheet! v)))))

;;; ------------------------------------------------------------------
;;; the window

(define (envs-toolbar v w)
  (let*-values
      ([(ui) (envs-view-ui v)] [(e) (envs-view-selected v)]
       [(all) (envs-all)]
       [(threads) (length (filter vat-of all))]
       [(count) (format "~a environments · ~a thread~a" (length all) threads
                        (if (= threads 1) "" "s"))]
       [(cw ch) (ui-label-size '(dim) count 0)]
       [(sw sh) (ui-text-box-size '(search) 0 1)])
    (ui-toolbar
     0 0 w
     (append
      (map (lambda (a)
             (let-values ([(bw bh) (ui-button-size (cadr a) (caddr a))])
               (list bw bh
                     (lambda (x y w h)
                       (let ([why (envs-why-not e (car a))])
                         (ui-button ui (list 'envs (car a)) x y w h (cadr a)
                                    (lambda () (envs-act! v (car a)))
                                    'glyph (caddr a)
                                    'primary (eq? (car a) 'fork)
                                    'danger (eq? (car a) 'drop)
                                    'disabled (and why #t)
                                    'tip (or why (cadddr a))))))))
           envs-actions)
      (list (list #f sh
                  (lambda (x y w h)
                    (ui-text-field ui '(envs query) x y w h
                                   (envs-view-query v)
                                   (lambda (s) (envs-view-query-set! v s))
                                   'search #t 'placeholder "Filter"
                                   'tip "Filter the tree  /"
                                   'on-enter (lambda () (ui-focus! ui #f)))))))
     'end (list (ceiling cw) sh
                (lambda (x y w h)
                  (ui-draw-label x y w h '(dim right) count))))))

(define (envs-draw v w h)
  (let ([ui (envs-view-ui v)])
    ;; dropped elsewhere (a REPL closed, say): its nearest that is not
    (let up ([e (envs-view-selected v)])
      (if (env-dropped? e)
          (up (env-parent e))
          (unless (eq? e (envs-view-selected v)) (envs-select! v e))))
    (ui-begin! ui)
    (ui-draw-surface 0 0 w h '(page))
    ;; the tree and the inspector share what is left, as the line between
    ;; them is dragged
    (let* ([top (envs-toolbar v w)]
           [tw (ui-split ui 'split 0 top w (- h top)
                         (max 300 (min (- w 380) (round (* w 0.55))))
                         280 320)])
      (envs-tree v 0 top tw (- h top))
      (envs-inspector v tw top (- w tw) (- h top)))
    (case (envs-view-sheet v)
      [(fork) (envs-fork-sheet v)]
      [(share) (envs-share-sheet v)]
      [(drop) (envs-drop-dialog v)])
    (ui-due! ui (+ (now-ms) 1000))   ; jobs and names come and go
    (ui-end! ui w h)))

;; With no field focused and no sheet open: arrows in the tree, Return a
;; REPL, Delete drops; F forks, / filters.
(define (envs-key v win key mods)
  (let ([ui (envs-view-ui v)])
    (and (not (envs-view-sheet v)) (not (ui-focused ui))
         (cond
           [(ui-tree-key ui 'tree key mods (envs-view-selected v)
                         (lambda (e) (envs-select! v e))
                         (lambda (e) (envs-act! v 'repl)))]
           [(and (eq? key 'delete) (null? mods)) (envs-act! v 'drop) #t]
           [else #f]))))

(define (envs-text v win str)
  (let ([ui (envs-view-ui v)])
    (and (not (envs-view-sheet v)) (not (ui-focused ui))
         (cond [(member str '("f" "F")) (envs-act! v 'fork) #t]
               [(string=? str "/") (ui-focus! ui '(envs query)) #t]
               [else #f]))))

(define (envs-natural-size)
  (let ([lh (font-line-height 'ui)])
    (cons (max 1080 (round (* 72 lh))) (max 660 (round (* 37 lh))))))

(define-app (envs) "Environments"
  (let ([existing (find-window 'envs)])
    (if (and existing (envs-view? (window-ref existing 'state #f)))
        (begin (focus-window! existing) existing)
        (let* ([ui (make-ui)]
               [v (make-envs-view ui (env-root) "" 'shared "" #f 'sandbox ""
                                  '() "" #f #f #f (cons #f #f)
                                  (make-weak-eq-hashtable))])
          ;; one opened by code from before a (reload!) that changed
          ;; envs-view can't draw any more: replace it
          (when existing (close-window! existing))
          (apply open-window "Environments"
                 'size (envs-natural-size)
                 'min-size '(640 . 420)
                 'state v
                 'draw (lambda (win w h) (envs-draw v w h))
                 (ui-handlers ui
                              'key (lambda (win key mods)
                                     (envs-key v win key mods))
                              'text (lambda (win str)
                                      (envs-text v win str))))))))
