;;; services/env.ss — environments: the world, and a tree of children, each
;;; given what its parent shares with it.
;;;
;;;   (env-fork (env-root) 'sandbox)          a child of a kind (below)
;;;   (env-fork e 'thread 'name 'tally 'share '(add! (clock . now-ns)))
;;;   (env-child (env-root) 'scratch 'chez)   a bare child: all of Chez, and
;;;                                           none of the world's names
;;;   (env-share! e '(windows (clock . now-ms)))   names of its parent's;
;;;                                           a pair renames: e's clock is
;;;                                           the parent's now-ms
;;;   (env-share-setter! e '*crew-size* 'crew-size-set!)
;;;   (env-share-world! e)                    every name of the world's,
;;;                                           and those it gains later,
;;;                                           at its next evaluation or
;;;                                           (env-sync! e)
;;;   (env-eval e '(windows))   (env-load e path)
;;;   (env-path e) => "/scratch"   (env-find "/scratch")   (env-drop! e)
;;;
;;; Root is the world (*world-env*), the superuser. A child has a base,
;;; chez (all of Chez) or worklet (the worklets'), and of its parent's
;;; names only those the parent shares, as read-only forwarders
;;; (forwarder-to, src/proc.ss): live, at no cost a call, and refusing a
;;; set! as it expands ("attempt to assign read-only variable"), as R6RS
;;; refuses set! of an import. The child's define of
;;; a name makes its own. A name not shared is unbound there, and the
;;; child's set! of it makes a variable of the child's. So a child changes
;;; its parent's state only through what the parent shares with it.
;;;
;;; A child on the chez base is a namespace: it can still reach the world
;;; through Chez (eval, the inspector, C), so it organizes state and
;;; confines nothing. Code shares a list of names; every name, followed,
;;; is a world namespace's, which a person asks for (tests/sources.ss).
;;;
;;; The tree is the main thread's: only it forks and drops.

;; An environment: its name and parent (root has none); its base (root's
;; is #f); its Chez environment, and an identifier made there, from which
;; identifiers of its names are made; its shares, name: the parent's name
;; it forwards to, or #f for a name a world namespace kept its own; the
;; setters of names shared through one, name: the setter's name; its
;; children, the oldest first; whether it shares every name of the
;; world's; its kind (#f: a bare child) and the kinds it may fork, a list
;; or all; whether it was dropped.
(define-record-type env
  (nongenerative schevana-env-2)
  (fields name parent base environment anchor shares setters
          (mutable children) (mutable world?) (mutable kind)
          (mutable may-fork) (mutable dropped?)))

;; #<env /private chez>: its path and base, not the tree it holds.
(record-writer (record-type-descriptor env)
  (lambda (e p wr)
    (if (env-base e)
        (fprintf p "#<env ~a ~a>" (env-path e) (env-base e))
        (fprintf p "#<env ~a>" (env-path e)))))

;; An identifier at environment's top level. A macro there, under a name
;; no code can type (a gensym), hands back the identifier it is used with:
;; so it needs nothing of the environment's own, not even syntax.
(define (env-anchor-in environment)
  (let ([grab (gensym "env-anchor")] [got #f])
    (define-top-level-syntax grab
      (lambda (x)
        (syntax-case x () [(_ id) (begin (set! got #'id) #''anchor)]))
      environment)
    (eval (list grab 'env-anchor) environment)
    got))

(define (env-new name parent base environment kind may-fork)
  (make-env name parent base environment (env-anchor-in environment)
            (make-eq-hashtable) (make-eq-hashtable) '() #f kind may-fork
            #f))

;; Root, with the worklets' environment its first child.
(defvar *env-root*
  (let ([root (env-new 'root #f #f *world-env* 'root 'all)])
    (env-children-set! root
      (list (env-new 'worklets root 'worklet *worklet-environment*
                     'worklets '())))
    root))

(define (env-root) *env-root*)

;; /worklets: the worklets live there, and the pool's threads run them.
(define (env-worklets)
  (find (lambda (c) (eq? (env-kind c) 'worklets)) (env-children *env-root*)))

;; Counts the forks and drops: what shows the tree keeps its rows until
;; it changes.
(defvar *env-generation* 0)
(define (env-generation) *env-generation*)

;; Chez's own names: the world's are the others.
(define *env-chez-names*
  (let ([t (make-eq-hashtable)])
    (for-each (lambda (s) (hashtable-set! t s #t))
              (environment-symbols (scheme-environment)))
    t))

(define (env-path e)
  (let ([p (env-parent e)])
    (cond [(not p) "/"]
          [(env-parent p) (format "~a/~a" (env-path p) (env-name e))]
          [else (format "/~a" (env-name e))])))

;; The environment at path ("/", "/private"), or #f.
(define (env-find path)
  (let loop ([e *env-root*]
             [names (remp (lambda (s) (string=? s ""))
                          (string-split path #\/))])
    (cond
      [(null? names) e]
      [(find (lambda (c) (string=? (symbol->string (env-name c)) (car names)))
             (env-children e))
       => (lambda (c) (loop c (cdr names)))]
      [else #f])))

(define (env-on-main who)
  (unless (main-thread?)
    (errorf who "the tree of environments is the main thread's")))

;; A bare child of parent on base, named name, or name-2, name-3 ... where
;; a sibling has it: shared nothing, forking nothing. A sandbox's child is
;; a sandbox on its base: it may narrow what it hands down, never widen it.
(define (env-child parent name base)
  (env-on-main 'env-child)
  (when (and (eq? (env-base parent) 'worklet) (not (eq? base 'worklet)))
    (errorf 'env-child "~a is a sandbox: its children are sandboxes on its \
                        base, not ~s" (env-path parent) base))
  (when (env-dropped? parent)
    (errorf 'env-child "~a was dropped" (env-path parent)))
  (let ([e (env-new (env-child-name parent name) parent base
                    (case base
                      [(chez) (copy-environment (scheme-environment) #t)]
                      [(worklet) (worklet-base-environment)]
                      [else (errorf 'env-child
                                    "no base ~s (expected chez or worklet)"
                                    base)])
                    #f '())])
    (env-children-set! parent (append (env-children parent) (list e)))
    (set! *env-generation* (+ *env-generation* 1))
    e))

;; The name a child named name gets under parent: name, or the first of
;; name-2, name-3 ... no sibling has.
(define (env-child-name parent name)
  (let ([taken (map env-name (env-children parent))])
    (let loop ([n 1])
      (let ([s (if (= n 1) name (string->symbol (format "~a-~a" name n)))])
        (if (memq s taken) (loop (+ n 1)) s)))))

(define (env-parent-anchor who e)
  (let ([p (env-parent e)])
    (unless p (errorf who "the world has no parent to share with it"))
    (env-anchor p)))

;; Forwards e's names to those of anchor's environment: each a name, or
;; (e's name . the other's).
(define (env-forward! e anchor names)
  (for-each (lambda (n)
              (let ([name (if (pair? n) (car n) n)]
                    [target (if (pair? n) (cdr n) n)])
                (define-top-level-syntax name
                  (forwarder-to (datum->syntax anchor target))
                  (env-environment e))
                (hashtable-set! (env-shares e) name target)
                (hashtable-delete! (env-setters e) name)))
            names))

(define (env-share! e names)
  (env-forward! e (env-parent-anchor 'env-share! e) names))

;; Forwards e's name (or (name . parent's)) as env-share! does, but e's
;; set! of it calls the parent's setter with the value, which may refuse.
(define (env-share-setter! e n setter)
  (let* ([anchor (env-parent-anchor 'env-share-setter! e)]
         [name (if (pair? n) (car n) n)]
         [target (if (pair? n) (cdr n) n)]
         [id (datum->syntax anchor target)]
         [set (datum->syntax anchor setter)])
    (define-top-level-syntax name
      (make-variable-transformer
       (lambda (x)
         (syntax-case x (set!)
           [(set! _ v) #`(#,set v)]
           [_ (identifier? x) id]
           [(_ . rest) #`(#,id . rest)])))
      (env-environment e))
    (hashtable-set! (env-shares e) name target)
    (hashtable-set! (env-setters e) name setter)))

(define (env-share-world! e)
  (env-world?-set! e #t)
  (env-sync! e))

;; The world's names a world namespace has not been shared yet, as
;; (values new own): own, those of them it defined itself meanwhile.
(define (env-world-new e)
  (let ([new (filter (lambda (s)
                       (not (or (hashtable-contains? *env-chez-names* s)
                                (hashtable-contains? (env-shares e) s))))
                     (environment-symbols *world-env*))])
    (if (null? new)
        (values '() '())
        (let ([mine (make-eq-hashtable)])
          (for-each (lambda (s) (hashtable-set! mine s #t))
                    (environment-symbols (env-environment e)))
          (partition (lambda (s) (not (hashtable-contains? mine s))) new)))))

;; What a world namespace's next sync brings: the world's names it gained
;; since the last, but those e defined itself; none for another.
(define (env-unsynced e)
  (if (env-world? e)
      (let-values ([(new own) (env-world-new e)]) new)
      '()))

;; Shares with a world namespace the world's names it has not been
;; shared yet, but for those it defined itself meanwhile: they stay its
;; own. Before each evaluation and load in it; a diff, under a ms.
(define (env-sync! e)
  (when (env-world? e)
    (let-values ([(new own) (env-world-new e)])
      (env-forward! e (env-anchor *env-root*) new)
      (for-each (lambda (s) (hashtable-set! (env-shares e) s #f)) own))))

;; Where e's name is bound, as (values env name): up through the shares
;; to the environment holding the binding, so that what describes a name
;; (the REPL's help) describes what it forwards to. A shared name e
;; defined itself is e's.
(define (env-holder e name)
  (let ([target (and (env-parent e) (hashtable-ref (env-shares e) name #f))])
    (if (and target (not (top-level-bound? name (env-environment e))))
        (env-holder (env-parent e) target)
        (values e name))))

;; The same, as (values environment name).
(define (env-binding e name)
  (let-values ([(h n) (env-holder e name)])
    (values (env-environment h) n)))

;; form evaluated, and path loaded, in e, with interaction-environment
;; bound to it, so that defvar, eval and load inside mean e.
(define (env-eval e form)
  (env-sync! e)
  (parameterize ([interaction-environment (env-environment e)])
    (eval form (env-environment e))))

(define (env-load e path)
  (env-sync! e)
  (parameterize ([interaction-environment (env-environment e)])
    (load-file! path)))

;; Drops e, and with it its children, from the tree; they are marked
;; dropped, so that what holds one (a REPL) knows. Not the world, nor
;; /worklets, where the worklets live.
(define (env-drop! e)
  (env-on-main 'env-drop!)
  (let ([p (env-parent e)])
    (cond [(not p) (errorf 'env-drop! "the world is not dropped")]
          [(eq? (env-kind e) 'worklets)
           (errorf 'env-drop! "/worklets is not dropped: the worklets live \
                               there")])
    (env-children-set! p (remq e (env-children p)))
    (let mark ([e e])
      (env-dropped?-set! e #t)
      (for-each mark (env-children e)))
    (set! *env-generation* (+ *env-generation* 1))))

;;; ------------------------------------------------------------------ names
;;; What an environment holds: what it was shared and still forwards, and
;;; what is its own, its base's names aside.

;; The worklet base's names, once.
(define *env-worklet-names* #f)
(define (env-base-names base)
  (case base
    [(chez) *env-chez-names*]
    [(worklet)
     (unless *env-worklet-names*
       (let ([t (make-eq-hashtable)])
         (for-each (lambda (s) (hashtable-set! t s #t))
                   (environment-symbols (worklet-base-environment)))
         (set! *env-worklet-names* t)))
     *env-worklet-names*]
    [else *env-chez-names*]))   ; root: the world's are its own

;; The names e forwards: ((name parent's-name setter) ...), setter #f for a
;; read-only one, by name. With look? #f, those e defined over are not
;; looked for in e (a vat's sandbox, its thread's to read).
(define (env-shared-names e . look?)
  (let ([env (env-environment e)] [look? (or (null? look?) (car look?))])
    (sort (lambda (a b) (string<? (symbol->string (car a))
                                  (symbol->string (car b))))
          (filter-map (lambda (n)
                        (let ([t (hashtable-ref (env-shares e) n #f)])
                          (and t (not (and look? (top-level-bound? n env)))
                               (list n t (hashtable-ref (env-setters e) n
                                                        #f)))))
                      (vector->list (hashtable-keys (env-shares e)))))))

;; The names e defined itself, by name: not its base's, not forwarded.
(define (env-own-names e)
  (let ([env (env-environment e)] [base (env-base-names (env-base e))])
    (sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
          (filter (lambda (s)
                    (not (or (gensym? s)
                             (hashtable-contains? base s)
                             (and (hashtable-ref (env-shares e) s #f)
                                  (not (top-level-bound? s env))))))
                  (environment-symbols env)))))

;;; ------------------------------------------------------------------ kinds
;;; A child is of a kind, which says its base, what it is shared at once
;;; and what it may fork in turn: a registry (env-register-kind!), vats
;;; adding thread. An environment forks only the kinds its own allows
;;; (env-forks): the world every one, a world namespace too (it holds the
;;; world's names), a sandbox sandboxes and threads, the others none. One
;;; that forks holds fork, a procedure of its own:
;;;   (fork)                                  => the kinds it forks
;;;   (fork 'sandbox 'share '(add!))          => the child's path
;;;   private  a namespace shared what reads the world (env-private-names)
;;;   world    a namespace shared every name of the world's, followed
;;;   pure     a namespace shared nothing
;;;   sandbox  Scheme's pure core, and what is shared with it
;;;   thread   a sandbox in a vat of its own (services/vat.ss)

;; ((kind . plist) ...), in the order registered. plist: 'title, 'base,
;; 'forks (the kinds a child of it forks: a list, or all), 'doc, 'share
;; (procedure (child): its own shares, first) and 'start (procedure
;; (child), last).
(defvar *env-kinds* '())

(define (env-register-kind! kind . plist)
  (set! *env-kinds* (registry-add *env-kinds* (cons kind plist) car)))

(define (env-kinds) (map car *env-kinds*))

(define (env-kind-ref kind key default)
  (let ([k (assq kind *env-kinds*)])
    (if k (plist-ref (cdr k) key default) default)))

;; What e may fork: a list of kinds.
(define (env-forks e)
  (let ([f (env-may-fork e)])
    (if (eq? f 'all) (env-kinds) (filter (lambda (k) (assq k *env-kinds*)) f))))

;; What a kind is called, the world's and the worklets' too; a bare child
;; by its base.
(define (env-kind-title kind)
  (case kind
    [(root) "The world"]
    [(worklets) "Worklets"]
    [else (env-kind-ref kind 'title (format "~a" kind))]))

(define (env-title e)
  (if (env-kind e)
      (env-kind-title (env-kind e))
      (if (eq? (env-base e) 'worklet) "Sandbox" "Namespace")))

;; What a private namespace is shared of the world's, read-only and live:
;; what reads it. Acting on it is the world's.
(define env-private-names
  '(windows
    focused-window
    find-window
    window-title
    window-ref
    apps
    procs
    styles
    current-style
    palettes
    wallpapers
    theme
    theme-keys
    now-ms
    uptime-ms
    source-files))

;; A child of e of kind, which e must fork (env-forks), as e's fork makes
;; it. opts: 'name, its name (the kind's by default; numbered where a
;; sibling has it); 'share, names e holds, as env-share! takes them;
;; 'setters, ((name . setter) ...), names e holds shared through e's
;; setters, as env-share-setter! takes them. The kind's own shares come
;; first, then these; a thread's vat starts last. Anything wrong is
;; refused before the child is made. => the child
(define (env-fork e kind . opts)
  (let ([name (plist-ref opts 'name kind)]
        [shares (plist-ref opts 'share '())]
        [setters (plist-ref opts 'setters '())])
    (env-fork-check e kind opts name shares setters)
    (let ([c (env-child e name (env-kind-ref kind 'base #f))])
      (env-kind-set! c kind)
      (env-may-fork-set! c (env-kind-ref kind 'forks '()))
      ((env-kind-ref kind 'share (lambda (c) #f)) c)
      (env-share! c shares)
      (for-each (lambda (s) (env-share-setter! c (car s) (cdr s))) setters)
      (unless (null? (env-forks c)) (env-give-fork! c))
      ((env-kind-ref kind 'start (lambda (c) #f)) c)
      c)))

(define (env-fork-check e kind opts name shares setters)
  (let ([kinds (env-forks e)])
    (cond
      [(env-dropped? e) (errorf 'fork "~a was dropped" (env-path e))]
      [(not (assq kind *env-kinds*))
       (errorf 'fork "no kind ~s (expected one of ~a)" kind (env-kinds))]
      [(null? kinds) (errorf 'fork "~a forks nothing, not ~s" (env-path e)
                             kind)]
      [(not (memq kind kinds))
       (errorf 'fork "~a forks ~a, not ~s" (env-path e) kinds kind)]))
  (let loop ([o opts])
    (when (pair? o)
      (unless (and (memq (car o) '(name share setters)) (pair? (cdr o)))
        (errorf 'fork "fork takes name, share and setters, not ~s" (car o)))
      (loop (cddr o))))
  (unless (and (symbol? name) (not (gensym? name))
               (let ([s (symbol->string name)])
                 (and (> (string-length s) 0)
                      (not (memv #\/ (string->list s))))))
    (errorf 'fork "a name is a symbol without a slash, not ~s" name))
  (env-shares-check 'fork e shares setters))

;; Refuses shares and setters, as env-share! and env-share-setter! take
;; them from e, unless they are of that shape and name what e holds.
(define (env-shares-check who e shares setters)
  (let ([held (make-eq-hashtable)])
    (for-each (lambda (s) (hashtable-set! held s #t))
              (environment-symbols (env-environment e)))
    (let ([check (lambda (n)
                   (unless (and (symbol? n) (hashtable-contains? held n))
                     (errorf who "~s is not ~a's to share" n (env-path e))))]
          [shape (lambda (n)
                   (unless (or (symbol? n)
                               (and (pair? n) (symbol? (car n))
                                    (symbol? (cdr n))))
                     (errorf who "a share is a name or (name . ~a's), \
                                  not ~s" (env-path e) n))
                   (if (pair? n) (cdr n) n))])
      (unless (list? shares)
        (errorf who "share takes a list of names, not ~s" shares))
      (unless (and (list? setters) (for-all pair? setters))
        (errorf who "setters takes ((name . setter) ...), not ~s" setters))
      (for-each (lambda (n) (check (shape n))) shares)
      (for-each (lambda (s) (check (shape (car s))) (check (cdr s)))
                setters))))

;; e's fork, a procedure of its own: (fork) => the kinds; (fork kind opt
;; ...) => the child's path.
(define (env-give-fork! e)
  (define-top-level-value 'fork
    (case-lambda
      [() (env-forks e)]
      [(kind . opts) (env-path (apply env-fork e kind opts))])
    (env-environment e)))

(env-register-kind! 'private
  'title "Private namespace"
  'base 'chez
  'forks '()
  'doc "All of Chez, and what reads the world, read-only and live"
  'share (lambda (e) (env-share! e env-private-names)))

(env-register-kind! 'world
  'title "World namespace"
  'base 'chez
  'forks 'all
  'doc "All of Chez, and every name of the world's, those it gains later \
        too"
  'share (lambda (e) (env-share-world! e)))

(env-register-kind! 'pure
  'title "Pure namespace"
  'base 'chez
  'forks '()
  'doc "All of Chez, and nothing of the world's")

(env-register-kind! 'sandbox
  'title "Sandbox"
  'base 'worklet
  'forks '(sandbox thread)
  'doc "Scheme's pure core, and only what is shared with it")
