;;; services/env.ss — environments: the world, and a tree of children, each
;;; given what its parent shares with it.
;;;
;;;   (env-fork (env-root) 'scratch 'chez)    a child: all of Chez, and
;;;                                           none of the world's names
;;;   (env-share! e '(windows (clock . now-ms)))   names of its parent's;
;;;                                           a pair renames: e's clock is
;;;                                           the parent's now-ms
;;;   (env-share-setter! e '*crew-size* 'crew-size-set!)
;;;   (env-share-world! e)                    every name of the world's,
;;;                                           and those it gains later
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
;;; is for the person at the REPL, and only the REPL asks for it
;;; (tests/sources.ss).

;; An environment: its name and parent (root has none); its base (root's
;; is #f); its Chez environment, and an identifier made there, from which
;; identifiers of its names are made, or #f where Chez's syntax is out of
;; reach; its shares, name: the parent's name it forwards to, or #f for a
;; name a whole-world child kept its own; its children, the oldest first;
;; whether it shares every name of the world's.
(define-record-type env
  (nongenerative schevana-env-1)
  (fields name parent base environment anchor shares
          (mutable children) (mutable world?)))

;; #<env /private chez>: its path and base, not the tree it holds.
(record-writer (record-type-descriptor env)
  (lambda (e p wr)
    (if (env-base e)
        (fprintf p "#<env ~a ~a>" (env-path e) (env-base e))
        (fprintf p "#<env ~a>" (env-path e)))))

(define (env-anchor-in environment base)
  (and (memq base '(#f chez)) (eval '(syntax env-anchor) environment)))

(define (env-new name parent base environment)
  (make-env name parent base environment (env-anchor-in environment base)
            (make-eq-hashtable) '() #f))

;; Root, with the worklets' environment its first child.
(defvar *env-root*
  (let ([root (env-new 'root #f #f *world-env*)])
    (env-children-set! root
      (list (env-new 'worklets root 'worklet *worklet-environment*)))
    root))

(define (env-root) *env-root*)

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

;; A new child of parent on base, named name, or name-2, name-3 ... where
;; a sibling has it.
(define (env-fork parent name base)
  (let* ([taken (map env-name (env-children parent))]
         [name (let loop ([n 1])
                 (let ([s (if (= n 1)
                              name
                              (string->symbol (format "~a-~a" name n)))])
                   (if (memq s taken) (loop (+ n 1)) s)))]
         [e (env-new name parent base
                     (case base
                       [(chez) (copy-environment (scheme-environment) #t)]
                       [(worklet) (worklet-base-environment)]
                       [else (errorf 'env-fork
                                     "no base ~s (expected chez or worklet)"
                                     base)]))])
    (env-children-set! parent (append (env-children parent) (list e)))
    e))

(define (env-parent-anchor who e)
  (let ([p (env-parent e)])
    (cond [(not p) (errorf who "the world has no parent to share with it")]
          [(env-anchor p)]
          [else (errorf who "~a shares nothing: Chez's syntax is out of \
                             its reach" (env-path p))])))

;; Forwards e's names to those of anchor's environment: each a name, or
;; (e's name . the other's).
(define (env-forward! e anchor names)
  (for-each (lambda (n)
              (let ([name (if (pair? n) (car n) n)]
                    [target (if (pair? n) (cdr n) n)])
                (define-top-level-syntax name
                  (forwarder-to (datum->syntax anchor target))
                  (env-environment e))
                (hashtable-set! (env-shares e) name target)))
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
    (hashtable-set! (env-shares e) name target)))

(define (env-share-world! e)
  (env-world?-set! e #t)
  (env-sync! e))

;; Shares with a whole-world child the world's names it has not been
;; shared yet, but for those it defined itself meanwhile: they stay its
;; own. Before each evaluation and load in it; a diff, under a ms.
(define (env-sync! e)
  (when (env-world? e)
    (let ([new (filter (lambda (s)
                         (not (or (hashtable-contains? *env-chez-names* s)
                                  (hashtable-contains? (env-shares e) s))))
                       (environment-symbols *world-env*))])
      (unless (null? new)
        (let ([own (make-eq-hashtable)])
          (for-each (lambda (s) (hashtable-set! own s #t))
                    (environment-symbols (env-environment e)))
          (env-forward! e (env-anchor *env-root*)
                        (remp (lambda (s) (hashtable-contains? own s)) new))
          (for-each (lambda (s)
                      (unless (hashtable-contains? (env-shares e) s)
                        (hashtable-set! (env-shares e) s #f)))
                    new))))))

;; Where e's name is bound, as (values environment name): up through the
;; shares to the environment holding the binding, so that what describes
;; a name (the REPL's help) describes what it forwards to. A shared name
;; e defined itself is e's.
(define (env-binding e name)
  (let ([target (and (env-parent e) (hashtable-ref (env-shares e) name #f))])
    (if (and target (not (top-level-bound? name (env-environment e))))
        (env-binding (env-parent e) target)
        (values (env-environment e) name))))

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

;; Drops e, and with it its children, from the tree.
(define (env-drop! e)
  (let ([p (env-parent e)])
    (unless p (errorf 'env-drop! "the world is not dropped"))
    (env-children-set! p (remq e (env-children p)))))
