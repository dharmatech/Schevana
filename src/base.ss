;;; base.ss — small utilities shared by everything else.

;; State variables that must survive (reload!) are declared with defvar,
;; like in Emacs: the initial value is only used if the name is unbound.
(define-syntax defvar
  (syntax-rules ()
    [(_ name init)
     (define name (if (top-level-bound? 'name) (top-level-value 'name) init))]))

(define (now-ms) (real-time))

(define (clamp x lo hi) (max lo (min hi x)))

(define (string-split str ch)
  (let loop ([i 0] [start 0] [acc '()])
    (cond
      [(= i (string-length str)) (reverse (cons (substring str start i) acc))]
      [(char=? (string-ref str i) ch) (loop (+ i 1) (+ i 1) (cons (substring str start i) acc))]
      [else (loop (+ i 1) start acc)])))

(define (string-join strs sep)
  (if (null? strs)
      ""
      (apply string-append (car strs) (map (lambda (s) (string-append sep s)) (cdr strs)))))

(define (string-prefix? prefix str)
  (and (<= (string-length prefix) (string-length str))
       (string=? prefix (substring str 0 (string-length prefix)))))

(define (string-contains-ci? str sub)
  (let ([n (string-length str)] [m (string-length sub)])
    (let loop ([i 0])
      (cond
        [(> (+ i m) n) #f]
        [(string-ci=? (substring str i (+ i m)) sub) #t]
        [else (loop (+ i 1))]))))

(define (truncate-string s n)
  (if (<= (string-length s) n) s (string-append (substring s 0 (max 0 (- n 1))) "…")))

;; Property lists: '(key val key val ...)
(define (plist-ref plist key default)
  (cond
    [(or (null? plist) (null? (cdr plist))) default]
    [(eq? (car plist) key) (cadr plist)]
    [else (plist-ref (cddr plist) key default)]))

(define (plist-keys plist)
  (if (or (null? plist) (null? (cdr plist))) '() (cons (car plist) (plist-keys (cddr plist)))))

(define (condition->string c)
  (call-with-string-output-port
   (lambda (p)
     (if (condition? c)
         (display-condition c p)
         (fprintf p "non-condition raised: ~s" c)))))

(define (list-index pred lst)
  (let loop ([l lst] [i 0])
    (cond [(null? l) #f] [(pred (car l)) i] [else (loop (cdr l) (+ i 1))])))
