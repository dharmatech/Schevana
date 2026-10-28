;;; apps/repl.ss — a Scheme REPL living inside the world it can change.
;;;
;;; Each evaluation runs as a process (see proc.ss), so long computations
;;; keep the desktop alive and Ctrl+C interrupts them.

(define-record-type repl
  (nongenerative schevana-repl)
  (fields (mutable lines)      ; transcript, newest first: ((style . text) ...)
          (mutable count)
          (mutable open?)      ; is the newest line unterminated output?
          (mutable input)      ; the text being edited
          (mutable point)      ; cursor index into input
          (mutable history)    ; previous inputs, newest first
          (mutable hist-pos)   ; -1 while editing fresh input
          (mutable saved)      ; fresh input stashed while browsing history
          (mutable scroll)     ; visual rows scrolled back from the bottom
          (mutable proc)       ; the running evaluation, or #f
          (mutable pending)    ; output from the process, newest first
          (mutable port)
          (mutable blink)      ; cursor visible?
          (mutable blink-at)
          (mutable cols)       ; layout from the last draw
          (mutable rows)
          (mutable queued?)))  ; Enter was pressed while an evaluation ran

(define repl-max-lines 4000)
(define repl-prompt "λ> ")
(define repl-cont   "   ")

(define repl-keywords
  '(define lambda let let* letrec letrec* if cond case when unless begin set! do
    quote quasiquote and or define-syntax syntax-rules syntax-case let-values
    let*-values define-record-type guard parameterize case-lambda define-app
    else import library export fluid-let delay define-values with-syntax))

;;; ------------------------------------------------------------------ transcript

(define (repl-push-line! r style text)
  (repl-lines-set! r (cons (cons style text) (repl-lines r)))
  (repl-count-set! r (+ 1 (repl-count r)))
  (when (> (repl-count r) (+ repl-max-lines 500))
    (repl-lines-set! r (list-head (repl-lines r) repl-max-lines))
    (repl-count-set! r repl-max-lines)))

;; Append text that may contain newlines; output can arrive in pieces.
(define (repl-append! r style str)
  (let loop ([pieces (string-split str #\newline)] [first? #t])
    (unless (null? pieces)
      (let ([p (car pieces)] [last? (null? (cdr pieces))])
        (cond
          [(and first? (repl-open? r) (eq? (caar (repl-lines r)) style))
           (set-cdr! (car (repl-lines r)) (string-append (cdar (repl-lines r)) p))]
          [(and last? (string=? p "")) (void)]
          [else (repl-push-line! r style p)])
        (when last? (repl-open?-set! r (not (string=? p ""))))
        (loop (cdr pieces) #f)))))

;; Add complete lines, starting on a fresh line.
(define (repl-add! r style str)
  (repl-open?-set! r #f)
  (repl-append! r style (string-append str "\n"))
  (repl-scroll-set! r 0)
  (invalidate!))

;; Called from inside the evaluation process: queue, don't touch the transcript.
(define (repl-emit! r style str)
  (with-interrupts-disabled
   (repl-pending-set! r (cons (cons style str) (repl-pending r)))))

(define (repl-drain! r)
  (let ([chunks (with-interrupts-disabled
                 (let ([c (repl-pending r)]) (repl-pending-set! r '()) c))])
    (unless (null? chunks)
      (for-each (lambda (c)
                  (if (eq? (car c) 'output)
                      (repl-append! r 'output (cdr c))
                      (repl-add! r (car c) (cdr c))))
                (reverse chunks))
      (repl-scroll-set! r 0)
      (invalidate!))))

(define (make-repl-port r)
  (let ([p (make-custom-textual-output-port "repl"
             (lambda (s start n)
               (repl-emit! r 'output (substring s start (+ start n)))
               n)
             #f #f #f)])
    (set-textual-port-output-size! p 0)
    p))

;;; ------------------------------------------------------------------ evaluation

(define (read-all-from-string src)
  (let ([p (open-input-string src)])
    (let loop ([acc '()])
      (let ([x (read p)])
        (if (eof-object? x) (reverse acc) (loop (cons x acc)))))))

(define (value->string v)
  (let ([s (with-output-to-string (lambda () (pretty-print v)))])
    (if (and (> (string-length s) 0) (char=? #\newline (string-ref s (- (string-length s) 1))))
        (substring s 0 (- (string-length s) 1))
        s)))

(define (repl-echo! r src)
  (let loop ([lines (string-split src #\newline)] [first? #t])
    (unless (null? lines)
      (repl-add! r 'input (string-append (if first? repl-prompt repl-cont) (car lines)))
      (loop (cdr lines) #f))))

(define (repl-submit! r win)
  (let ([src (repl-input r)])
    (repl-echo! r src)
    (unless (string=? "" (string-trim src))
      (repl-history-set! r (cons src (remove src (repl-history r)))))
    (repl-input-set! r "")
    (repl-point-set! r 0)
    (repl-hist-pos-set! r -1)
    (let ([forms (guard (c [#t (repl-add! r 'error (condition->string c)) #f])
                   (read-all-from-string src))])
      (when (and forms (pair? forms))
        (repl-eval! r win forms)))))

(define (repl-eval! r win forms)
  (let ([port (repl-port r)] [width (max 20 (- (repl-cols r) 1))])
    (window-title-set! win "REPL — running · Ctrl+C to interrupt")
    (repl-proc-set! r
      (spawn "repl"
             (lambda ()
               (parameterize ([current-output-port port]
                              [current-error-port port]
                              [current-input-port (open-input-string "")]
                              [pretty-line-length width])
                 (for-each
                  (lambda (form)
                    (call-with-values (lambda () (eval form))
                      (lambda vals
                        (for-each (lambda (v)
                                    (unless (eq? v (void))
                                      (repl-emit! r 'result (value->string v))))
                                  vals))))
                  forms)))
             (lambda (status value)
               (repl-drain! r)
               (case status
                 [(error) (repl-add! r 'error (condition->string value))]
                 [(killed) (repl-add! r 'info "; interrupted")])
               (repl-proc-set! r #f)
               (window-title-set! win "REPL")
               (when (repl-queued? r)
                 (repl-queued?-set! r #f)
                 (when (input-complete? (repl-input r)) (repl-submit! r win)))
               (invalidate!))))))

;;; ------------------------------------------------------------------ editing

(define (string-trim s)
  (let* ([n (string-length s)]
         [a (let loop ([i 0]) (if (and (< i n) (char-whitespace? (string-ref s i))) (loop (+ i 1)) i))]
         [b (let loop ([i n]) (if (and (> i a) (char-whitespace? (string-ref s (- i 1)))) (loop (- i 1)) i))])
    (substring s a b)))

(define (line-start s i)
  (let loop ([i i]) (if (and (> i 0) (not (char=? (string-ref s (- i 1)) #\newline))) (loop (- i 1)) i)))

(define (line-end s i)
  (let ([n (string-length s)])
    (let loop ([i i]) (if (and (< i n) (not (char=? (string-ref s i) #\newline))) (loop (+ i 1)) i))))

;; Scan s[0, end) and return (values unclosed-opener-indices in-string?),
;; innermost opener first.
(define (scan-parens s end)
  (let loop ([i 0] [stack '()] [str? #f])
    (if (>= i end)
        (values stack str?)
        (let ([c (string-ref s i)])
          (cond
            [str? (cond [(char=? c #\\) (loop (+ i 2) stack #t)]
                        [(char=? c #\") (loop (+ i 1) stack #f)]
                        [else (loop (+ i 1) stack #t)])]
            [(char=? c #\") (loop (+ i 1) stack #t)]
            [(char=? c #\;) (loop (line-end s i) stack #f)]
            [(and (char=? c #\#) (< (+ i 1) end) (char=? (string-ref s (+ i 1)) #\\))
             (loop (+ i 3) stack #f)]
            [(memv c '(#\( #\[)) (loop (+ i 1) (cons i stack) #f)]
            [(memv c '(#\) #\])) (loop (+ i 1) (if (pair? stack) (cdr stack) stack) #f)]
            [else (loop (+ i 1) stack #f)])))))

(define (input-complete? s)
  (let-values ([(stack str?) (scan-parens s (string-length s))])
    (and (null? stack) (not str?))))

;; Index of the paren matching the one at i, or #f.
(define (matching-paren s i)
  (and (>= i 0) (< i (string-length s))
       (let ([c (string-ref s i)])
         (cond
           [(memv c '(#\) #\]))
            (let-values ([(stack str?) (scan-parens s i)])
              (and (not str?) (pair? stack) (car stack)))]
           [(memv c '(#\( #\[))
            (let ([n (string-length s)])
              (let loop ([j (+ i 1)] [depth 0] [str? #f])
                (and (< j n)
                     (let ([c (string-ref s j)])
                       (cond
                         [str? (cond [(char=? c #\\) (loop (+ j 2) depth #t)]
                                     [(char=? c #\") (loop (+ j 1) depth #f)]
                                     [else (loop (+ j 1) depth #t)])]
                         [(char=? c #\") (loop (+ j 1) depth #t)]
                         [(char=? c #\;) (loop (line-end s j) depth #f)]
                         [(and (char=? c #\#) (< (+ j 1) n) (char=? (string-ref s (+ j 1)) #\\))
                          (loop (+ j 3) depth #f)]
                         [(memv c '(#\( #\[)) (loop (+ j 1) (+ depth 1) #f)]
                         [(memv c '(#\) #\])) (if (= depth 0) j (loop (+ j 1) (- depth 1) #f))]
                         [else (loop (+ j 1) depth #f)])))))]
           [else #f]))))

(define (repl-insert! r str)
  (let ([s (repl-input r)] [p (repl-point r)])
    (repl-input-set! r (string-append (substring s 0 p) str (substring s p (string-length s))))
    (repl-point-set! r (+ p (string-length str)))))

(define (repl-delete! r from to)
  (let ([s (repl-input r)])
    (when (< from to)
      (repl-input-set! r (string-append (substring s 0 from) (substring s to (string-length s))))
      (repl-point-set! r from))))

;; Newline plus Lisp-style indentation: two past the innermost open paren.
(define (repl-newline! r)
  (let-values ([(stack str?) (scan-parens (repl-input r) (repl-point r))])
    (let ([indent (if (pair? stack)
                      (let ([open (car stack)])
                        (+ 2 (- open (line-start (repl-input r) open))))
                      0)])
      (repl-insert! r (string-append "\n" (make-string indent #\space))))))

(define (repl-history-move! r dir)
  (let* ([h (repl-history r)] [pos (+ (repl-hist-pos r) dir)])
    (when (and (>= pos -1) (< pos (length h)))
      (when (= (repl-hist-pos r) -1) (repl-saved-set! r (repl-input r)))
      (repl-hist-pos-set! r pos)
      (repl-input-set! r (if (= pos -1) (or (repl-saved r) "") (list-ref h pos)))
      (repl-point-set! r (string-length (repl-input r))))))

(define (repl-up! r)
  (let* ([s (repl-input r)] [p (repl-point r)] [ls (line-start s p)])
    (if (= ls 0)
        (repl-history-move! r 1)
        (let* ([col (- p ls)] [pls (line-start s (- ls 1))])
          (repl-point-set! r (+ pls (min col (- (- ls 1) pls))))))))

(define (repl-down! r)
  (let* ([s (repl-input r)] [p (repl-point r)] [le (line-end s p)])
    (if (= le (string-length s))
        (repl-history-move! r -1)
        (let* ([col (- p (line-start s p))] [nls (+ le 1)])
          (repl-point-set! r (+ nls (min col (- (line-end s nls) nls))))))))

(define (symbol-char? c)
  (not (or (char-whitespace? c) (memv c '(#\( #\) #\[ #\] #\" #\' #\` #\, #\;)))))

(define (common-prefix strs)
  (fold-left (lambda (a b)
               (let loop ([i 0])
                 (if (and (< i (string-length a)) (< i (string-length b))
                          (char=? (string-ref a i) (string-ref b i)))
                     (loop (+ i 1))
                     (substring a 0 i))))
             (car strs) (cdr strs)))

;; Tab: complete the symbol before the cursor from the live environment.
(define (repl-complete! r)
  (let* ([s (repl-input r)] [p (repl-point r)]
         [start (let loop ([i p]) (if (and (> i 0) (symbol-char? (string-ref s (- i 1)))) (loop (- i 1)) i))]
         [prefix (substring s start p)])
    (if (string=? prefix "")
        (repl-insert! r "  ")
        (let ([cands (sort string<?
                           (filter (lambda (n) (string-prefix? prefix n))
                                   (map symbol->string (environment-symbols (interaction-environment)))))])
          (cond
            [(null? cands) (void)]
            [(null? (cdr cands))
             (repl-insert! r (substring (car cands) (string-length prefix) (string-length (car cands))))]
            [else
             (let ([common (common-prefix cands)])
               (if (> (string-length common) (string-length prefix))
                   (repl-insert! r (substring common (string-length prefix) (string-length common)))
                   (repl-add! r 'info
                              (string-append
                               (string-join (list-head cands (min 60 (length cands))) "  ")
                               (if (> (length cands) 60) (format "  … ~a more" (- (length cands) 60)) "")))))])))))

(define (repl-key r win key mods)
  (repl-blink-set! r #t)
  (repl-blink-at-set! r (now-ms))
  (let ([s (repl-input r)] [p (repl-point r)] [ctrl? (memq 'ctrl mods)])
    (cond
      [(and ctrl? (eqv? key #\c))
       (if (repl-proc r)
           (kill! (repl-proc r))
           (begin (repl-echo! r (string-append s "^C")) (repl-input-set! r "") (repl-point-set! r 0)))]
      [(and ctrl? (eqv? key #\l)) (repl-lines-set! r '()) (repl-count-set! r 0) (repl-open?-set! r #f)]
      [(and ctrl? (eqv? key #\a)) (repl-point-set! r (line-start s p))]
      [(and ctrl? (eqv? key #\e)) (repl-point-set! r (line-end s p))]
      [(and ctrl? (eqv? key #\k)) (repl-delete! r p (max (line-end s p) (min (+ p 1) (string-length s))))]
      [(and ctrl? (eqv? key #\u)) (repl-delete! r (line-start s p) p)]
      [(or (and ctrl? (eqv? key #\v)) (and (eq? key 'insert) (memq 'shift mods)))
       (repl-insert! r (list->string (remv #\return (string->list (clipboard-get)))))]
      [ctrl? (void)]
      [else
       (case key
         [(return)
          (cond
            [(memq 'shift mods) (repl-newline! r)]
            [(not (input-complete? s)) (repl-newline! r)]
            [(repl-proc r) (repl-queued?-set! r #t)]
            [else (repl-submit! r win)])]
         [(backspace) (when (> p 0) (repl-delete! r (- p 1) p))]
         [(delete) (when (< p (string-length s)) (repl-delete! r p (+ p 1)))]
         [(left) (repl-point-set! r (max 0 (- p 1)))]
         [(right) (repl-point-set! r (min (string-length s) (+ p 1)))]
         [(home) (repl-point-set! r (line-start s p))]
         [(end) (repl-point-set! r (line-end s p))]
         [(up) (repl-up! r)]
         [(down) (repl-down! r)]
         [(tab) (repl-complete! r)]
         [(page-up) (repl-scroll-set! r (+ (repl-scroll r) (max 1 (- (repl-rows r) 2))))]
         [(page-down) (repl-scroll-set! r (max 0 (- (repl-scroll r) (max 1 (- (repl-rows r) 2)))))]
         [(escape) (repl-scroll-set! r 0)])])
    (invalidate! win)))

(define (repl-text r win str)
  (repl-insert! r str)
  (repl-scroll-set! r 0)
  (repl-blink-set! r #t)
  (repl-blink-at-set! r (now-ms))
  (invalidate! win))

;;; ------------------------------------------------------------------ drawing

(define (style-color style)
  (case style
    [(input) 'text]
    [(output) 'text-dim]
    [(result) 'teal]
    [(error) 'red]
    [(banner) 'accent]
    [(info) 'text-faint]
    [else 'text]))

;; A colour key per character, for the Scheme text in s (from index `from`).
(define (scheme-colors s from)
  (let* ([n (string-length s)] [v (make-vector n 'text)])
    (define (fill! a b color) (do ([i a (+ i 1)]) ((>= i (min b n))) (vector-set! v i color)))
    (define (delimiter? c) (or (char-whitespace? c) (memv c '(#\( #\) #\[ #\] #\" #\;))))
    (let loop ([i from] [after-open? #f])
      (when (< i n)
        (let ([c (string-ref s i)])
          (cond
            [(char=? c #\;) (fill! i n 'text-faint)]
            [(char=? c #\")
             (let str ([j (+ i 1)])
               (cond
                 [(>= j n) (fill! i n 'green) (loop n #f)]
                 [(char=? (string-ref s j) #\\) (str (+ j 2))]
                 [(char=? (string-ref s j) #\") (fill! i (+ j 1) 'green) (loop (+ j 1) #f)]
                 [else (str (+ j 1))]))]
            [(memv c '(#\( #\[ #\) #\]))
             (fill! i (+ i 1) 'text-faint)
             (loop (+ i 1) (memv c '(#\( #\[)))]
            [(memv c '(#\' #\` #\,)) (fill! i (+ i 1) 'cyan) (loop (+ i 1) #f)]
            [(char-whitespace? c) (loop (+ i 1) after-open?)]
            [(and (char=? c #\#) (< (+ i 1) n) (char=? (string-ref s (+ i 1)) #\\))
             (let tok ([j (+ i 3)])
               (if (and (< j n) (not (delimiter? (string-ref s j))))
                   (tok (+ j 1))
                   (begin (fill! i j 'green) (loop j #f))))]
            [else
             (let tok ([j i])
               (if (and (< j n) (not (delimiter? (string-ref s j))))
                   (tok (+ j 1))
                   (let ([t (substring s i j)])
                     (fill! i j
                            (cond
                              [(string->number t) 'orange]
                              [(member t '("#t" "#f" "#true" "#false")) 'orange]
                              [(memq (string->symbol t) repl-keywords) 'accent-2]
                              [after-open? 'accent]
                              [else 'text]))
                     (loop j #f))))]))))
    v))

;; Draws the visual rows [row0, row1) of one wrapped logical line.
(define (draw-repl-line x y text colors cols row0 row1 cw lh)
  (let ([n (string-length text)])
    (do ([row row0 (+ row 1)] [yy y (+ yy lh)]) ((>= row row1))
      (let ([a (* row cols)] [b (min n (* (+ row 1) cols))])
        (if (symbol? colors)
            (when (< a b) (draw-text x yy (substring text a b) colors 'mono))
            (let run ([i a])
              (when (< i b)
                (let ([c (vector-ref colors i)])
                  (let end ([j (+ i 1)])
                    (if (and (< j b) (eq? (vector-ref colors j) c))
                        (end (+ j 1))
                        (begin
                          (draw-text (+ x (* cw (- i a))) yy (substring text i j) c 'mono)
                          (run j))))))))))))

(define (rows-for len cols) (max 1 (quotient (+ len cols -1) cols)))

(define (repl-draw r win w h)
  (let* ([cw (font-char-width 'mono)]
         [lh (+ 3 (font-line-height 'mono))]
         [pad 10]
         [cols (max 8 (exact (floor (/ (- w pad pad) cw))))]
         [rows (max 1 (exact (floor (/ (- h pad pad) lh))))]
         [focused? (eq? win (focused-window))]
         [running? (and (repl-proc r) #t)]
         [input (repl-input r)]
         [ilines (string-split input #\newline)]
         [prompt (cond [(repl-queued? r) "↳  "] [running? "·· "] [else repl-prompt])]
         ;; logical lines, bottom-up: input lines then transcript
         [input-entries
          (let loop ([ls ilines] [i 0] [start 0] [acc '()])
            (if (null? ls)
                acc
                (loop (cdr ls) (+ i 1) (+ start (string-length (car ls)) 1)
                      (cons (list 'edit (string-append (if (= i 0) prompt repl-cont) (car ls)) start) acc))))]
         [paren (let ([p (repl-point r)])
                  (or (and (> p 0) (memv (string-ref input (- p 1)) '(#\) #\]))
                           (let ([m (matching-paren input (- p 1))]) (and m (list m (- p 1)))))
                      (and (< p (string-length input)) (memv (string-ref input p) '(#\( #\[))
                           (let ([m (matching-paren input p)]) (and m (list p m))))))]
         [total (+ (fold-left (lambda (acc e) (+ acc (rows-for (string-length (cadr e)) cols))) 0 input-entries)
                   (fold-left (lambda (acc l) (+ acc (rows-for (string-length (cdr l)) cols))) 0 (repl-lines r)))])
    (repl-cols-set! r cols)
    (repl-rows-set! r rows)
    (repl-scroll-set! r (clamp (repl-scroll r) 0 (max 0 (- total rows))))
    (fill-rect 0 0 w h 'bg-0)
    ;; Lay out bottom-up; while everything fits, hang it from the top instead.
    (let loop ([entries (append input-entries (repl-lines r))]
               [bottom (if (< total rows) (+ pad (* total lh)) (- h pad))]
               [skip (repl-scroll r)])
      (when (and (pair? entries) (> bottom 0))
        (let* ([e (car entries)]
               [edit? (and (pair? e) (eq? (car e) 'edit))]
               [text (if edit? (cadr e) (cdr e))]
               [k (rows-for (string-length text) cols)]
               [shown (- k (min skip k))])
          (if (= shown 0)
              (loop (cdr entries) bottom (- skip k))
              (let ([top (- bottom (* shown lh))])
                (cond
                  [edit?
                   (let* ([start (caddr e)]
                          [colors (scheme-colors text 3)])
                     (vector-fill-range! colors 0 3 (if running? 'orange 'accent))
                     (when paren
                       (for-each (lambda (i)
                                   (let ([j (+ 3 (- i start))])
                                     (when (and (>= i start) (< j (string-length text)))
                                       (let ([row (quotient j cols)] [col (remainder j cols)])
                                         (when (< row shown)
                                           (fill-round-rect (+ pad (* col cw)) (+ top (* row lh)) cw lh 3
                                                            (alpha 'yellow 0.25))))
                                       (vector-set! colors j 'yellow))))
                                 paren))
                     (draw-repl-line pad top text colors cols 0 shown cw lh)
                     ;; the cursor
                     (let ([p (repl-point r)] [end (+ start (- (string-length text) 3))])
                       (when (and (>= p start) (<= p end))
                         (let* ([j (+ 3 (- p start))] [row (quotient j cols)] [col (remainder j cols)])
                           (when (< row shown)
                             (cond
                               [(not focused?)
                                (stroke-rect (+ pad (* col cw)) (+ top (* row lh)) cw lh 'text-faint)]
                               [(repl-blink r)
                                (fill-rect (+ pad (* col cw)) (+ top (* row lh) 1) 2 (- lh 2) 'accent)]))))))]
                  [(eq? (car e) 'input)
                   (let ([colors (scheme-colors text 3)])
                     (vector-fill-range! colors 0 3 'text-faint)
                     (draw-repl-line pad top text colors cols 0 shown cw lh))]
                  [else
                   (draw-repl-line pad top text (style-color (car e)) cols 0 shown cw lh)])
                (loop (cdr entries) top 0))))))
    ;; scrollbar when scrolled back
    (when (> (repl-scroll r) 0)
      (let* ([track (- h 8)] [frac (/ rows (max rows total))]
             [bar (max 20 (* track frac))]
             [pos (* (- track bar) (- 1 (/ (repl-scroll r) (max 1 (- total rows)))))])
        (fill-round-rect (- w 7) (+ 4 pos) 4 bar 2 (alpha 'text 0.25))))))

(define (vector-fill-range! v a b x)
  (do ([i a (+ i 1)]) ((>= i (min b (vector-length v)))) (vector-set! v i x)))

;;; ------------------------------------------------------------------ the app

(define (repl-banner! r)
  (repl-add! r 'banner (format "Schevana — a world made of Scheme.   ~a" (scheme-version)))
  (repl-add! r 'info "Every window, colour and key binding here is a live Scheme value. Try:")
  (for-each (lambda (l) (repl-add! r 'input (string-append "   " l)))
            '("(clock)                          ; apps are just procedures"
              "(windows)                        ; the window list, as data"
              "(set-theme! 'accent #xff9e64)    ; recolour the world"
              "(spawn \"spin\" (lambda () (let loop () (loop))))"))
  (repl-add! r 'info "F1 launcher · Ctrl+Tab switch · Tab completes · Shift+Enter newline · Ctrl+C interrupts")
  (repl-add! r 'info ""))

(define-app (repl) "REPL"
  (let ([r (make-repl '() 0 #f "" 0 '() -1 #f 0 #f '() #f #t (now-ms) 80 24 #f)])
    (repl-port-set! r (make-repl-port r))
    (repl-banner! r)
    (open-window "REPL"
      'size '(720 . 440)
      'min-size '(240 . 120)
      'cursor 'ibeam
      'bg 'bg-0
      'draw (lambda (win w h) (repl-draw r win w h))
      'key (lambda (win key mods) (repl-key r win key mods))
      'text (lambda (win str) (repl-text r win str))
      'wheel (lambda (win dx dy)
               (repl-scroll-set! r (max 0 (+ (repl-scroll r) (* 3 dy))))
               (invalidate! win))
      'tick (lambda (win)
              (repl-drain! r)
              (when (and (eq? win (focused-window)) (> (- (now-ms) (repl-blink-at r)) 530))
                (repl-blink-set! r (not (repl-blink r)))
                (repl-blink-at-set! r (now-ms))
                (invalidate! win)))
      'close (lambda (win) (when (repl-proc r) (kill! (repl-proc r))))
      'state r)))
