;;; main.ss — boot Schevana OS.
;;;
;;;   scheme --script main.ss [--size 1280x800] [--headless] [--eval EXPR] ...
;;;
;;; --eval expressions run after boot, e.g.
;;;   --eval '(after 2000 (lambda () (screenshot "shot.png") (quit!)))'

(define *schevana-root*
  (let ([dir (path-parent (car (command-line)))])
    (if (string=? dir "") "." dir)))

(load (string-append *schevana-root* "/load.ss"))

(define (parse-size s)
  (let ([parts (string-split s #\x)])
    (cons (string->number (car parts)) (string->number (cadr parts)))))

(let loop ([args (command-line-arguments)] [mode 'sdl] [size '(1280 . 800)] [evals '()])
  (cond
    [(null? args)
     (schevana-boot! mode (car size) (cdr size))
     (load-saved-theme!)
     (load-user-init!)
     (let ([w (screen-w)])
       (clock)
       (move-window! (find-window 'clock) (- w 270) 60)
       (repl))
     (for-each (lambda (e) (eval (read (open-input-string e)))) (reverse evals))
     (schevana-run!)]
    [(string=? (car args) "--headless") (loop (cdr args) 'headless size evals)]
    [(and (string=? (car args) "--size") (pair? (cdr args)))
     (loop (cddr args) mode (parse-size (cadr args)) evals)]
    [(and (string=? (car args) "--eval") (pair? (cdr args)))
     (loop (cddr args) mode size (cons (cadr args) evals))]
    [else
     (fprintf (current-error-port) "usage: schevana [--size WxH] [--headless] [--eval EXPR]...\n")
     (exit 1)]))
