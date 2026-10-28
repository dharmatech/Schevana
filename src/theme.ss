;;; theme.ss — colours and metrics of the world, as live data.
;;;
;;; (theme 'accent)            => #x7aa2f7
;;; (set-theme! 'accent #xff9e64)  recolours everything on the next frame.
;;; Anywhere a colour is expected you may pass a theme key symbol instead.

(defvar *theme* (make-eq-hashtable))
(defvar *theme-version* 0)

(define (theme key)
  (let ([v (hashtable-ref *theme* key #f)])
    (or v (errorf 'theme "unknown theme key ~s" key))))

(define (set-theme! key value)
  (hashtable-set! *theme* key value)
  (set! *theme-version* (+ *theme-version* 1))
  (invalidate!))

(define (theme-keys)
  (vector->list (vector-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                             (hashtable-keys *theme*))))

(define default-theme
  '(;; palette
    bg-0 #x13141c   bg-1 #x1a1b26   bg-2 #x1f2335   bg-3 #x292e42
    text #xc0caf5   text-dim #x9aa5ce  text-faint #x565f89
    accent #x7aa2f7 accent-2 #xbb9af7
    red #xf7768e    orange #xff9e64  yellow #xe0af68  green #x9ece6a
    cyan #x7dcfff   teal #x73daca    magenta #xc678dd
    selection #x2e3c64
    ;; desktop
    desktop-top #x1b1d2e  desktop-bottom #x2b2140  watermark #xffffff
    ;; panels
    panel #x111219  panel-text #xc0caf5  panel-dim #x6b7394  panel-hover #x24283b
    ;; windows
    window-bg #x1a1b26  window-border #x30364f  window-border-focus #x4a5687
    title-bg #x1f2233   title-bg-focus #x262b40  title-text #xc0caf5  title-text-dim #x6b7394
    button-close #xf7768e  button-min #xe0af68  button-max #x9ece6a  button-idle #x3b4261
    ;; metrics
    title-height 30  corner-radius 9  topbar-height 28  taskbar-height 38  shadow 14))

(define (reset-theme!)
  (let loop ([p default-theme])
    (unless (null? p)
      (hashtable-set! *theme* (car p) (cadr p))
      (loop (cddr p))))
  (set! *theme-version* (+ *theme-version* 1)))

(when (= 0 (hashtable-size *theme*)) (reset-theme!))
