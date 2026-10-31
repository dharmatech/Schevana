;;; styles.ss — window styles: the frame around each window's content.
;;;
;;; A style decides how thick the frame is (insets), where its buttons are,
;;; and how it is drawn, in two passes: draw-under before the content and
;;; draw-over after it. The global style is the theme key 'window-style;
;;; one window can override it with its own 'window-style property.
;;;
;;;   (set-theme! 'window-style 'win98)     ; or mac, windows, interlisp, genera
;;;   (open-window "Odd one out" 'window-style 'windows)
;;;
;;; Each style is a file in styles/. Add your own there, in
;;; ~/.schevana.d/styles/, or from the REPL:
;;;
;;;   (define-window-style bare "Bare"
;;;     'insets (lambda (win) (values 0 8 0 0))
;;;     'draw-under (lambda (win x y w h focused? hover pressed)
;;;                   (fill-rect x y w 8 (if focused? 'accent 'bg-3))))
;;;
;;; Buttons are (kind x y w h) in window coordinates. Kinds: close,
;;; minimize, maximize and depth (send to back / bring to front). A style
;;; whose title is not along the top lists its drag handle as a 'title
;;; area (Genera's label sits at the bottom).
;;; hover is the region under the mouse, pressed the drag in progress.

(define-record-type window-style
  (nongenerative schevana-window-style)
  (fields name title insets radius buttons draw-under draw-over))

(defvar *window-styles* '())   ; in registration order

;; A style from define-window-style's options. What they leave out is a
;; plain frame: a title bar's height on top, and nothing drawn.
(define (options->window-style name title opts)
  (make-window-style
   name title
   (plist-ref opts 'insets (lambda (win) (values 0 (title-height) 0 0)))
   (plist-ref opts 'radius (lambda (win) 0))
   (plist-ref opts 'buttons (lambda (win) '()))
   (plist-ref opts 'draw-under (lambda args (void)))
   (plist-ref opts 'draw-over (lambda args (void)))))

(define (register-window-style! name title . opts)
  (set! *window-styles*
    (registry-add *window-styles* (options->window-style name title opts)
                  window-style-name))
  (wm-chrome-changed!)
  name)

;; (define-window-style name "Title" 'insets ... 'draw-under ...) adds a
;; style, or replaces the one of that name; the next frame shows it.
(define-syntax define-window-style
  (syntax-rules ()
    [(_ name title opt ...) (register-window-style! 'name title opt ...)]))

;; What windows wear while no style is registered.
(define bare-window-style (options->window-style 'bare "Bare" '()))

(define (find-window-style name)
  (find (lambda (s) (eq? (window-style-name s) name)) *window-styles*))

(define (window-styles) (map window-style-name *window-styles*))

;;; ------------------------------------------------------------------ previews

;; A miniature focused window in the given style, for pickers.
(define (draw-window-style-preview name x y w h scale)
  (let ([win (make-window 0 'preview "Preview" 16 14
                          (- (/ w scale) 32) (- (/ h scale) 6) 'normal #f
                          (make-eq-hashtable) (make-eq-hashtable) #f 40 20)])
    (hashtable-set! (window-props win) 'window-style name)
    (hashtable-set! (window-handlers win) 'draw
      (lambda (win cw ch)
        (for-each (lambda (i color frac)
                    (fill-round-rect 14 (+ 14 (* i 16)) (* frac (- cw 28))
                                     7 3.5 color))
                  '(0 1 2 3) '(accent-2 accent green text-dim)
                  '(0.45 0.7 0.5 0.35))))
    (call-with-clip x y w h
      (lambda ()
        (call-with-translation x y
          (lambda ()
            (call-with-scale scale (lambda () (draw-window* win #t)))))))))
