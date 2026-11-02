;;; styles.ss — the frame around each window's content: parts of the style
;;; (theme.ss) that the window manager calls with the window, in its theme.
;;;
;;;   frame-insets   (win) -> (values left top right bottom), its thickness
;;;   frame-regions  (win) -> ((kind x y w h) ...) in window coordinates:
;;;                  close, minimize, maximize and depth (send to back /
;;;                  bring to front) act on release; title is a drag
;;;                  handle (Genera's label sits at the bottom); resize-br
;;;                  starts a resize (a classic size gadget)
;;;   frame-radius   (win) -> its corners' radius
;;;   frame-bleed    (win) -> how far, in logical pixels, it draws past
;;;                  its frame (a glow, say): a window is drawn clipped to
;;;                  its extent (window-extent in wm.ss), its frame and at
;;;                  least that bleed, and only that much is drawn again
;;;                  when it changes
;;;   frame-under, frame-over   (win x y w h focused? hover pressed) draw
;;;                  it before the content and after; hover is the region
;;;                  under the mouse, pressed the drag in progress
;;;
;;; A style gives the procedures by name. Each style is a file in styles/;
;;; add your own there, in ~/.schevana.d/styles/, or from the REPL:
;;;
;;;   (define (bare-under win x y w h focused? hover pressed)
;;;     (fill-rect x y w (title-height) (if focused? 'accent 'bg-3)))
;;;   (define-style bare "Bare" 'frame-under 'bare-under)
;;;   (set-style! 'bare)
;;;   (open-window "Odd one out" 'window-style 'windows)   ; one window's

;; The plain frame, what a style leaves out: a title bar's height on top,
;; and nothing drawn.
(define (frame-insets* win) (values 0 (title-height) 0 0))
(define (frame-regions* win) '())
(define (frame-radius* win) 0)
(define (frame-bleed* win) 0)
(define (frame-under* win x y w h focused? hover pressed) (void))
(define (frame-over* win x y w h focused? hover pressed) (void))

(for-each theme-default!
          '(frame-insets frame-regions frame-radius frame-bleed frame-under
            frame-over)
          '(frame-insets* frame-regions* frame-radius* frame-bleed*
            frame-under* frame-over*))

;; The form styles were written in before: its options are the frame's
;; parts under their old names, given as procedures.
(define (register-window-style! name title . opts)
  (register-style! name title
    (let loop ([o opts])
      (if (and (pair? o) (pair? (cdr o)))
          (cons* (case (car o)
                   [(insets) 'frame-insets] [(buttons) 'frame-regions]
                   [(radius) 'frame-radius] [(bleed) 'frame-bleed]
                   [(draw-under) 'frame-under] [(draw-over) 'frame-over]
                   [else (car o)])
                 (cadr o) (loop (cddr o)))
          '()))))

(define-syntax define-window-style
  (syntax-rules ()
    [(_ name title opt ...) (register-window-style! 'name title opt ...)]))

;;; ------------------------------------------------------------------ previews

;; A miniature focused window in the given style, for pickers.
(define (draw-style-preview name x y w h scale)
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
