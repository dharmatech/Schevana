;;; styles.ss — window styles: the frame around each window's content.
;;;
;;; A style decides how thick the frame is (insets), where its buttons are,
;;; and how it is drawn, in two passes: draw-under before the content and
;;; draw-over after it. The global style is the theme key 'window-style;
;;; one window can override it with its own 'window-style property.
;;;
;;;   (set-theme! 'window-style 'win98)       ; or mac, windows, interlisp, genera
;;;   (open-window "Odd one out" 'window-style 'windows)
;;;
;;; Styles are data, so you can add your own from the REPL:
;;;
;;;   (define-window-style! 'bare "Bare"
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

(defvar *window-styles* '())

(define (define-window-style! name title . opts)
  (let ([style (make-window-style
                name title
                (plist-ref opts 'insets (lambda (win) (values 0 (title-height) 0 0)))
                (plist-ref opts 'radius (lambda (win) 0))
                (plist-ref opts 'buttons (lambda (win) '()))
                (plist-ref opts 'draw-under (lambda args (void)))
                (plist-ref opts 'draw-over (lambda args (void))))])
    (set! *window-styles*
      (append (remp (lambda (s) (eq? (window-style-name s) name)) *window-styles*) (list style)))
    (wm-chrome-changed!)
    name))

(define (find-window-style name)
  (find (lambda (s) (eq? (window-style-name s) name)) *window-styles*))

(define (window-styles) (map window-style-name *window-styles*))

;; A 1-pixel bevel: light on the top and left, dark on the bottom and right.
;; Swap the colors for a recessed look.
(define (bevel-rect x y w h light dark)
  (fill-rect x y w 1 light)
  (fill-rect x y 1 h light)
  (fill-rect x (+ y h -1) w 1 dark)
  (fill-rect (+ x w -1) y 1 h dark))

(define (draw-resize-grip x y)
  (for-each (lambda (d) (draw-line (- x d) (- y 3) (- x 3) (- y d) (alpha 'text 0.18) 1))
            '(7 11)))

;;; ------------------------------------------------------------------ Mac
;;; Traffic lights on the left, glyphs on hover, centered title.

(define (mac-radius win) (if (maximized? win) 0 (theme 'corner-radius)))

(define (mac-buttons win)
  (let ([cy (/ (title-height) 2)])
    (map (lambda (kind cx) (list kind (- cx 9) (- cy 9) 18 18))
         '(close minimize maximize) '(18 38 58))))

(define (mac-draw-under win x y w h focused? hover pressed)
  (let ([th (title-height)] [r (mac-radius win)]
        [glyphs? (memq hover '(close minimize maximize))])
    (window-shadow win x y w h r focused?)   ; shadow
    (fill-round-rect x y w h r (window-bg win))
    (call-with-round-clip x y w h r
      (lambda ()
        (fill-gradient x y w th
                       (if focused? (mix 'title-bg-focus 'text 0.04) 'title-bg)
                       (if focused? 'title-bg-focus 'title-bg))
        (draw-line x (+ y th -0.5) (+ x w) (+ y th -0.5) (alpha #x000000 0.5))
        (for-each (lambda (cx color glyph)
                    (let ([cy (+ y (/ th 2))])
                      (fill-circle (+ x cx) cy 6 (if (or focused? glyphs?) color 'button-idle))
                      (when glyphs?
                        (draw-text-centered (+ x cx -6) (- cy 7) 12 13 glyph (alpha #x000000 0.6) 'glyph))))
                  '(18 38 58) '(button-close button-min button-max) '("×" "–" "+"))
        (draw-text-centered (+ x 70) y (- w 140) th (fit-text (window-title win) (- w 160) 'title)
                            (if focused? 'title-text 'title-text-dim) 'title)))))

(define (mac-draw-over win x y w h focused? hover pressed)
  (stroke-round-rect x y w h (mac-radius win) (if focused? 'window-border-focus 'window-border))
  (unless (or (maximized? win) (< w 14) (< h 14))   ; (the grip reaches 11 pixels in)
    (draw-resize-grip (+ x w) (+ y h))))

(define-window-style! 'mac "Mac"
  'radius mac-radius
  'buttons mac-buttons
  'draw-under mac-draw-under
  'draw-over mac-draw-over)

;;; ------------------------------------------------------------------ Windows
;;; App icon and title on the left; minimize, maximize and close on the
;;; right as full-height buttons, close turning red on hover.

(define windows-button-width 46)

(define (windows-radius win) (if (maximized? win) 0 (min 8 (theme 'corner-radius))))

(define (windows-buttons win)
  (let ([w (window-w win)] [bw windows-button-width] [th (title-height)])
    (list (list 'close (- w bw) 0 bw th)
          (list 'maximize (- w bw bw) 0 bw th)
          (list 'minimize (- w bw bw bw) 0 bw th))))

(define (windows-draw-button win kind bx by bw bh focused? hover pressed)
  (let* ([down? (eq? pressed kind)]
         [hot? (or down? (eq? hover kind))]
         [close? (eq? kind 'close)]
         [ink (cond [(and close? hot?) #xffffff] [focused? 'title-text] [else 'title-text-dim])]
         [cx (+ bx (quotient bw 2))]
         [cy (+ by (quotient bh 2))])
    (when hot?
      (fill-rect bx by bw bh
                 (if close?
                     (if down? #xf1707a #xe81123)
                     (alpha 'text (if down? 0.16 0.09)))))
    (case kind
      [(minimize) (fill-rect (- cx 5) cy 10 1 ink)]
      [(maximize)
       (if (maximized? win)
           (begin
             (stroke-rect (- cx 5) (- cy 3) 8 8 ink)
             (fill-rect (- cx 3) (- cy 5) 8 1 ink)
             (fill-rect (+ cx 4) (- cy 5) 1 8 ink))
           (stroke-rect (- cx 5) (- cy 5) 10 10 ink))]
      [(close)
       (draw-line (- cx 5) (- cy 5) (+ cx 5) (+ cy 5) ink 1)
       (draw-line (+ cx 5) (- cy 5) (- cx 5) (+ cy 5) ink 1)])))

(define (windows-draw-under win x y w h focused? hover pressed)
  (let* ([th (title-height)] [r (windows-radius win)]
         [title (window-title win)]
         [icon (app-color (window-app win))]
         [is (max 16 (round (+ (font-line-height 'ui-small) 3)))])
    (window-shadow win x y w h r focused?)   ; shadow
    (fill-round-rect x y w h r (window-bg win))
    (call-with-round-clip x y w h r
      (lambda ()
        (fill-rect x y w th (if focused? 'title-bg-focus 'title-bg))
        (fill-round-rect (+ x 12) (+ y (quotient (- th is) 2)) is is 4 icon)
        (draw-text-centered (+ x 12) (+ y (quotient (- th is) 2)) is is
                            (if (string=? title "") "·" (string (char-upcase (string-ref title 0))))
                            (readable-on icon) 'ui-small)
        (draw-text (+ x is 22) (+ y (/ (- th (font-line-height 'ui)) 2))
                   (fit-text title (- w is 22 (* 3 windows-button-width) 12) 'ui)
                   (if focused? 'title-text 'title-text-dim) 'ui)
        (for-each (lambda (b)
                    (windows-draw-button win (car b) (+ x (list-ref b 1)) (+ y (list-ref b 2))
                                         (list-ref b 3) (list-ref b 4) focused? hover pressed))
                  (windows-buttons win))))))

(define (windows-draw-over win x y w h focused? hover pressed)
  (stroke-round-rect x y w h (windows-radius win) (if focused? 'window-border-focus 'window-border)))

(define-window-style! 'windows "Windows"
  'radius windows-radius
  'buttons windows-buttons
  'draw-under windows-draw-under
  'draw-over windows-draw-over)

;;; ------------------------------------------------------------------ Windows 98
;;; Matched against real Windows 98 screenshots: a 4-pixel frame of two
;;; bevels (light gray and white above, black and dark gray below) around
;;; the gray face; an 18-pixel caption, navy fading to blue while active
;;; and gray when not, holding the app's icon and a bold title; 16 × 14
;;; bevelled buttons on the right, minimize touching maximize and close 2
;;; pixels apart. No rounded corners, no hover effects. The
;;; face is the theme's title-bg; the caption runs from accent to accent-2.

(define win98-border 4)

;; The four bevel pens for a face color, lightest first: for the classic
;; #c0c0c0 that is white, #dfdfdf, #808080 and black.
(define (win98-pens face)
  (values #xffffff (mix face #xffffff 0.49) (mix face #x000000 1/3) #x000000))

(define (win98-caption-height)
  (exact (max 18 (- (title-height) 12) (round (+ (font-line-height 'title) 3)))))

;; (values width height): 16 × 14 in the classic 18-pixel caption.
(define (win98-button-size)
  (let ([bh (- (win98-caption-height) 4)])
    (values (exact (round (* bh 8/7))) bh)))

(define (win98-insets win)
  (values win98-border (+ win98-border (win98-caption-height) 1) win98-border win98-border))

(define (win98-buttons win)
  (let*-values ([(bw bh) (win98-button-size)]
                [(y) (+ win98-border 2)]
                [(close-x) (- (window-w win) win98-border 2 bw)]
                [(max-x) (- close-x 2 bw)])
    (list (list 'close close-x y bw bh)
          (list 'maximize max-x y bw bh)
          (list 'minimize (- max-x bw) y bw bh))))

;; A bevelled box, raised or (pressed) sunken.
(define (win98-box x y w h face down?)
  (let-values ([(hi lt sh dk) (win98-pens face)])
    (fill-rect x y w h face)
    (if down?
        (begin (bevel-rect x y w h dk hi) (bevel-rect (+ x 1) (+ y 1) (- w 2) (- h 2) sh lt))
        (begin (bevel-rect x y w h hi dk) (bevel-rect (+ x 1) (+ y 1) (- w 2) (- h 2) lt sh)))))

;; Button glyphs as rectangles in the pixels of a 16 × 14 button.
(define win98-glyphs
  '((minimize (4 9 6 2))
    (maximize (3 2 9 2) (3 2 1 9) (11 2 1 9) (3 10 9 1))
    (restore (5 1 6 2) (5 1 1 3) (10 1 1 6) (8 6 3 1)
             (3 4 6 2) (3 4 1 6) (8 4 1 6) (3 9 6 1))
    (close (4 3 2 1) (10 3 2 1) (5 4 2 1) (9 4 2 1) (6 5 4 1) (7 6 2 1)
           (6 7 4 1) (5 8 2 1) (9 8 2 1) (4 9 2 1) (10 9 2 1))))

(define (win98-draw-button win kind bx by bw bh face pressed)
  (let* ([down? (eq? pressed kind)]
         [glyph (if (and (eq? kind 'maximize) (maximized? win)) 'restore kind)]
         [k (max 1 (quotient bh 14))]           ; whole pixels stay crisp
         [ox (+ bx (quotient (- bw (* 16 k)) 2) (if down? 1 0))]
         [oy (+ by (quotient (- bh (* 14 k)) 2) (if down? 1 0))])
    (win98-box bx by bw bh face down?)
    (when (eq? glyph 'restore)   ; the front window hides the back one
      (fill-rect (+ ox (* 4 k)) (+ oy (* 6 k)) (* 4 k) (* 3 k) face))
    (for-each (lambda (r)
                (fill-rect (+ ox (* k (car r))) (+ oy (* k (cadr r))) (* k (caddr r)) (* k (cadddr r)) #x000000))
              (cdr (assq glyph win98-glyphs)))))

(define (win98-draw-under win x y w h focused? hover pressed)
  (let*-values ([(face) 'title-bg]
                [(hi lt sh dk) (win98-pens face)]
                [(b) win98-border]
                [(ch) (win98-caption-height)]
                [(cx cy cw) (values (+ x b) (+ y b) (- w b b))]
                [(is) (- ch 2)]
                [(icon) (app-color (window-app win))]
                [(title) (window-title win)]
                [(buttons) (win98-buttons win)])
    ;; a window too narrow for its buttons and icon shows only what fits
    (call-with-clip x y w h
      (lambda ()
        (fill-rect x y w h face)
        (bevel-rect x y w h lt dk)
        (bevel-rect (+ x 1) (+ y 1) (- w 2) (- h 2) hi sh)
        (if focused?
            (fill-gradient cx cy cw ch 'accent 'accent-2 'horizontal)
            (fill-gradient cx cy cw ch sh face 'horizontal))
        ;; the app's icon: a square in its color with its initial
        (fill-rect (+ cx 2) (+ cy 1) is is icon)
        (stroke-rect (+ cx 2.5) (+ cy 1.5) (- is 1) (- is 1) (mix icon #x000000 0.4))
        (draw-text-centered (+ cx 2) (+ cy 1) is is
                            (if (string=? title "") "·" (string (char-upcase (string-ref title 0))))
                            (readable-on icon) 'ui-small)
        (draw-text (+ cx is 5) (+ cy (/ (- ch (font-line-height 'title)) 2))
                   (fit-text title (- (list-ref (car (last-pair buttons)) 1) is b 10) 'title)
                   (if focused? #xffffff face) 'title)
        (for-each (lambda (bt)
                    (win98-draw-button win (car bt) (+ x (list-ref bt 1)) (+ y (list-ref bt 2))
                                       (list-ref bt 3) (list-ref bt 4) face pressed))
                  buttons)))))

(define-window-style! 'win98 "Windows 98"
  'insets win98-insets
  'buttons win98-buttons
  'draw-under win98-draw-under)

;;; ------------------------------------------------------------------ Interlisp
;;; Interlisp-D, later Medley (Xerox PARC, 1980s; on Unix workstations
;;; from 1988), matched against a real Medley screen: a 2-pixel border and
;;; a 15-pixel title bar in solid ink with the title in paper color, and
;;; no buttons: everything was done from the right-button menu (here:
;;; right-click the title). Ink is the theme's text color, paper the
;;; window's background, so any palette gets the inverse-video look.

(define interlisp-border 2)

(define (interlisp-title-height)
  (exact (max 15 (- (title-height) 15) (round (+ (font-line-height 'ui-small) 2)))))

(define (interlisp-insets win)
  (values interlisp-border (interlisp-title-height) interlisp-border interlisp-border))

(define (interlisp-draw-under win x y w h focused? hover pressed)
  (let ([th (interlisp-title-height)])
    (fill-rect x y w h 'text)
    (call-with-clip x y w th   ; even "…" may not fit a narrow window
      (lambda ()
        (draw-text (+ x 5) (+ y (floor (/ (- th (font-line-height 'ui-small)) 2)))
                   (fit-text (window-title win) (- w 10) 'ui-small)
                   (window-bg win) 'ui-small)))))

(define-window-style! 'interlisp "Interlisp"
  'insets interlisp-insets
  'draw-under interlisp-draw-under)

;;; ------------------------------------------------------------------ Genera
;;; Symbolics Genera, the Lisp Machine system of the 1980s, as its manuals
;;; describe it. Programming the User Interface (Genera 7, 1986): a pane's
;;; default margins are "a 1-pixel-wide border and a margin between the
;;; border and displayed output to the pane of 4 pixels", and its label
;;; "appears ... in the lower, left-hand corner of the pane (directly
;;; inside the border)" in the pane's default character style, a fixed-
;;; width font. No title bar and no buttons: the label strip is what you
;;; drag, and right-clicking it gives the window menu (Genera explained its
;;; mouse clicks in the documentation line instead).

(define genera-border 1)
(define genera-margin 4)

(define (genera-label-height)
  (exact (max 14 (- (title-height) 16) (round (+ (font-line-height 'mono) 2)))))

(define (genera-insets win)
  (let ([m (+ genera-border genera-margin)])
    (values m m m (+ genera-border (genera-label-height) genera-margin))))

;; The label strip is the window's title area. The corners and the bottom
;; edge stay free for resizing.
(define (genera-buttons win)
  (let ([w (window-w win)] [h (window-h win)] [lh (genera-label-height)])
    (list (list 'title 14 (- h genera-border lh) (- w 28) (- lh 4)))))

(define (genera-draw-under win x y w h focused? hover pressed)
  (let ([lh (genera-label-height)] [b genera-border])
    (fill-rect x y w h 'text)
    (fill-rect (+ x b) (+ y b) (- w b b) (- h b b) (window-bg win))
    (call-with-clip x y w h   ; even "…" may not fit a narrow window
      (lambda ()
        (draw-text (+ x b genera-margin)
                   (+ y (- h b lh) (floor (/ (- lh (font-line-height 'mono)) 2)))
                   (fit-text (window-title win) (- w (* 2 (+ b genera-margin))) 'mono)
                   'text 'mono)))))

(define-window-style! 'genera "Genera"
  'insets genera-insets
  'buttons genera-buttons
  'draw-under genera-draw-under)

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
                    (fill-round-rect 14 (+ 14 (* i 16)) (* frac (- cw 28)) 7 3.5 color))
                  '(0 1 2 3) '(accent-2 accent green text-dim) '(0.45 0.7 0.5 0.35))))
    (call-with-clip x y w h
      (lambda ()
        (call-with-translation x y
          (lambda ()
            (call-with-scale scale (lambda () (draw-window* win #t)))))))))
