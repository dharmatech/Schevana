;;; styles.ss — window styles: the frame around each window's content.
;;;
;;; A style decides how thick the frame is (insets), where its buttons are,
;;; and how it is drawn, in two passes: draw-under before the content and
;;; draw-over after it. The global style is the theme key 'window-style;
;;; one window can override it with its own 'window-style property.
;;;
;;;   (set-theme! 'window-style 'amiga)
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
;;; minimize, maximize and depth (send to back / bring to front).
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
;; Swap the colours for a recessed look.
(define (bevel-rect x y w h light dark)
  (fill-rect x y w 1 light)
  (fill-rect x y 1 h light)
  (fill-rect x (+ y h -1) w 1 dark)
  (fill-rect (+ x w -1) y 1 h dark))

(define (draw-resize-grip x y)
  (for-each (lambda (d) (draw-line (- x d) (- y 3) (- x 3) (- y d) (alpha 'text 0.18) 1))
            '(7 11)))

(define (window-shadow win x y w h r focused?)
  (unless (maximized? win)
    (draw-shadow x y w h r (if focused? (theme 'shadow) (quotient (theme 'shadow) 2)))))

;;; ------------------------------------------------------------------ Mac
;;; Traffic lights on the left, glyphs on hover, centred title.

(define (mac-radius win) (if (maximized? win) 0 (theme 'corner-radius)))

(define (mac-buttons win)
  (let ([cy (/ (title-height) 2)])
    (map (lambda (kind cx) (list kind (- cx 9) (- cy 9) 18 18))
         '(close minimize maximize) '(18 38 58))))

(define (mac-draw-under win x y w h focused? hover pressed)
  (let ([th (title-height)] [r (mac-radius win)]
        [glyphs? (memq hover '(close minimize maximize))])
    (window-shadow win x y w h r focused?)
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
                        (draw-text-centered (+ x cx -6) (- cy 7) 12 13 glyph (alpha #x000000 0.6) 'ui-bold))))
                  '(18 38 58) '(button-close button-min button-max) '("×" "–" "+"))
        (draw-text-centered (+ x 70) y (- w 140) th (fit-text (window-title win) (- w 160) 'title)
                            (if focused? 'title-text 'title-text-dim) 'title)))))

(define (mac-draw-over win x y w h focused? hover pressed)
  (stroke-round-rect x y w h (mac-radius win) (if focused? 'window-border-focus 'window-border))
  (unless (maximized? win) (draw-resize-grip (+ x w) (+ y h))))

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
         [icon (app-color (window-app win))])
    (window-shadow win x y w h r focused?)
    (fill-round-rect x y w h r (window-bg win))
    (call-with-round-clip x y w h r
      (lambda ()
        (fill-rect x y w th (if focused? 'title-bg-focus 'title-bg))
        (fill-round-rect (+ x 12) (+ y (quotient (- th 16) 2)) 16 16 4 icon)
        (draw-text-centered (+ x 12) (+ y (quotient (- th 16) 2)) 16 16
                            (if (string=? title "") "·" (string (char-upcase (string-ref title 0))))
                            (readable-on icon) 'ui-small)
        (draw-text (+ x 38) (+ y (/ (- th (font-line-height 'ui)) 2))
                   (fit-text title (- w 38 (* 3 windows-button-width) 12) 'ui)
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

;;; ------------------------------------------------------------------ Amiga
;;; Workbench 3.1, matched against a real screenshot: four pens (fill,
;;; shine white, shadow black, background grey); every part of the frame is
;;; a raised section with a white top-left and black bottom-right edge; the
;;; frame is filled with the accent while active. Close gadget on the left,
;;; zoom and depth on the right, a size gadget at the right end of the tall
;;; bottom border. Title text is black on blue, in a monospace font
;;; (Topaz). There is no minimize.

(define amiga-shine #xffffff)
(define amiga-shadow #x000000)

(define (amiga-title-height) (max 18 (- (title-height) 8)))
(define (amiga-bottom-height) (max 12 (exact (round (* 0.8 (amiga-title-height))))))
(define (amiga-close-width) (exact (round (* 1.15 (amiga-title-height)))))
(define (amiga-gadget-width) (exact (round (* 1.45 (amiga-title-height)))))
(define (amiga-size-width) (exact (round (* 1.2 (amiga-bottom-height)))))

(define (amiga-insets win)
  (values 4 (amiga-title-height) 4 (amiga-bottom-height)))

(define (amiga-buttons win)
  (let ([w (window-w win)] [h (window-h win)] [t (amiga-title-height)] [gw (amiga-gadget-width)])
    (append
     (list (list 'close 0 0 (amiga-close-width) t)
           (list 'maximize (- w gw gw) 0 gw t)
           (list 'depth (- w gw) 0 gw t))
     (if (maximized? win)
         '()
         (let ([b (amiga-bottom-height)] [sw (amiga-size-width)])
           (list (list 'resize-br (- w sw) (- h b) sw b)))))))

;; A raised section of the frame; pressed sections are recessed.
(define (amiga-section x y w h face down?)
  (fill-rect x y w h face)
  (if down?
      (bevel-rect x y w h amiga-shadow amiga-shine)
      (bevel-rect x y w h amiga-shine amiga-shadow)))

(define (amiga-gadget kind gx gy gw gh fill down?)
  (define (r v) (exact (round v)))
  (define (box x y w h color)
    (fill-rect (r x) (r y) (r w) (r h) color)
    (stroke-rect (r x) (r y) (r w) (r h) amiga-shadow))
  (amiga-section gx gy gw gh (if down? (mix fill #x000000 0.15) fill) down?)
  (case kind
    [(close)
     (let ([iw (max 4 (r (* 0.32 gh)))] [ih (max 6 (r (* 0.45 gh)))])
       (box (+ gx (/ (- gw iw) 2)) (+ gy (/ (- gh ih) 2)) iw ih amiga-shine))]
    [(maximize)
     (let* ([ow (r (* 0.56 gw))] [oh (r (* 0.7 gh))] [s (r (* 0.45 oh))]
            [ox (r (+ gx (/ (- gw ow) 2)))] [oy (r (+ gy (/ (- gh oh) 2)))])
       (box ox oy ow oh fill)
       (box ox oy s s amiga-shine))]
    [(depth)
     (let* ([rw (r (* 0.47 gw))] [rh (r (* 0.45 gh))]
            [dx (r (* 0.4 rw))] [dy (r (* 0.45 rh))]
            [bx (r (+ gx (/ (- gw rw dx) 2)))] [by (r (+ gy (/ (- gh rh dy) 2)))])
       (box bx by rw rh 'bg-1)
       (box (+ bx dx) (+ by dy) rw rh amiga-shine))]
    [(resize-br)
     (let ([x0 (+ gx 4.5)] [y0 (+ gy 3.5)] [x1 (+ gx gw -4.5)] [y1 (+ gy gh -3.5)])
       (fill-polygon (list (cons x0 y1) (cons x1 y1) (cons x1 y0)) fill)
       (draw-polyline (list (cons x0 y1) (cons x1 y1) (cons x1 y0) (cons x0 y1)) amiga-shadow 1))]))

(define (amiga-draw-under win x y w h focused? hover pressed)
  (let ([fill (if focused? 'accent 'bg-1)]
        [t (amiga-title-height)]
        [cw (amiga-close-width)]
        [gw (amiga-gadget-width)])
    (amiga-section x y w h fill #f)
    ;; the drag bar between the gadgets
    (amiga-section (+ x cw) y (- w cw gw gw) t fill #f)
    (for-each (lambda (b)
                (amiga-gadget (car b) (+ x (list-ref b 1)) (+ y (list-ref b 2))
                              (list-ref b 3) (list-ref b 4) fill (eq? pressed (car b))))
              (amiga-buttons win))
    (draw-text (+ x cw 6) (+ y (/ (- t (font-line-height 'mono)) 2))
               (fit-text (window-title win) (- w cw gw gw 12) 'mono)
               (readable-on fill) 'mono)
    ;; the content sits in a recessed well
    (let-values ([(cx cy cw ch) (content-rect win)])
      (bevel-rect (- cx 1) (- cy 1) (+ cw 2) (+ ch 2) amiga-shadow amiga-shine))))

(define-window-style! 'amiga "Amiga"
  'insets amiga-insets
  'buttons amiga-buttons
  'draw-under amiga-draw-under)

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
