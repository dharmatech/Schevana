;;; styles/slab.ss — the Slab window style.
;;; Neo-brutalist and toy-like. Every window is a flat card in a thick ink
;;; outline, standing on a slab: a solid, hard-edged copy of its outline,
;;; set off down and to the right. The focused window is lifted high off
;;; its slab, which is the accent color, and higher still while it is
;;; carried by its title; the others sit low on a faint one. The title bar
;;; is flat with a bold title, and the buttons are chunky square keys on
;;; slabs of their own: a pressed key goes down onto its slab, like a key
;;; on a toy keyboard, and comes up again if the mouse slides off it.

;; Lines are 2 pixels at any font, as thicker ones clog the keys. Keys
;; follow the title's font (18 square at the defaults, 2 above their
;; slabs, 3 apart), are even so that glyphs center, and keep 3 pixels
;; above and below in the bar, less in one too low for 14-pixel keys.
(define slab-line 2)
(define slab-lift 8)
(define slab-gap 3)
(define (slab-font) (exact (round (font-line-height 'title))))
(define (slab-bar) (- (title-height) slab-line slab-line))
(define (slab-key)
  (let ([r (- (slab-bar) slab-line)])
    (* 2 (quotient (max (min (+ (slab-font) 4) (- r 6)) (min 14 (- r 2)))
                   2))))

;; Above and below a key with its slab, in the bar.
(define (slab-margin) (quotient (- (slab-bar) (slab-key) slab-line) 2))

;; The card leaves room on the right and below for the slab at its
;; highest, so that nothing is drawn outside the window. Maximized, there
;; is no slab and no outline at the screen's edges.
(define (slab-room win) (if (maximized? win) 0 slab-lift))

(define (slab-insets win)
  (let ([t (if (maximized? win) 0 slab-line)] [r (slab-room win)])
    (values t (title-height) (+ t r) (+ t r))))

;; Each button is a key, its slab and the gap after it. The last key's
;; slab is as far from the outline as the keys are from the bar's top,
;; and at least a gap.
(define (slab-buttons win)
  (let* ([p (+ (slab-key) slab-line slab-gap)]
         [x0 (- (+ (window-w win) slab-gap) (slab-room win) slab-line
                (max slab-gap (slab-margin)) (* 3 p))])
    (map (lambda (kind i) (list kind (+ x0 (* i p)) 0 p (title-height)))
         '(minimize maximize close) '(0 1 2))))

;; Boxes ((x y w h) ...) filled with their edges moved to the nearest
;; device pixels, so that every line is crisp at any scale, in the
;; picker's half-size miniature too.
(define (slab-boxes boxes color)
  (cairo-get-matrix *cr* *scratch*)
  (let ([s (foreign-ref 'double *scratch* 0)]
        [ox (foreign-ref 'double *scratch* 32)]
        [oy (foreign-ref 'double *scratch* 40)])
    (define (snap v o) (/ (- (floor (+ (* v s) o 1/2)) o) s))
    (fill-rects (map (lambda (b)
                       (let ([x (snap (car b) ox)] [y (snap (cadr b) oy)])
                         (list x y (- (snap (+ (car b) (caddr b)) ox) x)
                               (- (snap (+ (cadr b) (cadddr b)) oy) y))))
                     boxes)
                color)))

(define (slab-box x y w h color) (slab-boxes (list (list x y w h)) color))

;; What shows of a slab o below and right of the box x y w h.
(define (slab-under x y w h o color)
  (slab-boxes (list (list (+ x w) (+ y o) o h) (list (+ x o) (+ y h) (- w o) o))
              color))

;; The title's y, its capitals centered on cy. The cap height is measured
;; on *cr* at the scale it is drawn (hinting makes it 9 pixels at 100%
;; and 10 at 200%), and the baseline goes on the nearest device pixel,
;; the higher one when two are as near.
(define (slab-title-y cy)
  (use-font! (get-font 'title))
  (cairo-text-extents *cr* "H" *scratch*)
  (let ([base (- cy (/ (foreign-ref 'double *scratch* 8) 2))])
    (- (/ (ceiling (- (* base *scale*) 1/2)) *scale*) (font-ascent 'title))))

;; ×, −, a square, or (to restore) a square on its slab, 2a wide around
;; the middle (cx, cy) of a key, on the key's face. The × reaches a
;; little past the square, so that it looks as big.
(define (slab-glyph win kind cx cy a ink face)
  (let ([t slab-line])
    (case kind
      [(close) (let ([e (- a (/ t 4))])
                 (draw-line (- cx e) (- cy e) (+ cx e) (+ cy e) ink t)
                 (draw-line (+ cx e) (- cy e) (- cx e) (+ cy e) ink t))]
      [(minimize) (slab-box (- cx a) (- cy (/ t 2)) (* 2 a) t ink)]
      [else
       (let* ([d (if (maximized? win) (/ t 2) 0)]
              [x (- cx a d)] [y (- cy a d)] [s (* 2 a)])
         (when (maximized? win) (slab-under x y s s t ink))
         (slab-box x y s s ink)
         (slab-box (+ x t) (+ y t) (- s t t) (- s t t) face))])))

(define (slab-draw-key win kind kx ky ink under hover pressed)
  (let* ([k (slab-key)] [t slab-line]
         [hot? (eq? hover kind)] [down? (and hot? (eq? pressed kind))]
         [face (cond [(not hot?) (window-bg win)]
                     [(eq? kind 'close) 'button-close]
                     [(eq? kind 'minimize) 'button-min]
                     [else 'button-max])]
         [kx (if down? (+ kx t) kx)] [ky (if down? (+ ky t) ky)])
    (unless down? (slab-under kx ky k k t under))
    (slab-box kx ky k k ink)
    (slab-box (+ kx t) (+ ky t) (- k t t) (- k t t) face)
    (slab-glyph win kind (+ kx (/ k 2)) (+ ky (/ k 2)) (round (* 2/9 k))
                (if hot? (readable-on face) ink) face)))

;; The card is filled with ink, and the bar and the content (which the
;; window manager fills) leave the outline and the bar's rule showing.
(define (slab-draw-under win x y w h focused? hover pressed)
  (let* ([t slab-line] [b (slab-bar)] [max? (maximized? win)]
         [cw (- w (slab-room win))] [ch (- h (slab-room win))]
         [ink (if focused? 'text 'text-faint)]
         [under (if focused? 'accent ink)]
         ;; how high the card stands: 2, 6 with the focus, and 8 while
         ;; it is carried, like a block picked up
         [o (cond [(eq? pressed 'move) slab-lift]
                  [focused? (- slab-lift t)]
                  [else t])]
         [bx (if max? x (+ x t))] [bw (if max? cw (- cw t t))]
         [buttons (slab-buttons win)] [ky (+ y t (slab-margin))]
         ;; as far from the outline as the capitals are from the bar's top
         [pad (quotient (- b (quotient (* 3 (slab-font)) 5)) 2)]
         [title (fit-text (window-title win)
                          (- (cadar buttons) t pad pad) 'title)])
    (unless max? (slab-under x y cw ch o under))
    (slab-box x y cw ch ink)
    (slab-box bx (+ y t) bw b (if focused? 'title-bg-focus 'title-bg))
    (call-with-clip bx (+ y t) bw b
      (lambda ()
        (draw-text (+ bx pad) (slab-title-y (+ y t (/ b 2))) title
                   (if focused? 'title-text 'title-text-dim) 'title)
        (for-each (lambda (k)
                    (slab-draw-key win (car k) (+ x (cadr k)) ky ink under
                                   hover pressed))
                  buttons)))))

(define-window-style slab "Slab"
  'insets slab-insets
  'buttons slab-buttons
  'draw-under slab-draw-under)
