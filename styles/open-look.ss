;;; styles/open-look.ss — the OPEN LOOK window style.
;;; Sun's OPEN LOOK as OpenWindows drew it: olwm, its window manager,
;;; through OLGX, its drawing library (XView 3.2 on the X11R6 contrib
;;; tape; Sun's Functional Specification of 1989 where the code is
;;; silent). A window is its window color, BG1, in an outline of ink, 2
;;; pixels, 3 while it has the focus. Its header holds the window button
;;; at the left (a click closes the window to the taskbar, as the
;;; button's default, Close, does) and the title, bold, centered in the
;;; rest; a pop-up window ('pop-up true) has a pushpin there instead,
;;; pinned in: pull it out and the window goes. An L-shaped resize
;;; corner sits at each corner, the only place a window resizes from.
;;; The focus is a band across the header, of ink in 2D and recessed in
;;; 3D, or, with open-look-focus lines, olwm's two lines. A window's
;;; 'footer, a string or (left . right), shows under it in the title's
;;; font. Everything comes in OPEN LOOK's four sizes, 10, 12, 14 and 19
;;; point, the nearest the text size: the fixed parts are OLGX's own
;;; glyphs (kits/olgl.ss) pixel for pixel, the numbers olwm's
;;; (winframe.c), and the shades OLGX's arithmetic, translated from
;;; Sun's code, whose notice goes with it:
;;;
;;;   (c) Copyright 1989, 1990, 1991 Sun Microsystems, Inc. Sun design
;;;   patents pending in the U.S. and foreign countries. OPEN LOOK is a
;;;   trademark of USL. Used by written permission of the owners.
;;;
;;;   (c) Copyright Bigelow & Holmes 1986, 1985. Lucida is a registered
;;;   trademark of Bigelow & Holmes. Permission to use the Lucida
;;;   trademark is hereby granted only in association with the images
;;;   and fonts described in this file.

;; The size: 10, 12, 14 or 19, the nearest the text size.
(define (open-look-size) (olgl-size (theme 'ui-font-size)))

;; One of four numbers, by the size.
(define (open-look-by-size a b c d)
  (case (open-look-size) [(10) a] [(12) b] [(14) c] [else d]))

(define (open-look-3d?) (eq? (theme 'open-look-depth) '3d))

;;; ------------------------------------------------------------------
;;; the frame's numbers, olwm's at each size (winframe.c:94-240)

;; The sides and the bottom, the outline in them: a resize corner's arm.
(define (open-look-arm) (open-look-by-size 5 5 6 6))
;; The header: the window button's height, or the title font's and 2.
(define (open-look-header) (open-look-by-size 13 15 17 23))
;; Above the pane: the header, an arm above it and one below.
(define (open-look-top) (+ (open-look-header) (* 2 (open-look-arm))))
;; A resize corner's square, its arms' length.
(define (open-look-corner) (open-look-by-size 10 11 12 14))
;; The window button, an abbreviated menu button, and the pushpin.
(define (open-look-button-w) (open-look-by-size 14 16 18 21))
(define (open-look-button-h) (- (open-look-button-w) 1))
(define (open-look-pin-w) (open-look-by-size 26 26 28 28))
(define (open-look-pin-h) (open-look-by-size 13 13 15 15))
;; The title's baseline under the header's top: Lucida Sans Bold's ascent.
(define (open-look-ascent) (open-look-by-size 9 11 13 17))
;; The footer, the title font's height and an arm; its baseline over the
;; bottom edge, the font's descent and an arm.
(define (open-look-footer-h)
  (+ (open-look-by-size 11 13 15 21) (open-look-arm)))
(define (open-look-footer-base) (open-look-by-size 7 7 8 10))

(define (open-look-pop-up? win) (and (window-ref win 'pop-up #f) #t))

;; Where the window button, or a pop-up's pushpin, sits: (x y w h), the
;; header's height centering it.
(define (open-look-gadget win)
  (let ([x (+ (open-look-size) 2)] [a (open-look-arm)]
        [hh (open-look-header)])
    (if (open-look-pop-up? win)
        (list x (+ a (quotient (- hh (open-look-pin-h)) 2))
              (open-look-pin-w) (open-look-pin-h))
        (list x (+ a (quotient (- hh (open-look-button-h)) 2))
              (open-look-button-w) (open-look-button-h)))))

(define (open-look-insets win)
  (let ([a (open-look-arm)])
    (values a (open-look-top) a
            (if (window-ref win 'footer #f) (+ a (open-look-footer-h)) a))))

;; The button closes, the pushpin dismisses; a base window's corners
;; resize it (a pop-up keeps its size, as XView's do).
(define (open-look-regions win)
  (let ([w (window-w win)] [h (window-h win)] [c (open-look-corner)])
    (if (open-look-pop-up? win)
        (list (cons 'close (open-look-gadget win)))
        (list (cons 'minimize (open-look-gadget win))
              (list 'resize-tl 0 0 c c)
              (list 'resize-tr (- w c) 0 c c)
              (list 'resize-br (- w c) (- h c) c c)
              (list 'resize-bl 0 (- h c) c c)))))

;;; ------------------------------------------------------------------
;;; OLGX's colors (olgx_calculate_3Dcolors, ol_color.c:45-244)

;; OLGX's HSV of #xRRGGBB: h 0-359, s and v 0-1000, integer for integer.
(define (open-look-hsv c)
  (let* ([r (fxand (fxsrl c 16) 255)] [g (fxand (fxsrl c 8) 255)]
         [b (fxand c 255)] [mx (max r g b)] [mn (min r g b)]
         [s (if (> mx 0) (quotient (* (- mx mn) 255) mx) 0)])
    (values
     (if (= s 0)
         0
         (let* ([d (- mx mn)]
                [rc (quotient (* (- mx r) 255) d)]
                [gc (quotient (* (- mx g) 255) d)]
                [bc (quotient (* (- mx b) 255) d)]
                [h (cond [(= r mx) (quotient (* (- bc gc) 60) 255)]
                         [(= g mx) (+ 120 (quotient (* (- rc bc) 60) 255))]
                         [else (+ 240 (quotient (* (- gc rc) 60) 255))])])
           (if (< h 0) (+ h 360) h)))
     (quotient (* s 1000) 255)
     (quotient (* mx 1000) 255))))

(define (open-look-rgb h s v)
  (let* ([s (quotient (* s 255) 1000)] [v (quotient (* v 255) 1000)]
         [h (if (or (= h 360) (= s 0)) 0 h)]
         [f (remainder h 60)]
         [p (quotient (* v (- 255 s)) 255)]
         [q (quotient (* v (- 255 (quotient (* s f) 60))) 255)]
         [t (quotient (* v (- 255 (quotient (* s (- 60 f)) 60))) 255)])
    (case (quotient h 60)
      [(0) (rgb v t p)] [(1) (rgb q v p)] [(2) (rgb p v t)]
      [(3) (rgb p q v)] [(4) (rgb t p v)] [else (rgb v p q)])))

;; A 3D shade of color c: bg2, its value x 0.9; bg3, x 0.5; highlight,
;; x 1.2, its saturation halved if that passes the top, its value at
;; least 0.4. So #cccccc's are #b7b7b7, #666666 and #f4f4f4.
(define (open-look-shade c which)
  (let-values ([(h s v) (open-look-hsv (color->fixnum c))])
    (case which
      [(bg2) (open-look-rgb h s (quotient (* v 9) 10))]
      [(bg3) (open-look-rgb h s (quotient v 2))]
      [else (let ([v (quotient (* v 12) 10)])
              (open-look-rgb h (if (> v 1000) (quotient s 2) s)
                             (max 400 (min v 1000))))])))

;;; ------------------------------------------------------------------
;;; OLGX's drawing

;; Glyph name of the size's font, its top-left pixel at (x, y), its set
;; pixels in color, a row's run in one px-fill; cut, if given, to the
;; window cw × ch that olwm draws it in (at 19 point some glyphs are a
;; pixel larger than theirs).
(define (open-look-glyph x y name color . cut)
  (let ([g (olgl-glyph (open-look-size) name)])
    (when g
      (let ([w (if (pair? cut) (min (car g) (car cut)) (car g))]
            [n (if (pair? cut) (cadr cut) (cadr g))])
        (let rows ([rs (cddr g)] [j 0])
          (unless (or (null? rs) (>= j n))
            (let ([on? (lambda (i) (and (< i w)
                                        (fxbit-set? (car rs)
                                                    (- (car g) 1 i))))])
              (let run ([i 0])
                (cond
                  [(>= i w) (void)]
                  [(on? i)
                   (let end ([e (+ i 1)])
                     (if (on? e)
                         (end (+ e 1))
                         (begin (px-fill (+ x i) (+ y j) (- e i) 1 color)
                                (run e))))]
                  [else (run (+ i 1))])))
            (rows (cdr rs) (+ j 1))))))))

;; olgx_draw_box (ol_draw.c:42-118): the top and left lines in ul, the
;; bottom and right in lr, which has the top right pixel; inside, fill.
(define (open-look-box x y w h ul lr fill)
  (px-fill (+ x 1) (+ y 1) (- w 2) (- h 2) fill)
  (px-fill x y w 1 ul)
  (px-fill x y 1 h ul)
  (px-fill (+ x 1) (+ y h -1) (- w 1) 1 lr)
  (px-fill (+ x w -1) y 1 h lr))

;; The vertical menu mark in 3D (olgx_draw_menu_mark, ol_button.c:485-549).
(define (open-look-menu-mark x y)
  (open-look-glyph x y 'vert-menu-mark-ul 'open-look-bg3)
  (open-look-glyph x y 'vert-menu-mark-lr 'open-look-highlight)
  (open-look-glyph x y 'vert-menu-mark-fill 'open-look-bg2))

;;; ------------------------------------------------------------------
;;; the frame

;; The focus band, (x y) the window's corner: ink in 2D, a recessed box
;; in 3D (winframe.c:545-574, 660-698).
(define (open-look-draw-band x y w)
  (let ([a (open-look-arm)] [hh (open-look-header)])
    (if (open-look-3d?)
        (open-look-box (+ x a) (+ y a) (- w a a) hh
                       'open-look-bg3 'open-look-highlight 'open-look-bg2)
        (px-fill (+ x a) (+ y a -1) (- w a a) (+ hh 1) 'open-look-ink))))

;; Or olwm's two lines, 2 pixels, under the outline o between the corners
;; and over the pane: ink, or raised in 3D (winframe.c:577-630, 701-751).
(define (open-look-draw-lines x y w o)
  (let ([a (open-look-arm)] [c (open-look-corner)])
    (define (line lx ly lw)
      (if (open-look-3d?)
          (begin (px-fill lx ly lw 1 'open-look-highlight)
                 (px-fill lx (+ ly 1) lw 1 'open-look-bg3))
          (px-fill lx ly lw 2 'open-look-ink)))
    (line (+ x c 1) (+ y o 1) (- w c c 2))
    (line (+ x a) (+ y (open-look-top) -3) (- w a a))))

;; The resize corners (olgx_draw_resize_corner, ol_misc.c:17-68): an L of
;; three layers in 3D, the pressed one's colors turned; an outline of ink
;; and its inside in 2D, of ink when pressed.
(define (open-look-draw-corners x y w h pressed)
  (let ([c (open-look-corner)])
    (for-each
     (lambda (corner region cx cy)
       (let ([down? (eq? pressed region)]
             [layer (lambda (l) (string->symbol
                                 (format "~a-resize-~a" corner l)))])
         (define (draw l color) (open-look-glyph cx cy (layer l) color c c))
         (if (open-look-3d?)
             (begin
               (draw 'ul (if down? 'open-look-bg3 'open-look-highlight))
               (draw 'lr (if down? 'open-look-highlight 'open-look-bg3))
               (draw 'fill (if down? 'open-look-bg2 'open-look-window)))
             (begin
               (draw 'outline 'open-look-ink)
               (draw 'fill (if down? 'open-look-ink 'open-look-window))))))
     '(ul ur lr ll) '(resize-tl resize-tr resize-br resize-bl)
     (list x (+ x w (- c)) (+ x w (- c)) x)
     (list y y (+ y h (- c)) (+ y h (- c))))))

;; The window button (olgx_draw_abbrev_button, ol_button.c:553-643) on
;; the window color olwm erases it to (winbutton.c:226-276), on the 2D
;; band in a ring of ink.
(define (open-look-draw-button x y pressed? band?)
  (let ([bw (open-look-button-w)] [bh (open-look-button-h)])
    (define (draw name color) (open-look-glyph x y name color bw bh))
    (px-fill x y bw bh 'open-look-window)
    (if (open-look-3d?)
        (let ([m (open-look-by-size 7 7 11 13)])   ; the menu mark's size
          (draw 'abbrev-menu-ul
                (if pressed? 'open-look-bg3 'open-look-highlight))
          (draw 'abbrev-menu-lr
                (if pressed? 'open-look-highlight 'open-look-bg3))
          (draw 'abbrev-menu-fill
                (if pressed? 'open-look-bg2 'open-look-window))
          (open-look-menu-mark (+ x (quotient (- bw m) 2))
                               (+ y (quotient (- (+ 1 bh) m) 2))))
        (begin
          (draw (if pressed? 'abbrev-menu-button-inverted 'abbrev-menu-button)
                'open-look-ink)
          (when band? (px-ring x y bw bh 'open-look-ink))))))

;; A pop-up's pushpin (olgx_draw_pushpin, ol_misc.c:70-147), pinned in or
;; pulled out, on what olwm erases under it (winpush.c:160-206): in 2D on
;; the band, the window color on ink.
(define (open-look-draw-pin x y in? band?)
  (let ([pw (open-look-pin-w)] [ph (open-look-pin-h)])
    (define (draw name color) (open-look-glyph x y name color pw ph))
    (if (open-look-3d?)
        (begin
          (px-fill x y pw ph (if band? 'open-look-bg2 'open-look-window))
          (draw (if in? 'pushpin-in-top 'pushpin-out-top) 'open-look-highlight)
          (draw (if in? 'pushpin-in-bottom 'pushpin-out-bottom) 'open-look-bg3)
          (draw (if in? 'pushpin-in-middle 'pushpin-out-middle) 'open-look-bg2))
        (begin
          (px-fill x y pw ph (if band? 'open-look-ink 'open-look-window))
          (draw (if in? 'menu-pin-in 'menu-pin-out)
                (if band? 'open-look-window 'open-look-ink))))))

;; str's longest start within w pixels in font, cut with no ellipsis.
(define (open-look-cut str w font)
  (let ([s (text-within str w)])
    (if (<= (text-width s font) w)
        s
        (let loop ([lo 0] [hi (string-length s)])   ; lo fits, hi does not
          (if (<= (- hi lo) 1)
              (substring s 0 lo)
              (let ([m (quotient (+ lo hi) 2)])
                (if (<= (text-width (substring s 0 m) font) w)
                    (loop m hi)
                    (loop lo m))))))))

;; olwm's title: too wide, it loses all up to its first colon, then what
;; does not fit at the right (winframe.c:1366-1396).
(define (open-look-fit-title str w font)
  (let* ([n (string-length str)]
         [wide? (or (> n (+ 1 (max 0 w))) (> (text-width str font) w))]
         [colon (let loop ([i 0])
                  (cond [(= i n) #f]
                        [(char=? (string-ref str i) #\:) i]
                        [else (loop (+ i 1))]))])
    (open-look-cut (if (and wide? colon) (substring str (+ colon 1) n) str)
                   w font)))

;; Text with its baseline at y, on a whole device row.
(define (open-look-text x y str color font)
  (draw-text x (- (px-baseline y round) (font-ascent font)) str color font))

;; The title, centered right of the button or pushpin (winframe.c:
;; 1313-1401), in the window color on the 2D band (inverse?).
(define (open-look-draw-title win x y w inverse?)
  (let* ([g (open-look-gadget win)]
         [off (+ (car g) (caddr g) (quotient (open-look-size) 4))]
         [room (- w (open-look-arm) off)])
    (when (> room 0)
      (let* ([t (open-look-fit-title (window-title win) room 'title)]
             [tw (text-width t 'title)])
        (open-look-text (+ x off (floor (/ (- room tw) 2)))
                        (+ y (open-look-arm) (open-look-ascent)) t
                        (if inverse? 'open-look-window 'open-look-ink)
                        'title)))))

;; The footer: its left text at the left margin, its right text at the
;; right, which keeps at most a quarter when both do not fit (winframe.c:
;; 787-858).
(define (open-look-draw-footer win x y w h)
  (let ([f (window-ref win 'footer #f)])
    (when f
      (let* ([left (if (pair? f) (car f) f)] [right (if (pair? f) (cdr f) "")]
             [m (+ (open-look-size) 2)] [gap (open-look-size)]
             [room (- w m m)] [base (+ y h (- (open-look-footer-base)))]
             [rw (text-width right 'title)]
             [right (if (> (+ (text-width left 'title) gap rw) room)
                        (open-look-cut right (min rw (/ room 4)) 'title)
                        right)]
             [rw (text-width right 'title)]
             [left (open-look-cut left (- room (if (string=? right "")
                                                   0
                                                   (+ gap rw)))
                                  'title)])
        (open-look-text (+ x m) base left 'open-look-ink 'title)
        (open-look-text (+ x w (- m) (- rw)) base right 'open-look-ink
                        'title)))))

(define (open-look-draw-under win x y w h focused? hover pressed)
  (let* ([o (if focused? 3 2)] [c (open-look-corner)]
         [band? (and focused? (not (eq? (theme 'open-look-focus) 'lines)))]
         [g (open-look-gadget win)] [gx (+ x (car g))] [gy (+ y (cadr g))])
    (px-fill x y w h 'open-look-ink)
    (px-fill (+ x o) (+ y o) (- w o o) (- h o o) 'open-look-window)
    ;; a base window's corners lie over the outline, the band over them
    (unless (open-look-pop-up? win)
      (px-fill x y c c 'open-look-window)
      (px-fill (+ x w (- c)) y c c 'open-look-window)
      (px-fill (+ x w (- c)) (+ y h (- c)) c c 'open-look-window)
      (px-fill x (+ y h (- c)) c c 'open-look-window))
    (cond [band? (open-look-draw-band x y w)]
          [focused? (open-look-draw-lines x y w o)])
    (if (open-look-pop-up? win)
        (open-look-draw-pin gx gy (not (eq? pressed 'close)) band?)
        (begin (open-look-draw-corners x y w h pressed)
               (open-look-draw-button gx gy (eq? pressed 'minimize) band?)))
    (open-look-draw-title win x y w (and band? (not (open-look-3d?))))
    (open-look-draw-footer win x y w h)))

;; The window color and the ink, which you may change and save, and the
;; shades OLGX takes from the window color.
(color-default! 'open-look-window 'window-bg)
(color-default! 'open-look-ink 'text)
(color-default! 'open-look-bg2
                (theme-rule (open-look-shade 'open-look-window 'bg2)))
(color-default! 'open-look-bg3
                (theme-rule (open-look-shade 'open-look-window 'bg3)))
(color-default! 'open-look-highlight
                (theme-rule (open-look-shade 'open-look-window 'highlight)))

(define-style open-look "OPEN LOOK"
  'options '(palette
             wallpaper
             ui-font
             ui-font-size
             mono-font
             mono-font-size
             (open-look-depth "Look" (("3D" . 3d) ("2D" . 2d)))
             (open-look-focus "Focus" (("Band" . band) ("Lines" . lines))))
  'palette 'open-look
  'wallpaper 'flat
  'ui-font "Lucida Sans"
  'ui-font-size 12
  'open-look-depth '3d
  'open-look-focus 'band
  'frame-insets 'open-look-insets
  'frame-regions 'open-look-regions
  'frame-under 'open-look-draw-under
  ;; only the corners resize
  'frame-edge 0)
