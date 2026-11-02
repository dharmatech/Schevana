;;; styles/fold.ss — the Fold window style.
;;; Each window is a sheet of paper with its top-right corner folded over:
;;; a dog-ear, cut along the diagonal, that shows the back of the sheet.
;;; The dog-ear is the close button: you fold the page away. The title is
;;; written at the sheet's top left, and minimize and maximize are two
;;; quiet marks beside the fold. The focused sheet is folded down to its
;;; content and its back is the accent, like origami paper; the others
;;; recede, folded less, their backs a shade off the paper.

(define fold-margin 10)   ; the title lines up with the REPL's text
(define fold-mark 9)      ; the marks' size: odd, so they have a middle

;; The dog-ear's square, which closes the window, is as tall as the top
;; strip. The marks are a gap apart, and a gap from it.
(define (fold-gap) (round (* 2/5 (title-height))))

;; How deep the corner is folded: down to the content on the focused
;; sheet or while it is the button in use, less on the others.
(define (fold-depth focused? close?)
  (if (or focused? close?)
      (title-height)
      (round (* 3/5 (title-height)))))

(define (fold-insets win)   ; a 1-pixel edge, none at the screen's edges
  (let ([e (if (maximized? win) 0 1)])
    (values e (title-height) e e)))

;; Each mark's cell reaches half a gap past it on either side.
(define (fold-regions win)
  (let* ([th (title-height)] [g (fold-gap)] [c (+ fold-mark g)]
         [x (- (window-w win) th c (quotient g 2))])
    (list (list 'close (- (window-w win) th) 0 th th)
          (list 'maximize x 0 c th)
          (list 'minimize (- x c) 0 c th))))

;; The y nearest to v on a whole device pixel, however *cr* is scaled.
(define (fold-snap v)
  (cairo-get-matrix *cr* *scratch*)
  (let ([k (foreign-ref 'double *scratch* 24)]
        [o (foreign-ref 'double *scratch* 40)])
    (/ (- (round (+ (* k v) o)) o) k)))

;; (values baseline middle): where the title stands so that the middle of
;; its capitals is the middle of the top strip under its 1-pixel edge.
;; The cap height is measured on *cr*, at the scale it is drawn: hinting
;; makes it 9 pixels at 100% but 10 at 200%.
(define (fold-baseline y th)
  (use-font! (get-font 'title))
  (cairo-text-extents *cr* "H" *scratch*)
  (let* ([cap (- (foreign-ref 'double *scratch* 8))]
         [base (fold-snap (+ y (/ (+ th 1) 2) (/ cap 2)))])
    (values base (- base (/ cap 2)))))

;; A polygon with a 1-pixel edge: outer in edge, inner (1 pixel in) in fill.
(define (fold-shape! outer inner edge fill)
  (fill-polygon outer edge)
  (fill-polygon inner fill))

;; The sheet, its corner cut f deep and its sides b thick.
(define (fold-sheet! x y w h f b edge paper)
  (let ([r (+ x w)] [d (- (sqrt 2) 1)] [bottom (+ y h (- b))])
    (fold-shape! (list (cons x y) (cons (- r f) y) (cons r (+ y f))
                       (cons r (+ y h)) (cons x (+ y h)))
                 (list (cons (+ x b) (+ y 1)) (cons (- r f d) (+ y 1))
                       (cons (- r b) (+ y f d (- 1 b)))
                       (cons (- r b) bottom) (cons (+ x b) bottom))
                 edge paper)))

;; The dog-ear: the cut corner folded down onto the sheet, its tip at the
;; bottom left, its right side on the sheet's right edge r.
(define (fold-flap! r y f edge back)
  (let ([d (sqrt 2)] [l (- r f)])
    (fold-shape! (list (cons l y) (cons r (+ y f)) (cons l (+ y f)))
                 (list (cons (+ l 1) (+ y 1 d)) (cons (- r 1 d) (+ y f -1))
                       (cons (+ l 1) (+ y f -1)))
                 edge back)))

;; A dash, a square, or two squares to restore, in the mark's box at (x, y).
(define (fold-draw-mark win kind x y ink)
  (let ([s fold-mark])
    (cond [(eq? kind 'minimize) (fill-rect x (+ y (quotient s 2)) s 1 ink)]
          [(maximized? win)   ; a sheet in front of another
           (stroke-rect x (+ y 2) (- s 2) (- s 2) ink)
           (fill-rect (+ x 2) y (- s 2) 1 ink)
           (fill-rect (+ x s -1) y 1 (- s 2) ink)]
          [else (stroke-rect x y s s ink)])))

(define (fold-draw-under win x y w h focused? hover pressed)
  (let*-values ([(th) (title-height)] [(g) (fold-gap)] [(paper) (window-bg win)]
                [(close?) (memq 'close (list hover pressed))]
                [(f) (fold-depth focused? close?)]
                [(ink) (if focused? 'title-text 'title-text-dim)]
                [(base mid) (fold-baseline y th)]
                [(top) (fold-snap (- mid (/ fold-mark 2)))]
                ;; the dog-ear: solid red, accent, or the paper's back
                [(back) (cond [(eq? pressed 'close)
                               (mix 'button-close #x000000 0.2)]
                              [close? 'button-close]
                              [focused? 'accent]
                              [else #f])])
    ;; from the content down, so that the cut corner stays clear   ; shadow
    (window-shadow win x (+ y th) w (- h th) 0 focused?)   ; shadow
    (when (maximized? win)   ; the desk, not a sliver of a window behind
      (fill-rect (+ x w (- f)) y f f 'desktop-top))
    (fold-sheet! x y w h f (if (maximized? win) 0 1)
                 (if focused? 'window-border-focus 'window-border) paper)
    (call-with-clip x y w th   ; even "…" may not fit a narrow window
      (lambda ()
        (draw-text (+ x fold-margin) (- base (font-ascent 'title))
                   (fit-text (window-title win)
                             (- w fold-margin th g (* 2 (+ fold-mark g)))
                             'title)
                   ink 'title)))
    (for-each
     (lambda (b)
       (let ([kind (car b)] [mx (+ x (cadr b) (quotient g 2))]
             [s (+ fold-mark 10)])
         (unless (eq? kind 'close)
           (when (memq kind (list hover pressed))   ; square, like the sheet
             (fill-rect (- mx 5) (- top 5) s s
                        (alpha 'text (if (eq? pressed kind) 0.22 0.12))))
           (fold-draw-mark win kind mx top
                           (if (eq? hover kind) 'title-text (alpha ink 0.6))))))
     (fold-regions win))
    (fold-flap! (+ x w) y f (or back 'window-border)
                (or back (mix paper 'text 0.1)))
    (when close?   ; an × in the middle of the dog-ear
      (let ([cx (+ (round (- (+ x w) (/ f (sqrt 2)))) 1/2)]
            [cy (+ (round (+ y (/ f (sqrt 2)))) 1/2)])
        (for-each (lambda (k)
                    (draw-line (- cx 3.5) (- cy (* k 3.5)) (+ cx 3.5)
                               (+ cy (* k 3.5)) (readable-on 'button-close)
                               1.5))
                  '(1 -1))))))

(define-style fold "Fold"
  'frame-insets 'fold-insets
  'frame-regions 'fold-regions
  'frame-under 'fold-draw-under)
