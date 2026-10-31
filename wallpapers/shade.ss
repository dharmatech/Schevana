;;; wallpapers/shade.ss — Interlisp's dotted gray: the top color with one
;;; dot of the bottom color in four.

;; The Interlisp desktop shade: paper with one ink pixel in four, from a
;; 4 × 4 tile; each tile pixel is one logical pixel.
(define (fill-shade x y w h paper ink)
  (let* ([tile (cairo-image-surface-create CAIRO_FORMAT_ARGB32 4 4)]
         [cr (cairo-create tile)])
    (fluid-let ([*cr* cr])
      (fill-rect 0 0 4 4 paper)
      (for-each (lambda (p) (fill-rect (car p) (cdr p) 1 1 ink))
                '((2 . 0) (0 . 1) (0 . 2) (2 . 3))))
    (cairo-destroy cr)
    (let ([pat (cairo-pattern-create-for-surface tile)])
      (cairo-pattern-set-extend pat CAIRO_EXTEND_REPEAT)
      (cairo-pattern-set-filter pat CAIRO_FILTER_NEAREST)
      (cairo-save *cr*)
      (cairo-set-source *cr* pat)
      (cairo-rectangle *cr* x y w h)
      (cairo-fill *cr*)
      (cairo-restore *cr*)
      (cairo-pattern-destroy pat)
      (cairo-surface-destroy tile))))

(define-wallpaper shade "Shade"
  (lambda (x y w h top bottom) (fill-shade x y w h top bottom)))
