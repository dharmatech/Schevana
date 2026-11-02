;;; wallpapers/shade.ss — Interlisp's dotted gray: the top color with one
;;; dot of the bottom color in four, a 4 × 4 stipple.

(define-wallpaper shade "Shade"
  (lambda (x y w h top bottom)
    (fill-rect x y w h (stipple bottom top "..#." "#..." "#..." "..#."))))
