;;; wallpapers/flat.ss — the top color alone.

(define-wallpaper flat "Flat"
  (lambda (x y w h top bottom) (fill-rect x y w h top)))
