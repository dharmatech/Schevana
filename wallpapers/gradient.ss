;;; wallpapers/gradient.ss — the top color fading to the bottom one, the
;;; default. draw-wallpaper adds its glow.

(define-wallpaper gradient "Gradient"
  (lambda (x y w h top bottom) (fill-gradient x y w h top bottom)))
