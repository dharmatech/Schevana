;;; kits/color.ss — color math: a color as hue, saturation and lightness,
;;; and back (Settings' color picker).

;; Hue in degrees, saturation and lightness in [0, 1].
(define (color->hsl c)
  (let-values ([(r g b a) (color-rgba c)])
    (let* ([hi (max r g b)] [lo (min r g b)] [l (/ (+ hi lo) 2)] [d (- hi lo)])
      (if (< d 1e-9)
          (values 0.0 0.0 l)
          (values (cond [(= hi r) (* 60 (mod (/ (- g b) d) 6))]
                        [(= hi g) (* 60 (+ (/ (- b r) d) 2))]
                        [else (* 60 (+ (/ (- r g) d) 4))])
                  (/ d (- 1 (abs (- (* 2 l) 1))))
                  l)))))

(define (color-from-hsl h s l)
  (let* ([c (* (- 1 (abs (- (* 2 l) 1))) s)]
         [hp (/ (mod h 360) 60)]
         [x (* c (- 1 (abs (- (mod hp 2) 1))))]
         [m (- l (/ c 2))])
    (define (byte v) (clamp (exact (round (* 255 (+ v m)))) 0 255))
    (let-values ([(r g b) (cond [(< hp 1) (values c x 0)]
                                [(< hp 2) (values x c 0)]
                                [(< hp 3) (values 0 c x)]
                                [(< hp 4) (values 0 x c)]
                                [(< hp 5) (values x 0 c)]
                                [else (values c 0 x)])])
      (rgb (byte r) (byte g) (byte b)))))
