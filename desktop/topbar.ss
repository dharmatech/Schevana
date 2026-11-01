;;; topbar.ss — the bar at the top: the launcher, the focused window's
;;; title and the status segments (status.ss), redrawn once a second.

(define (topbar-height)
  (max (theme 'topbar-height) (chrome-min-height 'topbar-height)))

(define (topbar-box) (values 0 0 (screen-w) (topbar-height)))

;; The accent on the panels, or the panels' text color where the accent
;; would vanish (Genera's black accent on its black panels).
(define (panel-accent)
  (let-values ([(ar ag ab aa) (color-rgba 'accent)]
               [(pr pg pb pa) (color-rgba 'panel)])
    (define (lum r g b) (+ (* 0.299 r) (* 0.587 g) (* 0.114 b)))
    (if (< (abs (- (lum ar ag ab) (lum pr pg pb))) 0.2) 'panel-text 'accent)))

(defvar *topbar-launcher-w* 0)

(define (draw-topbar)
  (let* ([w (screen-w)] [h (topbar-height)]
         [ty (/ (- h (font-line-height 'ui)) 2)]
         [gap (max 11 (round (* 0.6 (font-line-height 'ui))))]
         [dot-w (text-width "·" 'ui)])
    (fill-rect 0 0 w h (alpha 'panel 0.92))
    (draw-line 0 (- h 0.5) w (- h 0.5) (alpha 'panel-text 0.12))
    (let* ([label "λ  Schevana"]
           [bw (+ 24 (text-width label 'ui-bold))])
      (set! *topbar-launcher-w* (+ 6 bw))
      (when (or (equal? *hover* '(surface topbar launcher)) (launcher-open?))
        (fill-round-rect 6 4 bw (- h 8) 6 'panel-hover))
      (draw-text 18 ty label (panel-accent) 'ui-bold)
      (let ([title-end (draw-text (+ bw 20) ty
                                  (if *focus* (window-title *focus*) "Desktop")
                                  'panel-text 'ui-bold)])
        ;; status segments from the right, while they fit beside the title
        (let loop ([segs (reverse (shown-status-strings))]
                   [x (- w 14)]
                   [first? #t])
          (unless (null? segs)
            (let* ([s (car segs)]
                   ;; room for the separator
                   [right (if first? x (- x gap dot-w gap))]
                   [x0 (- right (text-width s 'ui))])
              (when (> x0 (+ title-end gap gap))
                (unless first?
                  (draw-text (- x gap dot-w) ty "·" 'panel-dim 'ui))
                (draw-text x0 ty s 'panel-text 'ui)
                (loop (cdr segs) x0 #f)))))))))

;; 'launcher, else 'bar: the bar takes every point of its strip.
(define (topbar-at x y)
  (and (< y (topbar-height))
       (if (< x *topbar-launcher-w*) 'launcher 'bar)))

(define (topbar-mouse kind what x y button clicks)
  (when (and (eq? kind 'down) (eq? what 'launcher)) (toggle-launcher!)))

(defvar *last-second* -1)

;; Once a second the bar asks for itself (the clock). The status shown
;; changes only with the bar that shows it: it is taken when the frame
;; this step draws holds the bar.
(define (topbar-tick)
  (let ([s (time-second (current-time))])
    (unless (= s *last-second*)
      (set! *last-second* s)
      (call-with-values topbar-box request-frame!)))
  (when (and *dirty*
             (holds? (frame-box *dirty*)
                     (call-with-values topbar-box pixel-box)))
    (refresh-status!)))

(define-surface topbar 20
  'draw 'draw-topbar 'box 'topbar-box 'at 'topbar-at 'mouse 'topbar-mouse
  'tick 'topbar-tick)
(define-strip topbar 'top 'topbar-height)
