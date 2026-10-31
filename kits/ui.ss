;;; kits/ui.ss — a few immediate-mode widgets.
;;;
;;; Widgets draw themselves during 'draw and record a hit region (with an
;;; id) in a ui object; the window's mouse handlers ask the ui object what
;;; was hit. There is no widget tree: the draw procedure is the layout.
;;;
;;;   (define ui (make-ui))
;;;   (apply open-window "Demo"
;;;     'draw (lambda (win w h)
;;;             (ui-begin! ui)
;;;             (ui-button ui 'hi 20 20 120 30 "Say hi"
;;;                        (lambda () (notify "hi"))))
;;;     (ui-handlers ui))
;;;
;;; Widgets invalidate the window whose handler is running, (current-window).
;;;
;;; This is a first experiment toward a GUI API, not a decision.

(define-record-type ui-state
  (nongenerative schevana-ui-state)
  (fields (mutable regions)   ; newest (topmost) first
          (mutable active)    ; region being dragged, or #f
          (mutable hover-id)
          (mutable mx) (mutable my)))

(define-record-type ui-region
  (nongenerative schevana-ui-region)
  (fields id x y w h on-press on-drag))

(define (make-ui) (make-ui-state '() #f #f -1 -1))

(define (ui-begin! ui) (ui-state-regions-set! ui '()))

;; A clickable area. on-press is a thunk; on-drag, if given, receives the
;; pointer position on press and on every move until release.
(define ui-area
  (case-lambda
    [(ui id x y w h on-press) (ui-area ui id x y w h on-press #f)]
    [(ui id x y w h on-press on-drag)
     (ui-state-regions-set! ui
       (cons (make-ui-region id x y w h on-press on-drag)
             (ui-state-regions ui)))]))

(define (ui-region-at ui x y)
  (find (lambda (r)
          (point-in? x y (ui-region-x r) (ui-region-y r)
                     (ui-region-w r) (ui-region-h r)))
        (ui-state-regions ui)))

(define (ui-find ui id)
  (find (lambda (r) (equal? (ui-region-id r) id)) (ui-state-regions ui)))

(define (ui-hover? ui x y w h)
  (point-in? (ui-state-mx ui) (ui-state-my ui) x y w h))

(define (ui-active? ui id)
  (let ([a (ui-state-active ui)]) (and a (equal? (ui-region-id a) id))))

;;; ------------------------------------------------------------------ events

(define (ui-mouse-down ui x y)
  (ui-state-mx-set! ui x)
  (ui-state-my-set! ui y)
  (let ([r (ui-region-at ui x y)])
    (when r
      (when (ui-region-on-press r) ((ui-region-on-press r)))
      (when (ui-region-on-drag r)
        (ui-state-active-set! ui r)
        ((ui-region-on-drag r) x y)))
    (invalidate! (current-window))
    (and r #t)))

(define (ui-mouse-move ui x y)
  (ui-state-mx-set! ui x)
  (ui-state-my-set! ui y)
  (let ([a (ui-state-active ui)])
    (if a
        (begin ((ui-region-on-drag a) x y) (invalidate! (current-window)))
        (let* ([r (ui-region-at ui x y)] [id (and r (ui-region-id r))])
          (unless (equal? id (ui-state-hover-id ui))
            (ui-state-hover-id-set! ui id)
            (invalidate! (current-window)))))))

(define (ui-mouse-up ui)
  (ui-state-active-set! ui #f)
  (invalidate! (current-window)))

;; Mouse handler options to splice into open-window.
(define (ui-handlers ui)
  (list 'mouse-down (lambda (win x y button clicks)
                      (when (eq? button 'left) (ui-mouse-down ui x y)))
        'mouse-move (lambda (win x y) (ui-mouse-move ui x y))
        'mouse-up (lambda (win x y button) (ui-mouse-up ui))
        'ui ui))

;;; ------------------------------------------------------------------ widgets

(define (ui-button ui id x y w h label on-press . opts)
  (let ([selected? (plist-ref opts 'selected #f)]
        [hover? (ui-hover? ui x y w h)])
    (fill-round-rect x y w h 6 (cond [selected? (alpha 'accent 0.25)]
                                     [hover? 'bg-3]
                                     [else 'bg-2]))
    (stroke-round-rect x y w h 6 (if selected? 'accent 'window-border))
    (draw-text-centered x y w h (fit-text label (- w 12) 'ui)
                        (if (or selected? hover?) 'text 'text-dim) 'ui)
    (ui-area ui id x y w h on-press)))

;; choices: ((label . value) ...); the one equal? to current is selected.
(define (ui-segmented ui id x y w h choices current on-select)
  (let ([bw (/ w (length choices))])
    (let loop ([cs choices] [i 0])
      (unless (null? cs)
        (let ([value (cdar cs)])
          (ui-button ui (list id value) (+ x (* i bw)) y (- bw 6) h (caar cs)
                     (lambda () (on-select value))
                     'selected (equal? current value)))
        (loop (cdr cs) (+ i 1))))))

;; A horizontal slider. on-change receives the new value in [lo, hi].
;; paint-track, if given, draws the track: (lambda (x y w h) ...).
(define ui-slider
  (case-lambda
    [(ui id x y w value lo hi on-change)
     (ui-slider ui id x y w value lo hi on-change #f)]
    [(ui id x y w value lo hi on-change paint-track)
     (let* ([h 24] [cy (+ y (/ h 2))]
            [frac (clamp (/ (- value lo) (- hi lo)) 0 1)]
            [kx (+ x (* frac w))]
            [big? (or (ui-active? ui id) (ui-hover? ui (- x 8) y (+ w 16) h))])
       (if paint-track
           (call-with-round-clip x (- cy 4) w 8 4
                                 (lambda () (paint-track x (- cy 4) w 8)))
           (begin
             (fill-round-rect x (- cy 3) w 6 3 'bg-3)
             (fill-round-rect x (- cy 3) (max 6 (- kx x)) 6 3 'accent)))
       (fill-circle kx cy (if big? 8 7) #xf2f2f5)
       (stroke-circle kx cy (if big? 8 7) (alpha #x000000 0.35) 1)
       (ui-area ui id (- x 8) y (+ w 16) h #f
                (lambda (mx my)
                  (on-change
                   (+ lo (* (- hi lo) (clamp (/ (- mx x) w) 0 1)))))))]))
