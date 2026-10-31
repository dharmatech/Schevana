;;; shadows.ss — window, menu and toast shadows.
;;;
;;; Shadows are on probation. Everything about them is in this file, or in
;;; the other files between a ";; shadow{" line and a ";; shadow}" line, or
;;; on a line that ends in "; shadow"; nothing else relies on them. Without
;;; this file, its line in load.ss and those parts, the world has none.
;;;
;;;   (draw-shadow x y w h r size)            a soft shadow under a box
;;;   (window-shadow win x y w h r focused?)  a window's, for its style: the
;;;                                           theme key 'shadow, half unfocused

;; A soft shadow under the box (x y w h) with corner radius r, spreading
;; size pixels, a little lower than the box.
(define (draw-shadow-directly x y w h r size)
  (let loop ([i size])
    (when (> i 0)
      (fill-round-rect (- x i) (+ (- y i) (/ size 3))
                       (+ w (* 2 i)) (+ h (* 2 i)) (+ r i)
                       (alpha #x000000 (/ 0.22 size)))
      (loop (- i 2)))))

;; v in device pixels as cairo's 24.8 fixed point.
(define (fixed-24.8 v) (exact (round (* 256 v))))

;; The logical rect a shadow of this size paints into.
(define (shadow-bounds x y w h size)
  (values (- x size) (+ (- y size) (/ size 3))
          (+ w size size) (+ h size size)))

;;; draw-shadow paints what draw-shadow-directly paints, middle included,
;;; from cached device-space tiles. Along a straight edge the shadow is the
;;; same in every column (or row), so a template drawn once per radius,
;;; size, device scale and sub-pixel phase of each box edge gives the four
;;; corners, a strip for each edge and one pixel for the middle. Pieces are
;;; only copied at whole device pixels, so a tile does not depend on which
;;; box first needed it, and a shadow painted in parts equals one painted
;;; whole. Boxes too small for tiles get the stack drawn whole onto a
;;; scratch image at the same sub-pixel phase, which is copied the same
;;; way; only a rotated or unevenly scaled shadow is drawn directly.

(define-record-type shadow-tile
  (nongenerative schevana-shadow-tile-3)
  (fields template top bottom left right center  ; owned surfaces
          tL0 tcL tcR tR1 tT0 tcT tcB tB1        ; zone edges in template pixels
          bytes))

;; key -> (tile . last use)
(defvar *shadow-tiles* (make-hashtable equal-hash equal?))
;; *owned-frees* the tiles were made after
(defvar *shadow-tiles-frees* 0)
(defvar *shadow-tile-bytes* 0)
(defvar *shadow-tile-clock* 0)
(define shadow-tile-budget (* 16 1024 1024))

(define (draw-shadow x y w h r size)
  (unless (<= size 0)
    (cairo-get-matrix *cr* *scratch*)
    (let ([xx (foreign-ref 'double *scratch* 0)]
          [yx (foreign-ref 'double *scratch* 8)]
          [xy (foreign-ref 'double *scratch* 16)]
          [yy (foreign-ref 'double *scratch* 24)]
          [x0 (foreign-ref 'double *scratch* 32)]
          [y0 (foreign-ref 'double *scratch* 40)])
      (cond
        ;; tiles hold device pixels, so only for a uniform scale without
        ;; rotation
        [(not (and (= yx 0.0) (= xy 0.0) (= xx yy) (> xx 0.0)))
         (draw-shadow-directly x y w h r size)]
        ;; boxes too small for their corners (the stack clamps the radius)
        [(or (not (integer? r)) (not (integer? size)) (< r 0)
             (< w (+ (* 2 r) 4)) (< h (+ (* 2 r) 4)))
         (draw-shadow-whole x y w h r size xx x0 y0)]
        [else (draw-shadow-tiled x y w h (exact r) (exact size) xx x0 y0)]))))

;; The stack drawn alone onto a scratch image whose pixels have the same
;; sub-pixel phase as the target's, then copied at whole device pixels.
(define (draw-shadow-whole x y w h r size s x0 y0)
  (let*-values ([(bx by bw bh) (shadow-bounds x y w h size)]
                [(X0) (- (exact (floor (+ (* s bx) x0))) 1)]
                [(Y0) (- (exact (floor (+ (* s by) y0))) 1)]
                [(X1) (+ (exact (ceiling (+ (* s (+ bx bw)) x0))) 1)]
                [(Y1) (+ (exact (ceiling (+ (* s (+ by bh)) y0))) 1)])
    (let* ([t (cairo-image-surface-create CAIRO_FORMAT_ARGB32
                                          (- X1 X0) (- Y1 Y0))]
           [cr (cairo-create t)])
      (cairo-translate cr (- x0 X0) (- y0 Y0))
      (cairo-scale cr s s)
      (fluid-let ([*cr* cr]) (draw-shadow-directly x y w h r size))
      (cairo-destroy cr)
      (cairo-surface-flush t)
      (cairo-save *cr*)
      (cairo-identity-matrix *cr*)
      (cairo-set-operator *cr* CAIRO_OPERATOR_OVER)
      (cairo-set-source-surface *cr* t X0 Y0)
      (cairo-paint *cr*)
      (cairo-restore *cr*)
      (cairo-surface-destroy t))))

(define (draw-shadow-tiled x y w h r size s x0 y0)
  ;; the box the stack is centered on, in device pixels; interrupts off so a
  ;; process is never switched out holding a tile main may flush
  (with-interrupts-disabled
    (let* ([X0 (+ (* s x) x0)] [Y0 (+ (* s (+ y (/ size 3))) y0)]
           [qX0 (fixed-24.8 X0)] [qY0 (fixed-24.8 Y0)]
           [qX1 (fixed-24.8 (+ X0 (* s w)))] [qY1 (fixed-24.8 (+ Y0 (* s h)))]
           [A (+ (exact (ceiling (* s size))) 2)]
           [n (+ (exact (ceiling (* 2 (* s r)))) 3)]
           [t (shadow-tile-for r size s (mod qX0 256) (mod qY0 256)
                               (mod qX1 256) (mod qY1 256))]
           ;; template -> device offsets: left and top zones, right and bottom
           ;; zones
           [dx (- (div qX0 256) A)] [dxr (- (div qX1 256) A n)]
           [dy (- (div qY0 256) A)] [dyb (- (div qY1 256) A n)])
      (if (or (<= (+ (shadow-tile-tcR t) dxr) (+ (shadow-tile-tcL t) dx))
              (<= (+ (shadow-tile-tcB t) dyb) (+ (shadow-tile-tcT t) dy)))
          (draw-shadow-whole x y w h r size s x0 y0)
          (paint-shadow-tile! t dx dxr dy dyb)))))

;; A tree without this file frees every owned surface without flushing
;; the tiles first, and reloaded live it leaves draw-shadow bound: tiles
;; made before that are gone, so forget them without touching them.
(define (forget-freed-shadow-tiles!)
  (unless (= *shadow-tiles-frees* *owned-frees*)
    (hashtable-clear! *shadow-tiles*)
    (set! *shadow-tile-bytes* 0)
    (set! *shadow-tiles-frees* *owned-frees*)))

(define (shadow-tile-for r size s pX0 pY0 pX1 pY1)
  (forget-freed-shadow-tiles!)
  (let ([key (list 'v2 draw-shadow-directly r size s pX0 pY0 pX1 pY1)])
    (set! *shadow-tile-clock* (+ *shadow-tile-clock* 1))
    (let ([e (hashtable-ref *shadow-tiles* key #f)])
      (if e
          (begin (set-cdr! e *shadow-tile-clock*) (car e))
          (let ([t (make-shadow-tile* r size s pX0 pY0 pX1 pY1)])
            (hashtable-set! *shadow-tiles* key (cons t *shadow-tile-clock*))
            (set! *shadow-tile-bytes*
                  (+ *shadow-tile-bytes* (shadow-tile-bytes t)))
            (evict-shadow-tiles!)
            t)))))

;; The template is the shadow of a box (TX0 TY0)-(TX1 TY1) with the given
;; edge phases, its straight zone at least one pixel wide, A pixels of
;; room around the spread m; it is cut into zones at whole pixels.
(define (make-shadow-tile* r size s pX0 pY0 pX1 pY1)
  (let* ([m (* s size)] [R (* s r)]
         [A (+ (exact (ceiling m)) 2)] [n (+ (exact (ceiling (* 2 R))) 3)]
         [TX0 (+ A (/ pX0 256))] [TY0 (+ A (/ pY0 256))]
         [Wd (+ n (/ (- pX1 pX0) 256))] [Hd (+ n (/ (- pY1 pY0) 256))]
         [TX1 (+ TX0 Wd)] [TY1 (+ TY0 Hd)]
         [tL0 (- (exact (floor (- TX0 m))) 1)] [tcL (exact (ceiling (+ TX0 R)))]
         [tcR (exact (floor (- TX1 R)))] [tR1 (+ (exact (ceiling (+ TX1 m))) 1)]
         [tT0 (- (exact (floor (- TY0 m))) 1)] [tcT (exact (ceiling (+ TY0 R)))]
         [tcB (exact (floor (- TY1 R)))] [tB1 (+ (exact (ceiling (+ TY1 m))) 1)]
         [tw (+ tR1 1)] [th (+ tB1 1)]
         [template (make-owned-surface 'shadow-tile tw th)])
    (let ([cr (cairo-create template)])
      (cairo-translate cr TX0 TY0)
      (cairo-scale cr s s)
      (fluid-let ([*cr* cr])
        (draw-shadow-directly 0 (- (/ size 3)) (/ Wd s) (/ Hd s) r size))
      (cairo-destroy cr))
    (cairo-surface-flush template)
    (let ([cut (lambda (x0 y0 x1 y1)
                 (let* ([p (make-owned-surface 'shadow-tile
                                               (- x1 x0) (- y1 y0))]
                        [cr (cairo-create p)])
                   (cairo-set-operator cr CAIRO_OPERATOR_SOURCE)
                   (cairo-set-source-surface cr template (- x0) (- y0))
                   (cairo-paint cr)
                   (cairo-destroy cr)
                   (cairo-surface-flush p)
                   p))])
      (make-shadow-tile template
                        (cut tcL tT0 (+ tcL 1) tcT) (cut tcL tcB (+ tcL 1) tB1)
                        (cut tL0 tcT tcL (+ tcT 1)) (cut tcR tcT tR1 (+ tcT 1))
                        (cut tcL tcT (+ tcL 1) (+ tcT 1))
                        tL0 tcL tcR tR1 tT0 tcT tcB tB1
                        (* 4 (+ (* tw th) (- tcT tT0) (- tB1 tcB) (- tcL tL0)
                                (- tR1 tcR) 1))))))

;; The nine pieces, at whole device pixels, with OVER.
(define (paint-shadow-tile! t dx dxr dy dyb)
  (let ([cr *cr*]
        [tL0 (shadow-tile-tL0 t)] [tcL (shadow-tile-tcL t)]
        [tcR (shadow-tile-tcR t)] [tR1 (shadow-tile-tR1 t)]
        [tT0 (shadow-tile-tT0 t)] [tcT (shadow-tile-tcT t)]
        [tcB (shadow-tile-tcB t)] [tB1 (shadow-tile-tB1 t)])
    ;; the template, its origin at (ox, oy)
    (define (corner! ox oy x0 y0 x1 y1)
      (cairo-set-source-surface cr (shadow-tile-template t) ox oy)
      (cairo-rectangle cr x0 y0 (- x1 x0) (- y1 y0))
      (cairo-fill cr))
    (define (repeat! surface x0 y0 x1 y1)     ; surface tiled from (x0, y0)
      (let ([pat (cairo-pattern-create-for-surface surface)])
        (cairo-pattern-set-extend pat CAIRO_EXTEND_REPEAT)
        (cairo-pattern-set-filter pat CAIRO_FILTER_NEAREST)
        (cairo-translate cr x0 y0)
        (cairo-set-source cr pat)
        (cairo-identity-matrix cr)
        (cairo-rectangle cr x0 y0 (- x1 x0) (- y1 y0))
        (cairo-fill cr)
        (cairo-pattern-destroy pat)))
    (cairo-save cr)
    (cairo-identity-matrix cr)
    (cairo-set-operator cr CAIRO_OPERATOR_OVER)
    (corner! dx dy (+ tL0 dx) (+ tT0 dy) (+ tcL dx) (+ tcT dy))
    (corner! dxr dy (+ tcR dxr) (+ tT0 dy) (+ tR1 dxr) (+ tcT dy))
    (corner! dx dyb (+ tL0 dx) (+ tcB dyb) (+ tcL dx) (+ tB1 dyb))
    (corner! dxr dyb (+ tcR dxr) (+ tcB dyb) (+ tR1 dxr) (+ tB1 dyb))
    (repeat! (shadow-tile-top t) (+ tcL dx) (+ tT0 dy) (+ tcR dxr) (+ tcT dy))
    (repeat! (shadow-tile-bottom t)
             (+ tcL dx) (+ tcB dyb) (+ tcR dxr) (+ tB1 dyb))
    (repeat! (shadow-tile-left t) (+ tL0 dx) (+ tcT dy) (+ tcL dx) (+ tcB dyb))
    (repeat! (shadow-tile-right t)
             (+ tcR dxr) (+ tcT dy) (+ tR1 dxr) (+ tcB dyb))
    (repeat! (shadow-tile-center t)
             (+ tcL dx) (+ tcT dy) (+ tcR dxr) (+ tcB dyb))
    (cairo-restore cr)))

(define (free-shadow-tile! t)
  (for-each free-owned-surface!
            (list (shadow-tile-template t) (shadow-tile-top t)
                  (shadow-tile-bottom t) (shadow-tile-left t)
                  (shadow-tile-right t) (shadow-tile-center t))))

;; Drops the least recently used tiles while over budget (never the newest).
(define (evict-shadow-tiles!)
  (let loop ()
    (when (> *shadow-tile-bytes* shadow-tile-budget)
      (let-values ([(keys vals) (hashtable-entries *shadow-tiles*)])
        (when (> (vector-length keys) 1)
          (let find ([i 1] [lru 0])
            (if (< i (vector-length keys))
                (find (+ i 1)
                      (if (< (cdr (vector-ref vals i))
                             (cdr (vector-ref vals lru)))
                          i
                          lru))
                (let ([t (car (vector-ref vals lru))])
                  (hashtable-delete! *shadow-tiles* (vector-ref keys lru))
                  (set! *shadow-tile-bytes*
                        (- *shadow-tile-bytes* (shadow-tile-bytes t)))
                  (free-shadow-tile! t)
                  (loop)))))))))

;; Frees every tile, including the layers the cache held before tiles v2.
;; Interrupts are off, so no frame is drawn from the cache half freed.
(define (flush-shadow-tiles!)
  (with-interrupts-disabled
    (forget-freed-shadow-tiles!)
    (vector-for-each
     (lambda (v)
       (cond
         [(and (pair? v) (shadow-tile? (car v))) (free-shadow-tile! (car v))]
         [(and (pair? v) (layer? (car v))) (free-layer! (car v))]))
     (hashtable-values *shadow-tiles*))
    (hashtable-clear! *shadow-tiles*)
    (set! *shadow-tile-bytes* 0)))

;; Tile caches from before tiles v2 hold layers that nothing frees otherwise;
;; tiles of an older record version go too (the reload's
;; free-owned-surfaces! frees their surfaces).
(when (exists (lambda (v) (not (and (pair? v) (shadow-tile? (car v)))))
              (vector->list (hashtable-values *shadow-tiles*)))
  (flush-shadow-tiles!))

;;; ------------------------------------------------------------------
;;; window shadows

(define (window-shadow win x y w h r focused?)
  (unless (maximized? win)
    (draw-shadow x y w h r
                 (if focused? (theme 'shadow) (quotient (theme 'shadow) 2)))))
