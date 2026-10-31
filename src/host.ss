;;; host.ss — the host window that the whole world lives in.
;;;
;;; 'sdl      : a real resizable window (SDL2 renderer + streaming texture)
;;; 'headless : no window at all, just the canvas (for tests and screenshots)
;;;
;;; Host events are translated into plain Scheme lists:
;;;   (mouse-move x y) (mouse-down x y button clicks) (mouse-up x y button)
;;;   (wheel dx dy) (key-down key mods) (text "a") (resize w h) (quit) ...
;;; key is a char for printable keys (#\a, #\space) or a symbol ('return,
;;; 'left, 'f1, ...); mods is a list drawn from (ctrl alt shift super).

(defvar *host* 'none)
(defvar *sdl-window* 0)
(defvar *sdl-renderer* 0)
(defvar *sdl-texture* 0)
(defvar *sdl-event* (foreign-alloc 64))
(defvar *host-scratch* (foreign-alloc 16))
(defvar *host-clipboard* "")
(defvar *cursors* (make-eq-hashtable))
(defvar *current-cursor* #f)
(defvar *host-fullscreen* #f)   ; what headless mode pretends
(defvar *host-scale* 1)         ; physical pixels per logical pixel, exact
;; physical pixels per SDL window unit (2 on a Retina Mac)
(defvar *pixel-ratio* 1)
;; the minimum size SDL last passed on to the window, or #f
(defvar *host-min-size* #f)
(defvar *headless-size* '(1280 . 800))

;;; ------------------------------------------------------------------ scale
;;; The world is laid out in logical pixels and drawn *host-scale* times
;;; bigger. The scale follows the desktop unless the theme key 'scale (a
;;; percentage; 0 for automatic) overrides it.

(define (wanted-scale)
  (let ([p (theme 'scale)])
    (if (and (fixnum? p) (> p 0)) (/ (clamp p 50 400) 100) (system-scale))))

;; What the desktop asks for. SDL scales by itself on macOS and Wayland
;; (the pixel ratio); under X11 desktops publish their scale as Xft.dpi
;; instead, 96 being 100%.
(define (system-scale)
  (* *pixel-ratio*
     (if (and (eq? *host* 'sdl) (equal? (sdl-get-current-video-driver) "x11"))
         (let ([dpi (xft-dpi)])
           (if dpi (/ (exact (round (* 100 (/ dpi 96)))) 100) 1))
         1)))

(define (system-scale-percent) (exact (round (* 100 (system-scale)))))

;; Xft.dpi from the X server's resources, or #f.
(define (xft-dpi)
  (let ([s (x-resource-string)])
    (and s
         (let loop ([lines (string-split s #\newline)])
           (cond
             [(null? lines) #f]
             [(string-prefix? "Xft.dpi:" (car lines))
              (let ([n (string->number
                        (list->string
                         (remp char-whitespace?
                               (string->list
                                (substring (car lines) 8
                                           (string-length (car lines)))))))])
                (and (real? n) (> n 0) n))]
             [else (loop (cdr lines))])))))

;; SDL window units (mouse positions) to logical pixels.
(define (host->world v) (exact (floor (/ (* v *pixel-ratio*) *host-scale*))))

;; After the 'scale setting changed: lay the world out again at the new scale.
(define (host-rescale!)
  (when (host-sync-size!)
    (wm-screen-resized!)))

;; The host window's size in window units for a w × h logical world drawn
;; at scale, with ratio physical pixels per window unit.
(define (host-window-size w h scale ratio)
  (values (exact (round (/ (* w scale) ratio)))
          (exact (round (/ (* h scale) ratio)))))

;; fullscreen?: start full screen instead of in a w × h window.
(define (host-init! mode w h . fullscreen?)
  (set! *host* mode)
  (set! *host-fullscreen* (and (pair? fullscreen?) (car fullscreen?)))
  (set! *host-min-size* #f)
  (case mode
    [(headless)
     (set! *headless-size* (cons w h))
     (set! *host-scale* (wanted-scale))
     (canvas-create! w h *host-scale*)]
    [(sdl)
     (sdl-set-hint "SDL_VIDEO_X11_NET_WM_BYPASS_COMPOSITOR" "0")
     (sdl-set-hint "SDL_MOUSE_FOCUS_CLICKTHROUGH" "1")
     ;; the window class desktops use to group and name the window
     (unless (getenv "SDL_VIDEO_X11_WMCLASS")
       (putenv "SDL_VIDEO_X11_WMCLASS" "Schevana"))
     (unless (getenv "SDL_VIDEO_WAYLAND_WMCLASS")
       (putenv "SDL_VIDEO_WAYLAND_WMCLASS" "schevana"))
     (unless (= 0 (sdl-init SDL_INIT_VIDEO))
       (errorf 'host-init! "SDL_Init failed: ~a" (sdl-get-error)))
     ;; w × h are logical pixels. How many physical pixels a window unit
     ;; holds is known once the window has a renderer, so it opens hidden at
     ;; one per unit (true under X11) and is sized and centered again before
     ;; it is shown.
     (set! *pixel-ratio* 1)
     (set! *sdl-window*
       (let-values ([(uw uh) (host-window-size w h (wanted-scale) 1)])
         (sdl-create-window "Schevana"
                            SDL_WINDOWPOS_CENTERED SDL_WINDOWPOS_CENTERED uw uh
                            (fxior SDL_WINDOW_RESIZABLE SDL_WINDOW_ALLOW_HIGHDPI
                                   SDL_WINDOW_HIDDEN
                                   (if *host-fullscreen*
                                       SDL_WINDOW_FULLSCREEN_DESKTOP
                                       0)))))
     (when (= 0 *sdl-window*)
       (errorf 'host-init! "SDL_CreateWindow failed: ~a" (sdl-get-error)))
     (set! *sdl-renderer*
       (sdl-create-renderer *sdl-window* -1
                            (fxior SDL_RENDERER_ACCELERATED
                                   SDL_RENDERER_PRESENTVSYNC)))
     (when (= 0 *sdl-renderer*)
       (set! *sdl-renderer* (sdl-create-renderer *sdl-window* -1 0)))
     (when (= 0 *sdl-renderer*)
       (errorf 'host-init! "SDL_CreateRenderer failed: ~a" (sdl-get-error)))
     (let*-values ([(uw uh pw ph) (host-read-sizes)]
                   [(nw nh)
                    (host-window-size w h (wanted-scale) *pixel-ratio*)])
       (unless (or *host-fullscreen* (and (= nw uw) (= nh uh)))
         (sdl-set-window-size *sdl-window* nw nh)
         (sdl-set-window-position *sdl-window* SDL_WINDOWPOS_CENTERED
                                  SDL_WINDOWPOS_CENTERED)))
     (sdl-show-window *sdl-window*)
     (sdl-start-text-input)
     (host-sync-size!)]
    [else (errorf 'host-init! "unknown host mode ~s" mode)]))

(define (host-shutdown!)
  (when (eq? *host* 'sdl)
    (sdl-destroy-texture *sdl-texture*)
    (sdl-destroy-renderer *sdl-renderer*)
    (sdl-destroy-window *sdl-window*)
    (sdl-quit)
    (set! *sdl-texture* 0)
    (set! *host* 'none)))

(define (host-set-title! title)
  (when (eq? *host* 'sdl) (sdl-set-window-title *sdl-window* title)))

;;; ------------------------------------------------------------------
;;; full screen

(define (fullscreen?)
  (if (eq? *host* 'sdl)
      (= SDL_WINDOW_FULLSCREEN_DESKTOP
         (fxlogand (sdl-get-window-flags *sdl-window*)
                   SDL_WINDOW_FULLSCREEN_DESKTOP))
      *host-fullscreen*))

;; Full screen at the desktop's own resolution: the world just gets a
;; bigger canvas (the host sends a resize event).
(define (set-fullscreen! on?)
  (when (eq? *host* 'sdl)
    (sdl-set-window-fullscreen *sdl-window*
                               (if on? SDL_WINDOW_FULLSCREEN_DESKTOP 0)))
  (set! *host-fullscreen* (and on? #t))
  (invalidate!))

(define (toggle-fullscreen!) (set-fullscreen! (not (fullscreen?))))

;;; ------------------------------------------------------------------ icon

;; The host window's icon, which the desktop's taskbar or dock shows (under
;; X11; Wayland compositors take icons from .desktop files instead).
;; draw is called with *cr* on a size × size transparent image.
(define (host-set-icon! size draw)
  (when (eq? *host* 'sdl)
    (let* ([surface (cairo-image-surface-create CAIRO_FORMAT_ARGB32 size size)]
           [cr (cairo-create surface)])
      (fluid-let ([*cr* cr]) (draw size))
      (cairo-destroy cr)
      (cairo-surface-flush surface)
      (let ([data (cairo-image-surface-get-data surface)]
            [stride (cairo-image-surface-get-stride surface)])
        (unpremultiply! data stride size size)
        (let ([icon (sdl-create-rgb-surface-with-format-from
                     data size size 32 stride SDL_PIXELFORMAT_ARGB8888)])
          (unless (= icon 0)
            (sdl-set-window-icon *sdl-window* icon)   ; SDL copies the pixels
            (sdl-free-surface icon))))
      (cairo-surface-destroy surface))))

;; Cairo keeps color premultiplied by alpha; SDL wants it straight.
(define (unpremultiply! data stride w h)
  (do ([y 0 (+ y 1)]) ((= y h))
    (do ([x 0 (+ x 1)]) ((= x w))
      (let* ([i (+ (* y stride) (* 4 x))]
             [p (foreign-ref 'unsigned-32 data i)]
             [a (fxsrl p 24)])
        (when (fx< 0 a 255)
          (let ([un (lambda (c)
                      (fxmin 255 (fxquotient (fx+ (fx* c 255) (fxquotient a 2))
                                             a)))])
            (foreign-set! 'unsigned-32 data i
                          (fxior (fxsll a 24)
                                 (fxsll (un (fxand (fxsrl p 16) 255)) 16)
                                 (fxsll (un (fxand (fxsrl p 8) 255)) 8)
                                 (un (fxand p 255))))))))))

;; The window's size in window units and its renderer's in physical
;; pixels, (values w h pw ph); sets *pixel-ratio* from them.
(define (host-read-sizes)
  (sdl-get-window-size *sdl-window* *host-scratch* (+ *host-scratch* 4))
  (let ([w (foreign-ref 'integer-32 *host-scratch* 0)]
        [h (foreign-ref 'integer-32 *host-scratch* 4)])
    (sdl-get-renderer-output-size *sdl-renderer* *host-scratch*
                                  (+ *host-scratch* 4))
    (let ([pw (foreign-ref 'integer-32 *host-scratch* 0)]
          [ph (foreign-ref 'integer-32 *host-scratch* 4)])
      (set! *pixel-ratio* (/ pw (max w 1)))
      (values w h pw ph))))

;; Re-reads the window's pixel size and the scale; recreates canvas and
;; texture when either changed. Returns #t if they did.
(define (host-sync-size!)
  (case *host*
    [(sdl)
     (let-values ([(w h pw ph) (host-read-sizes)])
       (let ([scale (wanted-scale)])
         (if (and (= pw (canvas-pixel-width)) (= ph (canvas-pixel-height))
                  (= scale *host-scale*)
                  (not (= 0 *sdl-texture*)))
             #f
             (let ([rescaled? (not (= scale *host-scale*))])
               (set! *host-scale* scale)
               (canvas-create-pixels! pw ph scale)
               ;; their metrics were measured at the old scale
               (when rescaled? (install-default-fonts!))
               ;; The host window may not shrink below 640 × 400 logical pixels.
               ;; SDL is told only when that changes: each call also asks for
               ;; the window's size again (under X11 a round trip to the window
               ;; manager, mid-drag). In full screen SDL only stores it, so it
               ;; is told again after.
               (let ([m (cons
                         (exact (ceiling (/ (* 640 scale) *pixel-ratio*)))
                         (exact (ceiling (/ (* 400 scale) *pixel-ratio*))))])
                 (unless (equal? m *host-min-size*)
                   (sdl-set-window-minimum-size *sdl-window* (car m) (cdr m))
                   (set! *host-min-size* (and (not (fullscreen?)) m))))
               (host-recreate-texture!)
               #t))))]
    [(headless)
     (let ([scale (wanted-scale)])
       (and (not (= scale *host-scale*))
            (begin
              (set! *host-scale* scale)
              (canvas-create! (car *headless-size*) (cdr *headless-size*) scale)
              (install-default-fonts!)
              #t)))]
    [else #f]))

;; A streaming texture the canvas's size; its pixels are undefined until
;; the next (full) upload.
(define (host-recreate-texture!)
  (when (eq? *host* 'sdl)
    (unless (= 0 *sdl-texture*) (sdl-destroy-texture *sdl-texture*))
    (set! *sdl-texture*
      (sdl-create-texture *sdl-renderer* SDL_PIXELFORMAT_ARGB8888
                          SDL_TEXTUREACCESS_STREAMING
                          (canvas-pixel-width) (canvas-pixel-height))))
  (set! *full-upload?* #t))

;;; ------------------------------------------------------------------ upload
;;; A step that drew a frame uploads the rows it changed, each run of them
;;; as one full-width rectangle, and shows the texture. A step that changed
;;; no row neither uploads nor presents (nor waits for vsync).

;; rows the canvas changed since the last upload, ((y0 . y1) ...)
(defvar *upload-rows* '())
;; the texture must be refreshed whole (it was just made, say)
(defvar *full-upload?* #t)
;; show the texture again, unchanged (the host window was exposed)
(defvar *present-only?* #f)
(defvar *upload-bytes* 0)      ; bytes uploaded so far (counted headless too)
(defvar *host-rect* (foreign-alloc 16))   ; an SDL_Rect

;; The whole texture afresh, whatever the frame changed.
(define (invalidate-screen!)
  (set! *full-upload?* #t)
  (request-frame!))

;; Back buffers are undefined after a present: the whole texture each time.
(define (host-show-texture!)
  (sdl-render-copy *sdl-renderer* *sdl-texture* 0 0)
  (sdl-render-present *sdl-renderer*))

(define (host-present-damage!)
  (let ([runs (cond [(eqv? *canvas* 0) '()]
                    [*full-upload?* (list (cons 0 *canvas-ph*))]
                    [else *upload-rows*])])
    (for-each (lambda (r)
                (set! *upload-bytes*
                      (+ *upload-bytes* (* 4 *canvas-pw* (- (cdr r) (car r))))))
              runs)
    (when (eq? *host* 'sdl)
      (cond
        [(pair? runs)
         (let ([data (cairo-image-surface-get-data *canvas*)]
               [stride (cairo-image-surface-get-stride *canvas*)])
           (cairo-surface-flush *canvas*)
           (for-each (lambda (r)
                       (foreign-set! 'integer-32 *host-rect* 0 0)
                       (foreign-set! 'integer-32 *host-rect* 4 (car r))
                       (foreign-set! 'integer-32 *host-rect* 8 *canvas-pw*)
                       (foreign-set! 'integer-32 *host-rect* 12
                                     (- (cdr r) (car r)))
                       (sdl-update-texture *sdl-texture* *host-rect*
                                           (+ data (* stride (car r))) stride))
                     runs))
         (host-show-texture!)]
        [*present-only?* (host-show-texture!)]))
    (set! *upload-rows* '())
    (set! *full-upload?* #f)
    (set! *present-only?* #f)))

;;; ------------------------------------------------------------------ events

(define sdl-special-keys
  `((13 . return) (8 . backspace) (9 . tab) (27 . escape) (127 . delete)
    (#x4000004F . right) (#x40000050 . left)
    (#x40000051 . down) (#x40000052 . up)
    (#x4000004A . home) (#x4000004D . end)
    (#x4000004B . page-up) (#x4000004E . page-down)
    (#x40000049 . insert) (#x40000058 . return)
    (#x400000E0 . ctrl) (#x400000E1 . shift)
    (#x400000E2 . alt) (#x400000E3 . super)
    (#x400000E4 . ctrl) (#x400000E5 . shift)
    (#x400000E6 . alt) (#x400000E7 . super)
    ,@(let loop ([i 0] [acc '()])
        (if (= i 12)
            acc
            (loop (+ i 1)
                  (cons (cons (+ #x4000003A i)
                              (string->symbol (format "f~a" (+ i 1))))
                        acc))))))

(define (sdl-key->key code)
  (cond
    [(assv code sdl-special-keys) => cdr]
    [(and (>= code 32) (< code 127)) (integer->char code)]
    [else (string->symbol (format "key-~x" code))]))

(define (sdl-mods->list m)
  (append (if (fxzero? (fxand m #xC0)) '() '(ctrl))
          (if (fxzero? (fxand m #x300)) '() '(alt))
          (if (fxzero? (fxand m #x3)) '() '(shift))
          (if (fxzero? (fxand m #xC00)) '() '(super))))

(define (sdl-button->symbol b)
  (case b [(1) 'left] [(2) 'middle] [(3) 'right] [else b]))

(define (sdl-event->list buf)
  (let ([type (foreign-ref 'unsigned-32 buf 0)])
    (cond
      [(= type SDL_QUIT) '(quit)]
      [(= type SDL_KEYDOWN)
       (list 'key-down (sdl-key->key (foreign-ref 'integer-32 buf 20))
             (sdl-mods->list (foreign-ref 'unsigned-16 buf 24)))]
      [(= type SDL_TEXTINPUT) (list 'text (c-string->string (+ buf 12)))]
      [(= type SDL_MOUSEMOTION)
       (list 'mouse-move
             (host->world (foreign-ref 'integer-32 buf 20))
             (host->world (foreign-ref 'integer-32 buf 24)))]
      [(= type SDL_MOUSEBUTTONDOWN)
       (list 'mouse-down
             (host->world (foreign-ref 'integer-32 buf 20))
             (host->world (foreign-ref 'integer-32 buf 24))
             (sdl-button->symbol (foreign-ref 'unsigned-8 buf 16))
             (foreign-ref 'unsigned-8 buf 18))]
      [(= type SDL_MOUSEBUTTONUP)
       (list 'mouse-up
             (host->world (foreign-ref 'integer-32 buf 20))
             (host->world (foreign-ref 'integer-32 buf 24))
             (sdl-button->symbol (foreign-ref 'unsigned-8 buf 16)))]
      [(= type SDL_MOUSEWHEEL)
       (let ([flip (if (= 1 (foreign-ref 'unsigned-32 buf 24)) -1 1)])
         (list 'wheel (* flip (foreign-ref 'integer-32 buf 16))
               (* flip (foreign-ref 'integer-32 buf 20))))]
      [(= type SDL_WINDOWEVENT)
       (let ([e (foreign-ref 'unsigned-8 buf 12)])
         (cond
           [(or (= e SDL_WINDOWEVENT_RESIZED)
                (= e SDL_WINDOWEVENT_SIZE_CHANGED))
            (and (host-sync-size!) (list 'resize *canvas-w* *canvas-h*))]
           [(= e SDL_WINDOWEVENT_EXPOSED) '(expose)]
           [(= e SDL_WINDOWEVENT_LEAVE) '(mouse-leave)]
           [else #f]))]
      [(= type SDL_RENDER_TARGETS_RESET) '(render-reset)]
      [(= type SDL_RENDER_DEVICE_RESET) '(device-reset)]
      [else #f])))

;; Waits up to wait-ms for the first event, then drains the queue.
(define (host-events wait-ms)
  (if (eq? *host* 'sdl)
      (let ([buf *sdl-event*])
        (if (= 0 (if (> wait-ms 0)
                     (sdl-wait-event-timeout buf wait-ms)
                     (sdl-poll-event buf)))
            '()
            (let loop ([acc (let ([e (sdl-event->list buf)])
                              (if e (list e) '()))])
              (if (= 0 (sdl-poll-event buf))
                  (reverse acc)
                  (loop (let ([e (sdl-event->list buf)])
                          (if e (cons e acc) acc)))))))
      '()))

;;; ------------------------------------------------------------------
;;; cursor & clipboard

(define cursor-ids
  '((arrow . 0) (ibeam . 1) (wait . 2) (crosshair . 3) (resize-nwse . 5)
    (resize-nesw . 6) (resize-ew . 7) (resize-ns . 8) (move . 9) (hand . 11)))

(define (host-set-cursor! kind)
  (when (and (eq? *host* 'sdl) (not (eq? kind *current-cursor*)))
    (set! *current-cursor* kind)
    (sdl-set-cursor
     (or (hashtable-ref *cursors* kind #f)
         (let ([c (sdl-create-system-cursor
                   (cdr (or (assq kind cursor-ids) '(arrow . 0))))])
           (hashtable-set! *cursors* kind c)
           c)))))

(define (clipboard-get)
  (if (eq? *host* 'sdl)
      (let* ([p (sdl-get-clipboard-text)] [s (c-string->string p)])
        (unless (= p 0) (sdl-free p))
        s)
      *host-clipboard*))

(define (clipboard-set! s)
  (if (eq? *host* 'sdl)
      (sdl-set-clipboard-text s)
      (set! *host-clipboard* s)))
