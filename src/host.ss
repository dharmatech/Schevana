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

(define (host-init! mode w h)
  (set! *host* mode)
  (case mode
    [(headless) (canvas-create! w h 1.0)]
    [(sdl)
     (sdl-set-hint "SDL_VIDEO_X11_NET_WM_BYPASS_COMPOSITOR" "0")
     (sdl-set-hint "SDL_MOUSE_FOCUS_CLICKTHROUGH" "1")
     (unless (= 0 (sdl-init SDL_INIT_VIDEO))
       (errorf 'host-init! "SDL_Init failed: ~a" (sdl-get-error)))
     (set! *sdl-window*
       (sdl-create-window "Schevana OS" SDL_WINDOWPOS_CENTERED SDL_WINDOWPOS_CENTERED w h
                          (fxior SDL_WINDOW_RESIZABLE SDL_WINDOW_ALLOW_HIGHDPI)))
     (when (= 0 *sdl-window*)
       (errorf 'host-init! "SDL_CreateWindow failed: ~a" (sdl-get-error)))
     (sdl-set-window-minimum-size *sdl-window* 640 400)
     (set! *sdl-renderer*
       (sdl-create-renderer *sdl-window* -1 (fxior SDL_RENDERER_ACCELERATED SDL_RENDERER_PRESENTVSYNC)))
     (when (= 0 *sdl-renderer*)
       (set! *sdl-renderer* (sdl-create-renderer *sdl-window* -1 0)))
     (when (= 0 *sdl-renderer*)
       (errorf 'host-init! "SDL_CreateRenderer failed: ~a" (sdl-get-error)))
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

;; Re-reads the window and pixel sizes; recreates canvas and texture when
;; they changed. Returns #t if they did.
(define (host-sync-size!)
  (sdl-get-window-size *sdl-window* *host-scratch* (+ *host-scratch* 4))
  (let ([w (foreign-ref 'integer-32 *host-scratch* 0)]
        [h (foreign-ref 'integer-32 *host-scratch* 4)])
    (sdl-get-renderer-output-size *sdl-renderer* *host-scratch* (+ *host-scratch* 4))
    (let ([pw (foreign-ref 'integer-32 *host-scratch* 0)]
          [ph (foreign-ref 'integer-32 *host-scratch* 4)])
      (if (and (= w *canvas-w*) (= h *canvas-h*) (= pw (canvas-pixel-width)) (not (= 0 *sdl-texture*)))
          #f
          (begin
            (canvas-create! w h (/ pw (max w 1)))
            (unless (= 0 *sdl-texture*) (sdl-destroy-texture *sdl-texture*))
            (set! *sdl-texture*
              (sdl-create-texture *sdl-renderer* SDL_PIXELFORMAT_ARGB8888 SDL_TEXTUREACCESS_STREAMING
                                  (canvas-pixel-width) (canvas-pixel-height)))
            #t)))))

(define (host-present!)
  (when (eq? *host* 'sdl)
    (cairo-surface-flush *canvas*)
    (sdl-update-texture *sdl-texture* 0 (cairo-image-surface-get-data *canvas*)
                        (cairo-image-surface-get-stride *canvas*))
    (sdl-render-copy *sdl-renderer* *sdl-texture* 0 0)
    (sdl-render-present *sdl-renderer*)))

;;; ------------------------------------------------------------------ events

(define sdl-special-keys
  `((13 . return) (8 . backspace) (9 . tab) (27 . escape) (127 . delete)
    (#x4000004F . right) (#x40000050 . left) (#x40000051 . down) (#x40000052 . up)
    (#x4000004A . home) (#x4000004D . end) (#x4000004B . page-up) (#x4000004E . page-down)
    (#x40000049 . insert) (#x40000058 . return)
    (#x400000E0 . ctrl) (#x400000E1 . shift) (#x400000E2 . alt) (#x400000E3 . super)
    (#x400000E4 . ctrl) (#x400000E5 . shift) (#x400000E6 . alt) (#x400000E7 . super)
    ,@(let loop ([i 0] [acc '()])
        (if (= i 12)
            acc
            (loop (+ i 1) (cons (cons (+ #x4000003A i) (string->symbol (format "f~a" (+ i 1)))) acc))))))

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
       (list 'mouse-move (foreign-ref 'integer-32 buf 20) (foreign-ref 'integer-32 buf 24))]
      [(= type SDL_MOUSEBUTTONDOWN)
       (list 'mouse-down (foreign-ref 'integer-32 buf 20) (foreign-ref 'integer-32 buf 24)
             (sdl-button->symbol (foreign-ref 'unsigned-8 buf 16)) (foreign-ref 'unsigned-8 buf 18))]
      [(= type SDL_MOUSEBUTTONUP)
       (list 'mouse-up (foreign-ref 'integer-32 buf 20) (foreign-ref 'integer-32 buf 24)
             (sdl-button->symbol (foreign-ref 'unsigned-8 buf 16)))]
      [(= type SDL_MOUSEWHEEL)
       (let ([flip (if (= 1 (foreign-ref 'unsigned-32 buf 24)) -1 1)])
         (list 'wheel (* flip (foreign-ref 'integer-32 buf 16)) (* flip (foreign-ref 'integer-32 buf 20))))]
      [(= type SDL_WINDOWEVENT)
       (let ([e (foreign-ref 'unsigned-8 buf 12)])
         (cond
           [(or (= e SDL_WINDOWEVENT_RESIZED) (= e SDL_WINDOWEVENT_SIZE_CHANGED))
            (and (host-sync-size!) (list 'resize *canvas-w* *canvas-h*))]
           [(= e SDL_WINDOWEVENT_EXPOSED) '(expose)]
           [(= e SDL_WINDOWEVENT_LEAVE) '(mouse-leave)]
           [else #f]))]
      [else #f])))

;; Waits up to wait-ms for the first event, then drains the queue.
(define (host-events wait-ms)
  (if (eq? *host* 'sdl)
      (let ([buf *sdl-event*])
        (if (= 0 (if (> wait-ms 0) (sdl-wait-event-timeout buf wait-ms) (sdl-poll-event buf)))
            '()
            (let loop ([acc (let ([e (sdl-event->list buf)]) (if e (list e) '()))])
              (if (= 0 (sdl-poll-event buf))
                  (reverse acc)
                  (loop (let ([e (sdl-event->list buf)]) (if e (cons e acc) acc)))))))
      '()))

;;; ------------------------------------------------------------------ cursor & clipboard

(define cursor-ids
  '((arrow . 0) (ibeam . 1) (wait . 2) (crosshair . 3) (resize-nwse . 5) (resize-nesw . 6)
    (resize-ew . 7) (resize-ns . 8) (move . 9) (hand . 11)))

(define (host-set-cursor! kind)
  (when (and (eq? *host* 'sdl) (not (eq? kind *current-cursor*)))
    (set! *current-cursor* kind)
    (sdl-set-cursor
     (or (hashtable-ref *cursors* kind #f)
         (let ([c (sdl-create-system-cursor (cdr (or (assq kind cursor-ids) '(arrow . 0))))])
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
