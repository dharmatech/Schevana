;;; ffi.ss — the only place where Schevana talks to C.
;;;
;;; SDL3 gives us a host window, input events and a clipboard.
;;; Cairo gives us 2D drawing into an in-memory image.
;;; Everything above this file is plain Scheme, and only the core (src/)
;;; uses these bindings: the folders draw, measure and save through the
;;; core's procedures (tests/sources.ss checks it).

(define (load-first-library names)
  (let loop ([ns names])
    (cond
      [(null? ns) (errorf 'load-first-library "could not load any of ~s" names)]
      [(guard (c [#t #f]) (load-shared-object (car ns)) #t) (car ns)]
      [else (loop (cdr ns))])))

(define sdl-library-names
  '("libSDL3.so.0" "libSDL3.so" "libSDL3.0.dylib" "libSDL3.dylib" "SDL3.dll"))

;; A process loads one SDL for good, so a session that started with another
;; (SDL2, before the switch) cannot reload into this one.
(when (and (top-level-bound? 'libsdl) (not (member libsdl sdl-library-names)))
  (errorf 'reload! "this session runs ~a; quit and start Schevana again"
          libsdl))

(define libsdl (load-first-library sdl-library-names))

(define libcairo
  (load-first-library
   '("libcairo.so.2" "libcairo.so" "libcairo.2.dylib" "libcairo.dylib"
     "libcairo-2.dll" "cairo.dll")))

;; Like foreign-procedure, but `double` arguments accept any real number,
;; and `bool` is C's one-byte bool (SDL3's) as #t or #f: Chez's boolean is
;; an int, whose upper bytes a C bool leaves undefined. Only the main
;; thread calls them: workers run worklets, which cannot name them.
;; collect-safe: a call that blocks, given foreign memory only, during
;; which a collection another thread needs does not wait for it.
(define-syntax define-foreign-as
  (lambda (x)
    (define (is? t name) (eq? (syntax->datum t) name))
    (define (c-type t) (if (is? t 'bool) #'unsigned-8 t))
    (syntax-case x ()
      [(_ how name cname (type ...) ret)
       (with-syntax ([(arg ...) (generate-temporaries #'(type ...))])
         (with-syntax ([(conv ...)
                        (map (lambda (t a)
                               (cond [(is? t 'double) #`(inexact #,a)]
                                     [(is? t 'bool) #`(if #,a 1 0)]
                                     [else a]))
                             #'(type ...) #'(arg ...))]
                       [(ctype ...) (map c-type #'(type ...))]
                       [cret (c-type #'ret)])
           #`(define name
               (let ([f #,(if (is? #'how 'collect-safe)
                              #'(foreign-procedure __collect_safe cname
                                                   (ctype ...) cret)
                              #'(foreign-procedure cname (ctype ...) cret))])
                 (lambda (arg ...)
                   #,(if (is? #'ret 'bool)
                         #'(not (fx= 0 (f conv ...)))
                         #'(f conv ...)))))))])))

(define-syntax define-foreign
  (syntax-rules () [(_ . spec) (define-foreign-as plain . spec)]))
(define-syntax define-foreign/collect-safe
  (syntax-rules () [(_ . spec) (define-foreign-as collect-safe . spec)]))

(define (c-string->string ptr)
  (if (= ptr 0)
      ""
      (let ([n (let loop ([i 0])
                 (if (= 0 (foreign-ref 'unsigned-8 ptr i)) i (loop (+ i 1))))])
        (let ([bv (make-bytevector n)])
          (do ([i 0 (+ i 1)]) ((= i n))
            (bytevector-u8-set! bv i (foreign-ref 'unsigned-8 ptr i)))
          (utf8->string bv)))))

;;; ------------------------------------------------------------------ SDL3

(define SDL_INIT_VIDEO #x20)
(define SDL_WINDOW_FULLSCREEN #x1)
(define SDL_WINDOW_HIDDEN #x8)
(define SDL_WINDOW_INPUT_FOCUS #x200)
(define SDL_WINDOW_RESIZABLE #x20)
(define SDL_WINDOW_HIGH_PIXEL_DENSITY #x2000)
(define SDL_WINDOWPOS_CENTERED #x2FFF0000)
(define SDL_PIXELFORMAT_ARGB8888 #x16362004)
(define SDL_TEXTUREACCESS_STREAMING 1)
(define SDL_SCALEMODE_NEAREST 0)

(define SDL_EVENT_QUIT #x100)
(define SDL_EVENT_WINDOW_EXPOSED #x204)
(define SDL_EVENT_WINDOW_RESIZED #x206)
(define SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED #x207)
(define SDL_EVENT_WINDOW_MOUSE_LEAVE #x20D)
(define SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED #x214)
(define SDL_EVENT_KEY_DOWN #x300)
(define SDL_EVENT_TEXT_INPUT #x303)
(define SDL_EVENT_MOUSE_MOTION #x400)
(define SDL_EVENT_MOUSE_BUTTON_DOWN #x401)
(define SDL_EVENT_MOUSE_BUTTON_UP #x402)
(define SDL_EVENT_MOUSE_WHEEL #x403)
(define SDL_EVENT_RENDER_TARGETS_RESET #x2000)
(define SDL_EVENT_RENDER_DEVICE_RESET #x2001)

(define-foreign sdl-init "SDL_Init" (unsigned-32) bool)
(define-foreign sdl-quit "SDL_Quit" () void)
(define-foreign sdl-get-error "SDL_GetError" () string)
(define-foreign sdl-get-ticks-ns "SDL_GetTicksNS" () unsigned-64)
(define-foreign sdl-get-num-logical-cpu-cores "SDL_GetNumLogicalCPUCores" ()
  int)
(define-foreign sdl-set-hint "SDL_SetHint" (string string) bool)
(define-foreign sdl-create-window "SDL_CreateWindow"
  (string int int unsigned-64) uptr)
(define-foreign sdl-destroy-window "SDL_DestroyWindow" (uptr) void)
(define-foreign sdl-set-window-title "SDL_SetWindowTitle" (uptr string) bool)
(define-foreign sdl-set-window-minimum-size "SDL_SetWindowMinimumSize"
  (uptr int int) bool)
(define-foreign sdl-set-window-size "SDL_SetWindowSize" (uptr int int) bool)
(define-foreign sdl-set-window-position "SDL_SetWindowPosition"
  (uptr int int) bool)
(define-foreign sdl-show-window "SDL_ShowWindow" (uptr) bool)
(define-foreign sdl-get-window-size "SDL_GetWindowSize" (uptr uptr uptr) bool)
(define-foreign sdl-get-window-display-scale "SDL_GetWindowDisplayScale"
  (uptr) float)
;; the name of a render driver, or #f to let SDL choose
(define-foreign sdl-create-renderer "SDL_CreateRenderer" (uptr string) uptr)
(define-foreign sdl-destroy-renderer "SDL_DestroyRenderer" (uptr) void)
(define-foreign sdl-get-current-render-output-size
  "SDL_GetCurrentRenderOutputSize" (uptr uptr uptr) bool)
(define-foreign sdl-create-texture "SDL_CreateTexture"
  (uptr unsigned-32 int int int) uptr)
(define-foreign sdl-set-texture-scale-mode "SDL_SetTextureScaleMode"
  (uptr int) bool)
(define-foreign sdl-destroy-texture "SDL_DestroyTexture" (uptr) void)
(define-foreign/collect-safe sdl-update-texture "SDL_UpdateTexture"
  (uptr uptr uptr int) bool)
(define-foreign sdl-render-texture "SDL_RenderTexture"
  (uptr uptr uptr uptr) bool)
(define-foreign/collect-safe sdl-render-present "SDL_RenderPresent"
  (uptr) bool)
(define-foreign sdl-set-render-vsync "SDL_SetRenderVSync" (uptr int) bool)
(define-foreign sdl-get-display-for-window "SDL_GetDisplayForWindow"
  (uptr) unsigned-32)
(define-foreign sdl-get-current-display-mode "SDL_GetCurrentDisplayMode"
  (unsigned-32) uptr)
(define-foreign sdl-poll-event "SDL_PollEvent" (uptr) bool)
(define-foreign/collect-safe sdl-wait-event-timeout "SDL_WaitEventTimeout"
  (uptr int) bool)
(define-foreign sdl-start-text-input "SDL_StartTextInput" (uptr) bool)
(define-foreign sdl-get-clipboard-text "SDL_GetClipboardText" () uptr)
(define-foreign sdl-set-clipboard-text "SDL_SetClipboardText" (string) bool)
(define-foreign sdl-free "SDL_free" (uptr) void)
(define-foreign sdl-create-system-cursor "SDL_CreateSystemCursor" (int) uptr)
(define-foreign sdl-set-cursor "SDL_SetCursor" (uptr) bool)
(define-foreign sdl-set-window-fullscreen "SDL_SetWindowFullscreen"
  (uptr bool) bool)
(define-foreign sdl-get-window-flags "SDL_GetWindowFlags" (uptr) unsigned-64)
(define-foreign sdl-create-surface-from "SDL_CreateSurfaceFrom"
  (int int unsigned-32 uptr int) uptr)
(define-foreign sdl-destroy-surface "SDL_DestroySurface" (uptr) void)
(define-foreign sdl-set-window-icon "SDL_SetWindowIcon" (uptr uptr) bool)
(define-foreign sdl-memcmp "SDL_memcmp" (uptr uptr size_t) int)
(define-foreign sdl-memcpy "SDL_memcpy" (uptr uptr size_t) uptr)

;;; ------------------------------------------------------------------ Cairo

(define CAIRO_FORMAT_ARGB32 0)
(define CAIRO_FONT_SLANT_NORMAL 0)
(define CAIRO_FONT_SLANT_ITALIC 1)
(define CAIRO_FONT_WEIGHT_NORMAL 0)
(define CAIRO_FONT_WEIGHT_BOLD 1)
(define CAIRO_LINE_CAP_ROUND 1)
(define CAIRO_LINE_JOIN_ROUND 1)

(define-foreign cairo-image-surface-create "cairo_image_surface_create"
  (int int int) uptr)
(define-foreign cairo-image-surface-create-for-data
  "cairo_image_surface_create_for_data"
  (uptr int int int int) uptr)
(define-foreign cairo-image-surface-get-width "cairo_image_surface_get_width"
  (uptr) int)
(define-foreign cairo-image-surface-get-height "cairo_image_surface_get_height"
  (uptr) int)
(define-foreign cairo-image-surface-get-data "cairo_image_surface_get_data"
  (uptr) uptr)
(define-foreign cairo-image-surface-get-stride "cairo_image_surface_get_stride"
  (uptr) int)
(define-foreign cairo-surface-flush "cairo_surface_flush" (uptr) void)
(define-foreign cairo-surface-mark-dirty "cairo_surface_mark_dirty" (uptr) void)
(define-foreign cairo-surface-destroy "cairo_surface_destroy" (uptr) void)
(define-foreign cairo-surface-write-to-png "cairo_surface_write_to_png"
  (uptr string) int)
(define-foreign cairo-create "cairo_create" (uptr) uptr)
(define-foreign cairo-destroy "cairo_destroy" (uptr) void)
(define-foreign cairo-save "cairo_save" (uptr) void)
(define-foreign cairo-restore "cairo_restore" (uptr) void)
(define-foreign cairo-scale "cairo_scale" (uptr double double) void)
(define-foreign cairo-translate "cairo_translate" (uptr double double) void)
(define-foreign cairo-rotate "cairo_rotate" (uptr double) void)
(define-foreign cairo-identity-matrix "cairo_identity_matrix" (uptr) void)
;; Writes the six doubles xx yx xy yy x0 y0 to the given address.
(define-foreign cairo-get-matrix "cairo_get_matrix" (uptr uptr) void)
(define CAIRO_OPERATOR_CLEAR 0)
(define CAIRO_OPERATOR_SOURCE 1)
(define CAIRO_OPERATOR_OVER 2)
(define CAIRO_OPERATOR_ADD 12)
(define CAIRO_OPERATOR_DIFFERENCE 23)
(define-foreign cairo-set-operator "cairo_set_operator" (uptr int) void)
(define-foreign cairo-set-source-rgba "cairo_set_source_rgba"
  (uptr double double double double) void)
(define-foreign cairo-set-source "cairo_set_source" (uptr uptr) void)
(define-foreign cairo-set-source-surface "cairo_set_source_surface"
  (uptr uptr double double) void)
(define-foreign cairo-new-path "cairo_new_path" (uptr) void)
(define-foreign cairo-new-sub-path "cairo_new_sub_path" (uptr) void)
(define-foreign cairo-close-path "cairo_close_path" (uptr) void)
(define-foreign cairo-move-to "cairo_move_to" (uptr double double) void)
(define-foreign cairo-line-to "cairo_line_to" (uptr double double) void)
(define-foreign cairo-curve-to "cairo_curve_to"
  (uptr double double double double double double) void)
(define-foreign cairo-rectangle "cairo_rectangle"
  (uptr double double double double) void)
(define-foreign cairo-arc "cairo_arc"
  (uptr double double double double double) void)
(define-foreign cairo-fill "cairo_fill" (uptr) void)
(define-foreign cairo-fill-preserve "cairo_fill_preserve" (uptr) void)
(define-foreign cairo-stroke "cairo_stroke" (uptr) void)
(define-foreign cairo-paint "cairo_paint" (uptr) void)
(define-foreign cairo-clip "cairo_clip" (uptr) void)
(define-foreign cairo-set-line-width "cairo_set_line_width" (uptr double) void)
(define-foreign cairo-set-line-cap "cairo_set_line_cap" (uptr int) void)
(define-foreign cairo-set-line-join "cairo_set_line_join" (uptr int) void)
(define-foreign cairo-toy-font-face-create "cairo_toy_font_face_create"
  (string int int) uptr)
(define-foreign cairo-set-font-face "cairo_set_font_face" (uptr uptr) void)
(define-foreign cairo-set-font-size "cairo_set_font_size" (uptr double) void)
(define-foreign cairo-show-text "cairo_show_text" (uptr string) void)
(define-foreign cairo-get-current-point "cairo_get_current_point"
  (uptr uptr uptr) void)
(define-foreign cairo-text-extents "cairo_text_extents" (uptr string uptr) void)
(define-foreign cairo-font-extents "cairo_font_extents" (uptr uptr) void)
(define-foreign cairo-pattern-create-linear "cairo_pattern_create_linear"
  (double double double double) uptr)
(define-foreign cairo-pattern-create-radial "cairo_pattern_create_radial"
  (double double double double double double) uptr)
(define-foreign cairo-pattern-add-color-stop-rgba
  "cairo_pattern_add_color_stop_rgba"
  (uptr double double double double double) void)
(define-foreign cairo-pattern-destroy "cairo_pattern_destroy" (uptr) void)
(define-foreign cairo-pattern-create-for-surface
  "cairo_pattern_create_for_surface"
  (uptr) uptr)
(define-foreign cairo-pattern-set-extend "cairo_pattern_set_extend"
  (uptr int) void)
(define-foreign cairo-pattern-set-filter "cairo_pattern_set_filter"
  (uptr int) void)
(define CAIRO_EXTEND_REPEAT 1)
(define CAIRO_FILTER_NEAREST 3)
;; Six doubles xx yx xy yy x0 y0 at the address, from user space.
(define-foreign cairo-pattern-set-matrix "cairo_pattern_set_matrix"
  (uptr uptr) void)
;; text drawn without antialiasing (a crisp font)
(define CAIRO_ANTIALIAS_NONE 1)
(define-foreign cairo-font-options-create "cairo_font_options_create" () uptr)
(define-foreign cairo-font-options-set-antialias
  "cairo_font_options_set_antialias" (uptr int) void)
(define-foreign cairo-set-font-options "cairo_set_font_options"
  (uptr uptr) void)
