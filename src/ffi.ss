;;; ffi.ss — the only place where Schevana talks to C.
;;;
;;; SDL2 gives us a host window, input events and a clipboard.
;;; Cairo gives us 2D drawing into an in-memory image.
;;; Everything above this file is plain Scheme.

(define (load-first-library names)
  (let loop ([ns names])
    (cond
      [(null? ns) (errorf 'load-first-library "could not load any of ~s" names)]
      [(guard (c [#t #f]) (load-shared-object (car ns)) #t) (car ns)]
      [else (loop (cdr ns))])))

(define libsdl
  (load-first-library
   '("libSDL2-2.0.so.0" "libSDL2.so" "libSDL2-2.0.0.dylib" "libSDL2.dylib" "SDL2.dll")))

(define libcairo
  (load-first-library
   '("libcairo.so.2" "libcairo.so" "libcairo.2.dylib" "libcairo.dylib" "libcairo-2.dll" "cairo.dll")))

;; Like foreign-procedure, but `double` arguments accept any real number.
(define-syntax define-foreign
  (lambda (x)
    (syntax-case x ()
      [(_ name cname (type ...) ret)
       (with-syntax ([(arg ...) (generate-temporaries #'(type ...))])
         (with-syntax ([(conv ...)
                        (map (lambda (t a)
                               (if (eq? (syntax->datum t) 'double) #`(inexact #,a) a))
                             #'(type ...) #'(arg ...))])
           #'(define name
               (let ([f (foreign-procedure cname (type ...) ret)])
                 (lambda (arg ...) (f conv ...))))))])))

(define (c-string->string ptr)
  (if (= ptr 0)
      ""
      (let ([n (let loop ([i 0]) (if (= 0 (foreign-ref 'unsigned-8 ptr i)) i (loop (+ i 1))))])
        (let ([bv (make-bytevector n)])
          (do ([i 0 (+ i 1)]) ((= i n)) (bytevector-u8-set! bv i (foreign-ref 'unsigned-8 ptr i)))
          (utf8->string bv)))))

;;; ------------------------------------------------------------------ SDL2

(define SDL_INIT_VIDEO #x20)
(define SDL_WINDOW_HIDDEN #x8)
(define SDL_WINDOW_RESIZABLE #x20)
(define SDL_WINDOW_ALLOW_HIGHDPI #x2000)
(define SDL_WINDOW_FULLSCREEN_DESKTOP #x1001)
(define SDL_WINDOWPOS_CENTERED #x2FFF0000)
(define SDL_RENDERER_ACCELERATED 2)
(define SDL_RENDERER_PRESENTVSYNC 4)
(define SDL_PIXELFORMAT_ARGB8888 #x16362004)
(define SDL_TEXTUREACCESS_STREAMING 1)

(define SDL_QUIT #x100)
(define SDL_WINDOWEVENT #x200)
(define SDL_KEYDOWN #x300)
(define SDL_KEYUP #x301)
(define SDL_TEXTINPUT #x303)
(define SDL_MOUSEMOTION #x400)
(define SDL_MOUSEBUTTONDOWN #x401)
(define SDL_MOUSEBUTTONUP #x402)
(define SDL_MOUSEWHEEL #x403)
(define SDL_WINDOWEVENT_EXPOSED 3)
(define SDL_WINDOWEVENT_RESIZED 5)
(define SDL_WINDOWEVENT_SIZE_CHANGED 6)
(define SDL_WINDOWEVENT_LEAVE 11)
(define SDL_RENDER_TARGETS_RESET #x2000)
(define SDL_RENDER_DEVICE_RESET #x2001)

(define-foreign sdl-init "SDL_Init" (unsigned-32) int)
(define-foreign sdl-quit "SDL_Quit" () void)
(define-foreign sdl-get-error "SDL_GetError" () string)
(define-foreign sdl-set-hint "SDL_SetHint" (string string) boolean)
(define-foreign sdl-create-window "SDL_CreateWindow" (string int int int int unsigned-32) uptr)
(define-foreign sdl-destroy-window "SDL_DestroyWindow" (uptr) void)
(define-foreign sdl-set-window-title "SDL_SetWindowTitle" (uptr string) void)
(define-foreign sdl-set-window-minimum-size "SDL_SetWindowMinimumSize" (uptr int int) void)
(define-foreign sdl-set-window-size "SDL_SetWindowSize" (uptr int int) void)
(define-foreign sdl-set-window-position "SDL_SetWindowPosition" (uptr int int) void)
(define-foreign sdl-show-window "SDL_ShowWindow" (uptr) void)
(define-foreign sdl-get-window-size "SDL_GetWindowSize" (uptr uptr uptr) void)
(define-foreign sdl-create-renderer "SDL_CreateRenderer" (uptr int unsigned-32) uptr)
(define-foreign sdl-destroy-renderer "SDL_DestroyRenderer" (uptr) void)
(define-foreign sdl-get-renderer-output-size "SDL_GetRendererOutputSize" (uptr uptr uptr) int)
(define-foreign sdl-create-texture "SDL_CreateTexture" (uptr unsigned-32 int int int) uptr)
(define-foreign sdl-destroy-texture "SDL_DestroyTexture" (uptr) void)
(define-foreign sdl-update-texture "SDL_UpdateTexture" (uptr uptr uptr int) int)
(define-foreign sdl-render-copy "SDL_RenderCopy" (uptr uptr uptr uptr) int)
(define-foreign sdl-render-present "SDL_RenderPresent" (uptr) void)
(define-foreign sdl-poll-event "SDL_PollEvent" (uptr) int)
(define-foreign sdl-wait-event-timeout "SDL_WaitEventTimeout" (uptr int) int)
(define-foreign sdl-start-text-input "SDL_StartTextInput" () void)
(define-foreign sdl-get-clipboard-text "SDL_GetClipboardText" () uptr)
(define-foreign sdl-set-clipboard-text "SDL_SetClipboardText" (string) int)
(define-foreign sdl-free "SDL_free" (uptr) void)
(define-foreign sdl-create-system-cursor "SDL_CreateSystemCursor" (int) uptr)
(define-foreign sdl-set-cursor "SDL_SetCursor" (uptr) void)
(define-foreign sdl-set-window-fullscreen "SDL_SetWindowFullscreen" (uptr unsigned-32) int)
(define-foreign sdl-get-window-flags "SDL_GetWindowFlags" (uptr) unsigned-32)
(define-foreign sdl-create-rgb-surface-with-format-from "SDL_CreateRGBSurfaceWithFormatFrom"
  (uptr int int int int unsigned-32) uptr)
(define-foreign sdl-free-surface "SDL_FreeSurface" (uptr) void)
(define-foreign sdl-set-window-icon "SDL_SetWindowIcon" (uptr uptr) void)
(define-foreign sdl-get-current-video-driver "SDL_GetCurrentVideoDriver" () string)
(define-foreign sdl-memcmp "SDL_memcmp" (uptr uptr size_t) int)
(define-foreign sdl-memcpy "SDL_memcpy" (uptr uptr size_t) uptr)

;;; ------------------------------------------------------------------ Xlib
;;; Only to read the desktop's scale (Xft.dpi) when SDL runs on X11.

(define libx11
  (guard (c [#t #f]) (load-first-library '("libX11.so.6" "libX11.so"))))

;; The X server's resource database as text (what `xrdb -query` prints), or #f.
(define x-resource-string
  (if libx11
      (let ([open (foreign-procedure "XOpenDisplay" (uptr) uptr)]
            [resources (foreign-procedure "XResourceManagerString" (uptr) string)]
            [close (foreign-procedure "XCloseDisplay" (uptr) int)])
        (lambda ()
          (let ([d (open 0)])
            (and (not (= d 0))
                 (let ([s (resources d)]) (close d) s)))))
      (lambda () #f)))

;;; ------------------------------------------------------------------ Cairo

(define CAIRO_FORMAT_ARGB32 0)
(define CAIRO_FONT_SLANT_NORMAL 0)
(define CAIRO_FONT_SLANT_ITALIC 1)
(define CAIRO_FONT_WEIGHT_NORMAL 0)
(define CAIRO_FONT_WEIGHT_BOLD 1)
(define CAIRO_LINE_CAP_ROUND 1)
(define CAIRO_LINE_JOIN_ROUND 1)

(define-foreign cairo-image-surface-create "cairo_image_surface_create" (int int int) uptr)
(define-foreign cairo-image-surface-create-for-data "cairo_image_surface_create_for_data"
  (uptr int int int int) uptr)
(define-foreign cairo-image-surface-get-width "cairo_image_surface_get_width" (uptr) int)
(define-foreign cairo-image-surface-get-height "cairo_image_surface_get_height" (uptr) int)
(define-foreign cairo-image-surface-get-data "cairo_image_surface_get_data" (uptr) uptr)
(define-foreign cairo-image-surface-get-stride "cairo_image_surface_get_stride" (uptr) int)
(define-foreign cairo-surface-flush "cairo_surface_flush" (uptr) void)
(define-foreign cairo-surface-mark-dirty "cairo_surface_mark_dirty" (uptr) void)
(define-foreign cairo-surface-destroy "cairo_surface_destroy" (uptr) void)
(define-foreign cairo-surface-write-to-png "cairo_surface_write_to_png" (uptr string) int)
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
(define-foreign cairo-set-operator "cairo_set_operator" (uptr int) void)
(define-foreign cairo-set-source-rgba "cairo_set_source_rgba" (uptr double double double double) void)
(define-foreign cairo-set-source "cairo_set_source" (uptr uptr) void)
(define-foreign cairo-set-source-surface "cairo_set_source_surface" (uptr uptr double double) void)
(define-foreign cairo-new-path "cairo_new_path" (uptr) void)
(define-foreign cairo-new-sub-path "cairo_new_sub_path" (uptr) void)
(define-foreign cairo-close-path "cairo_close_path" (uptr) void)
(define-foreign cairo-move-to "cairo_move_to" (uptr double double) void)
(define-foreign cairo-line-to "cairo_line_to" (uptr double double) void)
(define-foreign cairo-curve-to "cairo_curve_to" (uptr double double double double double double) void)
(define-foreign cairo-rectangle "cairo_rectangle" (uptr double double double double) void)
(define-foreign cairo-arc "cairo_arc" (uptr double double double double double) void)
(define-foreign cairo-fill "cairo_fill" (uptr) void)
(define-foreign cairo-fill-preserve "cairo_fill_preserve" (uptr) void)
(define-foreign cairo-stroke "cairo_stroke" (uptr) void)
(define-foreign cairo-paint "cairo_paint" (uptr) void)
(define-foreign cairo-clip "cairo_clip" (uptr) void)
(define-foreign cairo-set-line-width "cairo_set_line_width" (uptr double) void)
(define-foreign cairo-set-line-cap "cairo_set_line_cap" (uptr int) void)
(define-foreign cairo-set-line-join "cairo_set_line_join" (uptr int) void)
(define-foreign cairo-toy-font-face-create "cairo_toy_font_face_create" (string int int) uptr)
(define-foreign cairo-set-font-face "cairo_set_font_face" (uptr uptr) void)
(define-foreign cairo-set-font-size "cairo_set_font_size" (uptr double) void)
(define-foreign cairo-show-text "cairo_show_text" (uptr string) void)
(define-foreign cairo-get-current-point "cairo_get_current_point" (uptr uptr uptr) void)
(define-foreign cairo-text-extents "cairo_text_extents" (uptr string uptr) void)
(define-foreign cairo-font-extents "cairo_font_extents" (uptr uptr) void)
(define-foreign cairo-pattern-create-linear "cairo_pattern_create_linear" (double double double double) uptr)
(define-foreign cairo-pattern-create-radial "cairo_pattern_create_radial" (double double double double double double) uptr)
(define-foreign cairo-pattern-add-color-stop-rgba "cairo_pattern_add_color_stop_rgba" (uptr double double double double double) void)
(define-foreign cairo-pattern-destroy "cairo_pattern_destroy" (uptr) void)
(define-foreign cairo-pattern-create-for-surface "cairo_pattern_create_for_surface" (uptr) uptr)
(define-foreign cairo-pattern-set-extend "cairo_pattern_set_extend" (uptr int) void)
(define-foreign cairo-pattern-set-filter "cairo_pattern_set_filter" (uptr int) void)
(define CAIRO_EXTEND_REPEAT 1)
(define CAIRO_FILTER_NEAREST 3)
