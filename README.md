# Schevana OS (prototype)

A window manager that lives inside one host window and is written entirely
in Chez Scheme. Every window, colour, key binding, status-bar segment and
running process is a live Scheme value. You can inspect it, redefine it and
break it from a REPL that runs inside the world it changes.

The only C code involved is SDL2 (host window and input) and Cairo (2D
drawing), both called through Chez's FFI. There is no build step: the whole
world compiles from source in about 0.1 s at startup.

## Running

Requirements: Chez Scheme 10, `libSDL2-2.0`, `libcairo2` (both usually
installed already on a Linux desktop).

```bash
./schevana
```

```bash
./schevana --size 1600x1000 --eval '(life)'
```

`~/.schevana.ss` is loaded at boot if it exists, like `.emacs`.

| Key | Action |
| --- | --- |
| F1 / click "λ Schevana" / right-click desktop | launcher (type to filter) |
| Ctrl+Tab | cycle windows |
| Ctrl+Shift+W / M / N / T / R | close / maximize / minimize / tile / new REPL |
| F12 | screenshot to `~/schevana-N.png` |
| drag title, edges, corner; double-click title | move, resize, maximize |
| right-click title or taskbar button | window menu (incl. "Bind to `$w`") |

REPL: Enter evaluates when the form is complete (otherwise it inserts an
indented newline), Shift+Enter always adds a newline, Tab completes from the
live environment, Up/Down walk history, Ctrl+C interrupts, Ctrl+L clears,
Ctrl+V pastes.

## Things to try in the REPL

```scheme
(windows)                                  ; the window list, as data
(move-window! (find-window 'clock) 40 60)
(set-theme! 'accent #xff9e64)              ; recolour everything
(add-status-segment! 'hi (lambda () (format "~a windows" (length (windows)))))
(bind-key! '(ctrl shift #\l) life)
(spawn "busy" (lambda () (let loop () (loop))))   ; watch it in (monitor); Ctrl+C or kill!
(define (draw-wallpaper w h) (fill-gradient 0 0 w h #x102030 #x301020))
```

Define a new app and it appears in the launcher immediately:

```scheme
(define-app (counter) "Counter"
  (let ([n 0])
    (open-window "Counter" 'size '(240 . 100)
      'draw (lambda (win w h)
              (draw-text-centered 0 0 w h (number->string n) 'accent 'big))
      'mouse-down (lambda (win x y button clicks) (set! n (+ n 1)) (invalidate!)))))
```

## Architecture

```
apps/        repl  clock  life  sketch  monitor      ← ordinary procedures
src/system   main loop, timers, inject!, boot, reload!
src/shell    wallpaper, top bar + status segments, taskbar, menus, toasts
src/wm       window records, z-order, focus, hit testing, drag/resize, dispatch
src/proc     processes + watchdog (Chez engines)
src/host     SDL2 window ⇄ Scheme event lists   |  headless mode
src/gfx      immediate-mode drawing API, colours, fonts, layers
src/ffi      the only file that knows about C (SDL2 + Cairo)
```

Decisions worth keeping in mind for the real design:

- **Everything is top-level.** No R6RS libraries on purpose. Every
  definition lives in the interaction environment, so redefining
  `draw-window` or `draw-wallpaper` at the REPL changes the running system
  on the next frame. State that must survive `(reload!)` is declared with
  `defvar`, and record types are `nongenerative`.
- **Apps are procedures.** `define-app` defines a normal procedure that
  calls `open-window` with a property list of handlers (`'draw`, `'key`,
  `'text`, `'mouse-down`, `'tick`, ...). Every other option becomes a
  window property (`window-ref` / `window-set!`).
- **Kernel vs user space via engines.** Each app handler runs in a Chez
  engine with a fuel budget (`safe-call`). An error or an infinite loop
  "crashes" only that window, which then shows the condition and can be
  retried or fixed live with `window-on!`. REPL evaluations run as
  *processes*: engines that get time slices every frame, so `(let loop ()
  (loop))` never freezes the desktop. System code uses `safely` (errors are
  caught, but there is no fuel limit).
- **Events are data.** The host turns SDL events into lists like
  `(mouse-down 120 40 left 1)` and `(key-down #\w (ctrl shift))`.
  `(inject! ...)` feeds the same lists in, which is how the tests drive the
  world.
- **Immediate-mode drawing, full redraw on change.** Anything can call
  `(invalidate!)`, and the next frame repaints everything into a Cairo image
  that becomes an SDL texture. The wallpaper is cached in a layer. A typical
  frame with five windows takes about 4 ms.

## Tests

```bash
scheme --script tests/smoke.ss /tmp
```

This boots the world headlessly, types into the REPL, defines an app,
crashes windows, runs away in loops, drags and resizes windows, uses the
launcher, reloads the sources, and writes screenshots to the given folder.

## Known gaps and open questions

- One shared environment: apps can't be sandboxed or namespaced from each
  other yet. First-class environments (`copy-environment`) are an option.
- Full-screen redraw and no damage tracking. Per-window layers would make
  the cost depend on what changed instead of on screen size.
- No widget toolkit (buttons, text fields, lists, layout) yet. That is the
  "stable GUI API" question: immediate-mode procedures like today, or
  retained widget trees described as data?
- REPL lacks selection and copy, and there is no text editor app yet.
- No persistence. Windows and state vanish on exit (`fasl-write` of app
  state is a natural first step).
- Portability: library names for macOS and Windows are in `ffi.ss` but have
  not been tried. macOS will need the loop on the main thread, which it
  already is.
