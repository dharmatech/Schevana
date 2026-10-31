# Schevana

A Lisp Machine-style desktop that lives inside one host window and is written
entirely in Chez Scheme. Every window, color, key binding, status-bar segment
and running process is a live Scheme value. You can inspect it, redefine it and
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

```bash
./schevana --fullscreen
```

`~/.schevana.ss` is loaded at boot if it exists, like `.emacs`. The look
you choose in the Settings app is saved to `~/.schevana-theme.ss` (plain
data, never evaluated) and loaded at boot too. The host window's taskbar
icon is the wallpaper's λ in your theme's colors, drawn by `draw-app-icon`
(redefine it, then call `(update-host-icon!)`). GNOME and similar desktops
take app icons from `.desktop` files instead, so install one once
(it goes under `~/.local/share`; `--uninstall-desktop` removes it):

```bash
./schevana --install-desktop
```

| Key | Action |
| --- | --- |
| F1 / click "λ Schevana" / right-click desktop | launcher (type to filter); apps picked after a right-click open where you clicked |
| Ctrl+Tab | cycle windows |
| Ctrl+Shift+W / M / N / T / R | close / maximize / minimize / tile / new REPL |
| Ctrl+Shift+P | new Private REPL |
| F11 | full screen on and off (also in the launcher) |
| F12 | screenshot to `~/schevana-N.png` |
| drag title, edges, corner; double-click title | move, resize, maximize |
| right-click title or taskbar button | window menu (incl. "Bind to `$w`") |

REPL: Enter evaluates when the form is complete (otherwise it inserts an
indented newline), Shift+Enter always adds a newline, Up/Down walk history,
Ctrl+C interrupts, Ctrl+L clears, Ctrl+V pastes.

Tab opens a completion list under the cursor that narrows as you type
(Up/Down choose, Tab or Enter accept, Esc closes). It offers what fits
where the cursor is: names bound in that REPL's environment; theme keys,
apps, palettes or window styles after a quote in `(theme '…)`,
`(set-theme! '…)`, `(launch-app '…)` and the like; file names inside a
string; library names inside `(import (…))`, a part at a time. Names
match by prefix, substring, or initials (`stp` → `set-theme-plist!`), and
the chosen one shows its argument list and the comment above its
definition, read from the sources (a library shows what it exports).

There are three kinds of REPL, each in its own environment:

| | Opens with | What a `define` does |
| --- | --- | --- |
| REPL | `(repl)`, Ctrl+Shift+R | changes the running world |
| Private REPL | `(private-repl)`, Ctrl+Shift+P | stays in that window and vanishes with it, like private browsing |
| Pure Scheme REPL | `(pure-repl)` | plain Chez Scheme, none of Schevana's names exist |

A private REPL starts as a copy of the world (`copy-environment`), so it can
use everything, and redefining `draw-window` there breaks nothing. Its
protection covers names, not state: calling the world's procedures, such
as `(set-theme! ...)`, `(open-window ...)` or `(reload!)`, still acts on
the world. It is not a security sandbox either: the FFI is reachable from
any REPL. In private and pure REPLs, `(exit)` closes the window instead
of Schevana.

R6RS libraries work as in Chez: `(import (acme math))` in any REPL, in an
app's file, or inside a body (`(let () (import (acme math)) …)`). Besides
Chez's usual places, import looks in `~/.schevana.d/lib` (so
`~/.schevana.d/lib/acme/math.sls`), and `(add-library-directory! dir)`
adds more. Two things differ from Schevana's own code. A library is
loaded once per session: after editing one, `load` its file again and
import it again; `(reload!)` doesn't. And an import adds names to the
environment it runs in, the world's for apps and the REPL, so a library
exporting a name Schevana also defines changes what that name means in
code you type afterwards; `(only …)` or `(prefix … acme:)` keep them apart.

## Things to try in the REPL

```scheme
(windows)                                  ; the window list, as data
(move-window! (find-window 'clock) 40 60)
(set-theme! 'accent #xff9e64)              ; recolor everything
(apply-theme-preset! 'paper)               ; or: tokyo-night catppuccin nord gruvbox win98 interlisp genera
(set-theme! 'window-style 'win98)          ; or: mac windows interlisp genera
(open-window "Odd one out" 'window-style 'windows)   ; per-window override
(set-theme! 'ui-font-size 15)              ; fonts and sizes are theme keys too
(add-status-segment! 'hi (lambda () (format "~a windows" (length (windows)))))
(bind-key! '(ctrl shift #\l) life)
(spawn "busy" (lambda () (let loop () (loop))))   ; watch it in (monitor); Ctrl+C or kill!
(define (draw-wallpaper w h) (fill-gradient 0 0 w h #x102030 #x301020))
(define (draw-window win) (draw-window* win #t))   ; every window looks focused; (reload!) undoes it
```

Define a new app and it appears in the launcher immediately:

```scheme
(define-app (counter) "Counter"
  (let ([n 0])
    (open-window "Counter" 'size '(240 . 100)
      'draw (lambda (win w h)
              (draw-text-centered 0 0 w h (number->string n) 'accent 'big))
      'mouse-down (lambda (win x y button clicks) (set! n (+ n 1))))))
```

## Architecture

```
apps/        repl  clock  life  sketch  monitor  settings   ← ordinary procedures
src/system   main loop, timers, inject!, boot, reload!
src/ui       immediate-mode widgets: button, slider, segmented, clickable area
src/shell    wallpaper, top bar + status segments, taskbar, menus, toasts
src/styles   window styles (Mac, Windows, Windows 98, Interlisp, Genera): frames, buttons
src/wm       window records, z-order, focus, hit testing, drag/resize, dispatch
src/proc     processes + watchdog (Chez engines)
src/host     SDL2 window ⇄ Scheme event lists, uploads   |  headless mode
src/gfx      immediate-mode drawing API, colors, fonts, layers; the two screen buffers
src/shadows  window, menu and toast shadows (other files mark theirs ;; shadow{ … ;; shadow})
src/theme    colors, metrics and fonts as data; presets; save/load
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
- **Reloading holds the world still.** `(reload!)` typed in a REPL, or the
  launcher's "Reload sources", is a process too, so a source file whose top
  level loops can't freeze the desktop. While it loads, nothing else runs
  half-loaded code: no frames (the screen stays as it was), timers, ticks or
  other processes, and input waits, to be handled in order when it is done.
  Ctrl+C stops it (and closing the window still quits); a toast names the
  file it was loading when it is stopped or fails.
- **Events are data.** The host turns SDL events into lists like
  `(mouse-down 120 40 left 1)` and `(key-down #\w (ctrl shift))`.
  `(inject! ...)` feeds the same lists in, which is how the tests drive the
  world.
- **Settings are theme keys.** Colors, sizes and fonts all live in one
  table. The Settings app is only a front end to `set-theme!`, so anything
  it does can be scripted from the REPL, and the saved file is just the
  keys that differ from the defaults.
- **Logical pixels.** All layout is in logical pixels; the canvas is
  drawn `*scale*` times bigger with a single `cairo-scale`, so text and
  shapes are rendered from outlines at full resolution. The scale follows
  the desktop (SDL's pixel ratio on macOS and Wayland, `Xft.dpi` under
  X11, which is how GNOME publishes 200%), and the theme key `scale`
  (a percentage; 0 for automatic, in Settings → Display) overrides it.
  Mouse positions are converted at the host boundary, so apps never see
  physical pixels.
- **Sizes follow the fonts.** Title bars, panels, menus, the apps' bars
  and the Settings layout each take the larger of their own setting and
  what their text needs, so the interface stays usable from 9 to 36 px (44
  px for code). Where a window is too small, Settings scrolls and stacks
  its rows. A new REPL opens at 80 × 22 characters of the code font.
- **Window styles own the frame.** A style is a record of procedures:
  frame insets, a list of buttons (`close`, `minimize`, `maximize`,
  `depth`), and two drawing passes around the content. The window manager
  only does geometry and dispatch, so `define-window-style!` at the REPL
  adds a new look. Apps always get their content size, whatever the frame.
  The Windows 98 style matches real Windows 98 screenshots pixel for
  pixel. The two 1980s Lisp environments were matched against real
  screens: Interlisp (Interlisp-D/Medley, Xerox PARC) with its black title
  bars, and Genera (Symbolics Lisp Machines), built to the margins its
  1986 manuals specify, with the window's label in the bottom-left corner,
  which is also where you drag it: a style can list any area as its
  `'title`. Each pairs with the palette of the same name; the
  Interlisp one brings back the dotted desktop shade (the
  `desktop-pattern` theme key).
- **Every frame is drawn from scratch.** The canvas is the exact
  screen. When something asks for a frame (`(invalidate!)`,
  `(invalidate! win)`, input to a window, any change the window manager
  or the shell makes, and the clock once a second), the wallpaper, every
  window from the bottom up, the panels, the menu and the toasts are
  drawn again (`render-full-into!` in `src/wm.ss`). A step that nothing
  asked a frame of draws, uploads and presents nothing. Nothing is
  assumed about what a drawing procedure does, so redefining
  `draw-window`, a window style or `draw-taskbar` at the REPL simply
  changes the next frame. An app that changes what it shows says so with
  `(invalidate! win)`; `(invalidate! win x y w h)`, from when only part
  of a window was redone, means the same. The wallpaper is kept as a
  picture, and it is a picture of its key: the screen's size and
  scale, `draw-wallpaper` and the whole theme, whether changed by
  `set-theme!` or behind its back. The next frame paints it again first
  when one of those changes, and nothing else does: a `draw-wallpaper`
  that reads the clock shows a still picture.
- **Drawing is single-threaded.** Everything runs on the main thread,
  drawing included. A frame is drawn into a second screen buffer; then
  each of its rows is compared with the screen's, and only the rows that
  changed are copied onto the screen and uploaded, each run of them as
  one full-width rectangle. A frame that changes nothing uploads and
  presents nothing. Monitor shows the last frame's time (drawing it and
  comparing its rows) and the rows it uploaded. A process may be
  switched out at any call while frames go on, so typed in a REPL (or
  called by a handler) `(render-frame!)` only asks for a frame, and
  freeing the canvas or a cache of surfaces runs with interrupts off:
  no frame sees it half done. Parallel rasterizing would come with step
  6 of the renderer plan: every window drawn into its own image by a
  crew of threads, and the images composed on the GPU.

## Tests

```bash
mkdir -p out
scheme --script tests/smoke.ss out
```

This boots the world headlessly, types into the REPL, defines an app,
crashes windows, runs away in loops, drags and resizes windows, uses the
launcher, presses every button of each window style, clicks and drags
through every Settings tab (checking the theme file round-trips), reloads the sources, runs the host's SDL code
(uploads, presents, resize events, the window's size) against a stand-in
that replaces every SDL call, so no window opens, and writes screenshots
to the given folder. It never touches your real `~/.schevana-theme.ss`.

```bash
scheme --script tests/units.ss out
```

This tests the drawing layer piece by piece: the measuring
context, cached shadows against the stacked ones, pixmaps, Life's
generations against a plain reference (a step allocates nothing), its
grid against one rectangle per cell, and the invalidation calls.

```bash
scheme --script tests/frame.ss out
```

This tests the renderer: once the frames asked for are drawn, the
screen equals `render-full-into!` drawn directly, bit for bit, on
several desktops, every window style, a menu and a toast, at 100, 125,
150, 200 and 300%; a stand-in texture fed only the uploaded rows always
equals the screen, and a frame that changes nothing uploads and
presents nothing; with a stand-in window a boot, a resize or a rescale
shows its own frame at once, never an empty screen; a process, switched
out anywhere, may free surfaces, rescale and call `(render-frame!)`
while frames go on; drawing that raises crashes only its window, or
shows no frame; redefined `draw-window`, `draw-taskbar` and styles show
at once and cost the same; a theme change (by `set-theme!` or behind
its back) or a redefined `draw-wallpaper` paints the wallpaper again
for the next frame; and each generation of 30 Life windows shows on the
next frame.

```bash
scheme --script tests/bench.ss out               # [frames]
```

Times frames on four scenes at 100% and 200%: 30 Life windows tiled,
a typical desktop of five overlapping windows, 30 Life windows cascaded,
and those under a maximized REPL, with every Life window stepping every
frame. For each it prints the main thread's time per frame (all the
work: everything runs on it), the wall time, the bytes uploaded and the
collections, then times a keystroke in the REPL, an idle step (it must
stay under 0.2 ms and upload nothing) and a theme change, and prints
how much of the main thread a busy REPL evaluation gets while 30 Life
windows animate at 200%.

## Known gaps and open questions

- Apps all share one environment. REPLs can be private or pure, but apps
  can't yet be namespaced from each other the same way.
- Every frame draws the whole screen on the main thread and compares it
  with the screen row by row. With 30 animating Life windows that is
  about 4.3 ms a frame at 100% and 10 ms at 200%; 30 overlapping windows
  at 200% take about 40 ms a frame, and a keystroke shows about 10 ms
  later at 200% (`tests/bench.ss`). Meanwhile a busy REPL evaluation
  runs only between frames: with 30 Life windows animating at 200% it
  gets about half of what it gets with them paused. Step 6 of the
  renderer plan would draw each window into its own image, only when it
  changes, with a crew of threads.
- Every theme change paints the wallpaper again: a theme change takes
  about 18 ms at 100% and 68 ms at 200% until its frame is drawn, so
  dragging a color slider in Settings shows about 15 frames a second at
  200%.
- Only a minimal widget kit (`src/ui.ss`, written for Settings): no text
  fields, lists or layout yet. That is the "stable GUI API" question:
  keep growing these immediate-mode procedures, or describe retained
  widget trees as data?
- REPL lacks selection and copy, and there is no text editor app yet.
- No persistence. Windows and state vanish on exit (`fasl-write` of app
  state is a natural first step).
- Portability: library names for macOS and Windows are in `ffi.ss` but have
  not been tried. macOS will need the loop on the main thread, which it
  already is.
