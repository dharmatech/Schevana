# Schevana

A Lisp Machine-style desktop that lives inside one host window and is written
entirely in Chez Scheme. Every window, color, key binding, status-bar segment
and running process is a live Scheme value. You can inspect it, redefine it and
break it from a REPL that runs inside the world it changes.

The only C code involved is SDL3 (host window and input) and Cairo (2D
drawing), both called through Chez's FFI. There is no build step: the whole
world compiles from source in about 0.1 s at startup.

**WARNING:** Schevana is a toy. It's meant to be a happy place for Lisp
enthusiasts, and it doesn't pretend to be a serious desktop replacement.
Simplicity is priority #1. The desktop is just a function of the
environment, drawn by the CPU in a single thread, and the apps see the
whole environment. Future versions of Schevana might offload some work to
threads, use the GPU for window composition, and make it possible to write
sandboxed apps with limited permissions.

## Running

Requirements: Chez Scheme 10, SDL 3.2 or later, and `libcairo2` (usually
installed already on a Linux desktop). On Debian 13, SDL3 is `sudo apt
install libsdl3-0`. SDL chooses between Wayland and X11 by itself;
`SDL_VIDEO_DRIVER=x11 ./schevana` (or `wayland`) chooses for it.

On a Mac, install all three from [Homebrew](https://brew.sh):

```bash
brew install chezscheme sdl3 cairo
```

Homebrew's Chez Scheme is the command `chez`, not `scheme`: `./schevana`
runs whichever of the two it finds, and finds Homebrew's libraries in
`/opt/homebrew/lib` (Apple silicon) or `/usr/local/lib` (Intel). To run
the tests below on Apple silicon, type `chez` for `scheme` and put
`DYLD_FALLBACK_LIBRARY_PATH=/opt/homebrew/lib` before it.

```bash
./schevana
```

```bash
./schevana --size 1600x1000 --eval '(life)'
```

```bash
./schevana --fullscreen
```

`~/.schevana.ss` is loaded at boot if it exists, like `.emacs`, and so are
your own apps, styles, palettes and wallpapers in `~/.schevana.d/` (see
"Your own apps, styles, palettes and wallpapers" below). The look you choose
in the Settings app is saved to `~/.schevana-theme.ss` (plain data, never
evaluated) and loaded at boot too. The host window's taskbar icon is the
wallpaper's λ in your theme's colors, drawn by `draw-app-icon` (redefine
it, then call `(update-host-icon!)`). GNOME and similar desktops take app
icons from `.desktop` files instead, so install one once (it goes under
`~/.local/share`; `--uninstall-desktop` removes it):

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

Tab extends the name before the cursor to what the matches starting with
it share, and opens a list of the matches under the cursor that narrows
as you type: Up/Down choose, Tab and Shift+Tab go a page on or back,
Enter or a click takes one, Esc or a click elsewhere in the REPL closes
it, and so does a click on another window or the desktop, or anything
else that takes the keyboard from the REPL (a press on its title bar or
an empty part of a bar does not). With nothing to add, a popup says so:
the sole completion, with its description, or no completions; the next
key, text, click or wheel closes it. It offers what fits where the
cursor is: names bound in that REPL's environment; theme keys, apps,
palettes or styles after a quote in `(theme '…)`,
`(set-theme! '…)`, `(launch-app '…)` and the like; file names inside a
string; library names inside `(import (…))`, a part at a time. Names
match as in the editor e: in segments, each starting where a part of the
name starts, in any order (`dtc` puts `draw-text-centered` first,
`winfind` `find-window`); fewer segments, in order, earlier, tighter and
shorter come first, ignoring case. The chosen one shows its argument
list and the `;;` comment above its definition (a library shows what it
exports), as the code that runs has them: the text each file had when it
was loaded, kept by the core, not the file as it is now; and for a
procedure, the definition its code was compiled from, so one redefined
at the REPL shows what was typed there (input is read with its places,
so errors in it say where too), and one `set!` to another procedure
what that one tells.

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
`~/.schevana.d/lib/acme/math.sls`) and in the tree's `lib/`, for libraries
shipped with Schevana (none yet), and `(add-library-directory! dir)` adds
more. A library is compiled in memory when imported: nothing is written
next to its source (Chez's `compile-imported-libraries` stays off). Two
things differ from Schevana's own code. A library is loaded once per
session: after editing one, `load` its file again and import it again;
`(reload!)` doesn't. And an import adds names to the environment it runs
in, the world's for apps and the REPL, so a library exporting a name
Schevana also defines changes what that name means in code you type
afterwards; `(only …)` or `(prefix … acme:)` keep them apart.

## Things to try in the REPL

```scheme
(windows)                                  ; the window list, as data
(move-window! (find-window 'clock) 40 60)
(set-theme! 'accent #xff9e64)              ; recolor everything
(set-style! 'cons)                         ; or: mac genera fold ...
; or: tokyo-night catppuccin nord gruvbox interlisp genera
(set-theme! 'palette 'paper)               ; remembered for cons
(open-window "Odd one out" 'window-style 'windows)   ; one in its own style
(set-theme! 'ui-font-size 15)              ; fonts and sizes are theme keys too
(override! 'ui-font-size 15)               ; ... in every style
(theme-source 'accent)                     ; which layer gives it
(set-theme! 'title-double-click 'shade)    ; a double click rolls a window up
(override! 'mouse-doc #t)                  ; what the buttons do, in the top bar
(add-status-segment! 'hi (lambda () (format "~a windows" (length (windows)))))
(bind-key! '(ctrl shift #\l) life)
; watch it in (monitor); Ctrl+C or kill!
(spawn "busy" (lambda () (let loop () (loop))))
(tasks)                                    ; a task manager; tasks are data
(add-task! "Fix the menu" 'priority 'high 'tags '("ui"))   ; shows at once
(set-task! "Fix the" 'status 'review)      ; by title, id (27) or "TASK-27"
(set-task! 22 'blocked-by '(14 "TASK-24")) ; 22 waits for them; no cycles
(tasks-tree)                               ; what waits for what, nested
; in a task's description, Ctrl+E opens it in a window of its own
(tasks-list 'status 'doing 'assignee "Pavel")
(define (draw-wallpaper w h) (fill-gradient 0 0 w h #x102030 #x301020))
; every window looks focused; (reload!) undoes it
(define (draw-window win) (draw-window* win #t))
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

## Your own apps, styles, palettes and wallpapers

Styles, palettes (named colors) and wallpapers (desktop fills) have definers
of the same shape as `define-app`: a name, a title, then the definition.
Evaluate one in a REPL and the next frame shows it; define a name again
and the new definition takes the old one's place.

```scheme
(define-palette dusk "Dusk"
  '(bg-0 #x15131c bg-1 #x1c1a26 bg-2 #x24212f bg-3 #x2e2a3b
    text #xe0dcef text-dim #xb8b2cc text-faint #x6e6884
    accent #xf0a35e accent-2 #xc792ea
    red #xf07178 orange #xf78c6c yellow #xffcb6b green #xc3e88d
    cyan #x89ddff teal #x80cbc4 magenta #xc792ea
    desktop-top #x1c1a26 desktop-bottom #x3a2a40 watermark #xffffff
    panel #x111017))
(set-theme! 'palette 'dusk)                ; or click it in Settings
(define-wallpaper stripes "Stripes"
  (lambda (x y w h top bottom)
    (fill-rect x y w h top)
    (do ([i 0 (+ i 24)]) ((>= i h)) (fill-rect x (+ y i) w 12 bottom))))
(set-theme! 'wallpaper 'stripes)
(define (bare-insets win) (values 0 8 0 0))
(define (bare-under win x y w h focused? hover pressed)
  (fill-rect x y w 8 (if focused? 'accent 'bg-3)))
(define-style bare "Bare" 'frame-insets 'bare-insets 'frame-under 'bare-under)
(set-style! 'bare)
(define-style flat-mac "Flat Mac" 'parent 'mac 'corner-radius 0 'palette 'nord)
```

To keep one, drop it into a file of its own in `~/.schevana.d/`, under
`apps/`, `styles/`, `palettes/` or `wallpapers/`, with any code your apps
share under `kits/` and your own bars, menus or keys under `desktop/`.
These folders load at boot after Schevana's own (`kits` first, then
`desktop`, `styles`, `palettes`, `wallpapers` and `apps`, each in
file-name order) and again at every `(reload!)`. Settings lists your
styles and palettes with the others, and the saved look may name them.
`(apps)`, `(styles)`, `(palettes)`, `(wallpapers)` and
`(surfaces)` list what is registered.

## Architecture

```
apps/        ordinary procedures: repl  clock  life  sketch  monitor
             settings  tasks
wallpapers/  desktop fills: gradient  flat  shade
palettes/    named colors: catppuccin  genera  gruvbox  interlisp  nord
             paper  tokyo-night
styles/      styles: cadr  cons  curses  draft  fold  genera  halo
             interlisp  lambda  mac  reticle  slab  spine  windows
desktop/     the screen's own surfaces: wallpaper  taskbar  topbar  menu
             toasts; status segments, the crash panel, the app icon, the
             default keys, the commands that arrange the windows
kits/        complete: a completion list beside a caret (matching, keys,
             clicks, placement); edit: text editing, pure, and a text's
             rows, computed only as far as they are shown; px: pixel art
             on whole device pixels (rectangles, bevels, small pictures,
             text); ui:
             immediate-mode widgets (buttons, text fields and areas,
             dropdowns, tabs, checkboxes, a table, a tree, chips, a date
             picker, scroll views, tooltips, a dialog), keyboard focus
src/system   main loop, timers, inject!, boot, reload!, loading the folders
src/wm       window records, z-order, focus, hit testing, drag/resize,
             dispatch; the registries of surfaces and strips
src/proc     processes + watchdog (Chez engines); notify, report-error!
src/host     SDL3 window ⇄ Scheme event lists, uploads   |  headless mode
src/gfx      immediate-mode drawing API, colors and stipples, fonts and
             ink, layers; the two screen buffers
src/shadows  window, menu and toast shadows (marked ;; shadow{ … } elsewhere)
src/theme    the whole look: settings in layers, rules, palettes, styles,
             parts, the frame's parts; save/load
src/ffi      the only file that knows about C (SDL3 + Cairo)
```

`src/` is the core, the explicit, ordered list in `load.ss`. Its last
file loads the folders: `kits/`, `desktop/`, `styles/`, `palettes/`,
`wallpapers/` and `apps/`, one definition per file (`desktop/` aside),
each folder in file-name order. A file dropped into a folder loads at the
next boot or `(reload!)`, with no list to edit, and `(source-files)`
lists them all.

The core names nothing of the desktop layer's: the crash panel and the
window menu are parts (`draw-crash-panel`, `open-window-menu`) whose
built-ins the desktop layer gives. It keeps two registries:
the screen's **surfaces**, drawn and given input in depth order (the
desktop under the windows; the taskbar, top bar, menu and toasts over
them), and the **strips** they reserve at the screen's edges, which
`desktop-rect` (where windows go) leaves out:

```scheme
(define-surface dock 15                      ; over the taskbar
  'draw (lambda () (fill-rect 0 0 64 (screen-h) 'panel))
  'box (lambda () (values 0 0 64 (screen-h)))   ; what a frame redraws
  'at (lambda (x y) (and (< x 64) 'dock))       ; takes the points there
  'mouse (lambda (kind what x y button clicks) (notify "dock")))
(define-strip dock 'left (lambda () 64))     ; maximized windows beside it
```

A field may name a procedure instead (`'draw 'draw-taskbar`), looked up
at each call, so redefining it at the REPL shows on the next frame.

Decisions worth keeping in mind for the real design:

- **Everything is top-level.** No R6RS libraries on purpose. Every
  definition lives in the interaction environment, so redefining
  `draw-window` or `draw-wallpaper` at the REPL changes the running system
  on the next frame. State that must survive `(reload!)` is declared with
  `defvar`, and record types are `nongenerative`.
- **Folders, not lists.** Apart from the core, a definition is found by its
  folder. Each kind has a definer shaped like `define-app` that registers it by
  name, in a list kept in registration order, and a query that lists them. Kits
  are world code shared by apps: they load at boot, stay top-level and keep a
  name prefix (`ui-`, `edit-`, `complete-`). Code moves out of an app into a
  kit when a second app needs it, not before. R6RS libraries are one-way black
  boxes, like C libraries: the world imports them, they never call into the
  world.
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
- **Settings are theme keys, in layers.** Colors, sizes and fonts all
  live in one table, `(theme key)`, resolved from layers whenever one
  changes: your Overrides (`override!`, in every style), your changes to
  the chosen style (`set-theme!`: switch styles and back, and yours come
  back), the style and its parents, its palette (the setting `palette`),
  and the built-in defaults. A rule is a setting computed from others then,
  never per frame: the built-in `selection` is `(theme-rule (mix 'accent
  'bg-1 0.65))`, so a palette gives twenty colors and the rest follow it, and
  so does your accent. `(theme-source key)` says which layer gives one,
  `(theme-part key)` the procedure a part key gives.
  The Settings app is a front end to these, so anything it does can be
  scripted from the REPL: its Theme tab's style cards are the one
  switch (`set-style!`: frames, palette, wallpaper, fonts, behaviors and
  what you changed for that style at once); each row is one of the style's
  options (the common ones it honors, the behaviors, its own; those it
  does not honor show dimmed), and changing it is your change to this
  style (`set-theme!`; ↺ drops it, `reset-style!`); its pin makes it an
  Override (`override!`); Reset forgets your changes to the style; Save
  as new style writes them as a `define-style` with the style as its
  parent, `(save-style-as! 'my-mac "My Mac")`, into
  `~/.schevana.d/styles/` (`*style-save-directory*`). The saved file is
  plain data: the
  style, the scale, the Overrides and your changes per style; an older,
  flat file loads as the saved style's changes.
- **Logical pixels.** All layout is in logical pixels; the canvas is
  drawn `*scale*` times bigger with a single `cairo-scale`, so text and
  shapes are rendered from outlines at full resolution. The scale follows
  the desktop (SDL's display scale for the window: the pixel ratio on
  macOS and Wayland, `Xft.dpi` under X11, which is how GNOME publishes
  200% and SDL reads once, as it starts), and the display's `scale` (a
  percentage; 0 for automatic, in Settings → Display, saved apart from
  any style's settings) overrides it.
  Mouse positions are converted at the host boundary, so apps never see
  physical pixels.
- **Fonts are settings, and sizes follow them.** A font role (`'ui`,
  `'mono`, `'title`, ...) is a rule that makes the font from the four
  font settings, `(font family size . opts)`, one record per font, so
  every theme asking for it shares its face, its metrics and the text
  laid out in it; a style changes a role by setting it. The kit's shared
  sizes (`control-height`, `gap`, `pad`, ...) are rules from the fonts
  too, and a style may give its own pixels. Title bars and the bars take
  the larger of their own setting and what their text needs
  (`chrome-height`: 0 is none, and a style that fixes one means its
  pixels), as do menus, the apps' bars and the Settings layout, so the
  interface stays usable from 9 to 36 px; the screen's
  scale changing builds the theme again, since fonts measure differently
  there. Where a window is too small, Settings scrolls and stacks its
  rows. A new REPL opens at 80 × 22 characters of the code font.
- **Styles are settings; parts are procedures by name.** A style is a
  name, a title and settings (`define-style`), one of which may be
  `'parent`: a derived style changes what it sets and inherits the rest.
  A part is a setting whose value is a procedure or its name, looked up
  at each call, so redefining that procedure at the REPL shows on the
  next frame. The frame is six parts (`src/theme.ss`): `frame-insets`,
  `frame-regions` (`close`, `minimize`, `maximize`, `depth` buttons and
  `title` areas), `frame-radius`, `frame-bleed`, and two drawing passes
  around the content, `frame-under` and `frame-over`. The window manager
  only does geometry and dispatch. Apps always get their content size,
  whatever the frame. A window is drawn clipped to its extent, so a style
  that draws past its frame (a glow, say) gives how far as its
  `frame-bleed`. A window naming a style (`'window-style`) is drawn and
  hit-tested in that style's theme, content included; `(with-style name
  thunk)` draws in one. `define-part` makes a procedure a part: its body
  becomes the built-in `name*`, and the name calls the style's. Every
  widget's drawing, size and areas in the kit is one, as are the bars'
  (`draw-topbar`, `topbar-areas`: what each bar draws where, and where it
  takes the clicks), the menus' (`menu-areas`, `draw-menu`, and the
  numbers as settings: `menu-item-height`, `menu-font`, ...), the toasts'
  and the popups' pieces (`ui-draw-panel`, `ui-draw-highlight`,
  `ui-draw-separator`, `ui-draw-card`), which menus, toasts, dialogs,
  tooltips, the calendar and the REPL's completion list share. A
  container draws its inside in its place (`with-place`: a text box's
  `field`, a table's `list`, `dialog`, `tooltip`, `menu`, the bars'
  `bar`, `toast`), so a style can color what sits there, an app's own
  cells too, by its `'places`.
  Styles share their drawing too. A stipple, `(stipple ink paper row
  ...)`, is a pattern used wherever a color is (a fill, a line, text, a
  theme key's value), tiled on the screen's device pixels where px's
  rule puts each logical pixel, so it lines up across windows at any
  scale. `text-ink` measures what a string inks as `*cr*`
  draws it (capitals centered on a line), and a font made with `'crisp`
  is not antialiased. `kits/px.ss` draws on whole device pixels at any
  scale, never antialiased: `px-fill`, `px-bevel` (Motif's corners or
  the Mac's), `px-ring`, `px-bits` for small pictures, `px-text`. The
  kit's `ui-draw-glyph` draws a window's buttons (`close`, `minimize`,
  `maximize`, `restore`, `shade`) and the triangles (`up`, `down`,
  `left`, `right`), which the styles share.
  The 1980s Lisp environments: Interlisp, from Medley's window system
  sources (MIT licensed): a border of 2 pixels of ink and 2 of paper, a
  black title strip with the title at x 4, the window's title shade
  after it (`interlisp-title-shade`), Medley's #x8822 desktop, and its
  scroll bars, 24 pixels at the left under the pointer or, with the
  scroll bar side `outside`, beside the window in its frame's margin.
  Genera (Symbolics Lisp Machines), built to the margins its 1986 manuals
  specify, with the window's label in the bottom-left corner, which is
  also where you drag it: a style can list any area as its `'title`. It
  takes the whole screen: no top bar, and at the bottom the who line,
  what the mouse buttons do in reverse video ("Mouse-L: Move; Mouse-R:
  Window menu.") over the selected window, a field listing the windows
  and the status, in character columns; a window not selected is
  grayed over, and buttons are command words. Its numbers are settings
  (`genera-border`, `genera-margin`, `genera-label-font`), so the MIT
  CADR, from MIT's System 99 sources (BSD), is Genera in a few settings:
  a 1-pixel border and margin, the label upright in the code font at
  the top of its strip under the content, and no gray (the CADR grayed
  only covered windows). Each brings the palette of the same name and its
  wallpaper (the `wallpaper` theme key).
  Our own styles choose their feel where it suits them: Fold folds a
  window up to its title on a double click, Lambda rolls it into its tab
  on the wheel, Halo and Reticle show scroll bars under the pointer,
  Draft drags an outline that is a drawing with the live size, Reticle
  drags its brackets, Curses rings the bell on the whole screen and boxes
  menu entries, Interlisp drags Medley's dashed outline, chooses from
  menus on release and flashes the window.
- **How it feels is settings too.** Shared code does what a style
  chooses, each a setting whose default is today's feel: a double click
  on the title (`title-double-click`: `maximize`, `shade` or `none`) or
  on a frame button (`button-double-click`: `((menu . close))` is CDE's);
  a shaded window, only its frame's top and bottom (`shade-window!`,
  `unshade-window!`, `shaded?`), from a region of kind `shade`, the
  title's double click or its wheel (`title-wheel`); dragging live or as
  an outline (`drag-feedback`, drawn by the `draw-drag-outline` part);
  the window manager's resize edges (`frame-edge`, `frame-corner`; a
  style's regions may resize from the top too, `resize-t`, `resize-tl`,
  `resize-tr`); whether a click in a window raises it
  (`raise-on-click`); the bell, `(bell)` (`none`, or `screen` or
  `window` flashed inverted); menus (`menu-choose`: `click` or
  `release`, `menu-blink`, `menu-hover`: `fill` or `outline`); scroll
  bars (`scrollbar-side`: `right`, `left`, or `outside` in a region of
  kind `scrollbar` the frame leaves; `scrollbar-show`: `always` or
  `hover`); and the mouse documentation, `(mouse-doc)`, what each button
  does under the pointer as data (from the window's regions, a kit
  widget's `'doc`, an app's `'mouse-doc`, a surface's `'doc`), which the
  top bar shows where `mouse-doc` is on, and Genera's who line. A top
  bar 0 high is none; a style may make the taskbar another bar with its
  areas, drawing and tick (`taskbar-areas`, `draw-taskbar`,
  `taskbar-tick`).
- **Every frame is drawn from scratch, where something changed.** The
  canvas is the exact screen. A change says where it shows: input to a
  window and `(invalidate! win)` ask for the window's extent (its frame
  and what its style draws past it), and so does
  `(invalidate! win x y w h)`, from when only part of a window was
  redone; the hover for the window or bar it leaves and the one it
  enters; moving or resizing a window for where it was and where it is;
  the clock, once a second, for the top bar; `(invalidate!)` and the rest
  of what the window manager or the desktop changes, the menus and toasts
  included, for the whole screen. The next frame draws the wallpaper,
  every window from the bottom up (each clipped to its extent, so nothing
  it draws lies outside what it asks for), the panels, the menu and the
  toasts again (`render-full-into!` in `src/wm.ss`), clipped to one
  rectangle: the bounding box of what was asked for, grown until it holds
  whole every window and bar it touches (cairo draws what a clip cuts a
  little differently), or the whole screen while a menu or a toast shows.
  A step that nothing asked a frame of draws, uploads and presents
  nothing.
  Nothing is assumed about what a drawing procedure does inside its
  window, so redefining `draw-window`, a style's part or `draw-taskbar` at
  the REPL changes the next frame: an evaluation asks for the whole screen
  when it ends, and so does `(reload!)`. A window that changes by itself
  over time says so with `(invalidate! win)`, as the Clock and Life do
  when they tick; one whose change also shows outside it (its title, in
  the bars) asks for the whole screen. The wallpaper is kept as a picture,
  and it is a picture of its key: the screen's size and scale,
  `draw-wallpaper` and the whole theme, whether changed by `set-theme!` or
  behind its back. The next frame paints it again first when one of those
  changes, and is the whole screen; nothing else does: a `draw-wallpaper`
  that reads the clock shows a still picture.
- **Drawing is single-threaded.** Everything runs on the main thread,
  drawing included. A frame is drawn into a second screen buffer; then
  each row of its rectangle is compared with the screen's, and only the
  rows that changed are copied onto the screen and uploaded, each run of
  them as one full-width rectangle. The other rows need no look: after
  every frame the second buffer equals the screen, and a new canvas asks
  for the whole screen. A frame that changes nothing uploads and
  presents nothing. Monitor shows the last frame's time (drawing it and
  comparing its rows) and the rows it uploaded. A process may be
  switched out at any call while frames go on, so typed in a REPL (or
  called by a handler) `(render-frame!)` only asks for a frame, asking
  for an area is atomic, and freeing the canvas or a cache of surfaces
  runs with interrupts off: no frame sees it half done. Next on the
  compositor plan: every window drawn into an image of its own when it
  changes, and the images composed, on the CPU and then on the GPU;
  threads only when measured.

## Tests

```bash
mkdir -p out
scheme --script tests/smoke.ss out
```

This boots the world headlessly, types into the REPL (completing names: Tab's
pages, its popups, clicks, a small window), defines an app, crashes
windows, runs away in loops, drags and resizes windows, uses the launcher,
presses every button of each window style, clicks and drags through every
Settings tab (checking the theme file round-trips), uses Tasks by mouse and
keys (typing with selection, the clipboard and Tab, the focused field kept in
view, dropdowns, the calendar, tags, the checklist, dragging cards, sorting,
searching, filters, the context menu, the delete dialog, tooltips, the narrow
page, a description's 65,536 characters, its scroll bar, Ctrl+End and
Ctrl+Home, Page Down and Up scrolling as far as the caret moves, typing
that compares no texts, a key typed in another window that runs no scan,
and its editor's window: Save, Cancel and its question, its line and
column, Tasks waiting under a scrim, maximized, Save into a task deleted
meanwhile; what a task waits for and what waits for it in its details,
Add's menu of what makes no cycle, as much of it as fits on the screen
with many tasks (an id typed first), a choice the REPL made a cycle
meanwhile refused with a plain toast, a cycle the REPL refuses, a
blocked task moved to Done or added done; the Tree, its twisties and
keys, a shared blocker's later rows, a later row a change takes away, a
collapse that hides the selection, a search dimming rows or leaving
nothing to show, a keystroke that works out no tree again, a task
deleted there, the keys listed; a reload that changes the task record
and keeps every task) and from the REPL's procedures, reloads the
sources, runs the host's SDL code (uploads, presents, resize events, the
window's size) against a stand-in that replaces every SDL call, so no
window opens, and writes screenshots to the given folder. It never
touches your real `~/.schevana-theme.ss`, and no test reads
`~/.schevana.d`.

```bash
scheme --script tests/units.ss out
```

This tests the drawing layer piece by piece: the measuring context,
text cut to fit measuring no more of a long text than its width could
show, cached shadows against the stacked ones, pixmaps, Life's
generations against a plain reference (a step allocates nothing), its
grid against one rectangle per cell, and the invalidation calls; and,
with no window, the edit kit (insertion, erasing, caret moves, words,
selection, pages, the most characters a text may hold, what an edit
changed, wrapping against a plain rule with even and uneven widths and
against breaking a character at a time, the rows a layout keeps and
takes back through thousands of random edits, a keystroke's measures in
each worst case bounded by the rows in view, an edit's rows past it
taken back, not measured, and a keystroke's work in an engine's ticks
bounded by the rows in view at any length), dates, flow and table keys,
and Tasks' sorting, filtering, moving, a search looked through once for
each change, the editor's line and column against a plain count, the
REPL's checks, the seed's long descriptions, what a task waits for (each
once, a cycle refused and named, whom it blocks worked out, a deleted
task forgotten), the tree of what waits for what with blockers shared,
the seed's links and what Add offers, what is kept until a link or a
status changes, Add's entries for what is typed, ids and later rows'
notes as wide as they fit, a task record from before a reload carried
over, the tree widget's rows (only expanded nodes' children asked for),
keys, indentation and dimmed rows, and the characters a keystroke in
Tasks measures, the same with a title of 100,000 characters as with one
of 10,000.

```bash
scheme --script tests/frame.ss out
```

This tests the renderer: once the frames asked for are drawn, the screen
equals `render-full-into!` drawn directly, bit for bit, on several
desktops, every window style, a menu and a toast, at 100, 125, 150, 200
and 300%; a stand-in texture fed only the uploaded rows always equals
the screen, and a frame that changes nothing uploads and presents
nothing; every way of asking for an area (the clock's second, a window
or part of it, a window under another, hover across windows and both
bars, a window dragged and resized, another moved, a process asking for
areas while the main loop asks for others, one switched out after each
area it asks for) shows that frame at 100, 150 and 200%, the clock's
second comparing and uploading only the top bar's rows, a lone window
only its extent's; every style, and one that glows past its frame with
and without its `'bleed`, moved, resized and hovered, shows the
reference, and drawn unclipped each registered style keeps to its
extent; with a stand-in window a boot, a resize or a rescale shows
its own frame at once, never an empty screen; a process, switched out
anywhere, may free surfaces, rescale and call `(render-frame!)` while
frames go on; drawing that raises crashes only its window, or shows no
frame, and a `window-bleed` that raises is reported while frames and
input go on; redefined `draw-window`, `draw-taskbar` and styles show at
once and cost the same, and a REPL evaluation asks for the whole screen
as it ends; a theme change (by `set-theme!` or behind its back) or a
redefined `draw-wallpaper` paints the wallpaper again for the next
frame; each generation of 30 Life windows shows on the next frame; and
Tasks shows the reference after typing, the caret's blink, hover and a
tooltip, a drag, the wheel, a field scrolled back into view, a menu, the
calendar, the dialog, a change from the REPL and the narrow page, at 100,
150 and 200%, and asks for no frame once its window loses the focus, or
while the dialog covers a field that had the keyboard, and so does its
Tree (hover, twisties, keys, the wheel, a later row, dimmed rows, links,
the Add menu); and a long description's scroll bar, drawn from an
estimate, settles when idle steps finish its rows, and when its editor's
caret row brings them to the end in the same draw.

```bash
scheme --script tests/sources.ss
```

This checks the sources themselves, and quickly: every line of every
source file is at most 80 characters, no top-level name is defined in
two files, `src/` stays within its size budget (raise it on purpose),
every file in a folder registers what it defines, under its own name
(a kit's with its prefix, the desktop's plain), every style sets only
settings that exist, in the kits, the desktop layer, Tasks and Settings
only the drawing procedures (parts, `draw-…`, `ui-draw-…`,
`complete-draw-…`, `tasks-draw-…`, `settings-draw-…`) draw or name a
theme color, the edit kit never does, and only styles call a part's
built-in body.

```bash
scheme --script tests/bench.ss out               # [frames]
```

Times frames on four scenes at 100% and 200%: 30 Life windows tiled,
a typical desktop of five overlapping windows, 30 Life windows cascaded,
and those under a maximized REPL, with every Life window stepping every
frame. For each it prints the main thread's time per frame (all the
work: everything runs on it), the wall time, the bytes uploaded and the
collections, then times a keystroke in the REPL, an idle step (it must
stay under 0.2 ms and upload nothing), a theme change, and the clock's
second with no window and a keystroke in a REPL alone (at 100% and
200%, and on a 1920 × 1080 screen at 200%, a 4K one), Tasks maximized
on that screen (an idle step, a keystroke in a title, two in one step, a
card dragged), a keystroke in a long description's worst cases there,
in the details and in its editor maximized, against the same in an
empty one (the least of three rounds each: it must stay within 1.25
times the empty one's in the details and 1.6 times in the editor, which
draws a screenful of text, and a jump within 2.5 times), the Tree there
with the seed and with 2,000 tasks more in chains (a key, Space and the
wheel, the least of three rounds: within 1.5 times the seed's), and
prints how much of the main thread a busy REPL evaluation gets while 30
Life windows animate at 200%.

## Conventions

- Every line of every source file is at most 80 characters;
  `tests/sources.ss` checks it.
- American spelling: color, gray, center.

## Known gaps and open questions

- Apps all share one environment. REPLs can be private or pure, but apps
  can't yet be namespaced from each other the same way.
- A frame draws everything under its rectangle on the main thread. The
  rectangle is one bounding box (the top bar's clock and a window near
  the bottom make a frame of all between them) and grows to hold whole
  every window it touches, so overlapping windows are drawn again with
  one another, and while a toast or a menu shows every frame is the
  whole screen. Windows whose extent reaches into the top bar, as tiled
  ones do, are drawn again with the clock every second: with 30 tiled
  Life windows, paused, that frame takes about 3.4 ms at 100% and 7.5 ms
  at 200% (0.7 ms cascaded). With 30 animating Life windows a frame takes
  about 4.3 ms at 100% and 8.9 ms at 200%; 30 overlapping windows at 200%
  take about 39 ms a frame, and a keystroke in a REPL among four other
  windows shows about 9 ms later at 200%. A REPL alone shows it 2 ms
  later (2.5 ms on a 4K screen), and the clock's second on an empty 4K
  screen takes 0.1 ms (`tests/bench.ss`). Meanwhile a busy REPL
  evaluation runs only between frames: with 30 Life windows animating
  at 200% it gets about half of what it gets with them paused. Next on
  the compositor plan: each window drawn into an image of its own, only
  when it changes, and the images composed on the CPU, then on the GPU.
- Every theme change paints the wallpaper again: a theme change takes
  about 18 ms at 100% and 67 ms at 200% until its frame is drawn, so
  dragging a color slider in Settings shows about 15 frames a second at
  200%.
- The built-in styles change mostly the windows (Genera and the CADR
  their bars and buttons too, Interlisp its scroll bars), though
  everything is a part a style may replace: each widget's drawing, size
  and areas, the bars, menus, toasts, the popups' panel, highlight,
  separator and card.
  Settings draws with the kit's widgets and only its own pictures with
  its own code (the styles' miniatures, swatches, color tracks); its
  color list and its pane's scrolling are still its own. The
  "stable GUI API" question stays open: keep growing these immediate-mode
  procedures, or describe retained widget trees as data?
- Tasks redraws its whole window for a keystroke: about 9 ms on a 4K
  screen at 200%, maximized (half of it is any maximized window's frame:
  the wallpaper under it, its rows compared). In a description of 65,536
  characters a keystroke costs within a millisecond of that: a text's
  rows are computed only as far as they show, and kept; after an edit,
  those past it are taken back from before it, and the edit says where
  the text changed, so no keystroke looks at the whole text (the text
  and its rows' starts are still copied, by memcpy: about 0.02 ms at 64k
  characters). Only rows that do change are measured again, all those in
  view when an edit moves every row's break (a paragraph with no spaces,
  or rows exactly full). A jump far ahead (Ctrl+End, the scroll bar's
  thumb, a paste of the whole text) computes the rows to there once, 5
  to 8 ms; idle steps compute the rest for the scroll bar, a millisecond
  at a time. The description's editor, maximized, draws Tasks under it
  too, and a screenful of text: about 4 ms more a keystroke. With a
  search, Tasks looks through a task's texts only when they change:
  typing in a long description searches that one again at each keystroke
  (0.2 ms a word at 64k).
- Tasks keeps what it works out from which tasks there are, their
  order, statuses and links (the board order, whom each blocks, the
  Tree's shape and rows) until one of those changes, so typing costs
  nothing for them; it still passes over every task at each draw for
  its filters, the toolbar's names and tags, and the List's and Tree's
  assignee column, which measures every name, and the Board lays out
  every card of a column: with 2,000 tasks more in chains of 50, a
  keystroke in the List or the Tree costs within a millisecond of what
  it costs with the seed, in the Board about 6 ms more (with 8,000, 2
  and 18 ms more). Text cut to fit (a title, a tooltip, a menu entry)
  measures no more of it than its room could show. The details' Add
  shows the best matches that fit on the screen, then how many more:
  typing narrows them.
- The description's editor asks before Escape or Cancel drops changes,
  but its window's close button drops them without asking: the window
  manager closes a window at once and cannot ask it first.
- REPL lacks selection and copy, and there is no text editor app yet:
  the edit kit and Tasks' description editor are its start (undo, find
  and files come next).
- No persistence. Windows and state vanish on exit (`fasl-write` of app
  state is a natural first step).
- Portability: library names for macOS and Windows are in `ffi.ss` but have
  not been tried. macOS will need the loop on the main thread, which it
  already is.
