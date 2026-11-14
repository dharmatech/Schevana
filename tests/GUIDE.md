# Tests

What the suites are, how to run them and how to write a check. What a
change must pass before it's committed is in [AGENTS.md](../AGENTS.md).
A test that passes here and fails on another machine, Chez or font is a
broken test, even when this machine is the one it was written on.

## The suites

From the tree's root:

```bash
mkdir -p out
scheme --script tests/smoke.ss out
scheme --script tests/units.ss out
scheme --script tests/frame.ss out
scheme --script tests/sources.ss
```

- **smoke** boots the world headless and uses it as a person would: it
  types into the REPL, drives every app by mouse and keys, presses every
  button of each style, asks an app's permissions, crashes windows and
  reloads the sources. It writes screenshots to `out/`.
- **units** runs every file of `tests/units/`, a topic each (an app's, a
  kit's, a service's, a part of the core's), in a process of its own, so
  that none leans on what another left behind. `scheme --script
  tests/units.ss out life boids` runs those two alone.
- **frame** draws frames on lavapipe, or on Metal on a Mac, with no window
  but one offscreen. It compares the screen bit for bit with the world
  drawn into an image, in every style and at several scales, and checks
  that every change shows on its frame and that nothing asked for draws
  no frame.
- **sources** checks the sources themselves: 80 columns, names, the
  layers and what crosses them, the core's budget, who may name a color.
  It runs in seconds.

They run headless and never open a real window. They need `spirv-val`
(`sudo apt install spirv-tools`, `brew install spirv-tools`): the
shaders' topic writes every shader as SPIR-V, on a Mac too, and checks it,
since a module lavapipe takes may still be one another driver rejects. On
Linux they need lavapipe (`mesa-vulkan-drivers`). On a Mac type `chez` for
`scheme`, and on Apple silicon put
`DYLD_FALLBACK_LIBRARY_PATH=/opt/homebrew/lib` before it; the tests run on
Metal there, with no Dock icon and no window shown.

## Running less while working

The whole suites take minutes; one check takes seconds. While a check is
being written or fixed, run what it needs, and the whole suites, under
both Chez versions, before the commit (`AGENTS.md`).

- `tests/sources.ss` alone takes seconds, and a units topic runs alone:
  `scheme --script tests/units.ss out tasks`.
- `SCHEVANA_CHECK=text` stops any suite after the first check whose name has the
  text in it (case aside). Everything before it runs as in a whole run, so
  the check sees what it would there. A run fails when no check's name has
  the text; in units, name the topic too.

```bash
SCHEVANA_CHECK="a card's tags" scheme --script tests/units.ss out tasks
SCHEVANA_CHECK="Shift+Tab up onto a title" scheme --script tests/smoke.ss out
```

## A check

A topic is a file in `tests/units/`, loaded into a world booted headless.
`tests/units.ss` gives it `check`, which runs an expression and reports
it, and helpers more than one topic uses: `(steps n)` runs the world's
loop `n` times, `(raises? thunk)` says whether a call raises, and
`shown-picture`, `image` and `max-diff` compare what was drawn.

```scheme
(check "a window opens where it's asked"
       (let ([w (open-window "Here" 'pos '(40 . 60))])
         (steps 1)
         (let ([ok (and (= (window-x w) 40) (= (window-y w) 60))])
           (close-window! w)
           ok)))
```


- A change in behavior comes with a test of it.
- A unit check goes in its topic's file in `tests/units/` (an app's, a
  kit's, a service's, a part of the core's), which `tests/units.ss` runs in
  a process of its own: it leans on no other topic, and what several use is
  in `tests/units.ss`. `scheme --script tests/units.ss out life` runs one.
- A check's name says what is true when it passes, as a sentence.
- What can fail goes inside a check. A form outside one that raises fails
  the suite at its line, and the suite goes on, but what the form left
  undone fails the checks after it.

## What a test may touch

- Tests and scripts never touch the user's `~/.schevana-theme.ss`,
  `~/.schevana.ss` or `~/.schevana.d`. Point `*theme-file-path*` and
  `*grant-file-path*` at scratch files, as the suites do, or run with
  `HOME` set to a scratch folder.
- Nothing opens a real window: the suites run headless, on lavapipe, the
  Vulkan driver on the CPU (`mesa-vulkan-drivers`), or on Metal on a Mac.
- A file a test writes goes in the suite's output folder (`out/`), named
  relative to it, never at an absolute path or in `/tmp`.

## Every run alike

- No flaky tests. A check that fails once and passes when run again is a
  bug: find what varies, and make the check measure only what it says. It
  is fixed at its cause or removed, never run again until it passes, and a
  limit raised says what it now means.
- What varies is fixed: a seed for what's random (`random-seed`), the fake
  clock for what's timed, a size given for each window a check measures.

## Both Chez versions

- Every check holds under Chez Scheme 10.0.0 and 10.4.1, the two
  `AGENTS.md` asks for. They count differently: from 10.4.0 a copy counts
  against an engine's ticks by its size, up to a timer period.
- A bound on what a version counts (engine ticks, bytes allocated) has room
  for both, and its comment says what each counts. It doesn't test the
  version.
- A suite runs under the `scheme` first on `PATH`, and `tests/units.ss`
  runs its topics by `chez` if `PATH` has one, else by `scheme`. To run
  them under the other version, build it from its release tag and put its
  `bin` first on `PATH`.

## Any font

The world sizes what holds text from the font's metrics, and the fonts
differ by system: Debian and Ubuntu put Noto Sans first for `sans-serif`
once it's installed, a Mac has Lucida Grande, and a person picks another
in Settings. A check holds with any of them.

- No size measured from a font is written as a number. An expected size is
  the rule the code follows, in the font's terms: `(font-line-height
  'mono)`, `(text-width "+2" 'ui-small)`, a style's floor and its line.
- A scene makes room for what it shows: it sizes a window by the lines it
  holds, or brings a target into view before it clicks it, so a taller
  font moves the target, not whether it's there.
- Text that must wrap, or must not, is made so by the width it's given,
  measured, not by a length that wraps in one font.
- Ink is compared with ink: where a check lines up glyphs, it measures the
  font's own (its side bearings differ by family), not a margin in pixels.
- When in doubt, run the test under a few fonts, DejaVu and Noto at least
  (`sudo apt install fonts-dejavu-core fonts-noto-core`). A fontconfig
  file that puts a family first for the generic names chooses it for one
  run, whatever the system prefers; this one is Noto's, and DejaVu's is
  the same with `DejaVu Sans` and `DejaVu Sans Mono`:

```bash
FONTCONFIG_FILE=$PWD/out/noto.conf scheme --script tests/units.ss out tasks
```

```xml
<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">
<fontconfig>
  <include ignore_missing="no">/etc/fonts/fonts.conf</include>
  <match target="pattern">
    <test name="family"><string>sans-serif</string></test>
    <edit name="family" mode="prepend_first" binding="strong">
      <string>Noto Sans</string>
    </edit>
  </match>
  <match target="pattern">
    <test name="family"><string>monospace</string></test>
    <edit name="family" mode="prepend_first" binding="strong">
      <string>Noto Sans Mono</string>
    </edit>
  </match>
</fontconfig>
```

## Any platform

A stack differs by OS, windowing system, compositor, SDL, GPU API, driver,
monitor, CPU and Chez (`AGENTS.md`, "Layers"). A check holds on each.

- A check asks the platform as the world does, by its predicates and facts
  (`platform-mac?`, `platform-vulkan?`, `platform-cpu-count`), never by
  `machine-type` or a path that exists on one OS.
- A check of what one platform has (Metal's exact floats, Wayland's
  clock) says it was skipped elsewhere, in its output. A tool the checks
  need is required, never skipped: a check doesn't pass by checking
  nothing.
- What can be checked for another platform is checked everywhere: a Mac
  writes and validates the SPIR-V Linux runs.
- What's drawn is compared with what the same machine draws, as the frame
  suite does, never with pixels stored from one rasterizer.
- A bound in time is for the bench (`tests/bench.ss`), not a suite: a
  check's limit in milliseconds holds on a slow machine under load, and
  says how far it is from what it measures.
- Nothing depends on the monitor (its refresh, its scale, its size), the
  locale, the time zone, or a variable of the environment the suites don't
  set themselves.

## The docs' examples

The `docs` topic runs every `scheme` block of README.md, CREDITS.md and
`docs/*.md`, in order, as typed at a REPL at "/", each form in a process
as a REPL runs it, and each page in a world of its own. A comment `; =>
value` after a form, on its line or the next, is checked as what the form
returns, and an error, in a form, in what it left to run later or in a
window's handler, fails its block. A block fenced `scheme no-run` is shown
and not run: one that never ends, waits for a person, or needs what only a
test has.

## Timing

```bash
scheme --script tests/bench.ss out               # [frames]
```

The bench times frames on four scenes at 100% and 200%: 30 Life windows
tiled, a desktop of five overlapping windows, 30 Life windows cascaded,
and those under a maximized REPL. Then it times a keystroke in the REPL,
an idle step, a theme change, Tasks maximized on a 4K screen, what
profiling costs, and the crew with Life and Boids. Some of these have
bounds it checks: an idle step stays under 0.2 ms and uploads nothing.
