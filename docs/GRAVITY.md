# Gravity: what each level of Schevana takes away

Schevana has levels, each giving up some of Scheme for something in
return:

| level | runs in | gives up | for |
|---|---|---|---|
| L0 | a plain R6RS program and a library | nothing | |
| L1 | the world: a window's handlers | the main loop, unbounded calls, `display` | windows, live redefinition |
| L2 | a sandbox (`app-requests`) | `library` and `import`, most of Chez | safety: only what the person approved |
| L3 | the crew (worklets) | records, the app's names, closures, allocation | speed: every core |
| L4 | the GPU (shaders) | all but 32-bit numbers in buffers | speed: the GPU |

Gravity is a small sim written once per level, each version the smallest
change from the one before, so that the diff between two adjacent
versions is the lesson: what that level asks, and what it gives back in
speed, safety or memory. Boids went up the same levels first
(`docs/BOIDS-CLEAR.md` on the branch `2026-11-14-boids-clear` of
dharmatech/Schevana), but has too many moving parts to teach from: five
rules, a wrap, a field of view. Gravity is Boids with one rule. Every
body is pulled by all the others, so a step reads one copy of the bodies
and writes another, state carries over from frame to frame, and the work
grows with the square of the count.

A version is named "Gravity L<n> (<way>)": its level, and how its step
is written, as Boids' are (simple: lists, records, a procedure for each
part; flat: numbers side by side in one loop; clear: flat, written over
with names by macros). L0, L1 and L2 are simple. This page is also the
plan for L3 and L4, which later work builds from it and the code alone.

## Running them

From the tree's root:

```
scheme --libdirs lib --program lib/gravity-l0-simple.sps
scheme --libdirs lib --program lib/gravity-l0-simple.sps 30 300
scheme --script tests/units.ss out gravity
```

The first is L0, outside Schevana: 200 bodies, 2,000 steps, a line every
100 (4 s). The second is 30 bodies for 300 steps. The third is the test
topic, `tests/units/gravity.ss`, which runs L0 the same way and holds L1
and L2 to it.

L2 is in the launcher, "Gravity L2 (simple)". L1 loads into a world
namespace a person makes: in Environments, Fork… a world namespace, then
Load… `lib/gravity-l1-simple.ss` into it; or at a REPL at "/":

```scheme
(define gravity-lab (env-fork (env-root) 'world 'name 'gravity))
(env-load gravity-lab (source-path "lib/gravity-l1-simple.ss"))
(app-launch! 'gravity-l1-simple)
(window-title (focused-window))            ; => "Gravity L1 (simple)"
(top-level-bound? 'step (env-environment gravity-lab))   ; => #t
(top-level-bound? 'step *world-env*)       ; => #f
```

## The sim

`lib/gravity.sls`, the library `(gravity)`:

- A **body** is a record: its place and velocity, complex numbers x+yi,
  and its mass.
- `(ring n)` is n bodies on the unit circle, each moving along it at
  speed 0.5, a list. The ring's mass, 0.5, is shared among them, so it
  turns the same however many there are.
- `(gravity d)` is the pull toward a body of unit mass d away, softened
  so that a close pass stays finite: d / s^(3/2), s = |d|² + 0.01.
- `(step bodies dt pull)` pulls every body by all the others under the
  law `pull`, a procedure: for each body, the sum over the others of
  their mass times `(pull d)`, then velocity, then place (symplectic
  Euler). It makes a new list of new bodies.
- `(energy bodies)` is the total energy: each body's ½mv², and each
  pair's −m₁m₂/√(|d|² + 0.01), the potential whose slope is `gravity`.

L0's program, L1 and L2 take 200 bodies and `dt` 0.01. The ring then
breathes, its radius between 0.9 and 1, while it turns; after about
1,300 steps, a turn, it breaks into clumps, and some bodies are flung
out. The energy starts at −0.10552130811277936 and stays within 2.4e-4
of it, relatively, for the first 1,200 steps, while the ring holds, and
within 1.5e-3 to step 2,000 (checked at every step). Symplectic Euler
keeps it close, not exact: what it wanders by in doubles is the step's,
not the rounding's. Each version keeps the line L0 prints every
100 steps, `step 100  energy -0.10553332621382891`, to compare by.

A few things are written so that versions without complex numbers can
do the same arithmetic, and match to the last digit:

- `magnitude-squared` sums the parts' squares. `(magnitude d)` squared,
  as first sketched, rounds differently 17% of the time (Chez's
  magnitude scales against overflow), which a version in flonums could
  only match by copying Chez's algorithm.
- The rest is part by part already: in Chez, a complex number plus,
  minus, times or over a real one, and times `+0.5i`, is the same as
  the parts each (checked on 200,000 random pairs).
- The sums run in the list's order, which `ring` builds from the last
  angle down: body k = n−1 first.

## Where the files are, and why

| file | version |
|---|---|
| `lib/gravity.sls` | the sim, L0's library |
| `lib/gravity-l0-simple.sps` | Gravity L0 (simple), the program |
| `lib/gravity-l1-simple.ss` | Gravity L1 (simple), for a world namespace |
| `apps/gravity-l2-simple.ss` | Gravity L2 (simple), in the launcher |
| `tests/units/gravity.ss` | the three, checked against each other |

**L0 is in `lib/`.** `tests/sources.ss` holds the folders the world loads
(`kits`, `services`, `desktop`, `styles`, `palettes`, `wallpapers`,
`apps`) to their rules: every file there registers what it defines, a
kit's names carry its prefix, a file in `apps/` defines the app named
after it, and each is loaded into "/" at every boot. A plain library in
`apps/` defines no app; in `kits/` it would load into the world. `lib/`
is the tree's folder of R6RS libraries: nothing in it loads at boot, and
`import` finds what is there, since the world adds it to
`library-directories` (`load-world!`, `src/core1/system.ss`). So the
world can `(import (gravity))` with no path set, and plain Chez can with
`--libdirs lib`. `tests/sources.ss` now holds `lib/`'s files to the 80
columns, the only rule of its that reaches them.

**L1 is in a world namespace, and its file beside the library.** The ways
into the world, and why the others don't fit `AGENTS.md`:

- A tree app without `app-requests` loads into "/", but that is for apps
  that tune the world (Settings, Tasks). Its names would be the world's,
  each needing a `gravity-` prefix, and its `(import (gravity))` would
  put `ring` and `step` into "/".
- A world namespace made by the tree's own code breaks S6: a world
  namespace is one a person asks for, at a REPL or in Environments.
- A world namespace the person makes is S6's own case. It is all of Chez
  and every name of the world's, so the library is still imported, and
  its names and L1's stay there (the test checks "/" has none of them).
  Its `define-app` puts "Gravity L1 (simple)" in the launcher for the
  session, as an app defined at a REPL is.

So `lib/gravity-l1-simple.ss` isn't loaded at boot, and isn't an app of
the tree's: it waits for a person, or the test, to load it.

**L2 is a tree app in a sandbox**, `apps/gravity-l2-simple.ss`, approved
by its row in `grant-tree-apps` (`services/grant.ss`):
`(gravity-l2-simple window draw)`.

## What each level takes away

Each feature of L0's is there on purpose, because some level takes it
away:

| L0 has | taken away at | what replaces it |
|---|---|---|
| `library` and `import` | L2 | the library's body as top-level forms |
| its own loop | L1 | a `'frame` handler, which asks for the next frame |
| `display` | L1 | `'draw`, drawing records: `fill-circle`, `draw-text` |
| as many steps as it likes in one call | L1 | a step a frame, within a handler's fuel |
| a record | L3 | a vector or pairs (simple), slots of an flvector (flat), floats in GPU buffers (L4) |
| the force law `pull`, a procedure | L3 narrows it, L4 takes it | a worklet handed in (L3); a local function in the shader, inlined (L4) |
| `map`, and complex numbers that allocate | L3 flat, L4 | one loop over pairs in unboxed flonums, writing a second copy |
| complex numbers and the numeric tower | L4 (L3 flat by choice) | x and y as 32-bit floats; whole numbers are 32 bits |
| a loop as long as the list | L1 bounds it, L4 caps it | the fuel and the frame (L1, L2); the crew's 100 ms a call (L3); `do-bounded`, its cap written in the shader (L4) |
| `eq?` on bodies | L3 flat, L4 | an index: `(fx= j i)` |

And what each gives back:

- **L1**: windows, a picture instead of lines, the REPL and live
  redefinition, the world's crash handling (a runaway step crashes its
  window, not the desktop).
- **L2**: safety. The app holds the window and draw bundles alone, no
  files, no `eval`, no clock, no C, and the person (or the table, for a
  tree app) approves even those.
- **L3**: every core, and memory: flat allocates nothing.
- **L4**: the GPU: thousands of bodies a frame.

## The versions

### Gravity L0 (simple)

`lib/gravity-l0-simple.sps` imports `(rnrs)` and `(gravity)`, nothing of
Chez's own, and runs under plain `scheme --program`. It owns its loop
and its time: every step in one call, a line every 100 steps.

```scheme no-run
(define (run n steps)
  (let loop ([bodies (ring n)] [k 0])
    (when (= (mod k 100) 0)
      (report k bodies))
    (when (< k steps)
      (loop (step bodies dt gravity) (+ k 1)))))
```

### Gravity L1 (simple): the world owns the loop

The library is the same, imported. The program's loop becomes a window:

- The loop's two variables, the bodies and k, move into the window
  (`window-ref`, `window-set!`), and its body into `'frame`, which takes
  one step and asks for the next frame (`window-frame-soon!`).
- `report` makes the line instead of printing it, and `'draw` draws it,
  over the bodies: a dot each, `fill-circle`, the unit circle filling
  most of the window.
- `define-app` makes it an app, which the launcher opens.

```scheme no-run
'frame (lambda (win t)
         (window-frame-soon! win)
         (let ([bodies (step (window-ref win 'bodies) dt gravity)]
               [k (+ (window-ref win 'steps) 1)])
           (window-set! win 'bodies bodies)
           (window-set! win 'steps k)
           (when (= (mod k 100) 0)
             (window-set! win 'line (report k bodies)))))
```

A handler's call has a budget, 50M engine ticks (`handler-fuel`), and one
that runs out is stopped as a runaway: its window crashes and shows the
error. So a frame takes one step, not the 2,000 L0 takes in one call. A
step of 200 bodies takes 0.14M ticks; the fuel would stop one at about
3,800 bodies, long after the frame has: 2,000 bodies already take 200 ms
a step. L1 keeps L0's 200, about 2 ms a step.

### Gravity L2 (simple): a sandbox

`apps/gravity-l2-simple.ss` is L1 with what the sandbox forces:

- `(app-requests window draw)`, its first form: `open-window`,
  `window-frame-soon!`, `window-ref` and `window-set!`; `fill-box`,
  `fill-circle` and `draw-text`. No timers: it reads no clock, since
  `'frame` is handed its time and it steps once a frame anyway.
- No `library`, `export` or `import` in a sandbox, so the library's body
  is in the file, as top-level forms. The library's body isn't indented,
  so the two read the same, line for line.
- The record is `nongenerative`. At a reload the app's file loads again
  into its sandbox: a generative record type would be a new type, and
  the window's bodies, made before, would no longer be bodies, so the
  next step would raise and crash the window (the test checks this, and
  fails without it). L1 has no such problem: a reload leaves a
  namespace's definitions, and a library, alone.

The diff of the two L1 files against L2's, past their header comments,
is just that: `app-requests` in, the library form's lines and the
`import` out, `nongenerative` in, and the app's new name.

```
diff <(cat lib/gravity.sls lib/gravity-l1-simple.ss) apps/gravity-l2-simple.ss
```

Everything else L2 gives up, it didn't use: files, `eval`, threads,
`syntax-case`, `record-writer`, the world's other names.

### What they cost

Times are ms a step on one machine (Ubuntu 26.04, 16 threads, Chez
10.4.1); a step makes the same garbage at every level here.

| bodies | L0 | L1 | L2 | made a step | ticks, L1 | L2 |
|---|---|---|---|---|---|---|
| 100 | 0.6 | 0.5 | 0.55 | 2.2 MB | 0.04M | 0.05M |
| 200 | 2.4 | 1.9 | 2.1 | 9.0 MB | 0.14M | 0.18M |
| 400 | 9.1 | 7.7 | 8.3 | 36 MB | 0.56M | 0.72M |
| 1,000 | 55 | 50 | 53 | 224 MB | 3.5M | 4.5M |
| 2,000 | | 200 | 214 | 896 MB | 14M | 18M |

- **The square of the count.** Twice the bodies, four times the time
  and the garbage: every pair makes a few boxed complex numbers.
- **L2's extra ticks are the top level's**, not the sandbox's: the same
  forms at the top level of a world namespace take L2's 4.5M at 1,000.
  A library is compiled whole, so Chez can inline `body-place` and
  `magnitude-squared` into `step`; at the top level, where any name may
  be redefined live, each stays a call.
- **The energy costs half a step**: 1.3 ms at 200, so L1 and L2 work it
  out every 100 steps, when they make the line.

## How the versions are compared

The line every 100 steps, `step k  energy e`, is what each version shows
or prints. Chez writes a flonum in the fewest digits that read back as
it, so two lines are equal exactly when the energies are, bit for bit.

`tests/units/gravity.ss` checks:

- L0 runs under plain Chez, from the tree's root, a program importing
  `(rnrs)` and `(gravity)` alone, and prints its lines.
- L1 imports the library in a world namespace, where its names and the
  library's stay, and the launcher opens it.
- L2 runs in its sandbox with the window and draw bundles alone, as the
  table approves, its names there and not in "/"; and the sandbox
  holds: `import`, `library`, files, `eval`, `record-writer` and the
  clock all raise there.
- L0, L1 and L2 agree to the last digit: after 100, 200, 300 ticks both
  windows draw the lines L0 printed for 200 bodies, and hold the same
  bodies; L2 draws L1's picture, pixel for pixel.
- A tick takes a step in each window, well within a handler's fuel.
- `(reload!)` keeps both going: L2's file loads again, its bodies are
  still bodies, and both reach L0's line for step 400.

The topic takes about 20 s.

## The grid

| | simple | flat | clear |
|---|---|---|---|
| L0, R6RS | Gravity L0 (simple) | | |
| L1, the world | Gravity L1 (simple) | | |
| L2, a sandbox | Gravity L2 (simple) | | |
| L3, the crew | planned | planned | later |
| L4, the GPU | none can be | planned | |

The simple, flat and clear axis is shown at L3 alone, where it is the
lesson: the crew can't open a record and pays for every allocation, so
the level itself pushes toward flat. Below it a flat version would only
be faster (Boids' was 9 to 12 times, on one thread), with nothing new to
say about its level. A shader has no lists or procedures as values, so
L4 has no simple version.

## The plan for L3: the crew

Two apps, each a copy of L2 with what the crew forces, in sandboxes
requesting `(app-requests window draw crew)`, each with its row in
`grant-tree-apps`, its line in `docs/APPS.md`'s `(grant-apps)`, and its
checks in `tests/units/gravity.ss` or a topic of its own. Read
`docs/THREADS.md`, and `apps/boids-l3-simple.ss` and `apps/boids.ss`'s
`boids-steer!` on the Boids branch (`git show
2026-11-14-boids-clear:apps/boids-l3-simple.ss`).

What a worklet is (`src/core1/proc.ss`, checked for Gravity):

- It is compiled in an environment of its own: R6RS's base (complex
  numbers too: `make-polar`, `real-part`, `+0.5i`), control, lists,
  fixnums, flonums, bitwise operations, bytevectors and exceptions;
  fxvectors, flvectors, boxes, `now-ns`, and the other worklets. Not the
  app's names, macros or records, and no `define-record-type`.
- What it is handed it may open if it is pairs, vectors or boxes, not
  records. It may be handed another worklet, as a procedure to call,
  but not a lambda: "the crew runs worklets only".
- `(crew-for* chunk start end worklet arg ...)` calls it on slices of a
  range, each at least `chunk` long, the caller among the workers. A call
  writes only its own slice, should take 20 to 200 µs, and on core1 is
  cancelled after 100 ms, which raises and crashes the window. A
  collection stops every thread, so a call should allocate nothing.

### Gravity L3 (simple)

`apps/gravity-l3-simple.ss`: L2's sim, its step on the crew.

- A body becomes a vector, `#(place velocity mass)`, and `body-place`,
  `body-velocity` and `body-mass` become worklets that open it, so the
  step reads as before. `make-body` is `vector`.
- `gravity`, `magnitude-squared` and every procedure the step calls
  become worklets (`define-worklet` for `define`), defined before those
  that call them. `softening` becomes a worklet returning it, or the
  number written where it is used.
- `pull` stays an argument: `gravity` is a worklet, and the crew takes
  it.
- The step hands the crew a vector of the bodies and a vector to write,
  and a worklet, `step-slice!`, works out each body's next one as L0's
  lambda does: its `fold-left` over the others, its `eq?`, its complex
  numbers. The window keeps the vector, or a list made from it.
- `energy` can stay on core1, a plain procedure over the vectors: it is
  the gauge, not the work.

It allocates as L2 does, 9 MB a step at 200 bodies, so each collection
stops the crew. Boids L3 (simple) did so too, and still gained about 5
times on the crew of 8: expect the same, and measure it.

Its checks: the same lines as L0's, to the last digit, stepped on the
crew and on the caller alone (`crew-size-set!` 0); the sandbox holding
`crew` too; ticks; a reload; and a step at its count within the crew's
100 ms.

### Gravity L3 (flat)

`apps/gravity-l3-flat.ss`: the bodies as numbers side by side, one
worklet over every pair.

- The bodies are an flvector: x, y, vx, vy and mass for each, or x, y,
  vx, vy with the masses in an flvector of their own. A step reads one
  flvector and writes another, then the two swap: the read copy and the
  written one, as L0's `map` has them.
- One worklet, `crew-for*` over the bodies' indices, loops over every j
  for body i in unboxed flonums: no record, no complex number, no
  `map`, nothing allocated. `eq?` becomes `(fx= j i)`.
- The law is written in the loop, or a worklet of flonums. Chez keeps a
  flonum unboxed only where it can see it through: one returned from a
  call it doesn't inline, or two returned as values, may be boxed. Check
  that a step makes no garbage (`bytes-total` in `tests/units.ss`).
- To match L0 to the last digit, do L0's arithmetic in L0's order. For
  body i, over j ≠ i in the list's order: dx = x_j − x_i, dy = y_j −
  y_i; s = (dx·dx + dy·dy) + 0.01; k = s·√s; then ax += m_j·(dx/k) and
  ay += m_j·(dy/k). Then vx' = vx + dt·ax, x' = x + dt·vx', and the same
  for y. The ring's numbers come from L0's `ring`, or the same way:
  θ = 6.283185307179586 × (k/n), x = cos θ, y = sin θ, vx = −0.5·y,
  vy = 0.5·x, mass 0.5/n, from k = n−1 down to 0.
- L0's sums start at exact 0, the flat ones at 0.0: the same, but that
  a −0.0 becomes 0.0. Compare by the printed line, or by `=`.

Expect most of Boids' gain: Boids was 9 to 12 times as fast as Boids
(simple) on one thread, and 5 to 6 times more on the crew of 8. Measure
the count each can step in a frame, and set its cap there.

A clear version, flat written over with names by macros as Boids (clear)
is, would come after these.

## The plan for L4: the GPU

`apps/gravity-l4-flat.ss`, `(app-requests window draw gpu)`: L3 (flat)'s
step as a compute shader. Read `docs/SHADERS.md`, and
`apps/boids-l4-flat.ss` with its section in `docs/BOIDS-CLEAR.md` on the
Boids branch. What Boids L4 (flat) found, and Gravity should expect:

- **32-bit floats, in buffers.** The bodies are `fl-buffer`s, 4 floats
  a body (x, y, vx, vy), and the masses one of their own, set once. A
  step reads one buffer and writes the other: a buffer is read or
  written in a run, never both. `gpu-buffer-set!` rounds each double to
  a float as it goes up.
- **A loop has a cap, written in the shader.** `(do-bounded most
  ([j 0 (fx+ j 1)] [ax 0.0 ...] [ay 0.0 ...]) ((fx= j n) ...))`, with
  `most` a number in the code: the most bodies. A `do-bounded` has no
  body, so the sums are its variables, and each step does the pair's
  work, through local functions (`dx`, `dy`, `k`), which are inlined;
  j = i adds 0.0. Restate the cap in Scheme for the window, and have a
  test hold the two together.
- **No `flmod`.** Gravity has no wrap, so it doesn't miss it; the shader
  has `fl+`, `fl*`, `fl/` and `flsqrt`, all the law needs.
- **No waiting.** A handler can't wait for the GPU, and the sandbox's
  `gpu` bundle has no `gpu-compute!` or `gpu-buffer-read!`. Each frame
  `gpu-compute-soon!` sends a step, and `gpu-buffer-read-then!` asks for
  the buffer back, a step or two later, as bytes: 4 a word, read with
  `bytevector-ieee-single-native-ref`. L4 can draw from the places read
  back that way, with `fill-circle`, a frame late; a paint, as Boids L4
  (flat) draws, is the faster way and much more code. A buffer let go of
  meanwhile reads `#f`.
- **The energy on the CPU.** Work it out from the bodies read back,
  floats made doubles, by L0's arithmetic, every 100 steps. The step
  count is the CPU's: the GPU tells nothing of how many it ran, or how
  long one took.

The comparison is the point of L4:

- A step can't match L0's to the last digit. Hold one step to L0's
  within a tolerance, from the same float inputs, as Boids L4 (flat)
  held its own (1e-3 there, where floats came within 7.5e-5): measure,
  then choose.
- The energy should wander more: compare its drift over the first 1,200
  steps, before the ring breaks, with L0's 2.4e-4. And the ring should
  break sooner, since rounding is what starts the break; that it does,
  and by how much, is worth showing.
- After the break the bodies' paths are chaotic, so places can't be
  compared there at all: only the energy can.
- The twin, `(twin-procedure shader)`, runs the shader in Scheme as the
  language means it; hold the device to it within a tolerance, as
  Boids 3D and Boids L4 (flat) do, and use it to try the shader at a
  REPL.
- The suites run on lavapipe, the CPU's Vulkan. To time the real GPU
  headless, as Boids' bench does: `(core0-window-init! "offscreen")`
  then `(gpu-open! 'gpu)`, before `schevana-boot!` with `'headless`.
  SDL's GPU API has no timestamps: send k steps at once and time a read
  that waits for them, in a process.

The ring's mass is shared among its bodies, so a ring of 8,192 turns
and breathes as one of 200 does, only smoother: L4 can show the same
ring at its own count. Find the count a frame's work fits on this
machine's GPU, as Boids L4 (flat) found 8,192, and make that the cap.
