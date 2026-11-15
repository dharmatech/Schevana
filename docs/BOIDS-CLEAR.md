# Boids (clear): a fast step that reads like the original

Boids' step (`boids-steer!` in `apps/boids.ss`) is fast because it is
flat: a flock is an flvector of four numbers a boid, and one worklet
loops over every pair with its flonums unboxed. The first port read
differently, with a `vec` and a `boid` record and a procedure per rule
(`git show a24fe98:apps/boids.ss`); `apps/boids-simple.ss` ("Boids
(simple)") keeps it, running. The question here is how much of that
clarity can come back without losing the speed, and without changing a
number. `apps/boids-clear.ss` ("Boids (clear)") is the same app with its
step rewritten; `apps/boids.ss` stays as it is.

Two things keep each attempt honest:

- `tests/units/boids-clear.ss` checks every step against the reference
  flock in `tests/units.ss` to the last digit, and against Boids' own
  steps number for number (2,000 boids, 30 frames, the heaviest rules
  too). `tests/units/boids-simple.ss` and `tests/units/boids-l3-simple.ss`
  do the same for Boids (simple) and Boids L3 (simple), on smaller
  flocks.
- `tests/bench-boids-clear.ss` times both on the same flocks, in
  alternating rounds, and prints the medians and their ratio. Boids
  timed against itself gives 1.00. The grid's rows come last: Boids
  (simple), Boids L3 (simple) and Boids, alone and on the crew.

```
scheme --script tests/units.ss out boids-clear
scheme --script tests/units.ss out boids-simple
scheme --script tests/units.ss out boids-l3-simple
scheme --script tests/bench-boids-clear.ss out
```

Times below are from one machine (Ubuntu 26.04, 16 threads, Chez
10.4.1); the ratios are what carry over.

## The baseline: Boids (simple)

**What.** The first port's sim, as written, with its comments: the
plain version, which shows what a step means. A flock is a list of
`boid` records, each a position and a velocity, both `vec` records. A
rule is a procedure over a list of neighbors:

```scheme no-run
;; The boids in flock that b can see closer than r, not counting itself.
(define (boid-neighbors b flock r fov)
  (let ([r2 (* r r)])
    (filter (lambda (other)
              (let ([d2 (vec-dist2 (boid-position b) (boid-position other))])
                (and (> d2 0.0) (< d2 r2) (boid-sees? b other fov))))
            flock)))

;; Toward the center of the neighbors.
(define (boid-cohere b others)
  (if (null? others)
      vec-zero
      (vec- (vec-mean (map boid-position others)) (boid-position b))))
```

A boid's new velocity is its own plus the five weighted pushes,
`(fold-left vec+ (boid-velocity b) (list (push 'cohesion ...) ...))`,
and a step maps that over the flock.

The sim loaded into a v1 sandbox with two changes:

- Its two `record-writer` forms are gone. A sandbox has no
  `record-writer` (`services/env.ss`), so a boid prints as any record
  does, its type's uid and its fields.
- `boids-rule` uses `memq`, as Boids' does, because a sandbox has no
  `plist-ref`.

Its two plists are laid out a key a line, as `AGENTS.md` asks of
configurations, and its records' uids are its own
(`schevana-boids-simple-vec`). The window and the rules window are
Boids', with `length` for `boids-size`, a `for-each` for the drawing,
and no threads row. It asks for no crew: the step runs on the world's
thread, in the window's frame handler.

`tests/units/boids-simple.ss` checks it against the reference, against
Boids for 8 frames of 300 boids under the default and the heaviest
rules, and pixel for pixel against Boids' drawing.

**Speed.** About 10 times Boids' time, and 12 to 13 times under the
heaviest rules, from the bench (a step with the wrap, both on this
thread alone), and 90 to 700 times the memory:

| boids | rules | Boids (simple) | Boids | ratio | made: simple | Boids |
|---|---|---|---|---|---|---|
| 150 | default | 1.8 ms | 0.19 ms | 9.6 | 1.4 MB | 16 KB |
| 450 | default | 16 ms | 1.6 ms | 9.7 | 11.7 MB | 44 KB |
| 1,000 | default | 78 ms | 8.0 ms | 9.7 | 56 MB | 98 KB |
| 150 | heaviest | 3.8 ms | 0.31 ms | 12.3 | 3.6 MB | 16 KB |
| 450 | heaviest | 34 ms | 2.7 ms | 12.6 | 32 MB | 44 KB |

Each boid goes through the whole flock once for each rule with a
weight, and every `vec+` and `vec-` on the way makes a new record.

**The fuel cap.** A window's handler gets 50M engine ticks
(`handler-fuel`, `src/core1/proc.ss`), and one that runs out is stopped
as a runaway: the window crashes, and shows the error. A step's ticks,
default and heaviest rules:

| boids | ticks |
|---|---|
| 150 | 0.2M, 0.4M |
| 450 | 1.8M, 3.2M |
| 1,000 | 8.7M, 15.6M |
| 2,000 | 35M, 62M |

So the count stops at 450, as the first port's did: a step there is
one or two 60 Hz frames, well inside the fuel. 2,000 boids set from the
REPL, under the heaviest rules, would crash the window at its first
step.

**The three apps.**

| | Boids (simple) | Boids | Boids (clear) |
|---|---|---|---|
| file | `apps/boids-simple.ss` | `apps/boids.ss` | `apps/boids-clear.ss` |
| a flock | a list of boid records | an flvector | an flvector |
| a step | a procedure a rule | one worklet | one worklet, in vecs |
| runs on | the world's thread | the crew | the crew |
| boids, at most | 450 | 5,000 | 5,000 |
| 450 boids, default, alone | 16 ms | 1.6 ms | 1.1 ms |
| made a step, 450 boids | 11.7 MB | 44 KB | 45 KB |

All three step the same flock to the last digit.

## What each level takes away

Boids (simple)'s two changes were both the sandbox's. Schevana has
levels, each giving up some of Scheme for something in return, and the
speed of the apps above comes from climbing them. The bottom is a plain
R6RS program with a drawing library, which owns its main loop, its
files and its threads.

**Any window, in the world or a sandbox.**

- The world owns the loop. An app is handlers (`'frame` with the time,
  `'draw` with the size, `'key` ...), each of which returns;
  `window-frame-soon!` asks for the next frame. State lives in the
  window (`window-ref`, `window-set!`) or in a closure.
- A handler's call has a budget, 50M engine ticks (about 0.1 s,
  `handler-fuel`). One that runs out crashes its window, not the
  desktop. So a handler can't start engines of its own: Chez won't nest
  them ("cannot nest engines").
- Drawing is in `'draw` alone, as records: the draw bundle's boxes,
  lines, polygons, circles, gradients, transforms, clips and text, not a
  library's paths. A color is a theme key or `rgb`.
- One thread, core1, owns the windows, the drawing and every global.
- Definitions stay at the top level, so a REPL can redefine them live.

For Boids (simple) this level is its window code; the sim needed
nothing.

**Outside a sandbox.** The world, "/", is all of Chez (a copy of
`scheme-environment`) and every name of the world's: `syntax-case`,
`record-writer`, `eval`, files, threads and C are all there. Three ways
in:

- A world namespace, a child of the world on all of Chez, with every
  name of the world's and what it defines its own.
- A REPL at "/", `(repl)`, where what is defined is the world's itself.
- A tree app without `app-requests`, which loads into "/", as Settings,
  Monitor, Tasks and Environments do. An app of the person's own, in
  `~/.schevana.d`, gets a sandbox even without one.

The first port's sim, its `record-writer` forms and `plist-ref` and all,
loads into a world namespace unchanged, and its names stay there:

```scheme no-run
(define lab (env-fork (env-root) 'world 'name 'lab))
(env-load lab "boids-sim.ss")     ; a24fe98's vectors through screen
(env-eval lab '(car boids-demo))  ; => #<boid at 0.0 0.0 moving 1.0 0.0>
(top-level-bound? 'vec+ *world-env*)          ; => #f
```

What holds there is the tree's rules, which `tests/sources.ss` checks,
not the runtime: the world is one namespace (in "/" the sim's `vec+`
would be the world's, and `AGENTS.md` would have it `boids-vec+`), only
`src/` calls C, and only core1 writes the world's state. So for an app
in the tree the sandbox is the smaller change, since it keeps the plain
names; for one's own use, a world namespace is.

**A sandbox** (`app-requests`), and why:

- No `import` and no `library`: the confined base has neither, so an
  R6RS program's `(import (rnrs))` goes, and the file is top-level forms
  and `define-app`.
- No I/O, files, `eval`, record inspection, `exit`, threads, sleep or C:
  that is the confinement. `display` and `printf` write to the
  sandbox's log.
- `syntax-rules`, not `syntax-case`: `datum->syntax` on an identifier a
  world macro hands in would name any of the world's bindings.
- No `record-writer`: a type's writer is the whole process's, so one set
  on a condition type would change how the world prints. A record
  type's uid is taken under the sandbox's path, so a boid prints as
  `#[/apps/boids-simple:schevana-boids-simple-boid ...]`.
- Of the world's names, only its bundles' (window, draw, ui, timers,
  crew, gpu, toasts, launch): not `plist-ref`, which is the world's
  (`src/base.ss`), and not the clock without `timers`.
- No processes and no vats: no bundle has `spawn`, and an app's sandbox
  forks nothing. So long work can't run a slice at a time between
  frames: a step fits a handler's fuel, or goes to the crew or the GPU.
  Hence Boids (simple)'s cap.

In return it has a namespace of its own, permissions the person
approves, and records that open again after a reload.

**The crew.** The step becomes a worklet, compiled in an environment of
its own (`src/core1/proc.ss`): R6RS's base, control, lists, fixnums,
flonums, bitwise operations, bytevectors and exceptions; fxvectors,
flvectors, boxes, `now-ns`, and the other worklets.

- None of the app's names: not its procedures, not its macros (they are
  written inside the body), not its records. There's no
  `define-record-type`, and a record handed in can't be opened; pairs,
  vectors and boxes can.
- Nothing handed in but data and worklets, and no top-level name set.
- A call writes only its own slice and should allocate nothing, since a
  collection stops every thread: 20 to 200 µs a call, and on core1 one
  still running after 100 ms is cancelled.

A worker never touches the world's state: a worklet that could is
refused as it is defined, not when it races. That is Boids' flvector,
and why Boids (clear) writes its vec macros into the body. A step on
lists of pairs would still be accepted, `filter` and `fold-left` and
all, but it allocates, which is what the crew pays most for.

A vat is the crew's sibling: a thread, its mailbox, and a sandbox on
the worklets' base (no records), handing its results back to core1 (see
`docs/THREADS.md`).

**The GPU.** A shader (`docs/SHADERS.md`) has three types, 32-bit whole
numbers, 32-bit floats and booleans: no pairs, vectors, strings or
procedures as values. It has no recursion (a local function is
inlined), no loop without a bound (`do-bounded`), and no memory but the
buffers it is handed, each read or written in a run, not both. Its
results show through a paint or come back to Scheme later
(`gpu-buffer-read-then!`): a handler can't wait. The language asks only
what every GPU API has, and the twin runs it in Scheme to the same
answers. Its floats are 32 bits, so a flock there can't be the CPU's to
the last digit.

| level | runs in | gives up |
|---|---|---|
| R6RS and a library | its own program | nothing |
| any window | handlers | the main loop, unbounded calls, the library |
| world namespace | all of Chez | nothing; in the tree, its rules |
| sandbox | the confined base | `import`, I/O, `syntax-case`, `record-writer` |
| | | most of the world's names, processes, vats |
| crew | worklets | the app's names, records, closures, allocation |
| GPU | shaders | all but 32-bit numbers in buffers, recursion |

The crew and the GPU don't need a sandbox: an app in the world uses them
too. What a sandbox decides is what is in reach.

## The grid, and a name for each place

A version has a place on two axes: its level, what its step may reach
(above), and how its step is written:

- **simple**: the first port's way. A flock is a list, vecs and boids
  are small structures, each rule is a procedure, and every vec is new.
- **flat**: numbers side by side in an flvector, by hand, and one loop
  over every pair.
- **clear**: flat underneath, written over with names and vecs by
  macros (T1 and T2, below).

So a name gives both: "Boids L", the level, and the way in
parentheses. Boids L3 (simple) is level 3 written simple; "L" keeps the
level from reading as Boids 3D's 3. It is the first named so. The others
keep their names for now, and would be Boids L2 (simple), Boids L3
(flat) and Boids L3 (clear):

| | simple | flat | clear |
|---|---|---|---|
| L2, a sandbox | Boids (simple) | (Boids, its threads at 0) | |
| L3, the crew | Boids L3 (simple) | Boids | Boids (clear) |
| L4, the GPU | none can be | (Boids 3D, another sim) | |

A shader has no lists or procedures as values, so the GPU's row has no
simple version. Boids with its threads at 0 runs as a flat level 2
would, though it asks for the crew.

### Boids L3 (simple): the simple step on the crew

**What.** `apps/boids-l3-simple.ss` is Boids (simple)'s sim with what
the crew asks of it, and no more:

- A vec is a pair, `(x . y)`, and a boid a pair of them, `(position .
  velocity)`: of what it is handed, a worklet opens pairs, vectors and
  boxes, not records.
- Each procedure the step calls is a worklet, `define-worklet` for
  `define`, defined before those that call it: a worklet reaches no name
  but other worklets'.
- So the zero vec is a worklet, `(vec-zero)`, pi is written out (the
  world's is out of reach), and `boids-rule` raises with R6RS's `error`.
- `boids-step` hands the crew a chunk of boids a call,
  `boids-steer-slice!`, which writes each one's next state into a vector.
  The speed limits and the wrap stay on core1, as plain procedures.

The rest reads as Boids (simple) does: `boid-neighbors` with `filter`,
`vec-mean`, the `fold-left` of the pushes. Its rules window is Boids',
the threads row too. `tests/units/boids-l3-simple.ss` checks it against
the reference, on the crew and on the caller alone, and against Boids'
own steps, Boids' pixels, its keys, and a step at its cap on the crew.

**Speed.** One run of the bench, ms a step with the wrap, on this
thread alone and on the crew of 8 and the caller ("simple" is Boids
(simple)):

| boids | rules | simple | L3 alone | L3 crew | Boids alone | Boids crew |
|---|---|---|---|---|---|---|
| 150 | default | 1.74 | 1.65 | 0.47 | 0.19 | 0.19 |
| 450 | default | 15.5 | 13.8 | 2.59 | 1.59 | 0.42 |
| 1,000 | default | 78.3 | 66.0 | 12.7 | 7.72 | 1.46 |
| 150 | heaviest | 3.64 | 3.36 | 0.86 | 0.30 | 0.30 |
| 450 | heaviest | 32.2 | 29.4 | 5.71 | 2.67 | 0.64 |
| 1,000 | heaviest | 162 | 142 | 26.5 | 12.7 | 2.03 |

- **The layout and the threads multiply.** On one thread, flat is 8.6
  and 11 times as fast as simple at 1,000 boids (L3 alone against Boids
  alone). On the crew, each gains about 5 times there: L3 5.2 and 5.4,
  Boids 5.3 and 6.2. Together, Boids on the crew is 54 and 80 times as
  fast as Boids (simple).
- **Allocating costs the crew less than feared.** A step makes 56 MB at
  1,000 boids under the default rules and 144 MB under the heaviest,
  which stops every thread for each collection, and still gains about
  what Boids does, which allocates 97 KB. At 450 it gains more than
  Boids does, 5.3 times to 3.8: Boids' steps there are short enough for
  the crew's own costs to show. At 150, Boids' flock is one chunk, run on
  the caller.
- **Pairs and worklets cost less than records**: on one thread, L3 is 5
  to 16% faster than Boids (simple), and makes the same garbage.
- **The crew walks what it is handed**, for procedures that aren't
  worklets: a flock as lists is walked every step, 0.36 ms at 1,000
  boids, where Boids' flvector isn't.

In a window, a frame turn holds collections off till its frame is sent
(`src/core1/system.ss`), so a step's garbage piles up first; the bench
collects as it goes.

**Its cap.** 1,000 boids. On core1 the crew gives a step 100 ms, then
cancels it and raises, which crashes the window (`services/crew.ss`).
2,000 boids under the heaviest rules run past that on the crew of 8;
1,000 take 26 ms there and 78 on a crew of one. With no crew the step
runs on core1, as Boids (simple)'s does, with the handler's fuel alone:
142 ms at 1,000 under the heaviest rules, slow but whole.

## What the flat step costs a reader

- **The layout has no names.** A boid is x, y, vx, vy at offset 4i, but
  the worklet writes `(fx* 4 i)`, `(fx+ at 2)`, `(fx+ o 3)`. The app's
  `boid-x` can't be used there: a worklet reaches none of the app's
  procedures.
- **Rules are numbers whose meaning is elsewhere.** `(rule 7)` to
  `(rule 12)` and `(sees? 5)` mean whatever place `boids-steering` gives
  them with `apply flvector`; reorder that list and the step goes wrong
  quietly.
- **Sums are numbers too**, next to rule numbers: in `(toward 3 5 vx 1)`,
  3 and 5 are sums and 1 is a rule.
- **Every vector is written twice**, x and then y, and the new velocity
  is a five-deep `fl+` for each.
- **The rules have dissolved.** Each is a test in the pair loop, some
  sums, and a term in the velocity; only comments name them.
- **What keeps it fast is invisible**: a helper procedure or a vec bound
  to a variable quietly allocates.

## What a clear version is up against

Measured, before writing any of it:

- **A worklet's body is compiled from a quoted datum, in an environment
  of its own** (the core's base and its app's worklets). The app's
  macros, procedures and records don't reach it. Every abstraction in
  the kernel is a macro defined inside its body.
- **A sandbox has `syntax-rules`, not `syntax-case`** (left out on
  purpose, `services/env.ss`). Macros can't run code at expansion, and
  can't make one name from another (`boid` and `x` into `boid-x`).
- **What Chez 10.4 keeps unboxed**, in a loop over 4,000 flonums, bytes
  a call:

  | shape | bytes |
  |---|---|
  | sums in an flvector (Boids') | 32 |
  | named-let loop variables, even 12 | 0 |
  | flonums bound outside the loop, read inside | 0 |
  | a flonum variable `set!` in the loop | 64,000 |
  | a local procedure returning two flonums | 128,000 |
  | a small local procedure, one flonum | 0 |

  Loop variables as sums took half the time of an flvector of them;
  numbers bound once outside the loops, two thirds of the time of
  reading them from their flvector at each use.

- **Chez's optimizer does remove some records.** With the record type in
  the same scope, `(vec-x (make-vec a b))` folds to `a`, and straight-
  line `vec+`/`vec-dot` code allocates nothing. It stops at a vec chosen
  by an `if`, a vec in a loop variable, and a procedure too big to
  inline, all three of which a step has. A worklet has no
  `define-record-type` anyway.

## T1: names that cost nothing

As of commit `427cfbb`; T2 changed the worklet's side of these forms.

**What.** Four `syntax-rules` forms, at the top of the app:

```scheme no-run
(define-flat-record boid          ; records end to end in an flvector
  (boid-x boid-x-set!)
  (boid-y boid-y-set!)
  (boid-vx boid-vx-set!)
  (boid-vy boid-vy-set!))

(define-flat-names steering       ; one record, read as variables
  cohesion
  ...
  back-off-reach)

(define-flat-worklet (boid (steering rules))
  (boids-steer! lo hi flock next rules)
  (define-flat-variables sums     ; set!-able names over an flvector
    center-x
    ...
    back-y)
  ...)
```

`define-flat-record` defines the accessors as procedures for the app,
and a macro named after the record that hands its declaration on.
`define-flat-worklet` asks each record listed for its declaration and
writes them into the worklet, where the same names are macros: there,
`(boid-x flock j)` comes to `(flvector-ref flock (fx* 4 j))` and
`cohesion` to `(flvector-ref rules 0)`. `boids-steering` builds the rules by name,
`(make-flat-names steering 'cohesion ... )`, and a name missing or
unknown is an error.

The step then reads:

```scheme no-run
(when (and (fl< d2 cohesion-reach) (sees? view-cosine))
  (add! center-x (boid-x flock j))
  (add! center-y (boid-y flock j))
  (add! centered 1.0))
```

where Boids has `(when (and (fl< d2 (rule 8)) (sees? 5)) (add! 0 ...)`.

**Readability.** Gained: no number in the step means a place; the
layouts are declared once, for the app and the worklet both; the rules
are built by name. Paid:

- About 190 lines of macros and their comments (125 of code), written
  twice: once for the app and once inside a template for the worklet. They pass lists as a pair's tail,
  `(x . more)`, to stay clear of escaped ellipses, and pass declarations
  along in continuation-passing style.
- An accessor's name is written out (`boid-x`), as `syntax-rules` can't
  make it.
- One hygiene detail: inside the worklet, a name that has passed through
  a record's macro is a different name from the body's own. So a record
  of names is opened where the worklet is declared, `(steering rules)`,
  not by a form in its body.
- Still x and y written twice, and the velocity's `fl+` nest. Names
  don't fix structure; T2 is for that.

**Speed.** 5 to 20% slower than Boids, mostly 10 to 13%:

| boids | default rules, alone | crew | heaviest, alone | crew |
|---|---|---|---|---|
| 150 | 1.11 | 1.11 | 1.10 | 1.11 |
| 450 | 1.11 | 1.13 | 1.10 | 1.13 |
| 2,000 | 1.13 | 1.20 | 1.10 | 1.11 |
| 5,000 | 1.11 | 1.10 | 1.11 | 1.05 |

(Boids (clear)'s time over Boids'.) Every `(boid-x flock i)` multiplies
its index again, where Boids works out the offset once by hand (`at`,
`o`). Chez folds the constants (the record's size is 4) but shares no
common subexpressions, and each `fx*` is checked.

## T1b: the steered boid bound once

**What.** The boid being steered gets its numbers once, before the loop
over the others:

```scheme no-run
(let* ([x (boid-x flock i)]
       [y (boid-y flock i)]
       [vx (boid-vx flock i)]
       [vy (boid-vy flock i)]
       [h2 (fl+ (fl* vx vx) (fl* vy vy))])
  (do ([j 0 (fx+ j 1)]) ((fx= j n))
    (let* ([dx (fl- (boid-x flock j) x)] ...
```

Boids reads them again for every pair. Bound outside the loop and never
set, they stay unboxed.

**Readability.** Better than T1: the steered boid is named once, as the
original's `p` and `v` were.

**Speed.** 13 to 24% faster than Boids, in one run of the bench:

| boids | default rules, alone | crew | heaviest, alone | crew |
|---|---|---|---|---|
| 150 | 0.79 | 0.80 | 0.87 | 0.85 |
| 450 | 0.78 | 0.81 | 0.84 | 0.86 |
| 2,000 | 0.77 | 0.81 | 0.84 | 0.86 |
| 5,000 | 0.80 | 0.76 | 0.85 | 0.84 |

From one run to the next it moves: at 450 boids, default rules, alone,
it measured 0.78 to 0.84 over six runs, and at 2,000, 0.76 to 0.84.
Boids' step, timed against itself, stays at 0.99 to 1.00.

The speedup isn't the names' doing. Like for like (default rules, alone, 450
to 5,000 boids, two to five runs each):

| step | its time over Boids' |
|---|---|
| Boids, the steered boid bound once (as T1b) | 0.73 to 0.75 |
| Boids (clear), T1b | 0.76 to 0.84 |
| T1b, the other boid's offset worked out once by hand | 0.73 to 0.74 |

(The two variants were throwaway copies of the worklets, timed with
the bench's rounds; they aren't kept in the tree.)

So the named rules, the named sums and the bound boid cost nothing;
what costs, 3 to 15% depending on the run, is each accessor multiplying
the other boid's index again, `(fx* 4 j)` for its x and again for its y.
Done once, as Boids does by hand, the names are free. A loop form that
binds a record's offset once, in T2, can do that without the hand.

## T2: vec code, lowered by macros

**What.** A library of macros for worklets, `vec`, with the first
port's operations: `vec-x`, `vec-y`, `make-vec`, `vec-zero`, `vec+`,
`vec-`, `vec*`, `vec/`, `vec-dot`, `vec-mean`, and `let-vec*` to bind
vecs. A vec is never made. A vec form is asked for one component at a
time and becomes that component's flonum expression:

```scheme no-run
(vec-x (vec+ a b))   ; is (vec+ vec-x a b): (fl+ (vec-x a) (vec-x b))
```

So the three shapes Chez's optimizer leaves boxed don't come up. A vec
chosen by a test, `(vec-if test a b)`, is `(if test ax bx)` for x and
again for y. A vec variable bound by `let-vec*` is two flonum variables.
A vec form of one's own is a macro, `define-vec-syntax`, always inlined,
not a procedure that would have to return one. What adds up over the
neighbors is a vec sum, `(add! crowd away)`, or a mean that counts too,
`(vec-mean center)`, stored in an flvector as before.

Records are reached through references. `(for-each-flat boid (other
flock) ...)` steps through the flock four numbers at a time, and
`(boid-x other)` reads at an offset worked out once. That takes back the
index multiplies that cost T1.

`define-worklet-library` declares a library like a record: once, at
the app's top level, and listed by the worklets that use it,
`(define-flat-worklet (vec boid (steering rules)) ...)`. Its macro hands
the library on with its ellipses escaped, `(... template)`, so library
code is written with ordinary ellipses. T1's worklet side became such a
library, `flat`, which every flat worklet gets.

The step now reads:

```scheme no-run
(for-each-flat boid (me flock lo hi)
  (flvector-fill! sums 0.0)
  (let-vec* ([p (boid-position me)]
             [v (boid-velocity me)])
    (let ([h2 (vec-dot v v)])
      (for-each-flat boid (other flock)
        (let-vec* ([to-other (vec- (boid-position other) p)])
          (let ([d2 (vec-dot to-other to-other)])
            ;; not itself, and in somebody's reach: most pairs stop here
            (when (and (fl> d2 0.0) (fl< d2 longest-reach))
              ...
                ;; toward the center of the neighbors
                (when (and (fl< d2 cohesion-reach) (sees? view-cosine))
                  (add! center (boid-position other)))
                ...
                ;; a unit step sideways out from behind each blocker
                (when (and (fl< d2 clear-view-reach)
                           (sees? blocker-cosine))
                  (let-vec* ([quarter-turn (make-vec (fl- 0.0 (vec-y away))
                                                     (vec-x away))]
                             [side (vec-if (fl< (vec-dot v quarter-turn)
                                                0.0)
                                           (vec* quarter-turn -1.0)
                                           quarter-turn)])
                    (let ([d (flsqrt d2)])
                      (add! aside (vec/ side d)))))
                ...))))
      (let-vec* ([velocity (vec+ v
                                 (vec* (toward center p) cohesion)
                                 (vec* (toward heading v) alignment)
                                 (vec* crowd separation)
                                 (vec* aside clear-view)
                                 (vec* back back-off))])
        (boid-set! next me (vec+ p velocity) velocity)))))
```

The first port ended its step with `(fold-left vec+ (boid-velocity b)
(list (push 'cohesion ...) ...))`, and its `boid-step-aside` has the
same `quarter-turn` and `side`.

**Readability.** Gained: the step is in vecs and boids, as the original
was: position, velocity, to-other, away, a quarter turn and its side,
what each rule adds up (center, heading, crowd, aside, back), and the
velocity as v plus the five pushes, in the original's order. No
component is written twice, and no number names a place. Paid:

- The `vec` library, 125 lines with its comments (98 of code), on top of
  `flat`'s 221 (146). The step itself is 61 lines of code, Boids' 77.
- A vec isn't a value. `let-vec*` binds vecs and `let` numbers, a vec
  chosen by a test is `vec-if`, not `if`, and a vec form where a number
  is wanted is a syntax error, reported from inside an expansion.
- A vec form of one's own is a macro, not a procedure: nothing to pass
  around or call from the REPL.
- The rules are still fused into one loop over the others, as in Boids:
  each is a `when` adding one vec, not the original's procedure over a
  list of neighbors (`filter`, `map`, `fold-left`). `sees?` still works
  from `ahead` and `lengths`, worked out once a pair, as Boids does.

**Speed.** 17 to 31% faster than Boids, and the same from run to run:

| boids | default rules, alone | crew | heaviest, alone | crew |
|---|---|---|---|---|
| 150 | 0.71 | 0.72 | 0.81 | 0.81 |
| 450 | 0.70 | 0.70 | 0.81 | 0.79 |
| 2,000 | 0.69 | 0.71 | 0.80 | 0.83 |
| 5,000 | 0.70 | 0.70 | 0.80 | 0.81 |

Like for like, alone, two runs each, against Boids' own step given T2's
two changes, the steered boid bound once and the loop over offsets:

| step | default rules, 450 to 5,000 | heaviest, 450 and 2,000 |
|---|---|---|
| Boids, the steered boid bound once | 0.735 to 0.741 | 0.833 to 0.844 |
| Boids, that and the loop over offsets | 0.699 to 0.711 | 0.790 to 0.824 |
| Boids (clear), T2 | 0.690 to 0.695 | 0.801 to 0.808 |

So T2 runs at Boids' own speed for the same loop, within 2% either way:
the vecs and the names cost nothing. Its lead over Boids is the two
changes, which a flat step could make too. A step allocates what Boids'
does, its sums: 2,240 bytes in 20 steps of 300 boids.

## Along the way

- `flmod` is called out of line and boxes, 32 bytes a call, so each
  frame's wrap allocates 64 bytes a boid, in Boids too. `flsqrt` is
  inlined and allocates nothing.
- `fl-` of `0.0` and a difference, not a negation: `(fl- 0.0 dx)` gives
  `+0.0` where `(fl- dx)` would give `-0.0`, and the reference's `p - o`
  gives `+0.0`. The step's exactness rests on details like this.

## Next

- **T3: Chez itself.** What scalar replacement through `if`, loop
  variables and calls would take, as notes and the measurements above.
- **The sums as loop variables.** Twelve flonum loop variables cost
  nothing and ran in half the time of an flvector of sums; a fold over
  the others with vec-valued accumulators would drop the step's one
  allocation. Each pair would then name every sum it leaves alone.
- **The rules apart again.** The first port's per-rule procedures over
  lists of neighbors, fused into one loop by a macro: a bigger
  compiler, in `syntax-rules`.
