# Boids (clear): a fast step that reads like the original

Boids' step (`boids-steer!` in `apps/boids.ss`) is fast because it is
flat: a flock is an flvector of four numbers a boid, and one worklet
loops over every pair with its flonums unboxed. The first port read
differently, with a `vec` and a `boid` record and a procedure per rule
(`git show a24fe98:apps/boids.ss`). The question here is how much of that
clarity can come back without losing the speed, and without changing a
number. `apps/boids-clear.ss` ("Boids (clear)") is the same app with its
step rewritten; `apps/boids.ss` stays as it is.

Two things keep each attempt honest:

- `tests/units/boids-clear.ss` checks every step against the reference
  flock in `tests/units.ss` to the last digit, and against Boids' own
  steps number for number (2,000 boids, 30 frames, the heaviest rules
  too).
- `tests/bench-boids-clear.ss` times both on the same flocks, in
  alternating rounds, and prints the medians and their ratio. Boids
  timed against itself gives 1.00.

```
scheme --script tests/units.ss out boids-clear
scheme --script tests/bench-boids-clear.ss out
```

Times below are from one machine (Ubuntu 26.04, 16 threads, Chez
10.4.1); the ratios are what carry over.

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
