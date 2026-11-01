# bendtt: the BendTT kernel in Koka

This directory is a **learn-and-expand** of the BendTT kernel in Koka 3.2.9,
not a line-for-line port. BendTT is Bend's core type theory: a dependent
affine calculus with `Type : Type`, no universe levels and no native
datatypes. The theory, its metatheory and the proofs live in the Lean file
`bend2/bendtt.lean` of the Bend source, and the paper *BendTT: An Affine
Dependent Type Theory* (`bend2/docs/BendTT/main.typ`, `paper/BendTT.pdf`)
describes it. Koka cannot state the dependent `Prop` claims (subject
reduction, progress, consistency, adequacy), so those stay Lean-only.

The ticket is a learn-and-expand of that kernel in three moves:

1. **Certify live-eval termination with the untyped measure.** Implement
   `Term.size` (`bendtt.lean:2935`) — the size of a term's live part, where
   dead `q=0` positions weigh 0 — and restructure weak-head evaluation and
   the live check so that Koka's totality checker accepts them as total
   functions (a machine-checked termination certificate, `no div`).
2. **Align the descent relation** field-for-field with bend2's
   `term_descend` (`bend2/bend.ts:3253`), the running check that a recursive
   call is on smaller arguments.
3. **Port the λ2→λ1 quantity erasure** into conversion: a lambda's quantity
   says how it runs, so a lambda of quantity 2 converts as a lambda of
   quantity 1 (`Quan.lin`, `bendtt.lean:167`; `Term.parts`, `bendtt.lean:968`).

## Files

- `kernel.kk` — the kernel: quantities, terms, defs and books, renaming and
  substitution, the untyped measure, the evaluator, conversion, and the live
  check (affinity + descent). Every function is total except the console
  entry points in `meta.kk`; see "What Koka certifies" below.
- `meta.kk` — the paper's examples, written as books of terms: `Nat`,
  `add`, `R`, `app`, `ack`, `twice`, `two`, `map`, `loop`. Runs the live
  check on each book, checks that lambda erasure makes the right terms
  convertible, and checks that the measure ignores dead `q=0` parts.
- `check.sh` — runs the ticket's verify command and asserts the expected
  output lines and that no kernel function carries the `div` effect.

## Run

```sh
# the ticket's acceptance command (from the repository root)
koka -e bendtt/meta.kk

# or the asserting wrapper
./bendtt/check.sh
```

Koka 3.2.9 is required (the version is checked in this exercise). Koka caches
build artifacts outside the tree (under `$HOME/.koka` for `koka -e`, or in a
`.koka/` directory for library builds); `.koka/` is gitignored so the
repository stays clean.

Expected output (abridged):

```
-- live check: the paper's verdicts --
R: accepted
add: accepted
ack: accepted
twice: accepted
app: affinity-rejected
map: affinity-rejected
two: affinity-rejected
loop: descent-rejected

-- conversion: lambda quantity erasure --
conv lam+ ~ lam: convertible
conv lam+ ~ lam-: distinct
conv lam ~ lam: convertible
conv beta: convertible
conv mixed: convertible
conv def unfolds: convertible

-- measure: the untyped live measure --
measure ann (type weighs 0): ok
...
measure fuel = size (add): ok
measure fuel = size (ack): ok
verdicts 8/8, conversion 6/6, measure 7/7
ALL CHECKS PASS
```

## The three moves in the Koka code

### 1. The untyped live measure and total traversals

`size` (`kernel.kk`) is `Term.size` from `bendtt.lean:2935`: annotations
weigh their live term plus one; a `q=0` let value, application argument or
tuple first field weighs 0; everything else weighs its live children plus
one. The measure needs no types, which is the point: live evaluation
terminates from linearity alone, and types only rule out stuck terms.

Koka 3.2.9 has no measure annotations: its totality checker is a size-change
analysis over a **single self-recursive function**, and it assumes a mutual
recursion group diverges (`Core/Divergent.hs` in the Koka compiler). So the
kernel restructures the Lean checker's fuel-threaded mutual evaluator
(`Term.wnf/run/fire`, `bendtt.lean:861-947`) into **one** mode-threaded
machine `ev` whose only decreasing argument is a Peano `fuel` value, built
from `size` by the total function `fuel-of` (`fuel-add` and `fuel-of` are
structural; an `int` recursion would not be accepted). The budget is
`6 * (size of the term + sizes of the book's bodies)`: unfolding a def copies
its body, so the input term alone does not bound the work. The machine
reports budget exhaustion as a separate outcome (`EO`), never as a weak head
normal form, and `whnf`/`convertible` return `Nothing` ("inconclusive", like
the Lean checker's "out of fuel") rather than a partial term or a false
"distinct".

The live check already is structural recursion on the term in Lean, except
for the mutually recursive `Pat.cmp/Pat.fields/Pat.visit` triple
(`bendtt.lean:1300-1335`) which the Lean file accepts with
`termination_by 2 * sizeOf p`. Koka rejects that mutual group, so `pat-run`
is a **single** function with three modes (`MCmp`, `MFields`, `MVisit`) whose
every recursive call is on a strict subpattern.

`conv` is likewise a single worklist machine so no helper calls it back
mutually: the current pair is reduced, `parts` gives the sub-pairs, and the
rest of the worklist is processed with one less fuel.

### 2. Descent, aligned with `term_descend`

The descent relation lives in `pat-run`, `arg-cmp`, `arg-descend`,
`called` and `tree`, translated from the Lean live check, which in turn is
written to be "as bend2's `term_descend` decides against the column's
pattern":

- a dead column is equal (`Quan.live` false ⇒ `OEQ`);
- the argument is compared against the piece the case tree bound at the
  column's path (`guard-at`): a variable is equal only to itself, a label
  matched by a match is equal to that label;
- a constructor compares its fields in turn, down the right spine: each
  field no bigger, one smaller, makes it smaller; all equal makes it equal;
  a field that may be bigger is the bad path;
- when a field may be bigger, the argument is smaller iff it is no bigger
  than **another** of those fields: only the constructor's fields are tried,
  never its tag or the pairs that chain them;
- arguments are compared left to right, skipping erased columns, until one
  is smaller; the arguments after it are unconstrained.

The tag/guard machinery (columns, variable tags at `(column, path)`, the
labels a match hit, `Pat.ins`, `Guard.next/bind/pat/at`) is the Lean
kernel's, so a self-call accepted by the Lean live check is accepted here
and one rejected there is rejected here.

Two differences from bend2 are deliberate and are the kernel's, not gaps:
the kernel has no literal node, so there is no `lit_step` (the
Bend→BendTT elaborator unfolds literals one layer before emitting a
term); and there is no `Ann` stripping (the elaborator strips annotations
on live self-call arguments). The `bad` recovery field is a path in the
pattern, not an index; for a same-constructor comparison the two are in
bijection. The kernel therefore implements `term_descend`'s relation on
elaborated terms: stripped, literal-free, η-long bodies; on a raw
annotation the comparison is conservatively bigger, and the elaborator is
what makes that unreachable.

### 3. λ2→λ1 erasure in conversion

`Quan.lin` maps `Q2` to `Q1` and leaves the others. `parts` compares
`TLam q f` with `TLam p g` when `q.lin = p.lin`, so `λ+ x => x` converts
with `λ x => x` but not with `λ- x => x`. The evaluator's split rule uses
`Quan.fld` for the same quantity reasoning. `meta.kk` checks all three
cases, plus a beta case and an erasure case under an application.

## What Koka's totality checker certifies

Every function in `kernel.kk` is declared with an explicit `: total` effect
and Koka's checker accepts the file, so the generated interface contains no
`div` effect (`check.sh` asserts this). That is the machine-checked part:
the evaluator, conversion, the measure, and the descent comparison are
total functions, and their recursion is structurally accepted.

What is **not** machine-checked in Koka:

- the claim that the `fuel` budget derived from `size` is enough to reach a
  weak head normal form, or that live evaluation always terminates: the
  budget is a heuristic bound (K = 6 units per constructor of the term and
  the book), and running out is reported as inconclusive; the measure-decrease
  argument (the label/Dershowitz–Manna ordering of `bendtt.lean`'s termination
  proof) is a dependent claim and stays in Lean;
- the dependent type checker and the theory's `Prop` claims: subject
  reduction, progress, consistency, adequacy.

Koka 3.2.9 cannot be told a measure, and it rejects mutual recursion, so
this is the faithful restructure rather than a translation of the
`termination_by` clauses.

## Boundaries of this expansion

- **The typing checker is not ported.** `meta.kk` runs the type-free live
  check, which is what the eight paper verdicts test (affinity and descent
  do not read types). The bidirectional checker (`Term.check`/`infer`,
  `fits`, kinds) remains Lean-only.
- **`Term.val` is not ported.** The Lean evaluator normalises closed `q=2`
  `Data` values before binding them (`bendtt.lean:917-943`) so copies share
  the work. `ev` runs the lazy (open-term) case: it reduces weak heads but
  does not normalise under constructors, so a closed `add 2 2` reaches a
  pair whose recursive call is still a call, not the numeral. Conversion
  still decides the lambda-erasure cases, head applications and unfolding
  of a def whose case tree is walked, which is what the conversion checks
  exercise.
- **`Arg.size`/`labels` are not ported.** The Lean termination proof's label
  ordering uses a `Value`/`Closed` predicate; Koka states it only as a
  comment in `size` and `fuel-of`.
- **The examples are checked for the live check only.** `add` and `ack` were
  cross-checked against real Bend→BendTT elaborations; `R`/`R.arms`/`app`
  follow the paper's text; `map` uses a Nat-specialised `List` (the paper's
  `List` is quantity-polymorphic) and the verdict it demonstrates is the
  affinity rejection, which does not depend on the type. `two` is the
  Church numeral whose function is used twice.
