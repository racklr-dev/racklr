# Real-context unknown census

`tools/unknown-census.rkt` is the aiming instrument for the `unknown`
elimination obligation: it reports every call site the **real** classification
still leaves `unknown`, attributed to the receiver-type-dataflow lever that
would resolve it, and it emits the exact sites in a deterministic TSV.

## Why not the empty-context census

Two diagnostics can count unknowns, and they disagree by an order of magnitude:

- **Empty context** (`effervesce/tools/unknown-histogram.rkt`): classifies each
  call callee with *no known names*, so a call to another function in the tree,
  a `self.m()` edge, or an imported module handle counts as `unknown`. It
  reports 1463 non-edge call sites on `django/django/core`.
- **Real context** (`tools/unknown-census.rkt`): builds the module table,
  export graph, derived `Class.method` capabilities, type relations, and MRO,
  then runs the whole-program effect fixpoint (`infer-effects-all`). Only sites
  the real context cannot resolve are counted: 249 call sites on the same
  corpus at the same revision.

The empty-context number overcounts the surface and cannot aim work; a lever
chosen against it would be chosen against phantoms. This census is the real
surface. (The empty-context histogram is kept as a rough calibration probe,
not as an aiming tool.)

## Run

From the repo root, with the racklr collection on `PLTCOLLECTS` (the same
requirement as `effervesce/tools/type-graph.rkt`), and with the gitignored
`grammars-v4/` tree present for the Python front end:

```sh
PLTCOLLECTS="$(pwd):" racket tools/unknown-census.rkt django/django/core
PLTCOLLECTS="$(pwd):" racket tools/unknown-census.rkt django/django --tsv /tmp/full-census.tsv
```

Options:

- `--top N` — methods listed per lever (default 10)
- `--shape SHAPE` — restrict `--sites` lines to one receiver shape
- `--lever LEVER` — restrict `--sites` lines to one dataflow lever
- `--sites` — print per-site lines
- `--tsv PATH` — write the deterministic per-site TSV

The tool derives each corpus root's dotted package name by walking the
`__init__.py` chain (`django/django` -> `django`), so module paths match the
import sources in the scanned files.

## The lever taxonomy

Every unknown call site is attributed to the receiver-type-dataflow mechanism
that would resolve it (`census-lever-of` in `racklr/effects/census.rkt`):

| lever | receiver shape / binding | what resolves it |
| --- | --- | --- |
| `return-type-projection` | `call-result`, local bound from a call | project the return class of `f(...)` so `f(...).m()` resolves |
| `parameter-dataflow` | `param` | propagate parameter types from the concrete call chain |
| `self-attr-typing` | `self-attr` | type `self.attr` from its constructor/annotation and MRO |
| `element-type-dataflow` | local `loop-var` / `comp-var` | project iterable element types into loop/comprehension variables |
| `attribute-alias-typing` | local `attr` / `alias` | carry a receiver type through an attribute copy or alias |
| `mro-edge-resolution` | `self-method` | resolve `self.m()`/`cls.m()` through the class MRO to an edge |
| `multi-level-chain` | `chained` | walk a multi-level `a.b.m()` receiver chain to a class |
| `higher-order-row-hole` | `bare-call` whose callee names an enclosing parameter | type a callback parameter as a row hole at its call sites |
| `bare-call-lexicon` | `bare-call` to a free/global/imported name | add the missing bare stdlib/framework name to the DATA lexicon |
| `import-handle-lexicon` | `symbol` | tag the imported module/object handle's effect domain |
| `literal-constructor-typing` | local `literal` | give container/stdlib literals a receiver type |
| `unbound-binding-resolution` | local `unbound` | bind a module-global/import/closure name before typing the receiver |
| `exception-type-dataflow` | local `exception` | type the caught exception binding |
| `function-value-typing` | `bare-call` or receiver from a local `fn` | type a function value held in a local before calling it |
| `local-constructor-tracking` | other local | track the local's concrete constructor (`KnownClass(...)`) |
| `unclassified` | `var` / `other` | shape the census does not yet attribute to a lever |

## Aiming work

1. Run the census on the corpus that matters (`django/django/core` while a
   lever is in flight; `django/django` for the full-bar number).
2. Read the lever histogram highest-first, open the TSV, and filter to that
   lever:
   ```sh
   awk -F'\t' 'NR==1 || $7=="parameter-dataflow"' /tmp/full-census.tsv
   ```
3. Land one lever. Re-run the same command and diff the TSV (and the site /
   function / file counters) against the baseline below: a lever that does not
   move real sites did not move the goal.

`higher-order?` in the TSV is #t only for a callback parameter (row-hole
work); a bare call to a function-valued local is the separate
`function-value-typing` lever (binding-kind `fn`), and a bare free/imported
name stays `bare-call-lexicon` (binding-kind `unbound`), so the three
`bare-call` cases are not conflated. The `component` column is the dotted module path (for example
`django.core.cache.backends.base`), so the TSV does not depend on the checkout
directory and diffs cleanly across machines.

TSV fields are **space-normalized, not escaped**: every control character in a
field (tab, newline, carriage return, ...) is replaced by a space. The file is
a diff artifact, not a lossless serialization of arbitrary identifiers.

## Baseline (observed 2026-11-01, racklr `c349f37` + this change)

`django/django/core`:

```
files scanned: 112   parsed: 112   failed: 0
functions: 979 (in 101 files with inferred functions)
unknown call sites: 249   unknown functions: 250   unknown files: 66

receiver shapes: local 75, bare-call 55, self-attr 39, param 36,
                 chained 19, call-result 13, self-method 12
local binding kinds (of 75): call-result 44, loop-var 12, attr 10,
                 literal 6, comp-var 2, unbound 1

levers:
  return-type-projection   69   self-attr-typing         39
  parameter-dataflow       36   element-type-dataflow    22
  multi-level-chain        19   higher-order-row-hole    17
  attribute-alias-typing   15   mro-edge-resolution      12
  literal-constructor-typing 10 bare-call-lexicon         9
  unbound-binding-resolution 1
```

`django/django` (907 files, the acceptance-bar corpus):

```
files scanned: 907   parsed: 907   failed: 0
functions: 9503 (in 570 files with inferred functions)
unknown call sites: 3983   unknown functions: 3653   unknown files: 449

receiver shapes: local 1126, param 736, self-attr 669, bare-call 507,
                 chained 328, call-result 319, self-method 294, other 4
local binding kinds (of 1126): call-result 453, unbound 232, loop-var 192,
                 attr 155, comp-var 59, literal 22, exception 9, alias 4

levers:
  return-type-projection    834    parameter-dataflow        736
  self-attr-typing          669    multi-level-chain         328
  bare-call-lexicon         299    mro-edge-resolution       294
  element-type-dataflow     272    unbound-binding-resolution 232
  attribute-alias-typing    226    higher-order-row-hole      45
  literal-constructor-typing 35    exception-type-dataflow     9
  unclassified                4
```

One full-corpus run takes on the order of 8-15 min wall including parser
generation (723-908 s observed on this machine) and peaks at ~275 MB RSS, so
the census fits a small harness memory budget.

The independently pinned `effervesce/tools/unknown-shapes.rkt` reports the
same 249 sites / 250 functions / 66 files and the same shape histogram on
`django/django/core`, and the same 3983 / 3653 / 449 and the same shape and
local-binding-kind histograms on `django/django`, so the census agrees with
the diagnostic already used by the loop.

The effect-graph gate is unchanged by this diagnostic. On
`django/django/core`, `racket tools/baseline-check.rkt` reports
`baseline COVERAGE-FAILED sha256:14bac7860833329849c4d81740ecccd01870b72cb8355f947f70956e179623ba`:
the graph **matches the pin** (the pass/fail half is green — no drift) while the
Phase-0.3 unknown-elimination obligation is still open (250 unknown functions /
66 unknown files, `<= 0` required). The census in this document is the aiming
instrument for that open obligation, not a claim that it is discharged. The
same run reports 112 parsed / 0 failed, 979 named functions, db 169/54,
network 9/6, inherits 126, fn-effect-row 979.

Re-run the two commands above to reproduce the numbers; a lever PR should
update this block (or the committed baseline it points at) in the same change
that moves the number.

## Determinism

Sites are sorted by component, enclosing function, exact site string, shape and
op; the aggregations sort by count then printed key; the TSV header is fixed.
Two runs over the same corpus produce byte-identical TSV, so a lever's effect
is a diff, not a remembered number.
