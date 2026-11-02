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
  the real context cannot resolve are counted: 215 call sites on the same
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

A module's dotted name comes from its PACKAGE CHAIN, not from the scan root:
`path->module-path` walks the `__init__.py` chain up from each file, so a
repository checkout whose package lives under `src/` (`…/flask/src/flask/app.py`)
resolves as `flask.app` and the corpus's own imports resolve, while a scan root
that is itself a package directory (`django/django/core`) keeps the historical
dotted name (`django.core…`) and the pinned Django baseline is unchanged.

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

## Baseline (observed 2026-11-02, racklr `6fb6ba6` + this change)

`django/django/core` (the pinned baseline probe):

```
files scanned: 112   parsed: 112   failed: 0
functions: 979 (in 101 files with inferred functions)
unknown call sites: 305   unknown functions: 292   unknown files: 73
(this lever: 316 -> 305 call sites, 304 -> 292 functions, 75 -> 73 files)

receiver shapes: bare-call 159, local 61, param 32, self-attr 23,
                 chained 13, call-result 11, self-method 6
local binding kinds (of 61): call-result 30, loop-var 12, attr 10,
                 literal 6, comp-var 2, unbound 1

levers:
  bare-call-lexicon        116   return-type-projection    53
  parameter-dataflow        32   self-attr-typing         23
  element-type-dataflow     22   higher-order-row-hole    15
  attribute-alias-typing    14   multi-level-chain        13
  literal-constructor-typing 10  mro-edge-resolution       6
  unbound-binding-resolution 1
```

`django/django` (907 files, the acceptance-bar corpus):

```
files scanned: 907   parsed: 907   failed: 0
functions: 9503 (in 570 files with inferred functions)
unknown call sites: 4947   unknown functions: 4000   unknown files: 478
(this lever: 4969 -> 4947 call sites, 4018 -> 4000 functions,
 478 -> 478 files)

receiver shapes: bare-call 1687, local 1099, param 726, self-attr 647,
                 chained 330, call-result 313, self-method 141, other 4
local binding kinds (of 1099): call-result 429, unbound 232, loop-var 192,
                 attr 153, comp-var 59, literal 25, exception 9

levers:
  bare-call-lexicon        1484   return-type-projection   806
  parameter-dataflow        726   self-attr-typing         647
  multi-level-chain         330   element-type-dataflow    272
  unbound-binding-resolution 232  attribute-alias-typing   216
  mro-edge-resolution       141   higher-order-row-hole     41
  literal-constructor-typing 39   exception-type-dataflow     9
  unclassified                4
```

The full-package TSV diff for this lever is 22 removed rows and **no added
rows**: 12 `return-type-projection`, 9 `parameter-dataflow` and 1
`self-attr-typing` sites resolved, with no site gaining `unknown`. The effect
alphabet moves at the margin (network-touching functions 93 -> 95, db 4516 ->
4515), and `entries with typed ops` stays 72 (72 / 11,175 entries after the row
changes) because this lever does not change the operation vocabulary.

One full-corpus run takes on the order of 8-15 min wall including parser
generation and peaks at ~275 MB RSS. The relation join terminates on any input:
`type-relations-widen.rkt` detects a repeated state and widens exactly the
entries that moved in the cycle to unresolved, then freezes them. The 112-file
pinned probe converges without widening; the full corpus prints one 4-entry
widening event identically before and after this lever, so the full-package
counters are a deterministic tracked state.

The independently pinned `effervesce/tools/unknown-shapes.rkt` reports the same
305 sites / 292 functions / 73 files and the same shape histogram on
`django/django/core`, so the census agrees with the diagnostic already used by
the loop.

The effect-graph gate is unchanged by this diagnostic. On `django/django/core`,
`racket tools/baseline-check.rkt` reports
`baseline COVERAGE-FAILED sha256:995a25ab332c6e504bae4d0311016f9dfecd5cb2a59c9a221aadda5a10c17022`:
the graph **matches the re-pinned snapshot** (the pass/fail half is green;
2708 -> 2720 atom lines) while the Phase-0.3 unknown-elimination obligation is
still open (292 unknown functions / 73 unknown files, `<= 0` required). The
census in this document is the aiming instrument for that open obligation, not
a claim that it is discharged.

### Remaining surface after this lever

The call-result projection reaches the receiver-method-return and returned-call
sites of the `return-type-projection` family. The largest families in the
before-census are `bare-call-lexicon` (1484, dominated by the `kw-spread`
lowering), `return-type-projection` (818), `parameter-dataflow` (735),
`self-attr-typing` (648) and `multi-level-chain` (330). Within
`return-type-projection` the largest unresolved groups are `super().m()`
results (out-of-corpus parent-method classification, not return projection),
receiver-method returns limited by an unprojected receiver, imported/module
result-type DATA entries, and function values. `parameter-dataflow` is largely
outside call-chain reach: most of its sites have no statically visible
in-corpus call site (framework/dispatch-invoked callbacks and protocol
methods); the visible residual call sites pass locals/attributes, i.e.
upstream dataflow.

Two deliberate conservatisms of this lever: a method call projects only its
most-derived dispatch key, so an unprojected override fails closed instead of
inheriting a superclass projection, and the relation projection is not
compositional through call-result receivers (`x = f().m()` is not projected in
the relations; the classification-time receiver chain does handle `f().m()`).
The `class-self-attrs` seed path keeps the older skip-missing corpus lookup
(pre-existing).

Re-run the two commands above to reproduce the numbers; a lever PR should
update this block (or the committed baseline it points at) in the same change
that moves the number.

## Determinism

Sites are sorted by component, enclosing function, exact site string, shape and
op; the aggregations sort by count then printed key; the TSV header is fixed.
Two runs over the same corpus produce byte-identical TSV, so a lever's effect
is a diff, not a remembered number.
