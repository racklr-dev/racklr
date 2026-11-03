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
  the real context cannot resolve are counted: 202 call sites on the same
  corpus at the current revision.

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

## Baseline (observed 2026-11-02, racklr `ddb2915` + this change)

The last increment is the module-receiver `drg-module` change (memcache /
redis client handles and their methods, plus the `**kwargs` lowering). Its
measurement is below; the numbers before it were 305 sites / 292 functions /
73 files on the core probe and 4947 / 4000 / 478 on the full package.

`django/django/core` (the pinned baseline probe):

```
files scanned: 112   parsed: 112   failed: 0
functions: 979 (in 101 files with inferred functions)
unknown call sites: 202   unknown functions: 223   unknown files: 62
(this change: 305 -> 202 call sites, 292 -> 223 functions, 73 -> 62 files)

receiver shapes: bare-call 59, local 59, param 32, self-attr 23,
                 chained 13, call-result 10, self-method 6
local binding kinds (of 59): call-result 28, loop-var 12, attr 10,
                 literal 6, comp-var 2, unbound 1

levers:
  return-type-projection    50   parameter-dataflow        32
  self-attr-typing          23   element-type-dataflow     22
  bare-call-lexicon         16   higher-order-row-hole     15
  attribute-alias-typing    14   multi-level-chain         13
  literal-constructor-typing 10  mro-edge-resolution        6
  unbound-binding-resolution 1
```

`django/django` (907 files, the acceptance-bar corpus):

```
files scanned: 907   parsed: 907   failed: 0
functions: 9503 (in 570 files with inferred functions)
unknown call sites: 3764   unknown functions: 3549   unknown files: 453
(this change: 4947 -> 3764 call sites, 4000 -> 3549 functions,
 478 -> 453 files)
```

What moved, and what did not:

- The dominant family was the `**expr` argument/dict-unpack encoding
  (`kw-spread`), which the lowerer emits as a synthetic call node. Once the
  dialect declares it a synthetic callee (a language construct, not a
  runtime-dispatched call) the encoding stops counting as an unknown site:
  100 of the 103 removed core sites (1180 full-package rows) are `kw-spread`.
  This is also the ticket's "memcached kwargs chain": `self._class(..., **options)`
  in the memcache backends and `_pool_options = {..., **options}` in redis.
- The remaining 3 core sites are the module-receiver projections:
  `(self._lib.DriverInfo).add_upstream_driver` (module → class-ref DATA →
  pure `DriverInfo` method table) and `pipeline.expire` / `pipeline.mset`
  (`Redis.pipeline` result projection to the `Pipeline` cache type).
- The type-graph receiver alphabet moves `db` (core 165 -> 164 functions)
  because `RedisCacheClient`'s spurious `db` class capability and
  `RedisCacheClient.set_many`'s `db` entry are gone (the full-package graph now
  records db 4514 functions / 365 files). **`cache` does not rise** (core
  143 functions / 8 files, unchanged; the full-package graph records
  147 / 12): earlier increments had already classified these methods `cache`,
  so this change removes the `unknown` and the bogus `db` rather than
  re-classifying a non-cache row.
- The full-package TSV diff is 1183 removed rows and **no added rows**: no
  site gains `unknown`. The pinned-probe snapshot diff agrees: 70 changed
  `fn-effect-row` atoms (69 lose `unknown`, 1 keeps an already-present
  `unknown`), 11 file-level `unknown` links removed, and the only capability
  diff is the removal of `(RedisCacheClient) db`. No row gained any effect.
- Residual memcache/redis unknowns are exactly 4 core sites, all inside
  `RedisCacheClient`: `self._pool_class.from_url` and `self._serializer.dumps`
  / `loads`. Both attributes are OR-fallback defaults (`x or Default()`), so
  an explicitly passed `pool_class` / `serializer` is a genuinely
  caller-supplied receiver whose effects cannot be projected; they stay
  fail-closed `unknown` rather than borrowing the default's type.

The pinned baseline is re-pinned deliberately to the new graph:
`racket tools/baseline-check.rkt --update` -> `sha256:291dca50674fd1e45eb246e9cd794227d87ea7e21a39f6922d611ee8c6f6a1ed`
(4704 -> 4692 atom lines, 112 files). The gate answers
`baseline COVERAGE-FAILED sha256:291dca50…`: the graph matches the re-pinned
snapshot (the pass/fail half is green) while the Phase-0.3 unknown-elimination
obligation is still open (223 unknown functions / 62 unknown files, 3549 / 453
on the full package, `<= 0` required). The census in this document is the
aiming instrument for that open obligation, not a claim that it is discharged.

### Remaining surface after this lever

`bare-call-lexicon` falls from 116 to 16 core sites (1484 -> ~300 full
package) now that the `kw-spread` lowering is no longer counted. The largest
core families are `return-type-projection` (50), `parameter-dataflow` (32),
`self-attr-typing` (23) and `element-type-dataflow` (22). Within
`return-type-projection` the largest unresolved groups are `super().m()`
results (out-of-corpus parent-method classification, not return projection),
receiver-method returns limited by an unprojected receiver, imported/module
result-type DATA entries, and function values. `parameter-dataflow` is largely
outside call-chain reach: most of its sites have no statically visible
in-corpus call site (framework/dispatch-invoked callbacks and protocol
methods); the visible residual call sites pass locals/attributes, i.e.
upstream dataflow.

Two deliberate conservatisms: a method call projects only its most-derived
dispatch key, so an unprojected override fails closed instead of inheriting a
superclass projection, and the relation projection is not compositional
through call-result receivers (`x = f().m()` is not projected in the relations;
the classification-time receiver chain does handle `f().m()`). The
`class-self-attrs` seed path keeps the older skip-missing corpus lookup
(pre-existing).

Re-run the two commands above to reproduce the numbers; a lever PR should
update this block (or the committed baseline it points at) in the same change
that moves the number.

## Determinism

Sites are sorted by component, enclosing function, exact site string, shape and
op; the aggregations sort by count then printed key; the TSV header is fixed.
Two runs over the same corpus produce byte-identical TSV, so a lever's effect
is a diff, not a remembered number.
