# Content-addressed type-graph baseline

`type-graph.snapshot` is the canonical serialization of the shared baseline
run: the stable counter lines from
`racket effervesce/tools/type-graph.rkt django/django/core --dump`, followed by
one Atomese line per graph atom, sorted, with checkout paths rewritten relative
to the repository root. `type-graph.sha256` pins the SHA-256 of exactly those
bytes, so replay is an exact hash comparison instead of a judgment over
remembered integers.

| file                   | role                                          |
| ---------------------- | --------------------------------------------- |
| `type-graph.snapshot`  | canonical graph document (the hashed bytes)   |
| `type-graph.sha256`    | pinned content address, `sha256:<64 hex>`     |

## Gate

From the repository root, with the submodules initialised:

```
racket tools/baseline-check.rkt
```

Exit 0 prints `baseline OK sha256:<...>`. A mismatch prints the pinned and
current addresses, the summary counters that changed, and the exact atom lines
added or removed; it exits 1.

Prerequisites, the same as the pre-existing baseline command:

- `django` submodule: `git submodule update --init django`.
- the `grammars-v4` clone the Python front end reads:
  `grammars-v4/python/python3/` at the repository root.

`racket effervesce/tools/type-graph.rkt django/django/core` still prints the
full human-readable catalogue and is the right command for reading the graph;
it is no longer the contract.

## Reproducibility

The address depends only on the graph, not on where the checkout lives:
absolute paths in the dump are rewritten to repository-relative paths before
hashing, and atom lines are sorted. Two checkouts of the same commit (same
pinned `django` and `effervesce` revisions) compute the same address.

## Updating the pin

Re-pin only when the graph change is intended:

```
racket tools/baseline-check.rkt --update
```

The snapshot diff is then the review: every added, removed or changed atom line
is visible in the commit. Do not edit either file by hand.

## Design

Rainbow's `src/rainbow/canonical.clj` content-addresses its kernel by
deterministic serialization plus SHA-256, so checkpoints replay by exact hash
instead of remembered numbers. This baseline applies the same idea to the
type-graph projection: a deterministic document, a SHA-256 pin, and an exact
diff on drift.
