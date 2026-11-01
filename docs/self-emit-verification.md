# Self-emitted tower verification (P6.3)

P6.3 asks two questions about the reflexive tower:

1. Does a translation through the **self-emitted** tower produce the same
   UIR-plus-row output as the original tower?
2. Does the emitted compiler pass the shared racklr + effervesce gate?

This document is the acceptance contract for both. It is executable:
`tools/self-emit.rkt` is the oracle for (1) and the gate runner for (2).

## The emitted-tower contract

An emitted compiler is any executable, in any target language, that obeys:

```
<tower> dump-uir <language> <source-file>
```

and prints exactly one JSON document — the canonical form below — on stdout,
exiting 0. Nothing else about the implementation is constrained; the command
may be a compiled binary, a script with an interpreter, or a wrapper.

Supported source languages are the ones with tracked grammars and a lowering
path that the original tower can run without a `grammars-v4` checkout:

| language | parser |
| --- | --- |
| `python` | `grammars/python3/Python3Parser.g4` (INDENT/DEDENT aware) |
| `json` | `racklr/gen-json-parser.rkt` (static) |

Adding a language means adding an arm in `tools/self-emit/oracle.rkt`; the
comparison format does not change.

## The canonical document

Both sides produce the same shape, defined in
`tools/self-emit/canonical.rkt`:

```json
{
  "format": "racklr-self-emit/1",
  "language": "python",
  "schema": 1,
  "uir": ["uir", 1, ...],
  "rows": [
    {
      "fn": "fetch",
      "entries": [
        {"effect": "network", "modality": "may", "ops": [], "budget": null, "scope": null}
      ]
    }
  ]
}
```

`uir` is the UIR tree under the versioned envelope that `racklr/uir` already
defines (`uir->versioned-sexp`). `rows` carries the inferred effect row for
every named function: effect kind, modality, typed ops, and the optional
budget/scope constraints. Object keys are sorted, rows are sorted by function
name, entries by effect name, and ops alphabetically, so equal translations
serialize identically in either implementation.

The comparison is over parsed JSON values, so whitespace and number formatting
are not part of the contract; a real difference names its path, e.g.
`$/rows/0/entries/0/effect: "network" vs "io"`.

## Commands

From the repository root, with `PLTCOLLECTS` covering the checkout (the gate
scripts set it; a plain `raco pkg link` also works):

```
racket tools/self-emit.rkt languages
racket tools/self-emit.rkt oracle python docs/examples/python/calc.py
racket tools/self-emit.rkt oracle python docs/examples/python/calc.py --out /tmp/calc.json
racket tools/self-emit.rkt goldens python docs/examples/python/*.py --out-dir /tmp/goldens
racket tools/self-emit.rkt verify --tower ./emitted-tower docs/examples/python/calc.py
racket tools/self-emit.rkt verify --tower ./emitted-tower        # built-in fixtures
racket tools/self-emit.rkt tower          # run the P6.2 emitted compiler
racket tools/self-emit.rkt gate
racket tools/self-emit.rkt selftest
```

Exit codes: `0` every translation matches / gate green; `1` a mismatch or a
red gate; `2` the check could not run (absent or failing emitted tower, bad
input, unsupported language).

`tower` is the P6.2 seam: it emits the seed self-image to Python, runs the
emitted compiler over the seed-language corpus, compares every translation and
compiled-program result with the original's pinned translation contract
(`racklr/self/seed/compiler`), checks that rows survive in Python's native
vocabulary, reads the emitted `@__withEffects(...)` guards back and compares
each function's row with the original UIR program's row (the UIR-plus-row
half), and reads the emitted source back through the tower — parse, lower,
infer — requiring a row on every named function. If P6.2 is not present in the
checkout it exits 2 and names the missing modules instead of passing silently.

`goldens` writes the original tower's canonical documents for a set of inputs.
A self-emitted tower can be developed against those files before the
differential run.

`selftest` proves the checker itself: the oracle is deterministic, a matching
tower is accepted, a tower that mutates one field is caught with the exact
JSON path, and an absent tower is reported as unrunnable.

## The gate

`racket tools/self-emit.rkt gate` runs, from the checkout under test:

- `racket tools/baseline-check.rkt` — the content-addressed type-graph pin.
  The gate reports its verdict rather than collapsing it to a boolean:
  `baseline OK sha256:<hash>` (graph matches the pin, zero-unknown obligation
  discharged) is green; `baseline COVERAGE-FAILED sha256:<hash>` (graph matches
  the pin but the Phase-0.3 unknown-elimination obligation is open) is
  reported as `gate: COVERAGE-FAILED` and exit 3, mirroring the baseline tool;
  `DRIFT`/`GATE-FAILED` fail the gate;
- the shared racklr suites: `effects-infer`, `effects-imports`,
  `effects-contract`, `effects-python-annot`, `uir`, `baseline-check`,
  `lower-json`, `emit-contract`;
- the effervesce suites: `library`, `check/check`, `check/verify`;
- `raco make` over `racklr/`, `racklr-test/`, `tools/`, `effervesce/`.

A suite passes only with exit 0 **and** no `FAILURE`/`ERROR` line in its
output: this repository treats a zero exit from `raco test` as unproven, so
the marker scan is part of the gate, not a belt-and-braces extra.

The reflexive half of the gate — the emitted tower's own source read back
through the tower — is `tools/self-emit/gate.rkt`'s `absorb-tower-source`:
parse → lower → infer, and every named function must carry a row (`unknown`
is permitted but counted, because it is a tracked state with an elimination
obligation, not a pass). Wire it into the gate once P6.2 produces the emitted
tower source.

## Status

P6.3's acceptance runs end to end on this branch. P6.2's self-emission work
(the seed self-image and the emit driver) merged to `main` as PR #94, and this
branch is rebased onto that `main` keeping only the P6.3 files. The checker
still treats an absent P6.2 as a reported state rather than a crash, so it can
be reviewed or run on a checkout without it.

| Requirement | Status |
| --- | --- |
| Canonical UIR-plus-row comparison | done (`tools/self-emit/`) |
| Oracle from the original tower | done (`languages`: python, json) |
| Checker self-test | done (`selftest`, `racklr-test/self-emit-verify-test.rkt`) |
| Shared gate runner | done (`gate`) |
| Emitted compiler to run | present (P6.2's seed self-image) |
| Translation through the self-emitted tower | **verified** on 8/8 corpus cases |
| Row-for-row parity with the original UIR program | **verified** on 7/7 functions |
| Emitted source read back through the tower | **verified**: 17/17 functions carry a row, 6 unknown tracked |
| Shared gate | suites 15/15 and `raco make` green; baseline graph matches the pin, with the repository's documented Phase-0.3 coverage obligation open (`COVERAGE-FAILED`; 236 unknown functions in 65 files on the Django-core probe) |

Observed transcript (the emitted compiler is a real Python program built from
the UIR self-image, not a fixture):

```
$ racket tools/self-emit.rkt tower
emitted compiler: /tmp/.../seed-compiler.py
rows native in emitted source: yes
effect rows vs original UIR program: MATCH (7 functions)
absorb emitted source: OK (17/17 functions carry a row; 6 unknown)
MATCH     "(+ 1 (* 2 3))" -> "print((1 + (2 * 3)))"
MATCH     "(- 10 4)" -> "print((10 - 4))"
MATCH     "(* 2 (+ 3 4))" -> "print((2 * (3 + 4)))"
MATCH     "(foo 1 2)" -> "print(foo(1, 2))"
MATCH     "(+ 1 2 3)" -> "print(+(1, 2, 3))"
MATCH     "42" -> "print(42)"
MATCH     "hello" -> "print(hello)"
MATCH     "(+ 1 2) (* 3 4)" -> "print((1 + 2))\nprint((3 * 4))"

8/8 translations match the original contract
```

The remaining stricter half is `verify --tower`, which needs an emitted tower
that can dump UIR-plus-row JSON (`<tower> dump-uir <language> <file>`). P6.2's
seed compiler translates into target source rather than UIR JSON, so `tower`
compares the native `@__withEffects(...)` guards against the original UIR
program's rows instead. When a full self-image (P6.1, issue #38) is emitted and
can run as a translator, `verify --tower` is the check to add on top.

## Known limitations

- The static JSON front end mis-tokenizes strings that contain characters in
  the escape class it was generated with (`u`, `0`, `1`, `F`, backslash are
  treated as excluded code points; a `count` key is enough to fail). This is a
  pre-existing `gen-json-parser` generation bug, not a P6.3 change; the JSON
  fixture in `tools/self-emit/fixtures/` avoids it. Python is the primary
  language of the comparison and is unaffected.
- `raco setup` is expressed as `raco make` in the gate because a git worktree
  can be shadowed by packages linked into the host Racket installation; the
  explicit-path `raco make` still resolves collections through `PLTCOLLECTS`.
- Only `python` and `json` are wired into the oracle. JavaScript, TypeScript,
  TSX, Clojure, and Koka grammars are either untracked (`grammars-v4/`) or not
  yet a stable lowering path; adding them is an oracle arm plus a fixture.
