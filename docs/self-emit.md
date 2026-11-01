# Self-emit: the compiler emits itself

P6.2 of the roadmap is the reflexive step: the compiler is a UIR program with
effect rows, and that program — the *self-image* — is emitted into a target
language through the same one emit contract every other unit uses. The driver
here does that end to end: it emits a compiler-as-UIR program through
`racklr/emit` into a target language, writes runnable source, runs it when a
target runtime is wired up, and checks that the effect rows appear in the
target's **native** vocabulary.

The seed compiler of this phase lives in `racklr/self/seed/` (built separately);
it is one self-image, not *the* driver's assumption.

## The seam to P6.1 (compiler-as-UIR)

`racklr/self-emit.rkt` never requires a concrete compiler. A self-image enters
the driver one of two ways:

1. **Registry.** A caller registers a named image that builds a UIR program:

   ```racket
   (register-self-image! 'kernel description (λ () compiler-uir))
   (self-image-ids)          ; '(kernel ...)
   (self-image-uir 'kernel)  ; the UIR program
   (self-image-description 'kernel)
   ```

   The CLI (`tools/emit-self.rkt`) registers the seed compiler as `kernel`; a
   test registers whatever image it exercises. P6.1 can register its own image
   with no driver change.

2. **Serialized UIR on disk.** `load-self-image-file` reads a program written
   with the UIR serializers: `uir->sexp` (bare or the versioned
   `(uir <version> <payload>)` envelope) for `--uir-format sexp`, and
   `uir->json` for `--uir-format json`. Both round-trip effect rows, so a
   registered image and its serialized form emit identically (the tests assert
   this for the seed image). The CLI exposes this as `--uir-file`, so an image
   can be emitted without linking it into Racket at all.

The driver stays free of a static `require` on any image module; registration
happens at the call site.

## Usage

CLI, from the repository root:

```sh
# Emit the seed compiler to Python under self-emitted/kernel/ and run it on the
# seed sample source; it prints the compiled Python program.
racket tools/emit-self.rkt --target python --run --stdin '(+ 1 (* 2 3))'

# Emit a serialized self-image instead of a registered one.
racket tools/emit-self.rkt --target python --out /tmp/kernel \
    --uir-file /tmp/kernel.sexp --run --stdin '(+ 1 (* 2 3))'

racket tools/emit-self.rkt --list
```

- `--target` (default `python`): any target `(supported-targets)` names.
- `--out` (default `self-emitted/<image>`, or the file stem for `--uir-file`).
- `--image` (default `kernel`) / `--uir-file` — mutually exclusive.
- `--uir-format sexp|json` (default `sexp`).
- `--run` runs the emitted source and reports path, rows, run status and the
  emitted program's own output; `--stdin` feeds it input.

Exit codes: `0` emitted (and ran) as requested; `1` verification failure — the
run failed, or a row-capable target did not render the image's rows natively;
`2` usage/emit error (unknown target or image, unreadable image, conflicting
flags). No silent passes.

Library entry points (`racklr/self-emit`):

```racket
(emit-self target uir out-dir #:row row #:filename fname) -> path
(self-emit-targets)                     -> (listof symbol)   ; from supported-targets
(target-filename target)                -> string
(target-row-marker target)              -> string | #f
(emitted-rows-native? target source)    -> boolean
(run-emitted-source path #:stdin s #:args a) -> (values boolean string)
(emit-self-and-run target uir out-dir #:stdin s #:filename f) -> hash
(load-self-image-file path #:format 'sexp|'json) -> uir
```

`emit-self-and-run` raises only on emit/write errors; a runtime failure is
reported in the hash (`'path 'target 'ok? 'output 'rows-native? 'source`).

## Target behavior and native row vocabulary

The row marker is derived from the emit contract's `emit-target-row-syntax`,
not from a duplicated target list. Koka is the one target with two legitimate
native forms: a typed `: <effects>` row when the function declares a result
type, and the documented `// @effects {...}` row otherwise (`emit-effects`
owns that spelling); both count as native.

| target | row syntax | native marker |
| --- | --- | --- |
| python | guard-decorator | `@__withEffects(` |
| javascript, html | guard-wrapper | ` __withEffects(` |
| koka | effect-annotation | ` : <` (typed row) or `// @effects` (documented row) |
| clojure | metadata | `:__effects__` |
| jsx, dom-js, json, effervesce | none | — (cannot carry a row) |

`emitted-rows-native?` is `#t` only when the target has a native vocabulary and
the emitted source contains one of its native markers. A `none` target is never claimed, and a
row-less image is not reported as a lost row. Emitting an explicit row to a
`none` target is refused by the emit contract instead of silently dropped.

## Mini loss model

Honest and explicit, not papered over:

- **Python runs.** Emitted Python is executed with `python3`, in the tests and
  in the CLI acceptance; that is the verified runnable target of this phase.
- **JavaScript/JSX/DOM-JS** have runtime plumbing (`.js` runs under `node`), but
  JS emission still has known gaps for arbitrary UIR (e.g. a one-argument call
  to a plain symbol renders as `f x`), so that is plumbing, not a verified
  runnable target for the seed image. A failed run is reported, not hidden.
- **Clojure and HTML** emit source through the same contract but have no
  runtime wired up here; `run-emitted-source` reports a failed run that says
  so, and the CLI exits nonzero under `--run`; it does not pretend the program
  ran.
- **JSON, JSX and DOM-JS** refuse to render program UIR at all (they target
  data / component trees, not executable programs), and they have no native
  row vocabulary; the CLI reports the emitter error and exits nonzero rather
  than claiming success.
- Per-target emitter coverage also varies. In particular, `emit-koka` today
  requires `uir-typed-param` parameters, while the seed image's functions
  carry plain `uir-symbol` parameters, so emitting the seed image to Koka
  fails with the emitter's error (the CLI exits 2 and reports it). Koka emits
  param-less images fine, rows native. The P6.2 acceptance target is Python.

## Tests

`racklr-test/self-emit-test.rkt` covers the driver on tiny inline fixture
programs (independent of the seed compiler): contract-identical file contents,
filenames and markers derived from the contract, real Python runs including
stdin and a raising program, the serialized-UIR round trip, and the registry.
The seed-compiler integration assertion registers `racklr/self/seed/compiler`
as `kernel` when the module is present, emits it to Python, runs it on the seed
sample source, and runs the program it compiles; it prints an explicit `SKIP`
line (never silently) when that module is absent.

Run from the repository root:

```sh
cd racklr-test && PLTCOLLECTS="$PWD/..:" racket self-emit-test.rkt
```
