#lang racket

;; End-to-end receiver-type dataflow for the TS/JS encodings: lower real
;; TypeScript source, then run whole-program effect inference over the UIR with
;; the JavaScript lexicon (the host's implicit receiver is `this`, not `self`).
;; This is the integration counterpart of the hand-built UIR tests in
;; effects-infer-test.rkt, and pins the lowering shapes the dataflow has to
;; read: JS/TS member access is a `(call "dot" ...)`, assignments are
;; `(call "=" ...)`, `?.` is a null-guard `if`, and `new C(...)` is `uir-new`.
;;
;; Run from inside racklr-test/ (the grammar path is relative to the cwd).

(require rackunit
         racklr/uir
         racklr/effects/contract
         racklr/effects/infer
         racklr/effects/lexicon
         racklr/effects/lexicons/javascript
         racklr/gen-test
         (prefix-in ts: racklr/lower-typescript))

(define-values (ts-parse ts-tokenize ts-tok-type ts-tok-value)
  (gen-and-load "../grammars-v4/javascript/typescript-cleaned/TypeScriptParser.g4"))

(define (ts->uir source)
  (ts:lower-program (ts-parse source) ts-tok-type ts-tok-value))

(define (rows-of uir)
  (infer-effects-all (list (cons "app.ts" uir)) #:lexicon javascript-lexicon))

(define (row-of uir key)
  (hash-ref (rows-of uir) (cons "app.ts" key)))

(define (row-names row)
  (sort (map (λ (e) (string->symbol (uir-symbol-name (uir-effect-entry-name e))))
             (uir-effect-row-entries row))
        symbol<?))

;; ── tests ───────────────────────────────────────────────────────────

(define (test-optional-chain-call)
  ;; `const s = new Service(); s?.m();` — the lowered guard's present branch
  ;; is the call; `new` seeds the receiver type and the call is an edge to
  ;; Service.m, whose io effect run inherits.
  (define row
    (row-of
     (ts->uir
      "class Service { m() { console.log(\"x\"); } }
       function run() { const s = new Service(); s?.m(); }")
     'run))
  (check-equal? (row-names row) '(io))
  (check-false (row-lookup row 'unknown)))

(define (test-optional-chain-this)
  ;; `this?.m()` inside a method resolves through the class MRO (io), not
  ;; unknown: `this` is the host implicit receiver and the guard's present
  ;; branch is the call.
  (define row
    (row-of
     (ts->uir
      "class Svc { m() { console.log(\"x\"); } run() { this?.m(); } }")
     'Svc.run))
  (check-equal? (row-names row) '(io))
  (check-false (row-lookup row 'unknown)))

(define (test-optional-chain-call-in-method)
  ;; The same dataflow in a plain member call `s.m()` (no `?.`): both encodings
  ;; of member access feed the same receiver-type rule.
  (define row
    (row-of
     (ts->uir
      "class Service { m() { console.log(\"x\"); } }
       function run() { const s = new Service(); s.m(); }")
     'run))
  (check-equal? (row-names row) '(io)))

(define (test-this-attr-assignment)
  ;; `this.cache = new Cache(); this.cache.m();` — the JS assignment encoding
  ;; is `(call "=" ...)`, whose member-access target seeds the implicit
  ;; receiver's self-attribute env exactly like Python's `self.cache = ...`;
  ;; the method call then resolves as an edge to Cache.m (io).
  (define row
    (row-of
     (ts->uir
      "class Cache { m() { console.log(\"x\"); } }
       class Svc { run() { this.cache = new Cache(); this.cache.m(); } }")
     'Svc.run))
  (check-equal? (row-names row) '(io))
  (check-false (row-lookup row 'unknown)))

;; ── runner ──────────────────────────────────────────────────────────

(define (main)
  (test-optional-chain-call)
  (test-optional-chain-this)
  (test-optional-chain-call-in-method)
  (test-this-attr-assignment)
  (printf "All TS infer tests passed.\n"))

(main)
