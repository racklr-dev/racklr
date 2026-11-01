#lang racket

;; racklr/loss-model.rkt — the declared loss model for absorb→emit.
;;
;; Two languages with different concurrency or memory semantics cannot
;; round-trip perfectly. Absorb then emit is identity *within a declared loss
;; model*, not a loss the tool hides. This module is that declaration: for
;; every supported source→target pair it says what is preserved, what is
;; approximated, and what is refused, and which normalizations the verifier
;; applies before it compares the before and after UIR.
;;
;; The model is data. The verifier (racklr/loss/verify.rkt) and the round-trip
;; harness (racklr-test/examples-roundtrip-test.rkt) read it; neither carries
;; pair-specific behavior. Changing translation behavior is a deliberate edit
;; here whose diff is the review.

(require racket/list
         racket/string)

(provide
 (struct-out loss-rule)
 (struct-out loss-model)
 loss-features
 loss-dispositions
 loss-normalizations
 supported-loss-models
 supported-source-languages
 loss-model-for
 loss-model-for-language
 supported-pair?
 loss-model->sexp
 loss-models->sexp
 loss-model-canonical-string
 loss-model-coverage-check)

;; ── The vocabulary ──────────────────────────────────────────────────
;;
;; A rule names a *feature* and a *disposition*. The verifier treats a
;; `preserved` feature as a hard obligation: a difference is a violation. An
;; `approximated` feature is allowed to differ (the detail says how), and a
;; `refused` feature is dropped on purpose. Dispositions describe the intended
;; translation; the verifier records whether the observed translation still
;; matches them.

;; A feature is a projection of the canonical UIR serialization the verifier
;; knows how to extract (see racklr/loss/verify.rkt).
(define loss-features
  '(definitions      ; top-level bound names (functions, classes, bindings)
    signatures       ; function name, arity, parameter names
    classes          ; class names and method names
    literals         ; string and number literal inventory
    control-flow     ; control-flow node census (if/for/while/try/...)
    effect-rows      ; per-function effect rows in the target's native form
    types            ; type-level declarations (interfaces, aliases, enums)
    jsx              ; JSX element tree before target emission
    statement-shape  ; how statements/assignments/operators are regrouped
    runtime-guards   ; emitter-injected effect-guard support code
    imports          ; import/export shape
    formatting      ; layout, indentation, statement re-flow
    comments))      ; source comments

(define loss-dispositions '(preserved approximated refused))

;; Normalizations the verifier applies to both sides before comparing. Each is
;; a pure UIR-serialization transform; `annotate-effects` is a pre-pass on the
;; absorbed side that reproduces what the emitters do (they infer rows at emit
;; time, so the absorbed UIR the emitter actually sees carries them).
(define loss-normalizations
  '(annotate-effects      ; run effect inference before emit (what emitters do)
    drop-generated        ; remove emitter-injected declarations/imports
    drop-effect-guards    ; remove effect-guard annotations from the structure
    drop-parens           ; parentheses are not semantic
    erase-js-types))      ; erase TypeScript type syntax for the JS target

;; ── Model data ──────────────────────────────────────────────────────

(struct loss-rule (feature disposition detail) #:transparent)
(struct loss-model (source target mode normalizations injected rules) #:transparent)
;; mode: 'exact    — normalized UIRs must be structurally equal.
;;       'projected — only `preserved` feature projections must match.
;; injected: top-level names/imports the emitter adds (dropped before comparison).

(define (rule feature disposition detail)
  (unless (memq feature loss-features)
    (error 'loss-model "unknown loss feature: ~a" feature))
  (unless (memq disposition loss-dispositions)
    (error 'loss-model "unknown disposition: ~a" disposition))
  (unless (and (string? detail) (not (string=? detail "")))
    (error 'loss-model "a loss rule needs a detail: ~a" feature))
  (loss-rule feature disposition detail))

(define (model source target mode normalizations injected rules)
  (loss-model source target mode normalizations injected rules))

;; Preserved is the common case; the helper keeps the declaration readable.
(define (preserved feature detail) (rule feature 'preserved detail))
(define (approximated feature detail) (rule feature 'approximated detail))
(define (refused feature detail) (rule feature 'refused detail))

;; The declared supported pairs. A pair is supported when the round-trip
;; harness has a complete absorb+emit+re-absorb pipeline for it; the model and
;; the harness's dispatch table are checked against each other by
;; `loss-model-coverage-check` and the harness itself.
(define supported-loss-models
  (list
   ;; Same-language round trips through UIR.
   (model 'python 'python 'exact
          '(annotate-effects drop-generated drop-effect-guards drop-parens)
          '("inspect")
          (list (preserved 'definitions
                           "top-level functions, classes, and module bindings keep their names")
                (preserved 'signatures
                           "function names, arity, and parameter names are unchanged")
                (preserved 'classes
                           "class and method names are unchanged")
                (preserved 'literals
                           "string and number literals survive unchanged")
                (preserved 'control-flow
                           "if/for/while/try/with/await/yield stay the same kind of node")
                (preserved 'effect-rows
                           "inferred rows are rendered as @__withEffects guards; the verifier re-absorbs that native form")
                (approximated 'runtime-guards
                              "emit inserts __perform/__withEffects/__handle and `import inspect`; the verifier strips them")
                (refused 'comments
                         "UIR carries no comments, so the Python front end drops them")
                (approximated 'formatting
                              "the emitter re-indents and re-flows statements")))
   (model 'javascript 'javascript 'exact
          '(annotate-effects drop-generated drop-effect-guards drop-parens)
          '()
          (list (preserved 'definitions
                           "top-level declarations keep their names")
                (preserved 'signatures
                           "function names, arity, and parameter names are unchanged")
                (preserved 'classes
                           "class and method names are unchanged")
                (preserved 'literals
                           "string and number literals survive unchanged")
                (preserved 'control-flow
                           "control-flow nodes keep their kind")
                (preserved 'effect-rows
                           "inferred rows are rendered as __withEffects(row, fn); the verifier re-absorbs that native form")
                (approximated 'runtime-guards
                              "emit inserts the effect-guard runtime; the verifier strips it")
                (refused 'comments
                         "UIR carries no comments, so the JavaScript front end drops them")
                (approximated 'formatting
                              "the emitter re-indents and re-flows statements")))
   (model 'clojure 'clojure 'exact
          '(drop-parens)
          '()
          (list (preserved 'definitions
                           "top-level def/defn names are unchanged")
                (preserved 'signatures
                           "function names, arity, and parameter names are unchanged")
                (preserved 'literals
                           "string and number literals survive unchanged")
                (preserved 'control-flow
                           "control-flow nodes keep their kind")
                (preserved 'effect-rows
                           "declared rows render as the {:__effects__ ...} attr-map and are re-absorbed")
                (refused 'comments
                         "UIR carries no comments, so the Clojure front end drops them")
                (approximated 'formatting
                              "the emitter collapses the original layout onto canonical lines")))
   (model 'json 'json 'exact
          '()
          '()
          (list (preserved 'literals
                           "object keys and string, number, boolean, and null values survive unchanged")
                (refused 'effect-rows
                         "JSON has no function or effect-row concept")
                (refused 'comments
                         "JSON has no comments")
                (approximated 'formatting
                              "the emitter re-serializes with its own spacing")))
   ;; TypeScript and TSX emit JavaScript: type-level syntax has no runtime
   ;; meaning in the target, and JSX is lowered before emission.
   (model 'typescript 'javascript 'projected
          '(annotate-effects drop-generated drop-effect-guards drop-parens
            erase-js-types)
          '()
          (list (preserved 'definitions
                           "runtime declarations keep their names after type erasure")
                (preserved 'signatures
                           "function names, arity, and parameter names are unchanged")
                (preserved 'classes
                           "class and method names survive type erasure")
                (preserved 'effect-rows
                           "inferred rows are rendered as __withEffects(row, fn); the verifier re-absorbs that native form")
                (approximated 'statement-shape
                              "the TypeScript and JavaScript front ends lower emitted assignment and infix chains into different statement shapes, so only declarations, signatures, classes, and effect rows are required to match")
                (approximated 'literals
                              "operator-chain regrouping can move literals between nodes")
                (refused 'types
                         "interfaces, type aliases, enums, and annotations have no JavaScript runtime form")
                (approximated 'runtime-guards
                              "emit inserts the effect-guard runtime; the verifier strips it")
                (refused 'comments
                         "UIR carries no comments, so the TypeScript front end drops them")
                (approximated 'formatting
                              "the emitter re-indents and re-flows statements")))
   (model 'tsx 'javascript 'projected
          '(annotate-effects drop-generated drop-effect-guards drop-parens)
          '()
          (list (preserved 'definitions
                           "component and helper names survive lowering")
                (preserved 'signatures
                           "component and helper names, arity, and parameter names are unchanged")
                (preserved 'effect-rows
                           "inferred rows are rendered as __withEffects(row, fn); the verifier re-absorbs that native form")
                (approximated 'jsx
                              "JSX elements lower to React.createElement calls before emit, so the re-absorbed UIR sees calls, not elements")
                (refused 'types
                         "interfaces, type aliases, and annotations have no JavaScript runtime form")
                (approximated 'literals
                              "prop text and attribute literals change shape with JSX lowering")
                (approximated 'control-flow
                              "JSX child expressions are re-shaped into call arguments")
                (approximated 'statement-shape
                              "the TypeScript and JavaScript front ends lower emitted assignment and infix chains into different statement shapes")
                (approximated 'runtime-guards
                              "emit inserts the effect-guard runtime; the verifier strips it")
                (refused 'comments
                         "UIR carries no comments, so the TSX front end drops them")
                (approximated 'formatting
                              "the emitter re-indents and re-flows statements")))
   (model 'koka 'koka 'exact
          '()
          '()
          (list (preserved 'definitions
                           "top-level function and type names are unchanged")
                (preserved 'signatures
                           "function names, arity, and parameter names are unchanged")
                (preserved 'control-flow
                           "control-flow nodes keep their kind")
                (preserved 'literals
                           "string and number literals survive unchanged")
                (preserved 'effect-rows
                           "Koka return-type rows carry effect names only on both sides, so the names survive the round trip")
                (refused 'comments
                         "UIR carries no comments, so the Koka front end drops them")
                (approximated 'formatting
                              "the emitter re-indents and re-flows statements")))))

;; ── Queries ─────────────────────────────────────────────────────────

(define (supported-source-languages)
  (remove-duplicates (map loss-model-source supported-loss-models)))

(define (loss-model-for-language lang)
  ;; The target language an absorbed source language is emitted into.
  (define m (findf (λ (x) (eq? (loss-model-source x) lang)) supported-loss-models))
  (if m m #f))

(define (loss-model-for source target)
  (or (findf (λ (m) (and (eq? (loss-model-source m) source)
                         (eq? (loss-model-target m) target)))
             supported-loss-models)
      #f))

(define (supported-pair? source target)
  (and (loss-model-for source target) #t))

;; ── Canonical serialization (for the artifact and the model digest) ──

(define (loss-rule->sexp r)
  `(rule ,(loss-rule-feature r)
         ,(loss-rule-disposition r)
         ,(loss-rule-detail r)))

(define (loss-model->sexp m)
  `(loss-model ,(loss-model-source m)
               ,(loss-model-target m)
               ,(loss-model-mode m)
               ,(loss-model-normalizations m)
               ,(loss-model-injected m)
               ,(map loss-rule->sexp (loss-model-rules m))))

(define (loss-models->sexp)
  (map loss-model->sexp supported-loss-models))

(define (loss-model-canonical-string)
  (string-join (map (λ (s) (format "~s" s)) (loss-models->sexp)) "\n"))

;; ── Integrity / coverage checks ─────────────────────────────────────

;; Every declared model must be total and self-consistent. The harness adds
;; the pipeline-side check (every pipeline it runs resolves to a model); this
;; one keeps the declaration itself honest and is unit-tested.
(define (loss-model-coverage-check)
  (define problems '())
  (define (note! fmt . args) (set! problems (cons (apply format fmt args) problems)))
  (for ([m (in-list supported-loss-models)])
    (unless (memq (loss-model-mode m) '(exact projected))
      (note! "~a→~a: unknown mode ~a"
             (loss-model-source m) (loss-model-target m) (loss-model-mode m)))
    (for ([n (in-list (loss-model-normalizations m))])
      (unless (memq n loss-normalizations)
        (note! "~a→~a: unknown normalization ~a"
               (loss-model-source m) (loss-model-target m) n)))
    (unless (memq 'effect-rows (map loss-rule-feature (loss-model-rules m)))
      (note! "~a→~a: no declared effect-row disposition"
             (loss-model-source m) (loss-model-target m)))
    (define-values (seen-features dup-features)
      (for/fold ([seen '()] [dups '()])
          ([r (in-list (loss-model-rules m))])
        (define f (loss-rule-feature r))
        (values (cons f seen)
                (if (memq f seen) (cons f dups) dups))))
    (for ([dup (in-list (remove-duplicates dup-features))])
      (note! "~a→~a: duplicate rule for ~a"
             (loss-model-source m) (loss-model-target m) dup))
    (for ([r (in-list (loss-model-rules m))])
      (unless (memq (loss-rule-feature r) loss-features)
        (note! "~a→~a: unknown feature ~a"
               (loss-model-source m) (loss-model-target m)
               (loss-rule-feature r)))))
  (define-values (_seen-pairs raw-dup-pairs)
    (for/fold ([seen '()] [dups '()])
        ([m (in-list supported-loss-models)])
      (define key (cons (loss-model-source m) (loss-model-target m)))
      (values (cons key seen) (if (member key seen) (cons key dups) dups))))
  (for ([d (in-list (remove-duplicates raw-dup-pairs))])
    (note! "duplicate model for pair ~a→~a" (car d) (cdr d)))
  (if (null? problems)
      (void)
      (error 'loss-model-coverage-check "~a" (string-join (reverse problems) "\n"))))
