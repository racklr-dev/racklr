#lang racket

;; racklr/loss/pairs.rkt — the declared loss model, one record per supported
;; source→target pair.
;;
;; This is pure data. Changing what a pair preserves is an edit here whose diff
;; is the review. Every model declares every feature from
;; racklr/loss/declaration.rkt exactly once: `preserved` means a hard
;; obligation the verifier checks, `approximated` means the difference is
;; declared (the detail says how), and `refused` covers both intentional loss
;; and features the pair does not have.

(require racklr/loss/declaration)

(provide declared-loss-models)

(define (all-pairs . rules) rules)

(define declared-loss-models
  (list
   ;; ── Same-language round trips: exact mode ───────────────────────
   (loss-model 'python 'python 'exact
               '(annotate-effects drop-generated drop-effect-guards drop-parens)
               '("inspect")
               (all-pairs
                (preserved 'definitions "top-level functions, classes, and module bindings keep their names")
                (preserved 'signatures "function names, arity, and parameter names are unchanged")
                (preserved 'classes "class and method names are unchanged")
                (preserved 'literals "string and number literals survive unchanged")
                (preserved 'control-flow "if/for/while/try/with/await/yield stay the same kind of node")
                (preserved 'effect-rows "racklr/emit-effects renders inferred rows as @__withEffects guards plus a documented `# @effects {...}` annotation; the verifier re-absorbs the guard form")
                (preserved 'types "PEP 695 type aliases lower to type-alias nodes and are emitted back")
                (refused 'jsx "Python source cannot contain JSX")
                (preserved 'statement-shape "exact mode requires the normalized serialization to be equal")
                (approximated 'runtime-guards "emit inserts __perform/__withEffects/__handle and `import inspect`; the verifier strips them")
                (preserved 'imports "import statements keep their module and bound names")
                (approximated 'formatting "the emitter re-indents and re-flows statements")
                (refused 'comments "UIR carries no comments, so the Python front end drops them")))
   (loss-model 'javascript 'javascript 'exact
               '(annotate-effects drop-generated drop-effect-guards drop-parens)
               '()
               (all-pairs
                (preserved 'definitions "top-level declarations keep their names")
                (preserved 'signatures "function names, arity, and parameter names are unchanged")
                (preserved 'classes "class and method names are unchanged")
                (preserved 'literals "string and number literals survive unchanged")
                (preserved 'control-flow "control-flow nodes keep their kind")
                (preserved 'effect-rows "racklr/emit-effects renders inferred rows as __withEffects(row, fn) plus a documented `/** @effects {...} */` annotation; the verifier re-absorbs the guard form")
                (refused 'types "JavaScript has no type-level declarations")
                (refused 'jsx "JavaScript source in this pipeline has no JSX syntax")
                (preserved 'statement-shape "exact mode requires the normalized serialization to be equal")
                (approximated 'runtime-guards "emit inserts the effect-guard runtime; the verifier strips it")
                (preserved 'imports "import/export statements keep their shape")
                (approximated 'formatting "the emitter re-indents and re-flows statements")
                (refused 'comments "UIR carries no comments, so the JavaScript front end drops them")))
   (loss-model 'clojure 'clojure 'exact
               '(drop-parens)
               '()
               (all-pairs
                (preserved 'definitions "top-level def/defn names are unchanged")
                (preserved 'signatures "function names, arity, and parameter names are unchanged")
                (refused 'classes "Clojure source lowers to functions and data, not class nodes")
                (preserved 'literals "string and number literals survive unchanged")
                (preserved 'control-flow "control-flow nodes keep their kind")
                (preserved 'effect-rows "racklr/emit-effects renders rows as the {:__effects__ ...} defn attr-map; the verifier re-absorbs the metadata form")
                (refused 'types "Clojure has no type-declaration nodes in this lowering")
                (refused 'jsx "Clojure source cannot contain JSX")
                (preserved 'statement-shape "exact mode requires the normalized serialization to be equal")
                (refused 'runtime-guards "this emitter renders rows as metadata and injects no runtime guard code")
                (preserved 'imports "ns/require forms keep their module references")
                (approximated 'formatting "the emitter collapses the original layout onto canonical lines")
                (refused 'comments "UIR carries no comments, so the Clojure front end drops them")))
   (loss-model 'json 'json 'exact
               '()
               '()
               (all-pairs
                (refused 'definitions "JSON has no declarations or bindings")
                (refused 'signatures "JSON has no functions")
                (refused 'classes "JSON has no classes")
                (preserved 'literals "object keys and string, number, boolean, and null values survive unchanged")
                (refused 'control-flow "JSON has no control-flow nodes")
                (refused 'effect-rows "JSON has no function or effect-row concept")
                (refused 'types "JSON has no type-level declarations")
                (refused 'jsx "JSON cannot contain JSX")
                (preserved 'statement-shape "exact mode requires the normalized serialization to be equal")
                (refused 'runtime-guards "no emitter support code is injected for JSON")
                (refused 'imports "JSON has no imports")
                (approximated 'formatting "the emitter re-serializes with its own spacing")
                (refused 'comments "JSON has no comments")))
   (loss-model 'koka 'koka 'exact
               '()
               '()
               (all-pairs
                (preserved 'definitions "top-level function and type names are unchanged")
                (preserved 'signatures "function names, arity, and parameter names are unchanged")
                (refused 'classes "Koka lowers to functions and type declarations, not class nodes")
                (preserved 'literals "string and number literals survive unchanged")
                (preserved 'control-flow "control-flow nodes keep their kind")
                (preserved 'effect-rows "racklr/emit-effects renders the names as a typed return-type row (plus a documented `// @effects {...}` annotation when lossy); the verifier compares the typed names")
                (preserved 'types "Koka type declarations lower to enum nodes and are emitted back")
                (refused 'jsx "Koka source cannot contain JSX")
                (preserved 'statement-shape "exact mode requires the normalized serialization to be equal")
                (refused 'runtime-guards "the Koka emitter injects no runtime guard code")
                (preserved 'imports "import declarations keep their module references")
                (approximated 'formatting "the emitter re-indents and re-flows statements")
                (refused 'comments "UIR carries no comments, so the Koka front end drops them")))
   ;; ── One-way erasure to JavaScript: projected mode ───────────────
   (loss-model 'typescript 'javascript 'projected
               '(annotate-effects drop-generated drop-effect-guards drop-parens
                 erase-js-types)
               '()
               (all-pairs
                (preserved 'definitions "runtime declarations keep their names after type erasure")
                (preserved 'signatures "function names, arity, and parameter names are unchanged")
                (preserved 'classes "class and method names survive type erasure")
                (approximated 'literals "operator-chain regrouping can move literals between nodes")
                (approximated 'control-flow "the TypeScript and JavaScript front ends regroup emitted control constructs differently")
                (preserved 'effect-rows "racklr/emit-effects renders inferred rows as __withEffects(row, fn) plus a documented `/** @effects {...} */` annotation; the verifier re-absorbs the guard form")
                (refused 'types "interfaces and type aliases have no JavaScript runtime form; each erased declaration is documented as a comment")
                (refused 'jsx "TypeScript source in this pipeline has no JSX syntax")
                (approximated 'statement-shape "the two front ends lower emitted assignment and infix chains into different statement shapes")
                (approximated 'runtime-guards "emit inserts the effect-guard runtime; the verifier strips it")
                (approximated 'imports "type-only imports are erased and import shape can change with erasure")
                (approximated 'formatting "the emitter re-indents and re-flows statements")
                (refused 'comments "UIR carries no comments, so the TypeScript front end drops them")))
   (loss-model 'tsx 'javascript 'projected
               '(annotate-effects drop-generated drop-effect-guards drop-parens
                 erase-js-types)
               '()
               (all-pairs
                (preserved 'definitions "component and helper names survive lowering")
                (preserved 'signatures "component and helper names, arity, and parameter names are unchanged")
                (preserved 'classes "class components keep their class and method names")
                (approximated 'literals "prop text and attribute literals change shape with JSX lowering")
                (approximated 'control-flow "JSX child expressions are re-shaped into call arguments")
                (preserved 'effect-rows "inferred rows are rendered as __withEffects(row, fn); the verifier re-absorbs that native form")
                (refused 'types "interfaces and type aliases have no JavaScript runtime form; each erased declaration is documented as a comment")
                (approximated 'jsx "JSX elements lower to React.createElement calls before emit, so the re-absorbed UIR sees calls, not elements")
                (approximated 'statement-shape "the two front ends lower emitted assignment and infix chains into different statement shapes")
                (approximated 'runtime-guards "emit inserts the effect-guard runtime; the verifier strips it")
                (approximated 'imports "type-only imports are erased and JSX lowering can change import shape")
                (approximated 'formatting "the emitter re-indents and re-flows statements")
                (refused 'comments "UIR carries no comments, so the TSX front end drops them")))))
