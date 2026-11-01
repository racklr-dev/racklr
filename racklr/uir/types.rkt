#lang racket

(provide
 ;; Tier 0: Core Computational IR
 uir-null uir-null?
 uir-bool uir-bool? uir-bool-value
 uir-number uir-number? uir-number-value
 uir-string uir-string? uir-string-value
 uir-fstring uir-fstring? uir-fstring-value
 uir-list uir-list? uir-list-items
 uir-record uir-record? uir-record-entries
 uir-symbol uir-symbol? uir-symbol-name
 ;; Functions and control flow
 uir-fn uir-fn? uir-fn-name uir-fn-params uir-fn-body
 uir-fn-return-type uir-fn-effects
 uir-typed-param uir-typed-param? uir-typed-param-name uir-typed-param-type uir-typed-param-default
 uir-call uir-call? uir-call-callee uir-call-args
 uir-let uir-let? uir-let-name uir-let-value uir-let-body
 uir-var uir-var? uir-var-name
 uir-set! uir-set!? uir-set!-name uir-set!-value
 uir-walrus uir-walrus? uir-walrus-name uir-walrus-value
 uir-ann-set! uir-ann-set!? uir-ann-set!-lhs uir-ann-set!-type uir-ann-set!-value
 uir-if uir-if? uir-if-test uir-if-then uir-if-else
 uir-block uir-block? uir-block-stmts
 uir-return uir-return? uir-return-value
 uir-for-each uir-for-each? uir-for-each-var uir-for-each-iterable uir-for-each-body uir-for-each-else-body
 uir-while uir-while? uir-while-test uir-while-body uir-while-else-body
 uir-try uir-try? uir-try-body uir-try-catches uir-try-else-body uir-try-finally-body
 uir-with uir-with? uir-with-items uir-with-body
 uir-await uir-await? uir-await-expr
 uir-yield uir-yield? uir-yield-value uir-yield-from?
 uir-decorated uir-decorated? uir-decorated-decorators uir-decorated-inner
 uir-get uir-get? uir-get-base uir-get-field
 uir-paren uir-paren? uir-paren-inner
 uir-type-alias uir-type-alias? uir-type-alias-name uir-type-alias-type-params uir-type-alias-value
 ;; Pattern matching (match/case)
 uir-match uir-match? uir-match-subject uir-match-cases
 uir-case uir-case? uir-case-pattern uir-case-guard uir-case-body
 uir-pat-literal uir-pat-literal? uir-pat-literal-value
 uir-pat-capture uir-pat-capture? uir-pat-capture-name
 uir-pat-wildcard uir-pat-wildcard?
 uir-pat-value uir-pat-value? uir-pat-value-path
 uir-pat-or uir-pat-or? uir-pat-or-alts
 uir-pat-as uir-pat-as? uir-pat-as-pattern uir-pat-as-name
 uir-pat-sequence uir-pat-sequence? uir-pat-sequence-elements
 uir-pat-star uir-pat-star? uir-pat-star-name
 uir-pat-mapping uir-pat-mapping? uir-pat-mapping-entries uir-pat-mapping-rest
 uir-pat-double-star uir-pat-double-star? uir-pat-double-star-name
 uir-pat-class uir-pat-class? uir-pat-class-cls uir-pat-class-positional uir-pat-class-keyword
 uir-pat-group uir-pat-group? uir-pat-group-pattern
 ;; Tier 1: OOP / Module IR
 uir-class uir-class? uir-class-name uir-class-super uir-class-fields uir-class-methods
 uir-method uir-method? uir-method-name uir-method-params uir-method-body uir-method-visibility
 uir-field uir-field? uir-field-name uir-field-type uir-field-init
 uir-new uir-new? uir-new-class uir-new-args
 uir-interface uir-interface? uir-interface-name uir-interface-methods
 uir-module uir-module? uir-module-name uir-module-imports uir-module-exports uir-module-body
 uir-import uir-import? uir-import-source uir-import-names
 uir-export uir-export? uir-export-names
 ;; Rust enums
 uir-enum uir-enum? uir-enum-name uir-enum-variants
 uir-enum-variant uir-enum-variant? uir-enum-variant-name uir-enum-variant-fields uir-enum-variant-discriminant
 ;; Tier 2: UI Component IR
 uir-component uir-component? uir-component-name uir-component-props uir-component-states uir-component-effects uir-component-template
 uir-element uir-element? uir-element-tag uir-element-attrs uir-element-children uir-element-events
 uir-attribute uir-attribute? uir-attribute-name uir-attribute-value
 uir-event uir-event? uir-event-name uir-event-handler
 uir-slot uir-slot? uir-slot-name uir-slot-fallback
 uir-text-node uir-text-node? uir-text-node-content
 uir-style uir-style? uir-style-styles
 uir-effect uir-effect? uir-effect-deps uir-effect-body
  uir-state uir-state? uir-state-name uir-state-init
  uir-jsx-expr uir-jsx-expr? uir-jsx-expr-content
  ;; Spread operator (...expr)
  uir-spread uir-spread? uir-spread-expr
  ;; Effect rows (contract algebra)
  uir-effect-row uir-effect-row? uir-effect-row-entries
  uir-effect-entry uir-effect-entry? uir-effect-entry-name uir-effect-entry-modality
  uir-effect-entry-ops uir-effect-entry-budget uir-effect-entry-scope
  uir-subtype uir-subtype? uir-subtype-left uir-subtype-right
  ;; Koka effect declarations and handlers (first-class typed effects)
  uir-effect-decl uir-effect-decl? uir-effect-decl-name uir-effect-decl-ops
  uir-effect-op uir-effect-op? uir-effect-op-name uir-effect-op-params uir-effect-op-result-type
  uir-effect-handler uir-effect-handler? uir-effect-handler-effect uir-effect-handler-ops
  uir-effect-handler-return-clause uir-effect-handler-body
  ;; Effervesce (declarative, check-only system descriptions)
  uir-eff-system uir-eff-system? uir-eff-system-name uir-eff-system-decls
  uir-eff-capability uir-eff-capability? uir-eff-capability-name uir-eff-capability-type-params uir-eff-capability-ops
  uir-eff-capability-op uir-eff-capability-op? uir-eff-capability-op-name uir-eff-capability-op-params uir-eff-capability-op-result-type
  uir-eff-ref uir-eff-ref? uir-eff-ref-name uir-eff-ref-type-args
  uir-eff-given uir-eff-given? uir-eff-given-row
  uir-eff-handler uir-eff-handler? uir-eff-handler-effect uir-eff-handler-row uir-eff-handler-ops
  uir-eff-component uir-eff-component? uir-eff-component-name uir-eff-component-needs uir-eff-component-provides uir-eff-component-ops
  uir-eff-state uir-eff-state? uir-eff-state-name uir-eff-state-type uir-eff-state-init
  uir-eff-action uir-eff-action? uir-eff-action-name uir-eff-action-steps
  uir-eff-invariant uir-eff-invariant? uir-eff-invariant-expr
  uir-eff-policy uir-eff-policy? uir-eff-policy-kind uir-eff-policy-predicate)

;; ── Tier 0: Core Computational IR ──────────────────────────────────

;; Literals
(struct uir-null () #:transparent)
(struct uir-bool (value) #:transparent)
(struct uir-number (value) #:transparent)  ;; string to preserve precision
(struct uir-string (value) #:transparent)
(struct uir-fstring (value) #:transparent)  ;; f-string literal

;; Data structures
(struct uir-list (items) #:transparent)    ;; list of uir?
(struct uir-record (entries) #:transparent) ;; list of (cons uir-string uir?)

;; Identifiers (for variable references, function names, field keys)
(struct uir-symbol (name) #:transparent)

;; Functions and control flow
(struct uir-fn (name params body return-type effects) #:transparent)  ;; name: #f or uir-symbol, return-type: #f or uir?, effects: #f or uir-effect-row
(struct uir-typed-param (name type default) #:transparent)   ;; name: uir-symbol, type/default: #f or uir?
(struct uir-call (callee args) #:transparent)    ;; callee: uir?, args: (listof uir?)
(struct uir-let (name value body) #:transparent) ;; name: uir-symbol, value: uir?, body: uir?
(struct uir-var (name) #:transparent)            ;; name: uir-symbol
(struct uir-set! (name value) #:transparent)     ;; name: uir-symbol, value: uir?
;; Annotated assignment: x: int = 5 or x: int (no value)
(struct uir-ann-set! (lhs type value) #:transparent)  ;; value can be #f for annotation-only
;; Assignment expression (Python walrus): (name := value). Unlike uir-set! or
;; uir-let this is an expression: it yields the assigned value where it appears.
(struct uir-walrus (name value) #:transparent)  ;; name: uir-var/uir-symbol, value: uir?
(struct uir-if (test then else) #:transparent)   ;; test/then/else: all uir?
(struct uir-block (stmts) #:transparent)         ;; stmts: (listof uir?)
(struct uir-return (value) #:transparent)        ;; value: uir?

;; For-each loop: for var in iterable: body (else: else-body)
(struct uir-for-each (var iterable body else-body) #:transparent)

;; While loop: test, body, optional else-body (uir-null if no else)
(struct uir-while (test body else-body) #:transparent)

;; Try/catch: try body, catch clauses, optional else and finally
(struct uir-try (body catches else-body finally-body) #:transparent)

;; With statement: context managers + body
(struct uir-with (items body) #:transparent)

;; Await expression
(struct uir-await (expr) #:transparent)

;; Yield expression: yield value (or yield from value)
(struct uir-yield (value from?) #:transparent)

;; Decorated definition
(struct uir-decorated (decorators inner) #:transparent)

;; Property access or subscript: base.field or base[key]
(struct uir-get (base field) #:transparent)

;; Parenthesized expression
(struct uir-paren (inner) #:transparent)
;; PEP 695 type alias declaration: `type Name[TypeParams] = value`.
;; name: uir-symbol, type-params: (listof uir-symbol), value: uir?
(struct uir-type-alias (name type-params value) #:transparent)

;; ── Match/Case (Python 3.10+) ────────────────────────────────────

;; Match statement
(struct uir-match (subject cases) #:transparent)
(struct uir-case (pattern guard body) #:transparent)

;; Pattern IR

;; Literal pattern
(struct uir-pat-literal (value) #:transparent)
;; Capture pattern
(struct uir-pat-capture (name) #:transparent)
;; Wildcard pattern
(struct uir-pat-wildcard () #:transparent)
;; Value pattern
(struct uir-pat-value (path) #:transparent)
;; OR pattern
(struct uir-pat-or (alts) #:transparent)
;; AS pattern
(struct uir-pat-as (pattern name) #:transparent)
;; Sequence pattern
(struct uir-pat-sequence (elements) #:transparent)
;; Star pattern
(struct uir-pat-star (name) #:transparent)
;; Mapping pattern
(struct uir-pat-mapping (entries rest) #:transparent)
;; Double-star pattern
(struct uir-pat-double-star (name) #:transparent)
;; Class pattern
(struct uir-pat-class (cls positional keyword) #:transparent)
;; Group pattern
(struct uir-pat-group (pattern) #:transparent)

;; ── Tier 1: OOP / Module IR ───────────────────────────────────────

;; Class definition
(struct uir-class (name super fields methods) #:transparent)
;; Method
(struct uir-method (name params body visibility) #:transparent)
;; Field declaration
(struct uir-field (name type init) #:transparent)
;; Object instantiation
(struct uir-new (class args) #:transparent)
;; Interface definition
(struct uir-interface (name methods) #:transparent)
;; Module
(struct uir-module (name imports exports body) #:transparent)
;; Import
(struct uir-import (source names) #:transparent)
;; Export
(struct uir-export (names) #:transparent)

;; Rust enum
(struct uir-enum (name variants) #:transparent)
;; Rust enum variant (fields as (listof uir-typed-param), discriminant as #f or uir?)
(struct uir-enum-variant (name fields discriminant) #:transparent)

;; ── Tier 2: UI Component IR ───────────────────────────────────────

;; Component
(struct uir-component (name props states effects template) #:transparent)
;; DOM element
(struct uir-element (tag attrs children events) #:transparent)
;; HTML attribute
(struct uir-attribute (name value) #:transparent)
;; Event binding
(struct uir-event (name handler) #:transparent)
;; Slot for component children
(struct uir-slot (name fallback) #:transparent)
;; Plain text content
(struct uir-text-node (content) #:transparent)
;; Inline or referenced styles
(struct uir-style (styles) #:transparent)
;; Reactive effect
(struct uir-effect (deps body) #:transparent)
;; Reactive state cell
(struct uir-state (name init) #:transparent)
;; JSX embedded expression
(struct uir-jsx-expr (content) #:transparent)

;; Spread operator (...expr)
(struct uir-spread (expr) #:transparent)

;; ── Effect Rows (contract algebra) ─────────────────────────────────

;; A single effect constraint: an opaque effect name plus a modality.
;;   name:     uir-symbol (opaque; never interpreted by racklr)
;;   modality: 'may | 'must | 'must-not | 'unknown
;;   ops:      (listof uir-symbol) — empty means "all operations"
;;   budget:   #f or integer (upper bound on invocations)
;;   scope:    #f or uir-symbol (resource / information-flow qualifier)
(struct uir-effect-entry (name modality ops budget scope) #:transparent)

;; An ordered set of effect entries attached to a function or mutator.
;; Absence of an entry is prohibition (must-not) by default.
(struct uir-effect-row (entries) #:transparent)

;; First-class subtype constraint: left ≤ right (left's may/budgets are
;; within right's). Survives lowering so downstream passes can check it.
(struct uir-subtype (left right) #:transparent)

;; ── Koka effect declarations and handlers ───────────────────────────

;; `effect name` declaration. ops: (listof uir-effect-op).
(struct uir-effect-decl (name ops) #:transparent)

;; An operation declared inside an `effect` block: `fun op(params) : result-type`.
;; params: (listof uir-typed-param), result-type: uir? (usually uir-symbol or #f).
(struct uir-effect-op (name params result-type) #:transparent)

;; A `with handler` expression. effect: #f or uir-symbol (explicitly named
;; handler binding; pass 1 leaves it #f). ops: list of `(name params body)`
;; triples — one per operation clause. return-clause: #f or uir (the
;; `return(x)` clause body). body: uir (the computation being handled).
(struct uir-effect-handler (effect ops return-clause body) #:transparent)

;; ── Effervesce (declarative, check-only system descriptions) ────────
;;
;; Effervesce is a check-only language for describing a system as typed
;; capabilities and rules. Unlike Koka's executable handlers, these nodes are
;; declarative: a capability is a named effect, `given` is a terminal
;; capability, `via` is an interpretation edge, and the checker (not built
;; here) walks that graph. The `uir-eff-` prefix avoids collision with the
;; React `uir-component`/`uir-state` and the Koka `uir-effect-*` nodes.

;; `system Name { ... }`. decls: (listof uir?) — one node per top-level decl.
(struct uir-eff-system (name decls) #:transparent)

;; `capability Read<A> { ... }`. name: uir-symbol. type-params: (listof
;; uir-symbol) (bare parameter names). ops: (listof uir-eff-capability-op).
(struct uir-eff-capability (name type-params ops) #:transparent)

;; An operation declared inside a `capability` block: `get() -> A`.
;; name: uir-symbol. params: (listof uir-typed-param). result-type: uir-symbol
;; or #f (the `-> type` is optional in the surface).
(struct uir-eff-capability-op (name params result-type) #:transparent)

;; A reference to a capability: `Read<Config>` or `Fs`.
;; name: uir-symbol. type-args: (listof uir-symbol), empty for a bare ref.
(struct uir-eff-ref (name type-args) #:transparent)

;; `given Fs, Log`. row: (listof uir-eff-ref).
(struct uir-eff-given (row) #:transparent)

;; `E via R { ... }`. effect: uir-eff-ref. row: (listof uir-eff-ref) (may be
;; empty). ops: (listof (list name params result-type body)) — one quad per
;; operation clause, where params is (listof uir-typed-param) and result-type
;; is uir-symbol or #f.
(struct uir-eff-handler (effect row ops) #:transparent)

;; `component C needs R provides P { ... }`. name: uir-symbol.
;; needs: (listof uir-eff-ref). provides: (listof uir-eff-ref), the capabilities
;; this component offers to concurrent peers. ops: same quad shape as handler.
(struct uir-eff-component (name needs provides ops) #:transparent)

;; `state cache: Config = expr`. name: uir-symbol. type: uir-symbol. init: uir.
(struct uir-eff-state (name type init) #:transparent)

;; `action A { ... }`. name: uir-symbol. steps: (listof uir) — each step is a
;; uir-set! (state update) or an effect-invocation expression.
(struct uir-eff-action (name steps) #:transparent)

;; `always expr`. expr: uir.
(struct uir-eff-invariant (expr) #:transparent)

;; `never predicate` / `must predicate`. kind: 'never | 'must. predicate: uir.
(struct uir-eff-policy (kind predicate) #:transparent)
