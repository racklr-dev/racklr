#lang racket

;; racklr/effects/whole-program.rkt
;;
;; Whole-program (cross-file) effect inference: a single monotone fixpoint over
;; the union of every component's call graph, with component-qualified node
;; identity and cross-file superclass resolution. The cross-file callee
;; classifier lives in classify-all.rkt; the type/effect relation
;; materialization and its deterministic join live in type-relations.rkt; this
;; module is the fixpoint driver that collects the tables, joins the relations,
;; classifies every call site (with the row-hole / dynamic refinements), and
;; folds the call graph through the shared evaluator in invocation-row.rkt.

(require racklr/uir
         racklr/effects/class-resolve
         racklr/effects/collect
         racklr/effects/walk
         racklr/effects/rows
         racklr/effects/invocation-row
         racklr/effects/invocation-scope
         (prefix-in alg: racklr/effects/algebra/main)
         racklr/effects/shape
         racklr/effects/receiver-type
         racklr/effects/classify-all
         racklr/effects/type-relations
         racklr/effects/library-tokens
         (only-in racklr/effects/library-stdlib-types declared-param-effect-rows
                  declared-function-effects-data)
         racklr/effects/imports
         racklr/effects/labels
         racklr/effects/provenance
         racklr/effects/lexicon)

(provide infer-effects-all classify-call-all)

;; Binding-kind helpers for the `#:unknown-tally` census: what kind of value a
;; local variable was bound from, so the census can attribute a `local`
;; receiver to the lever that would resolve it (call-result projection,
;; attribute copy, alias, loop variable, function value, or literal).
;; `unbound` means the name is not assigned in the body (a global, import,
;; closure capture, or class-level name).
;;
;; Each tally record is one call site that the REAL classification (whole
;; program, import-aware, receiver-typed) left `unknown`:
;;
;;   (component function shape op receiver binding-kind site higher-order?)
;;
;; shape/op/receiver/binding-kind are the aiming data (classify-all.rkt's
;; receiver-shape); `site` is the exact callee site string (shape.rkt's
;; callee-site-string), so a census names a call site instead of an aggregate
;; count; `higher-order?` is #t only when a bare callee names one of the
;; enclosing function's parameters (the row-hole lever). A bare callee that is
;; a local bound from a function value carries binding-kind 'fn (the
;; function-value-typing lever) and a free/global/imported name carries
;; 'unbound (the lexicon lever), so the three bare-call cases are separable.

(define (var-symbol v)
  (cond
    [(uir-var? v) (string->symbol (uir-symbol-name (uir-var-name v)))]
    [(uir-symbol? v) (string->symbol (uir-symbol-name v))]
    [else #f]))

(define (binding-value-kind v)
  (cond
    [(wrapped-fn v) 'fn]
    [(uir-call? v) 'call-result]
    [(uir-get? v) 'attr]
    [(uir-var? v) 'alias]
    [(uir-symbol? v) 'alias]
    [(uir-fn? v) 'fn]
    [else 'literal]))

;; Comprehension loop variable: `(call "TYPE-comp" (list result loop-var iter
;; filter))` binds `loop-var` over the iterable, so an element method call
;; inside the result expression is a comp-var (needs element-type dataflow),
;; not an unbound name. Returns the loop-var uir-symbol, or #f.
(define (comp-loop-var node [tokens default-effect-tokens])
  (and (uir-call? node)
       (let ([callee (uir-call-callee node)])
         (and (uir-symbol? callee)
              (set-member? (tokens-comprehension-callees tokens)
                           (uir-symbol-name callee))
              (pair? (cdr (uir-call-args node)))
              (second (uir-call-args node))))))

;; Whole-program effect rows: (component . name) → uir-effect-row, computed by
;; a single fixpoint over the union of every program's call graph. Takes a list
;; of `(component . program)` pairs.
(define (infer-effects-all comp-programs
                           #:import-env [import-env #f]
                           #:unknown-tally [unknown-tally #f]
                           #:provenance-record [rec-box #f]
                           #:lexicon [lex default-lexicon]
                           #:declared-param-effects [declared-param-effects
                                                    declared-param-effect-rows]
                           #:known-pure-names [known-pure-names '()]
                           #:declared-function-effects [declared-function-effects
                                                    declared-function-effects-data])
  (define tokens (lexicon-tokens lex))
  (define fns (make-hash))           ;; (cons comp name) → body
  (define methods (make-hash))       ;; (cons comp name) → body
  (define comp-supers (make-hash))   ;; (cons comp class) → (listof super-ref: symbol | (first . full-dotted))
  (define class-defs (make-hash))    ;; class-name → (listof comp)
  (define fn-defs (make-hash))       ;; fn-name → (listof comp)
  (define fn-parents (make-hash))    ;; (cons comp name) → enclosing node name
  (define fn-params (make-hash))     ;; (cons comp name) → params
  (define method-params (make-hash)) ;; (cons comp name) → params
  (for ([cp (in-list comp-programs)])
    (define comp (car cp))
    (define program (cdr cp))
    (for ([(n body) (in-hash (collect-functions program))])
      (hash-set! fns (cons comp n) body)
      (hash-update! fn-defs n (λ (l) (cons comp l)) '()))
    (for ([(n ps) (in-hash (collect-function-params program))])
      (hash-set! fn-params (cons comp n) ps))
    (for ([(n parent) (in-hash (collect-fn-parents program))])
      (hash-set! fn-parents (cons comp n) parent))
    (for ([(n body) (in-hash (collect-methods program))])
      (hash-set! methods (cons comp n) body))
    (for ([(n ps) (in-hash (collect-method-params program))])
      (hash-set! method-params (cons comp n) ps))
    (for ([(n _) (in-hash (collect-class-names program))])
      (hash-update! class-defs n (λ (l) (cons comp l)) '()))
    (for ([(n chain) (in-hash (collect-supers program))])
      (hash-set! comp-supers (cons comp n) chain)))

  ;; Resolve every super reference to its (component . real-class-name) pair: a
  ;; bare name resolves via class-defs or an import alias (`from x import C as
  ;; Alias`); a qualified name (`base.Serializer`) follows the module alias
  ;; through the import graph to the class it names. Pairs (not bare symbols)
  ;; are what every super-chain consumer now reads.
  (define mp->comp (modpath-index import-env))
  (for ([(k chain) (in-hash comp-supers)])
    (hash-set! comp-supers k
               (for/list ([s (in-list chain)])
                 (resolve-super-ref s (car k) class-defs import-env mp->comp))))

  ;; Materialize the type relations and close them with the deterministic join:
  ;; assign (locals), self-attr (per-class attrs), param (concrete call-chain
  ;; param types), and return (projected return class).
  (define tr (materialize-type-relations comp-programs class-defs import-env
                                         #:tokens tokens))
  (close-type-relations! tr
                         #:comp-programs comp-programs
                         #:class-defs class-defs
                         #:import-env import-env
                         #:fns fns
                         #:methods methods
                         #:fn-params fn-params
                         #:method-params method-params
                         #:fn-defs fn-defs
                         #:comp-supers comp-supers
                         #:fn-parents fn-parents
                         #:tokens tokens)
  (define assign (type-relations-assign tr))
  (define self-attr (type-relations-self-attr tr))
  (define return-types (type-relations-return tr))
  (define elem-specs (type-relations-elem-specs tr))
  (define globals (type-relations-globals tr))
  (define assign-fn (type-relations-assign-fn tr))
  (define self-attr-fn (type-relations-self-attr-fn tr))
  (define param (type-relations-param tr))

  (define known (make-hash))
  (for ([(k _) (in-hash fns)]) (hash-set! known k #t))
  (for ([(k _) (in-hash methods)]) (hash-set! known k #t))
  ;; Host-injected names (e.g. the compiler's own schema-generated UIR
  ;; constructors/predicates/accessors, which lower-racket sees only as a
  ;; macro use): resolve a bare call as a call-graph edge to a bodiless stub,
  ;; whose empty row classifies the call as pure. This is a host-neutral
  ;; injection point; the caller supplies the vocabulary, the engine carries
  ;; no name literals.
  (define known-pure-comp 'known-pure-stub)
  (for ([nm (in-list known-pure-names)])
    (hash-set! known (cons known-pure-comp nm) #t)
    (hash-update! fn-defs nm (λ (l) (cons known-pure-comp l)) '()))

  (define all-keys (append (hash-keys fns) (hash-keys methods)))

  ;; Per-key receiver env: locals from the closed assign relation, self-attrs
  ;; from the class's MRO-closed self-attr relation.
  (define (owner-of-key k)
    (or (and (hash-has-key? methods k) (class-name-of-name (cdr k)))
        (fn-owner-class (cdr k)
                        (λ (x) (hash-ref fn-parents (cons (car k) x) #f)))))

  ;; Closure-capture globals for the CLASSIFICATION pass: the closed assign
  ;; relation holds each frame's own locals (the type-relations join already
  ;; seeded enclosing params into the enclosing frame's locals), so a nested
  ;; function's free variables resolve to its enclosing frames' locals here,
  ;; exactly as the join's rebuild-assign! did when it typed the assignments.
  (define (closure-globals-of k comp base-globals)
    (define out (hash-copy base-globals))
    (define seen (make-hash))
    (let loop ([nm (cdr k)])
      (define parent (and nm (hash-ref fn-parents (cons comp nm) #f)))
      (when (and parent (not (hash-has-key? seen parent)))
        (hash-set! seen parent #t)
        (define pk (cons comp parent))
        (for ([(n cls) (in-hash (hash-ref assign pk (hash)))])
          (unless (hash-has-key? out n) (hash-set! out n cls)))
        (for ([i (in-naturals)] [p (in-list (or (hash-ref fn-params pk #f)
                                                 (hash-ref method-params pk #f)))])
          (define pnm (cond
                        [(uir-symbol? p) (string->symbol (uir-symbol-name p))]
                        [(uir-typed-param? p)
                         (and (uir-symbol? (uir-typed-param-name p))
                              (string->symbol (uir-symbol-name (uir-typed-param-name p))))]
                        [else #f]))
          (define cls (hash-ref param (cons pk i) #f))
          (when (and pnm cls (not (hash-has-key? out pnm)))
            (hash-set! out pnm cls)))
        (loop parent)))
    out)

  (define (rt-of k)
    (define owner (owner-of-key k))
    (define class-env
      (and owner (class-attr-env (car k) owner self-attr class-defs comp-supers
                                 self-attr-fn)))
    (receiver-types (hash-ref assign k (hash))
                    (or (and class-env (receiver-types-self-attrs class-env)) (hash))
                    (hash-ref elem-specs k (hash))
                    (closure-globals-of k (car k) (hash-ref globals (car k) (hash)))
                    (hash-ref assign-fn k (hash))
                    (or (and class-env (receiver-types-fn-self-attrs class-env)) (hash))))

  ;; Declared EFFECT for a parameter this function bare-calls: the host
  ;; protocol contract (ASGI/WSGI/socketserver transport callbacks) stated as
  ;; DATA, read only when the bare-call shape is a param. Returns an effect
  ;; domain symbol or #f. Absent from the table (polymorphic callbacks whose
  ;; effect is the caller's, not the host's) stays a row hole, exactly as
  ;; before.
  (define (declared-param-effect-row comp name param)
    (and import-env declared-param-effects param
         (let* ([modpath (hash-ref (import-env-comp-modpath import-env) comp #f)]
                [entry (and modpath
                            (hash-ref declared-param-effects
                                      (cons modpath name) #f))]
                [hit (and entry (assq param entry))])
           (and hit (cdr hit)))))

  ;; Declared function effect: the producer (reducer) names a function's
  ;; effect domain directly when inference is structurally blind. Keyed by
  ;; (module-path . qualified-name-symbol), grounded as a declared capability
  ;; (provenance kind 'declared), never a silent default. It is AUTHORITATIVE:
  ;; the named set replaces the structural inference (which can carry a coarse
  ;; name-table false positive). `pure` means the empty row.
  (define (declared-function-fx k)
    (and declared-function-effects
         (let* ([comp (car k)] [name (cdr k)]
                [modpath (and import-env
                               (hash-ref (import-env-comp-modpath import-env)
                                         comp #f))])
           (hash-ref declared-function-effects (cons (or modpath comp) name) #f))))
  (define (row-has-unknown? uir-row)
    (for/or ([e (in-list (uir-effect-row-entries uir-row))])
      (eq? (entry-effect-name e) 'unknown)))
  ;; UIR-row replacement for a residual unknown the fixpoint joins in from a
  ;; non-declared callee: declared domains are permissions (`may`), `pure` is
  ;; the absence of an effect.
  (define (uir-row-replace-unknown uir-row fx)
    (uir-effect-row
     (append (filter (λ (e) (not (eq? (entry-effect-name e) 'unknown)))
                     (uir-effect-row-entries uir-row))
             (uir-effect-row-entries (effects->row (filter (λ (x) (not (eq? x 'pure)))
                                                           (if (list? fx) fx (list fx))))))))

  ;; Pass 3: classification + call-graph edges.
  (define bodies (make-hash))       ;; key → body
  (define class-tables (make-hash)) ;; key → (eq-hash call-node → classification)
  (define blessed-caps (make-hash)) ;; key → declared effect-cap, if any
  (define edges (make-hash))
  ;; Row-hole wiring: the params each function bare-calls (its holes) and the
  ;; argument list at every call to a known callee (the substitution sites).
  (define called-params (make-hash))  ;; k -> (listof param-name-symbol)
  (define edge-sites (make-hash))     ;; k -> (listof (cons callee-key args))
  ;; Structured provenance: every call-graph edge (with the callee expression
  ;; the classifier saw) and every direct ground. Propagation needs no path
  ;; storage — the fixpoint only moves effects along these edges, so reachability
  ;; over the recorded call graph is exactly a provenance path.
  (define prov (and rec-box (make-provenance)))
  (for ([k (in-list all-keys)])
    (define body (if (hash-has-key? fns k) (hash-ref fns k) (hash-ref methods k)))
    (define comp (car k))
    (define owner (owner-of-key k))
    (define rt (rt-of k))
    (define blessed (and (hash-has-key? methods k)
                        (method-cap k import-env lex)))
    (when blessed (hash-set! blessed-caps k blessed))
    (define class-table (make-hash))
    ;; A blessed capability is asserted at the method definition by the DATA
    ;; config, so it is its own ground: the tokens are the module path, class,
    ;; and method the capability was derived from.
    (when (and prov blessed)
      (define parts (string-split (symbol->string (cdr k)) "."))
      (provenance-add-ground!
       prov
       (make-declared-site (function-fq k)
                           (effect-cap-domain blessed)
                           (filter values (list (and import-env
                                                     (hash-ref (import-env-comp-modpath import-env)
                                                               comp #f))
                                                (first parts)
                                                (last parts)))
                           (first parts)
                           #:file comp)))
    ;; A declared function effect is grounded once, at its definition site.
    (when (and prov (declared-function-fx k))
      (for ([d (in-list (filter (λ (x) (not (eq? x 'pure)))
                                (if (list? (declared-function-fx k))
                                    (declared-function-fx k)
                                    (list (declared-function-fx k)))))])
        (provenance-add-ground!
         prov
         (make-declared-site (function-fq k) d
                             (list (car k) (symbol->string (cdr k))) #f
                             #:file (car k)))))
    (define callees '())
    (define call-sites '())
    (define param-names
      (let ([ps (or (hash-ref fn-params k #f) (hash-ref method-params k #f))])
        (and ps
             (for/list ([p (in-list ps)]
                        #:when (and (uir-typed-param? p)
                                    (uir-symbol? (uir-typed-param-name p))))
               (string->symbol (uir-symbol-name (uir-typed-param-name p)))))))
    (define local-kinds (make-hash))   ;; var -> binding-kind, maintained in walk order
    ;; Stable positional discriminator of a site: the ordinal of the call node
    ;; in this body's pre-order walk (source structure only, identical across
    ;; runs). Reset per function.
    (define call-position 0)
    (define (visit! node)
      ;; Track what each local was bound from, so an unknown `local` receiver
      ;; can be attributed to its binding kind (dominant assignment).
      (cond
        [(and (uir-set!? node) (var-symbol (uir-set!-name node)))
         => (λ (lhs) (hash-set! local-kinds lhs (binding-value-kind (uir-set!-value node))))]
        [(and (uir-ann-set!? node) (uir-ann-set!-value node)
              (var-symbol (uir-ann-set!-lhs node)))
         => (λ (lhs) (hash-set! local-kinds lhs (binding-value-kind (uir-ann-set!-value node))))]
        [(and (uir-for-each? node) (var-symbol (uir-for-each-var node)))
         => (λ (v) (hash-set! local-kinds v 'loop-var))]
        [(comp-loop-var node tokens)
         => (λ (lv)
              (cond
                [(uir-list? lv)
                 (for ([v (in-list (uir-list-items lv))])
                   (define vs (var-symbol v))
                   (when vs (hash-set! local-kinds vs 'comp-var)))]
                [else
                 (define vs (var-symbol lv))
                 (when vs (hash-set! local-kinds vs 'comp-var))]))]
        [(and (uir-let? node) (uir-symbol? (uir-let-name node)))
         (hash-set! local-kinds
                    (string->symbol (uir-symbol-name (uir-let-name node)))
                    (binding-value-kind (uir-let-value node)))]
        [(uir-with? node)
         (for ([item (in-list (uir-with-items node))])
           (define v (and (pair? item) (var-symbol (second item))))
           (when v
             (hash-set! local-kinds v
                        (binding-value-kind (and (pair? item) (first item))))))]
        [(uir-try? node)
         (for ([c (in-list (uir-try-catches node))])
           (define v (and (pair? c) (var-symbol (second c))))
           (when v (hash-set! local-kinds v 'exception)))]
        [else (void)])
      (cond
        [(uir-call? node)
         (define position call-position)
         (set! call-position (add1 call-position))
         (let* ([callee (uir-call-callee node)]
                ;; The classifier's coverage side channel (only needed when
                ;; provenance is recorded): a reused box is reset per call.
                [cov-box (and prov (box 'structural))]
                [cls (classify-call-all callee known methods
                                        class-defs fn-defs comp-supers self-attr comp owner
                                        import-env rt return-types cov-box
                                        #:lexicon lex)]
                [site (callee-site-string callee)])
           (case (car cls)
             [(edge)
              (hash-set! class-table node cls)
              (set! callees (cons (cdr cls) callees))
              (set! call-sites (cons (cons (cdr cls) (uir-call-args node)) call-sites))
              (when prov
                (provenance-add-call! prov
                                      (prov-call (function-fq k)
                                                 (function-fq (cdr cls))
                                                 site
                                                 (make-call-origin (function-fq k) callee
                                                                   #:file comp
                                                                   #:position position))))]
             [(effect)
              (hash-set! class-table node cls)
              (when prov
                ;; The coverage label records what the classifier needed to
                ;; establish this ground: no receiver dataflow (a structural
                ;; rule or lexicon hit) is for-all 'universal; a receiver
                ;; resolved through a parameter is a 'census-param worked
                ;; instance; a receiver resolved from a tracked local /
                ;; attribute / call-result value is 'local-literal.
                (provenance-add-ground!
                 prov
                 (make-callee-site (function-fq k) (cdr cls) callee
                                   #:coverage (ground-coverage
                                               callee
                                               (and cov-box (unbox cov-box))
                                               param-names
                                               tokens)
                                   #:file comp
                                   #:position position)))]
             [(pure)
              (hash-set! class-table node cls)]
             [(unknown)
              ;; The bare-call-to-parameter shape is the row-hole lever: a
              ;; callback param stands for "the effects of whatever is passed",
              ;; so it becomes a hole, instantiated by callers (the edge
              ;; fixpoint in invocation-row.rkt) instead of `unknown`. The same
              ;; shape info feeds the census tally, so it is computed up front.
              (define shape (receiver-shape callee param-names tokens))
              (define rx
                (and (member-access-callee? callee) (dotted-root callee)))
              (define immediate-base
                (and (uir-get? callee) (var-symbol (uir-get-base callee))))
              (define bare-name
                (cond
                  [(uir-var? callee)
                   (string->symbol (uir-symbol-name (uir-var-name callee)))]
                  [(uir-symbol? callee)
                   (string->symbol (uir-symbol-name callee))]
                  [else #f]))
              (define binding-kind
                (cond
                  [(eq? shape 'param) 'param]
                  [(and (eq? shape 'bare-call) bare-name param-names
                        (memq bare-name param-names))
                   'param]
                  [(and (eq? shape 'bare-call) bare-name)
                   (hash-ref local-kinds bare-name 'unbound)]
                  [immediate-base (hash-ref local-kinds immediate-base 'unbound)]
                  [else #f]))
              ;; `higher-order?` marks the callback-parameter lever (row holes);
              ;; a bare call to a function-valued local is the separate
              ;; function-value-typing lever.
              (define higher-order?
                (and (eq? shape 'bare-call) (eq? binding-kind 'param)))
              ;; A bare-called LOCAL that holds a FUNCTION VALUE (bound from a
              ;; call result, a function literal, an attribute, or a loop
              ;; variable) is a runtime-dispatched callback exactly like a
              ;; bare-called param: its effects are whatever the held function
              ;; does, which is not statically known. It materializes as the
              ;; tracked `dynamic` deferral (modality may), never `unknown`.
              ;; A bare name that is NOT a param or local of this function but
              ;; IS a param of an enclosing function: a closure-captured
              ;; callback (`def deco(cb): def inner(): cb()`). Its effects are
              ;; whatever was captured, so it defers to `dynamic` too.
              (define (closure-captured-param? name)
                (let loop ([nm (cdr k)] [seen (make-hash)])
                  (define parent (and nm (hash-ref fn-parents (cons comp nm) #f)))
                  (and parent
                       (or (let ([ps (or (hash-ref fn-params (cons comp parent) #f)
                                         (hash-ref method-params (cons comp parent) #f))])
                             (and ps
                                  (for/or ([p (in-list ps)])
                                    (define pnm
                                      (cond
                                        [(uir-symbol? p)
                                         (string->symbol (uir-symbol-name p))]
                                        [(uir-typed-param? p)
                                         (and (uir-symbol? (uir-typed-param-name p))
                                              (string->symbol
                                               (uir-symbol-name (uir-typed-param-name p))))]
                                        [else #f]))
                                    (eq? pnm name))))
                           (and (not (hash-has-key? seen parent))
                                (begin (hash-set! seen parent #t)
                                       (loop parent seen)))))))
              (define function-value?
                (or (and (eq? shape 'bare-call) (uir-call? callee))
                    (and (eq? shape 'bare-call)
                         (memq binding-kind '(fn call-result attr loop-var comp-var)))
                    (and (eq? shape 'bare-call)
                         (eq? binding-kind 'unbound)
                         (closure-captured-param? bare-name))
                    (eq? shape 'self-method)
                    ;; a subscript into a param dict yields a server-provided
                    ;; callable (environ["wsgi.file_wrapper"]) — runtime.
                    (and (eq? shape 'param)
                         (uir-get? callee)
                         (uir-string? (uir-get-field callee)))))
              ;; A .m() call whose receiver is a runtime value — an
              ;; unresolvable call result (f(...).m()), or a local bound from
              ;; an unresolvable call result or a literal/conditional
              ;; (x = f(...); x.m(), x = A() if c else B(); x.m()) — is
              ;; runtime-dispatched: the receiver's class is not statically
              ;; known, so its methods defer to `dynamic`, never `unknown`.
              (define runtime-receiver?
                (or (eq? shape 'call-result)
                    (and (eq? shape 'local)
                         (memq binding-kind '(call-result literal)))))
              ;; A declared param effect (host protocol contract) resolves the
              ;; bare-call-to-param shape directly, so it never becomes a row
              ;; hole: the caller (ASGI/WSGI server, socketserver) is outside
              ;; the corpus, so no in-corpus call site could instantiate it.
              (define declared-fx
                (and higher-order?
                     (declared-param-effect-row comp (cdr k) bare-name)))
              (cond
                [declared-fx
                 (hash-set! class-table node (cons 'declared declared-fx))
                 (when prov
                   (provenance-add-ground!
                    prov
                    (make-callee-site (function-fq k) declared-fx callee
                                      #:kind 'declared
                                      #:coverage 'universal
                                      #:file comp
                                      #:position position)))]
                [higher-order?
                 (hash-set! class-table node (cons 'hole bare-name))
                 (hash-update! called-params k (λ (l) (cons bare-name l)) '())
                 ;; The bare-called param becomes a row HOLE; a hole still open
                 ;; at fixpoint end materializes as `dynamic` (runtime callback),
                 ;; so its ground must name `dynamic`, not the pre-hole `unknown`.
                 (when prov
                   (provenance-add-ground!
                    prov
                    (make-callee-site (function-fq k) 'dynamic callee
                                      #:kind 'unresolved
                                      #:coverage
                                      (unresolved-coverage callee param-names tokens)
                                      #:file comp
                                      #:position position)))]
                [(or function-value? runtime-receiver?)
                 (hash-set! class-table node (cons 'dynamic #f))
                 (when prov
                   (provenance-add-ground!
                    prov
                    (make-callee-site (function-fq k) 'dynamic callee
                                      #:kind 'unresolved
                                      #:coverage
                                      (unresolved-coverage callee param-names tokens)
                                      #:file comp
                                      #:position position)))]
                [else
                 (hash-set! class-table node (cons 'unknown #f))
                 (when prov
                   (provenance-add-ground!
                    prov
                    (make-callee-site (function-fq k) 'unknown callee
                                      #:kind 'unresolved
                                      #:coverage
                                      (unresolved-coverage callee param-names tokens)
                                      #:file comp
                                      #:position position)))])
              (when (and unknown-tally (not declared-fx))
                (set-box! unknown-tally
                          (cons (list (car k) (cdr k)
                                      shape
                                      (effect-op-of-callee callee)
                                      rx
                                      binding-kind
                                      site
                                      higher-order?)
                                (unbox unknown-tally))))]))]
        [else (void)]))
    (walk-body body visit!)
    (hash-set! bodies k body)
    (hash-set! class-tables k class-table)
    (hash-set! edge-sites k (reverse call-sites))
    (hash-set! edges k (sort (remove-duplicates callees) (λ (a b)
                                                            (string<? (format "~a.~a" (car a) (cdr a))
                                                                      (format "~a.~a" (car b) (cdr b)))))))

  ;; The whole-program row fixpoint: recompose every body from its classified
  ;; call sites and its callees' current rows until the table is stable. The
  ;; shared evaluator (invocation-row.rkt) owns the control-flow composition,
  ;; the SCC widening, and the row-hole instantiation at call sites; the
  ;; single-file pass runs the same driver, so the two cannot drift apart.
  (define key<? (λ (a b) (string<? (format "~a.~a" (car a) (cdr a))
                                   (format "~a.~a" (car b) (cdr b)))))
  (define (param-name-sym p)
    (cond
      [(uir-symbol? p) (string->symbol (uir-symbol-name p))]
      [(uir-typed-param? p)
       (and (uir-symbol? (uir-typed-param-name p))
            (string->symbol (uir-symbol-name (uir-typed-param-name p))))]
      [else #f]))
  (define (params-of k)
    (or (hash-ref fn-params k #f) (hash-ref method-params k #f)))
  (define (param-index ps name)
    (for/or ([i (in-naturals)] [p (in-list ps)])
      (and (eq? (param-name-sym p) name) i)))
  ;; An argument's row in caller k's context, given the current fixpoint table:
  ;; a pass-through hole (the caller bare-calls this same param), a known
  ;; function's row, a direct-effect row, or the dynamic fallback.
  (define (wp-arg-row k a rows)
    (define a-var (and (uir-var? a) (var-symbol a)))
    (cond
      [(and a-var (memq a-var (hash-ref called-params k '())))
       (alg:row-hole a-var)]
      [else
       (define cls (classify-call-all a known methods
                                      class-defs fn-defs comp-supers self-attr
                                      (car k) (owner-of-key k) import-env
                                      (rt-of k) return-types (box 'structural)
                                      #:lexicon lex))
       (case (car cls)
         [(edge) (hash-ref rows (cdr cls) (alg:empty-algebra-row))]
         [(effect) (effects->algebra-row (list (cdr cls)))]
         [(pure) (alg:empty-algebra-row)]
         ;; An argument that is neither an edge, an effect, nor pure is a
         ;; runtime-dispatched callback: defer to `dynamic` (tracked, modality
         ;; may), never `unknown` — the elimination obligation is only for
         ;; statically-nameable effects.
         [else (effects->algebra-row '(dynamic))])]))
  (define rows
    (inference-rows (sort all-keys key<?)
                    ;; A declared function effect is authoritative: its body is
                    ;; suppressed inside the fixpoint and the seed supplies the
                    ;; declared row, so callers inherit the declared domain
                    ;; (never the unknown or false positive it replaces).
                    (λ (k) (if (declared-function-fx k) (uir-block '()) (hash-ref bodies k)))
                    (λ (k)
                      (define table (hash-ref class-tables k))
                      (λ (node) (hash-ref table node (cons 'unknown #f))))
                    #:tokens tokens
                    #:known-class? (λ (s) (hash-has-key? class-defs s))
                    #:params-of params-of
                    #:arg-row wp-arg-row
                    #:seed (λ (k)
                             (define fx (declared-function-fx k))
                             (cond
                               [fx (declared-domain-row fx)]
                               [(hash-ref blessed-caps k #f) => capability-row]
                               [else (alg:empty-algebra-row)]))))
  (when rec-box (set-box! rec-box prov))

  ;; Summary: a polymorphic function's own row is the join of its
  ;; instantiations at every call site; a hole with no resolving site stays
  ;; open and materializes as the honest `dynamic` (a runtime callback).
  (define incoming (make-hash))
  (for ([(c sites) (in-hash edge-sites)])
    (for ([site (in-list sites)])
      (hash-update! incoming (car site) (λ (l) (cons (cons c (cdr site)) l)) '())))
  (define (summary-row k rows)
    (define r (hash-ref rows k (alg:empty-algebra-row)))
    (for/fold ([r r]) ([p (in-list (alg:row-holes r))])
      (define inst
        (for/fold ([acc (alg:empty-algebra-row)])
                  ([in (in-list (hash-ref incoming k '()))])
          (define c (car in))
          (define args (cdr in))
          (define idx (param-index (params-of k) p))
          (define a (and idx (< idx (length args)) (list-ref args idx)))
          (alg:join acc (if a (wp-arg-row c a rows) (alg:empty-algebra-row)))))
      ;; A hole with incoming sites is instantiated by them even when every
      ;; passed arg classifies pure (instantiate-with-empty closes it); only a
      ;; hole with no incoming site stays open for the dynamic deferral.
      (if (null? (hash-ref incoming k '()))
          r
          (alg:instantiate-row-hole r p inst))))

  ;; Materialize the algebra rows back to the UIR row contract; holes still
  ;; open become tracked `dynamic` (a runtime callback, surfaced in the graph),
  ;; never a ?-entry and no longer the `unknown` elimination obligation. A
  ;; still-unknown row the producer has declared a domain for takes that
  ;; declared domain (grounded, so the certificate checker still accepts it).
  (for/hash ([(k r) (in-hash rows)])
    (define uir-row (alg:algebra-row->uir (alg:row-holes->dynamic (summary-row k rows))))
    (define fx (declared-function-fx k))
    (cond
      [(and fx (row-has-unknown? uir-row))
       (when prov
         (for ([d (in-list (if (list? fx) fx (list fx)))])
           (provenance-add-ground!
            prov
            (make-declared-site (function-fq k) d
                                (list (car k) (symbol->string (cdr k))) #f
                                #:file (car k)))))
       (values k (uir-row-replace-unknown uir-row fx))]
      [else (values k uir-row)])))
