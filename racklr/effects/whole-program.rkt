#lang racket

;; racklr/effects/whole-program.rkt
;;
;; Whole-program (cross-file) effect inference: a single monotone fixpoint over
;; the union of every component's call graph, with component-qualified node
;; identity and cross-file superclass resolution. The cross-file callee
;; classifier lives in classify-all.rkt; the type/effect relation
;; materialization and its deterministic join live in type-relations.rkt; this
;; module is the fixpoint driver that collects the tables, joins the relations,
;; and folds the call graph.

(require racklr/uir
         racklr/effects/class-resolve
         racklr/effects/collect
         racklr/effects/walk
         racklr/effects/rows
         (prefix-in alg: racklr/effects/algebra/main)
         racklr/effects/shape
         racklr/effects/receiver-type
         racklr/effects/classify-all
         racklr/effects/type-relations
         racklr/effects/library-tokens
         (only-in racklr/effects/library-stdlib-types declared-param-effect-rows)
         racklr/effects/imports
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
                           #:known-pure-names [known-pure-names '()])
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

  ;; Pass 3: classification + call-graph edges.
  (define direct-rows (make-hash))
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
    (define effects (if blessed (list blessed) '()))
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
              (set! effects (cons (cdr cls) effects))
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
             [(pure) (void)]
             [(unknown)
              ;; The bare-call-to-parameter shape is the row-hole lever: a
              ;; callback param stands for "the effects of whatever is passed",
              ;; so it becomes a hole, instantiated by callers (the edge-sites
              ;; fixpoint below) instead of `unknown`. The same shape info feeds
              ;; the census tally, so it is computed up front.
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
                 (set! effects (cons declared-fx effects))
                 (when prov
                   (provenance-add-ground!
                    prov
                    (make-callee-site (function-fq k) declared-fx callee
                                      #:kind 'declared
                                      #:coverage 'universal
                                      #:file comp
                                      #:position position)))]
                [higher-order?
                 (set! effects (cons (list 'hole bare-name site) effects))
                 (hash-update! called-params k (λ (l) (cons bare-name l)) '())
                 (when prov
                   (provenance-add-ground!
                    prov
                    (make-callee-site (function-fq k) 'unknown callee
                                      #:kind 'unresolved
                                      #:coverage
                                      (unresolved-coverage callee param-names tokens)
                                      #:file comp
                                      #:position position)))]
                [(or function-value? runtime-receiver?)
                 (set! effects (cons 'dynamic effects))
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
                 (set! effects (cons 'unknown effects))
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
    ;; Direct row: the plain effect symbols, plus one hole per bare-called
    ;; param (grounded by the call site that created it).
    (define direct-row
      (let ([base (effects->algebra-row (filter (λ (x) (not (pair? x))) effects))])
        (for/fold ([r base]) ([item (in-list effects)] #:when (pair? item))
          (define pname (cadr item))
          (define hsite (caddr item))
          (alg:join r (alg:row-hole pname (alg:provenance (list hsite) #f
                                                       (list (symbol->string pname))))))))
    (hash-set! direct-rows k direct-row)
    (hash-set! edge-sites k (reverse call-sites))
    (hash-set! edges k (sort (remove-duplicates callees) (λ (a b)
                                                            (string<? (format "~a.~a" (car a) (cdr a))
                                                                      (format "~a.~a" (car b) (cdr b)))))))

  ;; Row-hole fixpoint. Each node's row is its direct row joined with, for every
  ;; call site, the callee's row with its holes instantiated by the argument
  ;; passed there. Instantiation is monotone in the rows, so full sweeps ascend
  ;; to the least fixed point; holes still open at the end are the honest
  ;; `unknown` deferral (materialized below), exactly the pre-hole result.
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
  ;; An argument's static shape in caller k's context: a pass-through hole,
  ;; a known function, a direct-effect row, or the unknown fallback.
  (define (arg-row k a)
    (define a-var (and (uir-var? a) (var-symbol a)))
    (cond
      [(and a-var (memq a-var (hash-ref called-params k '())))
       (list 'hole a-var)]
      [else
       (define cls (classify-call-all a known methods
                                      class-defs fn-defs comp-supers self-attr
                                      (car k) (owner-of-key k) import-env
                                      (rt-of k) return-types (box 'structural)
                                      #:lexicon lex))
       (case (car cls)
         [(edge) (list 'fn (cdr cls))]
         [(effect) (list 'row (effects->algebra-row (list (cdr cls))))]
         [(pure) (list 'row (alg:empty-algebra-row))]
         [else (list 'row (effects->algebra-row '(unknown)))])]))
  (define (arg->row k a rows)
    (define v (arg-row k a))
    (case (car v)
      [(hole) (alg:row-hole (cadr v))]
      [(fn) (hash-ref rows (cadr v) (alg:empty-algebra-row))]
      [else (cadr v)]))
  (define (compute-row k rows)
    (foldl
     (λ (site acc)
       (define g (car site))
       (define args (cdr site))
       (define g-row (hash-ref rows g (alg:empty-algebra-row)))
       (define inst
         (for/fold ([r g-row]) ([p (in-list (hash-ref called-params g '()))])
           (define idx (param-index (params-of g) p))
           (define a (and idx (< idx (length args)) (list-ref args idx)))
           (if a (alg:instantiate-row-hole r p (arg->row k a rows)) r)))
       (alg:join acc inst))
     (hash-ref direct-rows k (alg:empty-algebra-row))
     (hash-ref edge-sites k '())))
  (define rows (make-hash))
  (for ([k (in-list all-keys)])
    (hash-set! rows k (hash-ref direct-rows k (alg:empty-algebra-row))))
  (let sweep ([i 0])
    (define changed? #f)
    (for ([k (in-list all-keys)])
      (define new (compute-row k rows))
      (unless (alg:same-row? (hash-ref rows k) new)
        (set! changed? #t)
        (hash-set! rows k new)))
    (when changed?
      (when (> i 400) (error 'row-holes-fixpoint "no convergence after ~a sweeps" i))
      (sweep (add1 i))))
  (when rec-box (set-box! rec-box prov))
  ;; Summary: a polymorphic function's own row is the join of its
  ;; instantiations at every call site; a hole with no resolving site stays
  ;; open and materializes as the honest `unknown`.
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
          (alg:join acc (if a (arg->row c a rows) (alg:empty-algebra-row)))))
      ;; A hole with incoming sites is instantiated by them even when every
      ;; passed arg classifies pure (instantiate-with-empty closes it); only a
      ;; hole with no incoming site stays open for the dynamic/unknown deferral.
      (if (null? (hash-ref incoming k '()))
          r
          (alg:instantiate-row-hole r p inst))))
  ;; Materialize the algebra rows back to the UIR row contract; holes still
  ;; open become tracked `dynamic` (a runtime callback, surfaced in the graph),
  ;; never a ?-entry and no longer the `unknown` elimination obligation.
  (for/hash ([(k r) (in-hash rows)])
    (values k (alg:algebra-row->uir (alg:row-holes->dynamic (summary-row k rows))))))
