#lang racket

;; racklr/effects/whole-program.rkt
;;
;; Whole-program (cross-file) effect inference: a single monotone fixpoint over
;; the union of every component's call graph, with component-qualified node
;; identity and cross-file superclass resolution. The cross-file callee
;; classifier lives in classify-all.rkt; this module is the fixpoint driver.

(require racklr/uir
         racklr/effects/class-resolve
         racklr/effects/collect
         racklr/effects/walk
         racklr/effects/rows
         racklr/effects/shape
         racklr/effects/receiver-type
         racklr/effects/classify-all
         racklr/effects/library-tokens)

(provide infer-effects-all classify-call-all)

;; Self-attribute assignments (`self.attr = KnownClass(...)` plus class-level
;; field initializers) collected across a class, keyed by (cons comp class).
;; This is the per-CLASS counterpart of the per-function receiver env:
;; attributes set in __init__ (or any method, or at class level) flow to every
;; other method, so `self.attr.m(...)` resolves even when the assignment is not
;; in the method body itself.
;; Class-level field initializers (`attr = KnownClass` / `attr = KnownClass()`):
;; the per-class default attribute environment that method `self.attr`
;; assignments overlay. Harvested from `uir-class` field nodes, which is where
;; the Python lowerer puts class-body assignments.
(define (field-init-class init class-defs import-env comp)
  (cond
    [(bare-class-value init class-defs default-effect-tokens import-env comp)]
    [(uir-call? init) (call-result-type (uir-call-callee init) class-defs
                                        default-effect-tokens #f import-env comp)]
    [else #f]))

(define (class-field-attrs comp-programs class-defs import-env)
  (define h (make-hash))
  (for ([cp (in-list comp-programs)])
    (define comp (car cp))
    (let walk ([n (cdr cp)])
      (cond
        [(uir-class? n)
         (define owner
           (and (uir-class-name n) (uir-symbol? (uir-class-name n))
                (string->symbol (uir-symbol-name (uir-class-name n)))))
         (when owner
           (define key (cons comp owner))
           (define acc (hash-ref h key #f))
           (unless acc (set! acc (make-hash)) (hash-set! h key acc))
           (for ([f (in-list (uir-class-fields n))])
             (when (uir-field? f)
               (define attr (and (uir-field-name f) (uir-symbol? (uir-field-name f))
                                 (string->symbol (uir-symbol-name (uir-field-name f)))))
               (define cls (field-init-class (uir-field-init f) class-defs import-env comp))
               (when (and attr cls) (hash-set! acc attr cls)))))]
        [(struct? n)
         (for ([f (in-list (cdr (vector->list (struct->vector n))))]) (walk f))]
        [(pair? n) (walk (car n)) (walk (cdr n))])))
  h)

;; Wrap a propagated param type as a typed-param so the annotation-seeding
;; path in infer-receiver-types picks it up. The Python lowerer encodes
;; unannotated params as typed-params with a #f type, so those are filled in
;; too; an explicitly annotated param already seeds its own (more precise)
;; type and is left untouched. self/cls (index 0 of a method) is never a
;; propagation target.
(define (seed-params params k param-types)
  (for/list ([i (in-naturals)] [p (in-list params)])
    (define cls (hash-ref param-types (cons k i) #f))
    (cond
      [(and cls (uir-symbol? p))
       (uir-typed-param p (uir-symbol (symbol->string cls)) #f)]
      [(and cls (uir-typed-param? p) (not (uir-typed-param-type p)))
       (uir-typed-param (uir-typed-param-name p)
                        (uir-symbol (symbol->string cls))
                        (uir-typed-param-default p))]
      [else p])))

;; Self-attribute assignments collected across every class: class-level field
;; initializers first, then `self.attr = KnownClass(...)` from every method
;; (which overlay the class-level defaults, mirroring instance attributes
;; shadowing class attributes in Python). Keyed by (cons comp class).
;; `param-types` seeds method params (self/cls excluded), so `self.attr = p`
;; where `p` is an unannotated param resolves through the concrete call chain
;; (`Page.__init__(self, object_list, number, paginator)` with `paginator`
;; passed `self` in `Paginator.page`) and flows to sibling methods.
(define (class-self-attrs comp-programs class-defs import-env [param-types (hash)])
  (define h (class-field-attrs comp-programs class-defs import-env))
  (for ([cp (in-list comp-programs)])
    (define comp (car cp))
    (define mparams (collect-method-params (cdr cp)))
    (for ([(n body) (in-hash (collect-methods (cdr cp)))])
      (define owner (class-name-of-name n))
      (define k (cons comp n))
      (define sa (receiver-types-self-attrs
                  (infer-receiver-types body class-defs (hash) import-env comp
                                        (seed-params (hash-ref mparams n '()) k param-types)
                                        owner)))
      (define key (cons comp owner))
      (define cur (hash-ref h key #f))
      (cond
        [(not cur) (unless (zero? (hash-count sa)) (hash-set! h key (hash-copy sa)))]
        [else (for ([(attr kls) (in-hash sa)]) (hash-set! cur attr kls))])))
  h)

;; Inherited self-attribute environment for a class, following the MRO across
;; files with the most-derived class first: each attribute keeps the first
;; (most-derived) assignment, so a subclass's `self.attr = ...` shadows a
;; base's. Returns a receiver-types with no locals (locals are per-function).
(define (class-attr-env comp class-name class-attrs class-defs comp-supers)
  (define seen (make-hash))
  (define acc (make-hash))
  (let loop ([c comp] [cls class-name])
    (when (and c cls (not (hash-has-key? seen (cons c cls))))
      (hash-set! seen (cons c cls) #t)
      (for ([(attr kls) (in-hash (hash-ref class-attrs (cons c cls) (hash)))])
        (unless (hash-has-key? acc attr) (hash-set! acc attr kls)))
      (for ([sr (in-list (hash-ref comp-supers (cons c cls) '()))])
        (define c2 (car sr))
        (define s (cdr sr))
        (when (and c2 s) (loop c2 s)))))
  (receiver-types (hash) acc))

(define (key<? a b)
  (string<? (format "~a.~a" (car a) (cdr a)) (format "~a.~a" (car b) (cdr b))))

;; Whole-program effect rows: (component . name) → uir-effect-row, computed by
;; a single fixpoint over the union of every program's call graph. Takes a list
;; of `(component . program)` pairs.
(define (infer-effects-all comp-programs #:import-env [import-env #f])
  (define fns (make-hash))           ;; (cons comp name) → body
  (define methods (make-hash))       ;; (cons comp name) → body
  (define comp-supers (make-hash))   ;; (cons comp class) → (listof super-ref: symbol | (first . full-dotted))
  (define class-defs (make-hash))    ;; class-name → (listof comp)
  (define fn-defs (make-hash))       ;; fn-name → (listof comp)
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

  (define param-types (make-hash))
  (define class-attrs (class-self-attrs comp-programs class-defs import-env param-types))

  (define known (make-hash))
  (for ([(k _) (in-hash fns)]) (hash-set! known k #t))
  (for ([(k _) (in-hash methods)]) (hash-set! known k #t))

  (define all-keys (append (hash-keys fns) (hash-keys methods)))

  ;; Pass 1: per-key receiver-type env (assignments + param seeding + the
  ;; class's inherited self-attrs). Classification and return-type projection
  ;; share this one env instead of rebuilding it.
  (define rt-table (make-hash))    ;; (cons comp name) → receiver-types
  (define body-table (make-hash))  ;; (cons comp name) → body
  (define owner-table (make-hash)) ;; (cons comp name) → owner class or #f
  ;; Parameter types propagated from call sites, grounded in the concrete call
  ;; chain: (cons callee-key param-index) → class-name-symbol. A param only
  ;; receives a class when EVERY resolved call site passes that same class and
  ;; no site passes an unresolvable argument (a site whose argument type the
  ;; receiver env cannot pin down blocks propagation). The fixpoint below folds
  ;; these seeds back into `infer-receiver-types` until stable, so an untyped
  ;; `handle_merge(self, loader, ...)` param resolves `loader.get_migration`
  ;; through the class the caller actually passed.
  (define (params-of k)
    (if (hash-has-key? fns k) (hash-ref fn-params k '()) (hash-ref method-params k '())))
  (define (build-rt-table!)
    (for ([k (in-list all-keys)])
      (define body (if (hash-has-key? fns k) (hash-ref fns k) (hash-ref methods k)))
      (define comp (car k))
      (define owner (and (hash-has-key? methods k) (class-name-of-name (cdr k))))
      (define class-env (and owner (class-attr-env comp owner class-attrs class-defs comp-supers)))
      (define params (params-of k))
      (define seeded-params (seed-params params k param-types))
      (define own-rt (infer-receiver-types body class-defs
                                           (or (and class-env (receiver-types-self-attrs class-env))
                                               (hash))
                                           import-env comp seeded-params owner))
      (define rt (if owner (receiver-types-merge class-env own-rt) own-rt))
      (hash-set! rt-table k rt)
      (hash-set! body-table k body)
      (hash-set! owner-table k owner)))
  (define (collect-param-types!)
    (define cands (make-hash))   ;; (cons k idx) → (listof class-symbol)
    (define unres (make-hash))   ;; (cons k idx) → count of unresolvable sites
    (define (note! key idx cls)
      (define kk (cons key idx))
      (if cls
          (hash-update! cands kk (λ (l) (cons cls l)) '())
          (hash-update! unres kk add1 0)))
    (for ([k (in-list all-keys)])
      (define body (hash-ref body-table k))
      (define comp (car k))
      (define owner (hash-ref owner-table k))
      (define rt (hash-ref rt-table k))
      (define (arg-class a) (value-class-of a rt class-defs default-effect-tokens import-env comp owner))
      (define (visit! node)
        (when (uir-call? node)
          (define callee (uir-call-callee node))
          (define args (uir-call-args node))
          (define nm (callee-name callee))
          (define fk (and nm (bare-function-key nm comp fn-defs known)))
          (cond
            [fk
             (for ([i (in-naturals)] [a (in-list args)])
               (when (< i (length (params-of fk)))
                 (note! fk i (arg-class a))))]
            [else
             (define mks (method-callee-keys callee comp owner methods class-defs comp-supers))
             (cond
               [(pair? mks)
                (for ([i (in-naturals)] [a (in-list args)])
                  ;; positional arg i maps to method param i+1 (param 0 is self/cls)
                  (when (< (+ i 1) (length (params-of (car mks))))
                    (for ([mk (in-list mks)])
                      (note! mk (+ i 1) (arg-class a)))))]
               [else
                ;; `KnownClass(...)` constructs a corpus class: propagate the
                ;; constructor args to its __init__ params (positional arg i
                ;; → param i+1), so `self.paginator = paginator` in
                ;; `Page.__init__` resolves the `paginator` param from the
                ;; class the caller actually constructed.
                (define cname (constructor-class callee class-defs))
                (define c (and cname (resolve-class-component cname comp class-defs)))
                (define ik (and c (cons c (qualified-name-symbol cname '__init__))))
                (when (and ik (hash-has-key? methods ik))
                  (for ([i (in-naturals)] [a (in-list args)])
                    (when (< (+ i 1) (length (params-of ik)))
                      (note! ik (+ i 1) (arg-class a)))))])])))
      (walk-body body visit!))
    ;; Strict fold: one observed class, zero unresolved sites.
    (define out (make-hash))
    (for ([(kk cls-list) (in-hash cands)])
      (define uniq (remove-duplicates cls-list))
      (when (and (= (length uniq) 1) (zero? (hash-ref unres kk 0)))
        (hash-set! out kk (car uniq))))
    out)
  (build-rt-table!)
  (let param-loop ()
    (define next-pt (collect-param-types!))
    (unless (equal? next-pt param-types)
      (set! param-types next-pt)
      ;; self.attr assignments seeded by params (`self.paginator = paginator`)
      ;; live in class-attrs, so re-derive them from the sharper param types
      ;; before rebuilding each method's receiver env.
      (set! class-attrs (class-self-attrs comp-programs class-defs import-env param-types))
      (build-rt-table!)
      (param-loop)))

  ;; Pass 2: return-type projection — the single class each function/method
  ;; returns, resolved through its own receiver env. `self._cache.set(...)` then
  ;; reaches the cached-property `_cache` method's projected result class.
  (define return-types (make-hash)) ;; (cons comp name) → class-name-symbol | #f
  (for ([k (in-list all-keys)])
    (hash-set! return-types k
               (return-class-of (hash-ref body-table k) (hash-ref rt-table k)
                                class-defs default-effect-tokens import-env (car k)
                                (hash-ref owner-table k))))

  ;; Pass 3: classification + call-graph edges.
  (define direct-rows (make-hash))
  (define edges (make-hash))
  (for ([k (in-list all-keys)])
    (define body (hash-ref body-table k))
    (define comp (car k))
    (define owner (hash-ref owner-table k))
    (define rt (hash-ref rt-table k))
    (define effects
      (let ([cap (and (hash-has-key? methods k) (method-cap k import-env))])
        (if cap (list cap) '())))
    (define callees '())
    (define (visit! node)
      (if (uir-call? node)
            (let ([cls (classify-call-all (uir-call-callee node) known methods
                                        class-defs fn-defs comp-supers class-attrs comp owner
                                        import-env rt return-types)])
            (case (car cls)
              [(edge) (set! callees (cons (cdr cls) callees))]
              [(effect) (set! effects (cons (cdr cls) effects))]
              [(pure) (void)]
              [(unknown) (set! effects (cons 'unknown effects))]))
          (void)))
    (walk-body body visit!)
    (hash-set! direct-rows k (effects->row effects))
    (hash-set! edges k (sort (remove-duplicates callees) key<?)))

  (define rows (make-hash))
  (for ([k (in-list all-keys)]) (hash-set! rows k (hash-ref direct-rows k)))
  (let loop ()
    (define changed? #f)
    (for ([k (in-list all-keys)])
      (define current (hash-ref rows k))
      (define callee-rows
        (map (λ (g) (hash-ref rows g (empty-row))) (hash-ref edges k)))
      (define new-row
        (foldl infer-row-union (hash-ref direct-rows k) callee-rows))
      (if (equal? current new-row)
          (void)
          (begin (set! changed? #t) (hash-set! rows k new-row))))
    (if changed? (loop) (void)))
  rows)

