#lang racket

;; racklr/effects/receiver-type.rkt
;;
;; Per-function receiver-type environment: tracks the class of local variables
;; and `self`/`cls` attributes from constructor assignments
;; (`x = KnownClass(...)`), simple aliases (`x = y`, `x = self.attr`), and bare
;; class references (`x = KnownClass`), so a later `x.m(...)` call can resolve
;; through the receiver class's derived effect capability (or its in-tree method
;; body, for a class in an untagged module) instead of the flat method-name
;; tables. This is the first half of receiver-type dataflow: the class name is
;; inferred from the call chain (a constructor whose callee names a known class,
;; or a bare class reference), not from a name table.

(require racklr/uir
         racklr/effects/shape
         racklr/effects/walk
         racklr/effects/class-resolve
         racklr/effects/imports
         racklr/effects/library-tokens)

(provide
  receiver-types? receiver-types receiver-types-locals receiver-types-self-attrs
  receiver-types-merge
  infer-receiver-types
  receiver-class-of
  constructor-class
  call-result-type
  value-class-of
  return-class-of
  bare-class-value)

(struct receiver-types (locals self-attrs) #:transparent)
;; locals:     hash name-symbol → class-name-symbol
;; self-attrs: hash attr-symbol → class-name-symbol (on self / cls)

(define (self-or-cls-var? v)
  (and (uir-var? v)
       (memq (string->symbol (uir-symbol-name (uir-var-name v))) '(self cls))))

;; A pure stdlib/builtin value constructor name (`Path`, `md5`, `dict`): pure to
;; construct, and therefore also a usable receiver type for a call result.
(define (pure-stdlib-type? n [tokens default-effect-tokens])
  (and n
       (let ([s (symbol->string n)])
         (or (set-member? (effect-tokens-pure-stdlib-bare-names tokens) s)
             (set-member? (effect-tokens-builtin-type-roots tokens) s)))))

;; Name symbol bound by a `set!` left-hand side. The Python lowerer emits a
;; uir-var target; the JS/TS lowerer emits a uir-symbol target. Returns #f for
;; a compound target (a uir-get attribute assignment is handled by the caller).
(define (set!-lhs-symbol lhs)
  (cond
    [(uir-var? lhs) (string->symbol (uir-symbol-name (uir-var-name lhs)))]
    [(uir-symbol? lhs) (string->symbol (uir-symbol-name lhs))]
    [else #f]))

;; Class name (symbol) tracked for a receiver expression `base`, or #f. `base`
;; is either a local variable (`qs`), a `self`/`cls` attribute (`self.cursor`),
;; or a bare class reference. Moved above `call-result-type` because
;; `call-result-type` consults it when a call's callee is a class-valued
;; receiver expression (`self.build_file_class(...)`).
(define (receiver-class-of env base)
  (cond
    [(not env) #f]
    [(uir-var? base)
     (hash-ref (receiver-types-locals env)
               (string->symbol (uir-symbol-name (uir-var-name base))) #f)]
    [(uir-symbol? base)
     (hash-ref (receiver-types-locals env)
               (string->symbol (uir-symbol-name base)) #f)]
    [(uir-get? base)
     (define attr (get-field-symbol (uir-get-field base)))
     (and attr (self-or-cls-var? (uir-get-base base))
          (hash-ref (receiver-types-self-attrs env) attr #f))]
    [else #f]))

;; A bare class reference used as a VALUE: `x = QuerySet` (not a call). The
;; referenced name is a known corpus class or a pure stdlib/builtin value type;
;; anything else returns #f (an arbitrary name is not a tracked class).
(define (bare-class-value value class-defs [tokens default-effect-tokens]
                           [import-env #f] [comp #f])
  (define nm
    (cond
      [(uir-var? value) (string->symbol (uir-symbol-name (uir-var-name value)))]
      [(uir-symbol? value) (string->symbol (uir-symbol-name value))]
      [(uir-get? value) (get-field-symbol (uir-get-field value))]
      [else #f]))
  (and nm
       (or (hash-has-key? class-defs nm)
           (pure-stdlib-type? nm tokens)
           (imported-class-ref nm comp import-env))
       nm))

;; Class name the constructor callee names, when it is a known class: a bare
;; callee (`QuerySet(...)`) or a dotted callee whose final field names a known
;; class (`models.CharField(...)`), plus pure stdlib/builtin value constructors
;; (`Path(...)`, `md5(...)`, `dict(...)` — pure to construct). Returns a
;; class-name symbol or #f. Effectful constructors (`open`, `ZipFile`) are
;; deliberately absent: `constructor-class` only names PURE construction.
(define (constructor-class callee class-defs [tokens default-effect-tokens])
  (define nm (callee-name callee))
  (cond
    [(and nm (or (hash-has-key? class-defs nm) (pure-stdlib-type? nm tokens))) nm]
    [(uir-get? callee)
     (define f (get-field-symbol (uir-get-field callee)))
     ;; A dotted callee rooted at self/cls is an attribute method call
     ;; (`self._cache.set(...)`), NOT a module-qualified constructor — the
     ;; final field `set` must not collide with the builtin `set` type here.
     (and f (not (memq (dotted-root callee) '(self cls)))
          (or (hash-has-key? class-defs f) (pure-stdlib-type? f tokens)) f)]
    [else #f]))

;; Result type of an imported module FUNCTION call (`r = httpx.get(...)`, `resp
;; = requests.post(...)`): the callee is a member access `mod.func(...)` whose
;; root resolves through the import graph to a module handle, and the module's
;; function table maps `func` to a result-type name (`httpx.get` → Response).
;; This is the receiver-TYPE half of call-result projection: `r` then tracks
;; Response, and `r.json()` classifies through stdlib-type-methods-data.
(define (imported-module-result-type callee comp import-env tokens)
  (and import-env comp (member-access-callee? callee)
       (let* ([root (dotted-root callee)]
              [func (effect-op-of-callee callee)]
              [modpath (hash-ref (import-env-comp-modpath import-env) comp #f)]
              [e (and modpath (hash-ref (import-env-exports import-env) modpath #f))]
              [h (and e root (hash-ref e (symbol->string root) #f))])
         (and root func (handle? h) (eq? (handle-kind h) 'module) (handle-path h)
              (let ([table (hash-ref (effect-tokens-module-result-types tokens)
                                     (handle-path h) #f)])
                (and table
                     (let ([t (hash-ref table (symbol->string func) #f)])
                       (and t (string->symbol t)))))))))

;; Result TYPE of a call expression: the class named by its callee when that
;; class is known, or the class a class-valued callee expression resolves to.
;; Covers corpus classes, pure stdlib value constructors, and any stdlib type
;; with a method table — `open` and `ZipFile` construct effectfully, so they are
;; result types but NOT pure constructors. `rt` (optional) is the receiver-type
;; environment at the call site, which lets a call through a tracked class
;; value (`self.build_file_class(...)`) carry the class through to the result.
(define (call-result-type callee class-defs [tokens default-effect-tokens] [rt #f]
                           [import-env #f] [comp #f])
  (or (and rt (receiver-class-of rt callee))
      (constructor-class callee class-defs tokens)
      (let ([nm (callee-name callee)])
        (and nm
             (hash-has-key? (effect-tokens-stdlib-type-methods tokens)
                            (symbol->string nm))
             nm))
      (let ([nm (callee-name callee)])
        (and nm comp import-env (imported-class-ref nm comp import-env)))
      (imported-module-result-type callee comp import-env tokens)))

;; Class a VALUE expression resolves to under a receiver-type environment `rt`:
;; a tracked call result, a local alias, a self/cls attribute, or a bare class
;; reference. This is the single value→type projection shared by assignment
;; tracking (infer-receiver-types) and return-type projection (return-class-of).
(define (value-class-of value rt class-defs [tokens default-effect-tokens]
                        [import-env #f] [comp #f] [owner #f])
  (cond
    [(uir-await? value) (value-class-of (uir-await-expr value) rt class-defs
                                        tokens import-env comp owner)]
    [(uir-call? value)
     (call-result-type (uir-call-callee value) class-defs tokens rt import-env comp)]
    [(uir-var? value)
     (define nm (string->symbol (uir-symbol-name (uir-var-name value))))
     (or (and owner (memq nm '(self cls)) owner)
         (hash-ref (receiver-types-locals rt) nm #f)
         (bare-class-value value class-defs tokens import-env comp))]
    [(uir-symbol? value)
     (define nm (string->symbol (uir-symbol-name value)))
     (or (and owner (memq nm '(self cls)) owner)
         (hash-ref (receiver-types-locals rt) nm #f)
         (bare-class-value value class-defs tokens import-env comp))]
    [(uir-get? value)
     (define attr (get-field-symbol (uir-get-field value)))
     (and attr (self-or-cls-var? (uir-get-base value))
          (hash-ref (receiver-types-self-attrs rt) attr #f))]
    [else #f]))

;; Return type of a function/method body, projected through its receiver-type
;; environment: the single class every non-#f `return` expression resolves to.
;; A body with no return, a mixed set of return types, or a return that does
;; not project to a tracked class yields #f (sound: no projection is better
;; than a wrong one). `rt` is the precomputed receiver env (see
;; infer-receiver-types), so callers share one env instead of rebuilding it.
(define (return-class-of body rt class-defs [tokens default-effect-tokens]
                         [import-env #f] [comp #f] [owner #f])
  (define classes '())
  (walk-body body (λ (n)
                    (when (uir-return? n)
                      (define c (value-class-of (uir-return-value n) rt class-defs
                                                tokens import-env comp owner))
                      (when c (set! classes (cons c classes))))))
  (define uniq (remove-duplicates classes))
  (and (pair? uniq) (= (length uniq) 1) (car uniq)))

;; Build the receiver-type environment for one function body. Assignments are
;; processed in source (pre-order) order; `class-defs` is the set of known class
;; names (a hash keyed by class-name symbol), which gates which constructor
;; callees / bare class references count as a known receiver type. `base-attrs`
;; (optional) seeds the self-attribute environment with the enclosing class's
;; field/`self.attr` defaults, so a method can resolve `self.build_file_class`
;; even when that attribute is declared at class level rather than in the
;; method body.
(define (infer-receiver-types body class-defs [base-attrs (hash)]
                              [import-env #f] [comp #f] [params '()] [owner #f]
                              #:return-types [return-types #f]
                              #:methods [methods #f]
                              #:fn-defs [fn-defs #f]
                              #:known [known #f]
                              #:comp-supers [comp-supers #f])
  (define locals (make-hash))
  (define self-attrs (hash-copy base-attrs))
  ;; Seed parameter types from type annotations (`def f(x: QuerySet)`): the
  ;; annotation names a known corpus / stdlib / imported class, so a later
  ;; `x.m(...)` resolves through receiver-type dataflow rather than falling to
  ;; `unknown`. Subscripted annotations (`List[Item]`) are not tracked yet.
  (for ([p (in-list params)])
    (when (uir-typed-param? p)
      (define pname (and (uir-symbol? (uir-typed-param-name p))
                         (string->symbol (uir-symbol-name (uir-typed-param-name p)))))
      (define pcls (bare-class-value (uir-typed-param-type p) class-defs
                                     default-effect-tokens import-env comp))
      (when (and pname pcls) (hash-set! locals pname pcls))))
  (define (value-class value)
    (or (and (uir-call? value) return-types methods fn-defs known comp-supers
             (corpus-call-return-type (uir-call-callee value) comp class-defs owner
                                      methods fn-defs known comp-supers return-types))
        (value-class-of value (receiver-types locals self-attrs)
                        class-defs default-effect-tokens import-env comp owner)))
  (define (process-set! n)
    (define cls (value-class (uir-set!-value n)))
    (when cls
      (define lhs (uir-set!-name n))
      (cond
        [(and (uir-get? lhs) (get-field-symbol (uir-get-field lhs))
              (self-or-cls-var? (uir-get-base lhs)))
         (hash-set! self-attrs (get-field-symbol (uir-get-field lhs)) cls)]
        [(set!-lhs-symbol lhs)
         => (λ (nm) (hash-set! locals nm cls))]
        [else (void)])))
  (walk-body body (λ (n) (when (uir-set!? n) (process-set! n))))
  (receiver-types locals self-attrs))

;; Overlay `overlay`'s entries onto `base` (overlay wins on a key collision).
(define (hash-overlay base overlay)
  (define out (hash-copy base))
  (for ([(k v) (in-hash overlay)]) (hash-set! out k v))
  out)

;; Merge two receiver-type environments; `overlay`'s locals and self-attrs win.
;; Used to layer a function's own receiver tracking over its class's inherited
;; self-attributes (collected across fields, __init__ and all methods).
(define (receiver-types-merge base overlay)
  (receiver-types
   (hash-overlay (receiver-types-locals base) (receiver-types-locals overlay))
   (hash-overlay (receiver-types-self-attrs base) (receiver-types-self-attrs overlay))))
