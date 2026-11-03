#lang racket

;; racklr/effects/self-attr-relation.rkt
;;
;; The self-attr relation of the whole-program receiver-type dataflow: the
;; class-level field/annotation table, the `self.attr` assignments inferred from
;; method bodies (constructor parameters included), the MRO-closed inherited
;; environment, and the parameter-annotation seed. Split out of
;; type-relations.rkt, which owns the four-relation join and depends on this
;; seed; the join's consumers read the closed relation, never these builders.
;;
;; The attribute-read projection lives here: a class field is typed from its
;; initializer or its written annotation, an instance attribute from its
;; constructor assignment (whose value projects through the shared call-result
;; rule, #119), and inheritance is resolved most-derived-first across the MRO.

(require racklr/uir
         racklr/effects/collect
         racklr/effects/receiver-type
         racklr/effects/return-type
         racklr/effects/library-tokens
         racklr/effects/lexicon)

(provide class-field-attrs class-self-attrs class-attr-env seed-params)

;; Class name of a class-body field initializer (`attr = KnownClass`), #f when
;; the initializer does not name a known class. (was field-init-class.)
(define (field-class init class-defs import-env comp [tokens default-effect-tokens])
  (cond
    [(bare-class-value init class-defs tokens import-env comp)]
    [(uir-call? init) (call-result-type (uir-call-callee init) class-defs
                                        tokens #f import-env comp)]
    [else #f]))

;; Class-level field initializers keyed by (comp . class) -> { attr -> class }.
;; A field's declared type is the fall-back when its initializer does not
;; project one: `attr: KnownClass` (annotation-only or annotated-with-value)
;; declares the attribute's receiver class, so `self.attr.m()` resolves through
;; it — the field half of the attribute-read projection. `attr: list[T]` and
;; other subscripted types name containers, not their element, and are not
;; unwrapped. The initializer wins when it projects (most-derived value).
(define (class-field-attrs comp-programs class-defs import-env
                           [tokens default-effect-tokens])
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
               (define cls (or (field-class (uir-field-init f) class-defs import-env comp tokens)
                               (annotation-class-of (uir-field-type f) class-defs
                                                    tokens import-env comp)))
               (when (and attr cls) (hash-set! acc attr cls)))))]
        [(struct? n)
         (for ([f (in-list (cdr (vector->list (struct->vector n))))]) (walk f))]
        [(pair? n) (walk (car n)) (walk (cdr n))])))
  h)

;; Wrap a propagated param type so the annotation-seeding path picks it up. A
;; var-positional parameter's entry is a tagged argument tuple, not a class
;; annotation, so it is left untouched (`infer-receiver-types` reads only the
;; fixed parameters).
(define (seed-params params k param-types)
  (for/list ([i (in-naturals)] [p (in-list params)])
    (define cls (hash-ref param-types (cons k i) #f))
    (cond
      [(and (symbol? cls) (uir-symbol? p))
       (uir-typed-param p (uir-symbol (symbol->string cls)) #f #f)]
      [(and (symbol? cls) (uir-typed-param? p) (not (uir-typed-param-type p)))
       (uir-typed-param (uir-typed-param-name p)
                        (uir-symbol (symbol->string cls))
                        (uir-typed-param-default p)
                        (uir-typed-param-kind p))]
      [else p])))

;; Self-attribute assignments across every class: class fields first, then
;; `self.attr = KnownClass(...)` from each method (overlaying the defaults).
;; Keyed by (comp . class), iterated over sorted method names so a conflicting
;; attr resolves deterministically instead of by hash order.
(define (class-self-attrs comp-programs class-defs import-env
                          [param-types (hash)]
                          [return-types #f]
                          [methods #f] [fn-defs #f] [known #f] [comp-supers #f]
                          #:tokens [tokens default-effect-tokens])
  (define h (class-field-attrs comp-programs class-defs import-env tokens))
  (for ([cp (in-list comp-programs)])
    (define comp (car cp))
    (define mparams (collect-method-params (cdr cp)))
    (define cmethods (collect-methods (cdr cp)))
    (for ([n (in-list (sort (hash-keys cmethods) symbol<?))])
      (define body (hash-ref cmethods n))
      (define owner (class-name-of-name n))
      (define k (cons comp n))
      (define sa (receiver-types-self-attrs
                  (infer-receiver-types body class-defs (hash) import-env comp
                                        (seed-params (hash-ref mparams n '()) k param-types)
                                        owner
                                        #:tokens tokens
                                        #:return-types return-types
                                        #:methods methods
                                        #:fn-defs fn-defs
                                        #:known known
                                        #:comp-supers comp-supers
                                        ;; The self-attr seed uses the SAME
                                        ;; call-result projection as the assign
                                        ;; relation (#119): an attribute
                                        ;; assigned from a method on a tracked
                                        ;; receiver projects, and an
                                        ;; unprojected override fails closed
                                        ;; instead of inheriting a superclass
                                        ;; return.
                                        #:call-result-class
                                        (λ (c env)
                                          (call-result-return-class
                                           c comp class-defs owner env methods fn-defs
                                           known comp-supers return-types tokens
                                           import-env)))))
      (define key (cons comp owner))
      (define cur (hash-ref h key #f))
      (cond
        [(not cur) (unless (zero? (hash-count sa)) (hash-set! h key (hash-copy sa)))]
        [else (for ([(attr kls) (in-hash sa)]) (hash-set! cur attr kls))])))
  h)

;; Inherited self-attribute environment, MRO across files (most-derived first);
;; returns receiver-types with no locals (locals are per-function).
(define (class-attr-env comp class-name self-attr class-defs comp-supers)
  (define seen (make-hash))
  (define acc (make-hash))
  (let loop ([c comp] [cls class-name])
    (when (and c cls (not (hash-has-key? seen (cons c cls))))
      (hash-set! seen (cons c cls) #t)
      (for ([(attr kls) (in-hash (hash-ref self-attr (cons c cls) (hash)))])
        (unless (hash-has-key? acc attr) (hash-set! acc attr kls)))
      (for ([sr (in-list (hash-ref comp-supers (cons c cls) '()))])
        (define c2 (car sr))
        (define s (cdr sr))
        (when (and c2 s) (loop c2 s)))))
  (receiver-types (hash) acc))
