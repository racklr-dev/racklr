#lang racket

;; racklr/effects/return-type.rkt
;;
;; Projected RESULT CLASS of a call expression, and the return class of a
;; function/method body. Shared by the whole-program type-relation join (the
;; assign/param/return relations) and the receiver chain, so a call result
;; projects through exactly one rule instead of one per consumer.
;;
;; The chain is:
;;   1. a corpus function/method call (`g()`, `self.g()`, `Known.m()`): the
;;      projected return type of the call-graph keys it can target;
;;   2. a method call through a receiver the relations already typed
;;      (`x.m()`, `self.attr.m()`): the projected return type of the receiver
;;      class's MRO method key — this is the receiver-type-dataflow half;
;;   3. a constructor, stdlib, imported or type-preserving call
;;      (`Known(...)`, `Path(...).is_absolute()`, `requests.get(...)`):
;;      `call-result-type` in receiver-type.rkt.
;;
;; Everything here is a projection: a mixed or unprojected return yields #f,
;; never a guess. Split out of receiver-type.rkt so the whole-program join and
;; the receiver chain can share it without either depending on the other.

(require racklr/uir
         racklr/effects/shape
         racklr/effects/walk
         racklr/effects/class-resolve
         racklr/effects/receiver-type
         racklr/effects/library-tokens
         racklr/effects/lexicon)

(provide call-result-return-class call-target-keys return-class-of)

;; The most-derived candidate key's projected return, or #f. For a method call on
;; a known static class the first key in MRO order is the method runtime dispatch
;; selects, so its projection is the sound one; an unprojected override fails
;; closed instead of inheriting a superclass projection. (The by-name corpus
;; lookup in class-resolve.rkt keeps its older skip-missing semantics.)
(define (most-derived-return keys return-types)
  (and (pair? keys) (hash-ref return-types (car keys) #f)))

;; Projected result class of a call expression, or #f. `rt` is the receiver-type
;; environment at the call site (locals from the assign relation, self-attributes
;; from the class's MRO-closed relation). `return-types` is the closed
;; (comp . name) -> class relation; when it is #f (single-file inference) only
;; the structural projections run, exactly as before.
;;
;; A method call projects from its MOST-DERIVED dispatch key only: the older
;; by-name corpus lookup skips an unprojected candidate and would let a body that
;; dispatches to an unprojected override inherit an unrelated superclass, so only
;; its bare-call half is reused here.
(define (call-result-return-class callee comp class-defs owner
                                  rt methods fn-defs known comp-supers return-types
                                  [tokens default-effect-tokens] [import-env #f])
  (or (and return-types
           (if (member-access-callee? callee)
               (most-derived-return
                (method-return-keys callee comp owner rt methods class-defs
                                    comp-supers tokens)
                return-types)
               (bare-call-return-class callee comp fn-defs known return-types)))
      (call-result-type callee class-defs tokens rt import-env comp)))

;; Bare-call result class: the unique corpus definition key's projected return.
;; A bare call has no MRO dispatch, so one key is the whole answer.
(define (bare-call-return-class callee comp fn-defs known return-types)
  (define nm (callee-name callee))
  (define k (and nm (bare-function-key nm comp fn-defs known)))
  (and k (hash-ref return-types k #f)))

;; Every (comp . Class.method) key a method call can dispatch to, MOST-DERIVED
;; FIRST: the syntactic keys (`self.m()`, `super().m()`, `KnownClass.m()`) plus
;; the receiver-type-dataflow keys for a tracked local / attribute receiver
;; (`x.m()`, `self.attr.m()`). `most-derived-return` reads the first key.
(define (method-return-keys callee comp owner rt methods class-defs comp-supers tokens)
  (define syntactic
    (method-callee-keys callee comp owner methods class-defs comp-supers tokens))
  (define dataflow
    (and rt
         (let* ([base (member-access-base callee)]
                [mname (member-access-field callee)]
                [rcls (and base mname (receiver-class-of rt base tokens))]
                [c (and rcls (resolve-class-component rcls comp class-defs))])
           (and c mname
                (mro-method-keys c rcls mname methods class-defs comp-supers)))))
  (append syntactic (or dataflow '())))

;; (comp . name) call-graph keys a call targets, when the call goes through a
;; receiver the type relations already typed (`self.storage.save(...)`: the
;; storage class's `save` through its MRO). Complements `method-callee-keys`,
;; which resolves syntactically (`self.m()`, `super().m()`, `KnownClass.m()`)
;; and cannot see an attribute or local receiver. Returns #f (or '()) when
;; neither resolves. Used by the parameter relation so a method called through a
;; typed receiver still contributes its argument types.
(define (call-target-keys callee comp owner rt methods class-defs comp-supers
                          [tokens default-effect-tokens])
  (define syntactic
    (and (member-access-callee? callee)
         (method-callee-keys callee comp owner methods class-defs comp-supers tokens)))
  (cond
    [(pair? syntactic) syntactic]
    [else
     (and rt (member-access-callee? callee)
          (let* ([base (member-access-base callee)]
                 [mname (member-access-field callee)]
                 [rcls (and base mname (receiver-class-of rt base tokens))]
                 [c (and rcls (resolve-class-component rcls comp class-defs))])
            (and c mname
                 (mro-method-keys c rcls mname methods class-defs comp-supers))))]))

;; Return type of a function/method body: the single class every non-#f
;; `return` expression resolves to. A body with no return, a mixed set of return
;; types, or a return that does not project to a tracked class yields #f (sound:
;; no projection is better than a wrong one). `rt` is the precomputed receiver
;; env (see infer-receiver-types), and a returned call projects through
;; `call-result-return-class` so a function that returns another call's result
;; carries that result class instead of stopping the chain.
(define (return-class-of body rt class-defs [tokens default-effect-tokens]
                         [import-env #f] [comp #f] [owner #f]
                         #:return-types [return-types #f]
                         #:methods [methods #f]
                         #:fn-defs [fn-defs #f]
                         #:known [known #f]
                         #:comp-supers [comp-supers #f])
  (define (value-class value)
    (or (and (uir-call? value) return-types methods fn-defs known comp-supers
             (call-result-return-class (uir-call-callee value) comp class-defs
                                       owner rt methods fn-defs known comp-supers
                                       return-types tokens import-env))
        (value-class-of value rt class-defs tokens import-env comp owner)))
  (define classes '())
  (walk-body body (λ (n)
                    (when (uir-return? n)
                      (define c (value-class (uir-return-value n)))
                      (when c (set! classes (cons c classes))))))
  (define uniq (remove-duplicates classes))
  (and (pair? uniq) (= (length uniq) 1) (car uniq)))
