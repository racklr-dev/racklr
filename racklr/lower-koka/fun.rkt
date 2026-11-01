#lang racket

;; racklr/lower-koka/fun.rkt — Koka function bodies and lambdas → `uir-fn`.
;;
;; A `funbody` is shared by named functions (`fun f(...) ...`) and lambdas
;; (`fn(...) ...`). Explicit type parameters have no `uir-fn` field, so they
;; ride on an `uir-decorated` wrapper holding a `uir-type-params` node; the
;; emitter places them between the name and the parameter list. The declared
;; result type is split into the UIR return type and the UIR effect row (see
;; type.rkt).

(require racklr/tree
         racklr/uir
         "helpers.rkt"
         "type.rkt"
         "params.rkt"
         "hooks.rkt")

(provide lower-funbody lower-fnexpr install-fun-hooks!)

(define (install-fun-hooks!)
  (lower-funbody-fn lower-funbody)
  (lower-fnexpr-fn lower-fnexpr))

(define (lower-fnexpr node)
  (define funbody (find-node node 'funbody))
  (lower-funbody funbody #f))

(define (lower-funbody node name)
  (define typeparams (find-node node 'typeparams))
  (define pparams (find-node node 'pparameters))
  (define params (if pparams (lower-pparameters pparams) '()))
  (define tresult (find-node node 'tresult))
  (define qualifier (find-node node 'qualifier))
  (define-values (ret effects)
    (if tresult
        (let-values ([(r e) (lower-tresult tresult)])
          (values (qualifier-type r qualifier) e))
        (values #f #f)))
  ;; The body is a braced `block` when a result type is declared, and a
  ;; `bodyexpr` (blockexpr or `-> blockexpr`) otherwise. `bodyexpr` is a
  ;; wrapper node, so its blockexpr has to be looked up inside it.
  (define body-block (find-node node 'block))
  (define body-expr (find-node node 'bodyexpr))
  (define inner-blockexpr (and body-expr (find-node body-expr 'blockexpr)))
  (define inner-expr (and body-expr (find-node body-expr 'expr)))
  (define body
    (cond
      [body-block ((lower-block-fn) body-block)]
      [inner-blockexpr ((lower-blockexpr-fn) inner-blockexpr)]
      ;; e.g. `fn() val x = e in body` — an expression form, not a block.
      [inner-expr ((lower-expr-fn) inner-expr)]
      [else (uir-null)]))
  (define fn (uir-fn name params body ret effects))
  (if (and typeparams (pair? (lower-type-params typeparams)))
      (uir-decorated (list (uir-type-params (lower-type-params typeparams))) fn)
      fn))

(define (qualifier-type ret qualifier)
  (define preds (and qualifier (find-node qualifier 'predicates1)))
  (if preds
      (uir-symbol (string-append (uir-symbol-name ret)
                                 " with(" (lower-type-text preds) ")"))
      ret))
