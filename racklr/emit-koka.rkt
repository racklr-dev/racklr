#lang racket

;; Koka 3.x UIR → source emitter. Inverse of racklr/lower-koka.rkt.
;;
;; Public call shape (unchanged): (emit-koka uir [row]) → string?
;; The emit contract is unchanged: an explicit effect row is attached to the
;; unit's entry function (`racklr/emit-contract`) and every named function's
;; inferred row is written back (`racklr/effects/infer`), so a Koka function
;; can carry its row in the typed effect annotation.
;;
;; Emission choices (see emit-koka/common.rkt):
;;   - every block is a braced ONE-LINE `{ a; b; c }`, so Koka's layout pass
;;     never inserts an implicit brace or semicolon inside a declaration;
;;   - every operator application is parenthesized, so no precedence table is
;;     needed and the parser's flat left-recursive chain round-trips;
;;   - declarations are one line each, separated by newlines.
;;
;; The emitter is split by responsibility:
;;   emit-koka/common.rkt  escaping, blocks, decoration, patterns/params,
;;                         type text and the Koka effect-row spelling
;;   emit-koka/expr.rkt    expressions, applications, control flow, handlers
;;   emit-koka/item.rkt    declarations and imports

(require racklr/uir
         racklr/emit-contract
         racklr/effects/infer
         "emit-koka/common.rkt"
         "emit-koka/expr.rkt"
         "emit-koka/item.rkt")

(provide emit-koka)

(define (emit-koka uir [row #f])
  (install-expr-hooks!)
  (install-item-hooks!)
  (define sourced (attach-root-row 'emit-koka uir row))
  (define annotated (annotate-effects sourced #:wrap-anonymous? #f))
  (define units
    (cond
      [(uir-block? annotated) (uir-block-stmts annotated)]
      [(uir-module? annotated) (list annotated)]
      [else (list annotated)]))
  (string-join (map emit-stmt units) "\n"))

(module+ main
  (displayln "racklr/emit-koka — UIR → Koka 3.x text emitter loaded."))
