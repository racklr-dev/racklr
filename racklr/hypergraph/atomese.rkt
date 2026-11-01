#lang racket

;; racklr/hypergraph/atomese.rkt
;;
;; Rendering a typed atom into OpenCog-compatible Atomese S-expression text.
;; Kept apart from the kernel because the faithful link/node vocabulary it
;; special-cases belongs to the capability/type vocabulary, not to the graph
;; algebra. The serializer is a pure function of the atom shape; it never
;; defines or requires the vocabulary constructors themselves.

(require "kernel.rkt")

(provide atom->atomese)

;; Capitalized link-type name with the "Link" suffix, e.g. 'effect-op →
;; 'EffectOpLink. Special-case the canonical OpenCog names (InheritanceLink,
;; EvaluationLink) in atom->atomese below.
(define (atomese-link-name ty)
  (define s (symbol->string ty))
  (string->symbol
   (string-append (string-upcase (substring s 0 1)) (substring s 1) "Link")))

;; Render one atom as an OpenCog Atomese S-expression string. Faithful node
;; names: type/domain/str/sym → ConceptNode, op/verb → PredicateNode, num →
;; NumberNode. Faithful link names for the effect vocabulary (InheritanceLink,
;; EvaluationLink); everything else uses the capitalized link type.
(define (atom->atomese a)
  (cond
    [(node? a)
     (define ty (node-type a))
     (define nm (node-name a))
     (cond
       [(memq ty '(op verb)) (format "(PredicateNode ~s)" nm)]
       [(eq? ty 'num) (format "(NumberNode ~s)" nm)]
       [else (format "(ConceptNode ~s)" nm)])]
    [(link? a)
     (define os (link-out a))
     (case (link-type a)
       [(effect-op)
        (define d (first os)) (define t (second os)) (define o (third os))
        (format "(EvaluationLink (PredicateNode ~s) (ConceptNode ~s))"
                (format "~a.~a" (node-name d) (node-name o))
                (node-name t))]
       [(inherits)
        (format "(InheritanceLink (ConceptNode ~s) (ConceptNode ~s))"
                (node-name (first os)) (node-name (second os)))]
       [else
        (format "(~a~a)"
                (atomese-link-name (link-type a))
                (if (null? os) ""
                    (string-append " " (string-join (map atom->atomese os) " "))))])]
    [(var? a) (format "~s" (var-name a))]
    [else (format "~s" a)]))

(module+ main
  (displayln "racklr/hypergraph/atomese — Atomese projection loaded."))
