#lang racket

;; racklr/graph/provenance-check.rkt
;;
;; The completeness check of the effect-edge provenance graph (P4.2): "no effect
;; edge exists without provenance", as a property of the projected atoms rather
;; than a promise from the record tables.
;;
;; Every effect edge must have a ground reachable from it: start at a function,
;; follow call-site links, and end at a function with a grounds link whose entry
;; has the same effect name. One reverse reachability pass per effect kind marks
;; every function that can reach a ground of that effect; an effect edge whose
;; function is not marked is a projection bug.
;;
;; Split out of racklr/graph/provenance.rkt for the 400-line modularity rule;
;; the provenance module re-exports this API.

(require "provenance-atoms.rkt"
         "provenance-index.rkt")

(provide unprovenanced-edges assert-provenance-complete!)

;; Every effect edge in the graph whose effect has no ground reachable from it.
;; One reverse reachability pass per effect kind: from every ground owner, walk
;; the reverse call-site graph and mark the functions that can reach it. This is
;; the same traversal `provenance-missing` computes over the provenance record,
;; but over the projected atoms — so it is the check that a broken projection
;; fails, not a check that merely trusts the tables.
(define (unprovenanced-edges graph)
  (define idx (ensure-index graph))
  (define reverse (make-hash))       ;; callee fq → (listof caller fq)
  (for ([(caller calls) (in-hash (graph-index-calls idx))])
    (for ([call (in-list calls)])
      (hash-update! reverse (first call)
                    (λ (l) (if (member caller l) l (cons caller l))) '())))
  (define reachable (make-hash))      ;; effect → (hash fn #t)
  (define (mark effect fn)
    (define seen (hash-ref! reachable effect make-hash))
    (let loop ([work (list fn)])
      (unless (null? work)
        (define f (car work))
        (if (hash-ref seen f #f)
            (loop (cdr work))
            (begin
              (hash-set! seen f #t)
              (loop (append (cdr work) (hash-ref reverse f '()))))))))
  (for ([(fn grounds) (in-hash (graph-index-grounds idx))])
    (for ([ground (in-list grounds)])
      (mark (second ground) fn)))
  (sort
   (for*/list ([(fq entries) (in-hash (graph-index-edges idx))]
               [entry (in-list entries)]
               #:unless (hash-ref (hash-ref reachable (hg-entry-effect entry) (hash)) fq #f))
     (list fq (hg-entry-effect entry)))
   (λ (a b) (string<? (format "~a/~a" (car a) (cadr a))
                      (format "~a/~a" (car b) (cadr b))))))

;; Raise unless every effect edge in the graph is provable.
(define (assert-provenance-complete! graph)
  (define missing (unprovenanced-edges graph))
  (if (null? missing)
      (void)
      (error 'assert-provenance-complete!
             "effect edges without provenance: ~a"
             (string-join (map (λ (m) (format "~a/~a" (car m) (cadr m))) missing)
                          ", "))))
