#lang racket

;; racklr/effects/algebra/certificate.rkt
;;
;; The kernel's certificate checker: the decision function of the trusted leaf
;; (ROADMAP.md "The kernel-leaf theory"; LOOP_PLAN P1.6).
;;
;; The producer (inference: the fixpoint, receiver-type dataflow, lexicons)
;; asserts a CLAIM about one function's effects — an algebra-row plus the
;; evidence trail that grounds every effect edge. The kernel re-verifies the
;; claim against a declared contract (a needs row + a handler interpretation)
;; using only the algebra's laws and subtype relation. A bug in the producer
;; can emit a wrong row; it cannot make the kernel accept a claim that is not
;; grounded or not interpretable, because the kernel checks both and never
;; re-derives.
;;
;; This module depends only on the kernel's own modules (provenance, row,
;; subtyping), never on inference, lexicons, or the host. The handler
;; interpretation is producer-supplied DATA, passed in, so the kernel stays
;; host-neutral.

(require (only-in racklr/uir uir-effect-entry-name uir-symbol-name)
         "provenance.rkt"
         "row.rkt"
         "subtyping.rkt")

(provide
  (struct-out claim)
  make-claim
  claim-evidence-of
  ungrounded-effects
  claim-violations
  check-claim
  check-claim-ok?)

;; A claim: what the producer asserts about one function's effects.
;;   row      — an algebra-row (the inferred effects).
;;   evidence — an immutable hash effect-name-symbol -> (listof provenance):
;;              the grounding trail for every claimed effect edge.
(struct claim (row evidence) #:transparent)

;; Build a claim from an algebra-row, taking the evidence from the row's own
;; provenance table (the rows inference writes already carry it).
(define (make-claim row)
  (claim row (row-provenance-table row)))

;; The evidence trail recorded for one claimed effect name (possibly empty).
(define (claim-evidence-of c name)
  (hash-ref (claim-evidence c) name '()))

;; Claimed effects that carry no evidence. An effect edge with no evidence is
;; not derivable from the certificate: the producer asserts it, but the kernel
;; cannot ground it, so the claim is rejected.
(define (ungrounded-effects c)
  (for/list ([name (in-list (row-effect-names (claim-row c)))]
             #:unless (pair? (claim-evidence-of c name)))
    name))

;; The effect-name symbol of a row entry (the violations come back as entries).
(define (entry-name e)
  (string->symbol (uir-symbol-name (uir-effect-entry-name e))))

;; Claimed effects the declared needs row does not interpret (the violations),
;; as effect-name symbols for symmetry with `ungrounded-effects`.
(define (claim-violations c needs interp)
  (map entry-name
       (row-uninterpreted-effects/interpretation (claim-row c) needs interp)))

;; Re-verify a claim against a declared contract. Returns three values:
;;   ok?        — #t when every claimed effect is grounded AND interpretable.
;;   violations — claimed effects the declared needs do not interpret.
;;   ungrounded — claimed effects with no evidence.
;; The producer's certificate is accepted only when both lists are empty.
(define (check-claim c needs interp)
  (values
   (and (null? (ungrounded-effects c))
        (null? (claim-violations c needs interp)))
   (claim-violations c needs interp)
   (ungrounded-effects c)))

;; The single boolean the gate consumes.
(define (check-claim-ok? c needs interp)
  (call-with-values (λ () (check-claim c needs interp))
    (λ (ok? violations ungrounded) ok?)))
