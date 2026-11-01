#lang racket

;; racklr/effects/algebra/subtyping.rkt
;;
;; Effect-row subtyping and handler interpretation, the Phase-1 generalization
;; of the bridge checker's capability-interpretation table
;; (effervesce/check/bridge.rkt).
;;
;; Two relations are provided:
;;
;;   row-subtype?         A ≤ B: every effect A records is permitted by B, with
;;                        A's budgets within B's and A's operations covered by
;;                        B's (docs/effects-proposal.md §3.3). This is the
;;                        contract order; the algebra's `join` is its least
;;                        upper bound.
;;   row-interpretable-by? A row's effects are each interpreted by a handler in
;;                        a declared capability row: the row-level form of the
;;                        bridge checker's capability-name → host-effect-names
;;                        table. A row whose effects are all interpreted by the
;;                        declared needs passes; the uncovered effects are the
;;                        violations.
;;
;; The laws exercised by the property tests: reflexivity and transitivity of
;; subtyping, the empty row as bottom, and join as the least upper bound.

(require racket/set
         (only-in racklr/effects/contract effect-subtype?)
         "row.rkt")

(provide
  row-subtype?
  row-equivalent?
  allowed-effect-names
  row-uninterpreted-effects
  row-interpretable-by?)

;; Row subtyping in the contract order. The algebra row materializes to the
;; canonical UIR row and the existing contract relation — which the checkers
;; already implement, and which stays the single definition of `≤` — decides.
(define (row-subtype? a b)
  (effect-subtype? (algebra-row->uir a) (algebra-row->uir b)))

;; Mutual subtyping: the same capability under two representations.
(define (row-equivalent? a b)
  (and (row-subtype? a b) (row-subtype? b a)))

;; The host effect names a set of declared capabilities interprets.
;; `interpretations` maps capability name (string) → allowed host effect names
;; (list of symbols or strings). This is the DATA table the bridge checker
;; already reads; no capability name is hard-coded here.
(define (allowed-effect-names needs interpretations)
  (for/fold ([allowed (set)]) ([need (in-list needs)])
    (for/fold ([allowed allowed])
              ([effect (in-list (hash-ref interpretations need '()))])
      (set-add allowed (if (symbol? effect) (symbol->string effect) effect)))))

;; The row's effects that the declared capabilities do not interpret, in
;; canonical order. `unknown` is never interpreted: it is the tracked
;; obligation, reported separately by the checker, not a permission.
(define (row-uninterpreted-effects row needs interpretations)
  (define allowed (allowed-effect-names needs interpretations))
  (for/list ([name (in-list (row-effect-names row))]
             #:unless (set-member? allowed (symbol->string name)))
    name))

;; A row is interpretable by a declared capability row when every effect it
;; carries is covered by a handler. This is `A ≤ B` with B expressed as the
;; capability interpretation table instead of a same-named host row.
(define (row-interpretable-by? row needs interpretations)
  (null? (row-uninterpreted-effects row needs interpretations)))
