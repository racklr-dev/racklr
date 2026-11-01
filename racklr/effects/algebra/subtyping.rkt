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
;;                        contract order *restricted to the rows the algebra
;;                        carries* (see the prohibition note below); the
;;                        algebra's `join` is its least upper bound.
;;   row-interpretable-by? A row's effects are each interpreted by a handler in
;;                        a declared capability row: the row-level form of the
;;                        bridge checker's capability-name → host-effect-names
;;                        table. A row whose effects are all interpreted by the
;;                        declared needs passes; the uncovered effects are the
;;                        violations.
;;   declared-prohibited-effects
;;                        The `must-not` effect names of a declared (UIR) row.
;;                        The read-through for prohibitions, which algebra rows
;;                        do not carry.
;;
;; Prohibition is not an algebra-row concept. An algebra row canonicalizes
;; `must-not` entries away because absence is prohibition on the *inferred*
;; side (docs §3.2) and the two are `≤`-equivalent there. That is exactly why
;; `row-subtype?` must not be used to enforce a declared prohibition: a declared
;; `must-not db` would look like "no statement about db". A contract check that
;; reads declared rows must take the prohibitions from the UIR row itself
;; (`declared-prohibited-effects`); `row-subtype?` then answers only the
;; permission/obligation part of the comparison. The property tests pin both
;; directions: an algebra row's subtype relation equals `effect-subtype?` on the
;; stripped UIR rows, and the declared prohibitions are still readable.
;;
;; The laws exercised by the property tests: reflexivity and transitivity of
;; subtyping, the empty row as bottom, join as the least upper bound, and the
;; must-not canonicalization agreement with `effect-subtype?`.

(require racket/set
         (only-in racklr/effects/contract effect-subtype? effect-name-symbol)
         (only-in racklr/uir uir-effect-row?
                  uir-effect-row-entries
                  uir-effect-entry-modality)
         "row.rkt")

(provide
  row-subtype?
  row-equivalent?
  declared-prohibited-effects
  allowed-effect-names
  row-uninterpreted-effects
  row-interpretable-by?)

;; Row subtyping in the contract order, restricted to the entries an algebra row
;; can carry: permissions (`may`), obligations (`must`), and deferrals
;; (`unknown`). Prohibitions (`must-not`) are canonicalized to absence in
;; algebra rows, so this relation is `≤` on those rows only — a declared
;; `must-not` must be read from the UIR row (`declared-prohibited-effects`), not
;; from this relation. This restriction is safe for inference rows (§3.2) and
;; deliberate for declared rows; do not use it alone to enforce a prohibition.
(define (row-subtype? a b)
  (effect-subtype? (algebra-row->uir a) (algebra-row->uir b)))

;; Mutual subtyping: the same capability under two representations.
(define (row-equivalent? a b)
  (and (row-subtype? a b) (row-subtype? b a)))

;; The `must-not` effect names of a declared (UIR) row, in canonical order.
;; Algebra rows drop prohibitions; a contract check over declared rows reads
;; them here so a prohibition is never silently treated as "no statement".
(define (declared-prohibited-effects row)
  (unless (uir-effect-row? row)
    (raise-argument-error 'declared-prohibited-effects "uir-effect-row?" row))
  (for/list ([e (in-list (uir-effect-row-entries row))]
             #:when (eq? (uir-effect-entry-modality e) 'must-not))
    (effect-name-symbol e)))

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
