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
;;   row-subtype/interpretation
;;                        A ≤ B when every effect in A is interpretable by a
;;                        handler in B, under a handler interpretation. The
;;                        interpretation generalizes the bridge's
;;                        capability → effect-names table to the full algebra
;;                        over rows: a handler maps to a typed row, so it can
;;                        bound the modality, budget and ops of the effects
;;                        it interprets. Scope is carried through but, as in
;;                        `row-subtype?`, not compared. The identity
;;                        interpretation makes this relation `row-subtype?`
;;                        itself.
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
         (only-in racklr/uir uir-effect-row? uir-effect-row
                  uir-effect-row-entries uir-effect-entry
                  uir-effect-entry-name uir-effect-entry-modality
                  uir-effect-entry-ops uir-effect-entry-budget
                  uir-effect-entry-scope uir-symbol uir-symbol-name)
         "row.rkt")

(provide
  row-subtype?
  row-equivalent?
  declared-prohibited-effects
  allowed-effect-names
  row-uninterpreted-effects
  row-interpretable-by?
  ;; row-valued handler interpretation
  interpretation interpretation? interpretation-table
  identity-interpretation identity-interpretation?
  interpretation-from-names interpretation-from-rows
  interpretation-closure
  handler-row
  handler-interprets?
  row-subtype/interpretation
  row-uninterpreted-effects/interpretation)

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

;; ── row-valued handler interpretation ───────────────────────────────
;;
;; `row-interpretable-by?` below is the bridge checker's relation exactly: a
;; capability name interprets a *set of host effect names*. The full algebra
;; generalizes a table value to a typed row, so an interpretation bounds the
;; modality, budget and ops of every effect it provides, and coverage is the
;; same entry comparison as `row-subtype?` instead of a name-membership test.
;; (The contract entry order does not compare scope; the interpreted entry
;; carries it for diagnostics and for callers that add a scope check.)
;;
;; Row A is a subtype of row B under an interpretation when every effect in A
;; is interpretable by a handler entry in B: for each `a` in A, some `b` in B
;; interprets it through the interpretation's row for `b`'s name. Under
;; `identity-interpretation` every name is its own handler, so the relation is
;; `row-subtype?` itself (the structural contract order).

;; #f is the identity; otherwise a table from handler name (symbol) to the full
;; declared UIR row of effects that handler interprets. Rows are the declared
;; form, so an interpreted entry may itself be `must-not` (the handler
;; prohibits that effect).
(struct interpretation (table) #:transparent)

(define (identity-interpretation) (interpretation #f))

(define (identity-interpretation? i)
  (and (interpretation? i) (not (interpretation-table i))))

;; Capability names are written as strings in the bridge table and as symbols
;; in a row; normalize to the symbol form the rows use.
(define (name-symbol x)
  (cond [(symbol? x) x]
        [(string? x) (string->symbol x)]
        [else (raise-argument-error 'name-symbol "(or/c symbol? string?)" x)]))

(define (->uir-row r)
  (cond [(uir-effect-row? r) r]
        [(algebra-row? r) (algebra-row->uir r)]
        [else (raise-argument-error 'interpretation "effect row" r)]))

;; The bridge checker's table shape: handler name → list of host effect names.
;; A name-only entry carries no constraint, so its interpreted entry sits at the
;; top of the contract order (`must`), which covers every modality of the effect
;; being interpreted. This is what makes the relation agree exactly with the
;; name-membership check it generalizes.
(define (interpretation-from-names name->effects)
  (interpretation
   (for/hash ([(name effects) (in-hash name->effects)])
     (values (name-symbol name)
             (uir-effect-row
              (for/list ([effect (in-list effects)])
                (uir-effect-entry
                 (uir-symbol (symbol->string (name-symbol effect)))
                 'must '() #f #f)))))))

;; The full generalization: handler name → typed row of interpreted effects.
(define (interpretation-from-rows name->row)
  (interpretation
   (for/hash ([(name row) (in-hash name->row)])
     (values (name-symbol name) (->uir-row row)))))

;; A declared-capability row: each name is a handler entry with no constraint
;; of its own (the bridge checker's `needs` shape).
(define (handler-row names)
  (uir-effect-row
   (for/list ([name (in-list names)])
     (uir-effect-entry (uir-symbol (symbol->string (name-symbol name)))
                       'may '() #f #f))))

;; The contract entry order on two single entries.
(define (entry-subtype? a b)
  (effect-subtype? (uir-effect-row (list a)) (uir-effect-row (list b))))

(define (entry-ops e)
  (map (λ (o) (string->symbol (uir-symbol-name o)))
       (uir-effect-entry-ops e)))

;; Bound an interpreted effect by the handler entry that provides it: the
;; handler's budget is an upper bound, its ops intersect, and a prohibition on
;; either side wins. The interpreted entry keeps its own (host) name, so the
;; bound applies across the capability → effect rename. A positive modality on
;; the handler does not lift the interpretation's own modality.
(define (bound-entry interpreted handler)
  (define hs (uir-effect-entry-scope handler))
  (define is (uir-effect-entry-scope interpreted))
  (define scope
    (cond [(not is) hs]
          [(not hs) is]
          [(string=? (uir-symbol-name is) (uir-symbol-name hs)) is]
          [else #f]))
  (define hops (entry-ops handler))
  (define iops (entry-ops interpreted))
  (define ops
    (cond [(null? hops) iops]
          [(null? iops) hops]
          [else (filter (λ (o) (memq o hops)) iops)]))
  (define hb (uir-effect-entry-budget handler))
  (define ib (uir-effect-entry-budget interpreted))
  (uir-effect-entry
   (uir-effect-entry-name interpreted)
   (if (eq? (uir-effect-entry-modality handler) 'must-not)
       'must-not
       (uir-effect-entry-modality interpreted))
   (for/list ([o (in-list ops)]) (uir-symbol (symbol->string o)))
   (cond [(not hb) ib] [(not ib) hb] [else (min hb ib)])
   scope))

;; The effects one handler entry interprets, with the handler's bounds applied.
;; Under the identity interpretation a handler is its own interpretation.
(define (handler-effects interp handler)
  (cond
    [(identity-interpretation? interp) (list handler)]
    [else
     (define interpreted
       (or (hash-ref (interpretation-table interp) (effect-name-symbol handler) #f)
           (uir-effect-row '())))
     (for/list ([e (in-list (uir-effect-row-entries interpreted))])
       (bound-entry e handler))]))

;; Is effect entry `a` interpretable by handler entry `b`?
(define (handler-interprets? a b interp)
  (for/or ([e (in-list (handler-effects interp b))])
    (entry-subtype? a e)))

(define (effect-handled? a handlers interp)
  (or (eq? (uir-effect-entry-modality a) 'must-not)
      (for/or ([b (in-list handlers)])
        (handler-interprets? a b interp))))

;; Every effect in A is interpretable by a handler in B. Both rows are accepted
;; as algebra rows or UIR rows.
(define (row-subtype/interpretation a b interp)
  (define be (uir-effect-row-entries (->uir-row b)))
  (andmap (λ (e) (effect-handled? e be interp))
          (uir-effect-row-entries (->uir-row a))))

;; The effects of A that no handler in B interprets, in A's row order.
;; `unknown` is the tracked obligation rather than an interpreted permission, so
;; it is reported here (a checker surfaces it as unverified, not a hard
;; violation).
(define (row-uninterpreted-effects/interpretation a b interp)
  (define be (uir-effect-row-entries (->uir-row b)))
  (for/list ([e (in-list (uir-effect-row-entries (->uir-row a)))]
             #:unless (effect-handled? e be interp))
    e))

;; Compose handlers: `Store via Db` plus `Db via db` means a handler for Store
;; also interprets db. Each handler's row keeps its direct entries and gains the
;; transitive expansion, with bounds composed along the chain. A cycle stops at
;; the repeated name, keeping that entry in its direct form.
(define (interpretation-closure interp)
  (cond
    [(identity-interpretation? interp) interp]
    [else
     (define table (interpretation-table interp))
     (define (expand-entry e visited)
       (define name (effect-name-symbol e))
       (define inner (hash-ref table name #f))
       (cond
         [(or (not inner) (set-member? visited name)) '()]
         [else
          (define visited* (set-add visited name))
          (append-map (λ (ie)
                        (define bounded (bound-entry ie e))
                        (cons bounded (expand-entry bounded visited*)))
                      (uir-effect-row-entries inner))]))
     (interpretation
      (for/hash ([(name row) (in-hash table)])
        (values name
                (uir-effect-row
                 (remove-duplicates
                  (append (uir-effect-row-entries row)
                          (append-map (λ (e) (expand-entry e (set name)))
                                      (uir-effect-row-entries row))))))))]))

;; ── the bridge checker's name-set relation ──────────────────────────
;;
;; Kept as the bridge-facing API, now derived from the row-valued relation so
;; the two cannot disagree.

;; The row's effects that the declared capabilities do not interpret, in
;; canonical order.
(define (row-uninterpreted-effects row needs interpretations)
  (map effect-name-symbol
       (row-uninterpreted-effects/interpretation
        row
        (handler-row needs)
        (interpretation-from-names interpretations))))

;; A row is interpretable by a declared capability row when every effect it
;; carries is covered by a handler. This is `A ≤ B` with B expressed as the
;; capability interpretation table instead of a same-named host row.
(define (row-interpretable-by? row needs interpretations)
  (null? (row-uninterpreted-effects row needs interpretations)))
