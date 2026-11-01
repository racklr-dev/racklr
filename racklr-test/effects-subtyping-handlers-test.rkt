#lang racket

;; racklr-test/effects-subtyping-handlers-test.rkt
;;
;; P1.4 — effect subtyping and handler interpretation.
;;
;; Pins the row-valued handler interpretation in
;; `racklr/effects/algebra/subtyping`: the full-algebra generalization of the
;; bridge checker's capability → effect-names table
;; (`effervesce/check/bridge.rkt`). The bridge's name table is the degenerate
;; interpretation; an interpretation's values are typed rows, so a handler can
;; bound the modality, budget and ops of the effects it interprets (scope is
;; carried through but, like `row-subtype?`, not compared).
;;
;; Covered here:
;;   1. the subtyping laws: reflexivity and transitivity (non-vacuous);
;;   2. the identity interpretation: the general relation is `row-subtype?`;
;;   3. the bridge's name-set table is recovered exactly by the general
;;      relation, so the two APIs cannot disagree;
;;   4. row-valued interpretations narrow coverage and compose through handler
;;      chains (Store via Db, Db via db);
;;   5. the bridge checker itself expressed as this subtyping check: on the
;;      bridge's own fixtures the algebra's verdicts equal `check-bridge`'s.

(require rackunit
         racklr/uir
         (only-in racklr/effects/contract effect-name-symbol)
         racklr/effects/infer
         racklr/effects/algebra/main
         effervesce/load
         effervesce/check/bridge)

;; ── fixture helpers (mirror effects-algebra-property-test) ──────────

(define (ent name modality #:ops [ops '()] #:budget [budget #f] #:scope [scope #f])
  (uir-effect-entry (uir-symbol (symbol->string name)) modality
                    (map (λ (s) (uir-symbol (symbol->string s))) ops)
                    budget
                    (if scope (uir-symbol scope) #f)))
(define (row . entries) (uir-effect-row entries))
(define (alg r) (algebra-row-of r))

;; ── 1. subtyping laws ───────────────────────────────────────────────

(define battery
  (list (row)
        (row (ent 'db 'may))
        (row (ent 'db 'may #:budget 3))
        (row (ent 'db 'may #:budget 5))
        (row (ent 'db 'must))
        (row (ent 'db 'may #:ops '(read)))
        (row (ent 'db 'may #:ops '(read write)))
        (row (ent 'llm 'may) (ent 'db 'may))
        (row (ent 'llm 'unknown))
        (row (ent 'llm 'may #:budget 2) (ent 'network 'may))
        (row (ent 'db 'may #:scope "reads=index"))
        (row (ent 'db 'may #:scope "reads=index" #:budget 4))))

(define (test-subtyping-laws)
  ;; Reflexivity: every row is a subtype of itself.
  (for ([r (in-list battery)])
    (check-true (row-subtype? (alg r) (alg r)) (format "reflexive: ~s" r)))
  ;; Transitivity: A ≤ B and B ≤ C implies A ≤ C, over every non-vacuous
  ;; triangle in the battery.
  (define triangles (box 0))
  (for* ([a (in-list battery)] [b (in-list battery)] [c (in-list battery)])
    (when (and (row-subtype? (alg a) (alg b)) (row-subtype? (alg b) (alg c)))
      (set-box! triangles (add1 (unbox triangles)))
      (check-true (row-subtype? (alg a) (alg c))
                  (format "transitive: ~s <= ~s <= ~s" a b c))))
  (check-true (> (unbox triangles) 20)
              "transitivity law exercised non-vacuously"))

;; ── 2. the identity interpretation is row-subtype? ──────────────────

(define (test-identity-interpretation)
  (for* ([a (in-list battery)] [b (in-list battery)])
    (check-equal?
     (row-subtype/interpretation (alg a) (alg b) (identity-interpretation))
     (row-subtype? (alg a) (alg b))
     (format "identity interpretation disagrees for ~s <= ~s" a b))))

;; ── 3. the bridge's name table is the degenerate interpretation ─────

(define bridge-table
  (hash "Db" '(db io) "Llm" '(llm) "Cache" '("cache")))

(define (test-name-table-agreement)
  ;; The bridge's `needs` are capability names; its interpretation table maps
  ;; each to host effect names.
  (check-true (row-interpretable-by? (alg (row (ent 'db 'may) (ent 'llm 'may)))
                                     '("Db" "Llm") bridge-table))
  (check-false (row-interpretable-by? (alg (row (ent 'network 'may)))
                                      '("Db" "Llm") bridge-table))
  (check-equal? (row-uninterpreted-effects
                 (alg (row (ent 'db 'may) (ent 'network 'may)))
                 '("Db") bridge-table)
                '(network))
  ;; `unknown` is the tracked deferral, not an interpreted permission.
  (check-equal? (row-uninterpreted-effects
                 (alg (row (ent 'db 'may) (ent 'unknown 'unknown)))
                 '("Db") bridge-table)
                '(unknown))
  ;; Table keys and effect names may be written as symbols or strings, as the
  ;; bridge fixtures do; the interpretation normalizes both.
  (check-true (row-subtype/interpretation
               (row (ent 'db 'may)) (handler-row '("Db"))
               (interpretation-from-names (hash 'Db '(db)))))
  (check-true (row-interpretable-by? (alg (row (ent 'db 'may)))
                                     '("Db") (hash "Db" '(db))))
  ;; The general relation agrees with the name-membership API on every battery
  ;; row under every needs subset. The general relation reports A's entries in
  ;; A's order; the name-set API reports canonical order, so compare as sets.
  (define interp (interpretation-from-names bridge-table))
  (for* ([r (in-list battery)]
         [needs (in-list (list '() '("Db") '("Db" "Llm") '("Db" "Llm" "Cache")))])
    (check-equal?
     (sort (map effect-name-symbol
                (row-uninterpreted-effects/interpretation r (handler-row needs) interp))
           symbol<?)
     (sort (row-uninterpreted-effects (alg r) needs bridge-table) symbol<?)
     (format "general relation disagrees with name table for ~s needs ~s" r needs))))

;; ── 4. the full algebra over rows ───────────────────────────────────

(define constrained
  (interpretation-from-rows
   (hash "Db" (row (ent 'db 'may #:ops '(read) #:budget 2))
         "Llm" (row (ent 'llm 'may #:ops '(query))))))
(define constrained-needs (handler-row '("Db" "Llm")))

(define (test-row-valued-interpretation)
  ;; Inside the interpreted budget and ops.
  (check-true (row-subtype/interpretation
               (row (ent 'db 'may #:ops '(read) #:budget 2))
               constrained-needs constrained))
  (check-true (row-subtype/interpretation
               (row (ent 'db 'may #:ops '(read) #:budget 1))
               constrained-needs constrained))
  ;; A budget over the interpretation's upper bound is a violation.
  (check-false (row-subtype/interpretation
                (row (ent 'db 'may #:ops '(read) #:budget 3))
                constrained-needs constrained))
  ;; An operation the interpretation does not provide is a violation.
  (check-false (row-subtype/interpretation
                (row (ent 'db 'may #:ops '(write) #:budget 1))
                constrained-needs constrained))
  ;; A `must` obligation needs a `must` interpretation; a typed `may` row is
  ;; not the top a name-only table entry is.
  (check-false (row-subtype/interpretation
                (row (ent 'db 'must #:ops '(read) #:budget 1))
                constrained-needs constrained))
  ;; Scope is not part of the contract entry order, so it does not narrow
  ;; coverage here either: this relation agrees with `row-subtype?` under the
  ;; identity interpretation.
  (check-true (row-subtype/interpretation
               (row (ent 'db 'may #:scope "reads=other"))
               (row (ent 'Db 'may))
               (interpretation-from-rows
                (hash "Db" (row (ent 'db 'may #:scope "reads=index"))))))
  ;; `unknown` is never interpreted by a capability row.
  (check-false (row-subtype/interpretation
                (row (ent 'unknown 'unknown))
                constrained-needs constrained))
  ;; A handler entry bounds what its interpretation provides: budgets take the
  ;; tighter of the two, and a `must-not` handler prohibits the effect.
  (define generous
    (interpretation-from-rows (hash "Db" (row (ent 'db 'may #:budget 10)))))
  (check-false (row-subtype/interpretation
                (row (ent 'db 'may #:budget 5))
                (row (ent 'Db 'may #:budget 3)) generous))
  (check-true (row-subtype/interpretation
               (row (ent 'db 'may #:budget 2))
               (row (ent 'Db 'may #:budget 3)) generous))
  (check-false (row-subtype/interpretation
                (row (ent 'db 'may))
                (row (ent 'Db 'must-not))
                (interpretation-from-names (hash "Db" '(db))))))

;; ── 5. handler chains compose (interpretation closure) ─────────────

(define (test-handler-chains)
  ;; Store is implemented via Db, Db via db.
  (define chain
    (interpretation-from-rows
     (hash 'Store (row (ent 'Db 'may))
           'Db (row (ent 'db 'may #:ops '(read))))))
  (define store (handler-row '("Store")))
  ;; Directly, Store interprets Db only.
  (check-true (row-subtype/interpretation (row (ent 'Db 'may)) store chain))
  (check-false (row-subtype/interpretation (row (ent 'db 'may)) store chain))
  ;; Closed, Store interprets db through Db, and Db's op bound carries.
  (define closed (interpretation-closure chain))
  (check-true (row-subtype/interpretation
               (row (ent 'db 'may #:ops '(read))) store closed))
  (check-false (row-subtype/interpretation
                (row (ent 'db 'may #:ops '(write))) store closed))
  ;; The chain's intermediate capability is still interpreted.
  (check-true (row-subtype/interpretation (row (ent 'Db 'may)) store closed))
  ;; A cycle terminates at the repeated name instead of expanding forever.
  (define cyclic
    (interpretation-closure
     (interpretation-from-rows
      (hash 'Store (row (ent 'Db 'may))
            'Db (row (ent 'Store 'may))))))
  (check-true (interpretation? cyclic))
  (check-true (row-subtype/interpretation (row (ent 'Db 'may)) store cyclic)))

;; ── 6. the bridge checker as a subtyping check ──────────────────────

(define bridge-spec
  (load-effervesce
   "system P14 {
      capability Db { query() -> Unit }
      capability Llm { complete(p: String) -> String }
      given Db, Llm
      component Api needs Db, Llm { run() = query() }
      component Store needs Db { run() = query() }
    }"))

(define bridge-needs (hash "Db" '(db) "Llm" '(llm)))

(define (sym s) (uir-symbol s))
(define (v s) (uir-var (uir-symbol s)))
(define (dot b f) (uir-get b (uir-symbol f)))
(define (fn name body) (uir-fn (uir-symbol name) '() body #f #f))
(define (prog . stmts) (uir-block stmts))

(define clean (prog (fn "handler" (uir-call (dot (v "db") "execute") '()))))
(define leaky (prog (fn "handler" (uir-call (dot (v "db") "execute") '()))
                    (fn "fetcher" (uir-call (v "fetch") '()))))
(define deferred (prog (fn "h" (uir-call (v "callback") '()))))
(define mixed (prog (fn "a" (uir-call (dot (v "db") "execute") '()))
                    (fn "b" (uir-call (v "openai") '()))))

;; Only components with host code participate, exactly as the bridge checker
;; does.
(define (components-with-code spec host-code)
  (for/list ([c (in-list (filter uir-eff-component? (uir-eff-system-decls spec)))]
             #:when (hash-ref host-code (uir-symbol-name (uir-eff-component-name c)) #f))
    c))

;; The bridge checker's decision procedure written as the algebra's subtyping
;; check: every inferred effect row must be a subtype of the component's
;; declared needs row under the bridge's name table.
(define (bridge-verdicts-by-subtyping spec host-code interpretations)
  (define interp (interpretation-from-names interpretations))
  (for/list ([c (in-list (components-with-code spec host-code))])
    (define cname (uir-symbol-name (uir-eff-component-name c)))
    (define program (hash-ref host-code cname))
    (define needs
      (handler-row
       (map (λ (ref) (uir-symbol-name (uir-eff-ref-name ref)))
            (uir-eff-component-needs c))))
    (define uncovered
      (append-map (λ (r) (row-uninterpreted-effects/interpretation r needs interp))
                  (hash-values (infer-effects program))))
    (define violations
      (sort (remove-duplicates
             (for/list ([e (in-list uncovered)]
                        #:unless (eq? (effect-name-symbol e) 'unknown))
               (effect-name-symbol e)))
            symbol<?))
    (list cname violations
          (if (for/or ([e (in-list uncovered)])
                (eq? (effect-name-symbol e) 'unknown))
              #t #f))))

;; The same verdicts read back from the real bridge checker's issues.
(define (bridge-verdicts issues spec host-code)
  (for/list ([c (in-list (components-with-code spec host-code))])
    (define cname (uir-symbol-name (uir-eff-component-name c)))
    (list cname
          (sort (remove-duplicates
                 (for/list ([i (in-list issues)]
                            #:when (and (bridge-violation? i)
                                        (equal? (bridge-violation-component i) cname)))
                   (bridge-violation-effect i)))
                symbol<?)
          (if (for/or ([i (in-list issues)])
                (and (bridge-unverified? i)
                     (equal? (bridge-unverified-component i) cname)))
              #t #f))))

(define (check-bridge-equiv host-code)
  (check-equal?
   (bridge-verdicts-by-subtyping bridge-spec host-code bridge-needs)
   (bridge-verdicts (check-bridge bridge-spec host-code bridge-needs)
                    bridge-spec host-code)
   (format "bridge verdict mismatch for host code ~s" host-code)))

(define (test-bridge-checker-as-subtyping)
  ;; No host code for a component: skipped, not flagged.
  (check-bridge-equiv (hash))
  ;; Effects covered by the declared needs.
  (check-bridge-equiv (hash "Api" clean))
  (check-bridge-equiv (hash "Store" clean))
  ;; A network effect no need interprets: a hard violation.
  (check-bridge-equiv (hash "Store" leaky))
  ;; Unresolvable calls: unverified, not a violation.
  (check-bridge-equiv (hash "Api" deferred))
  ;; A mixture of capabilities covers the program's effects.
  (check-bridge-equiv (hash "Api" mixed))
  ;; One component's code does not leak across components.
  (check-bridge-equiv (hash "Api" clean "Store" leaky))
  ;; The verdicts are the expected ones, not merely self-consistent.
  (check-equal? (bridge-verdicts-by-subtyping bridge-spec
                                              (hash "Store" leaky) bridge-needs)
                (list (list "Store" '(network) #f)))
  (check-equal? (bridge-verdicts-by-subtyping bridge-spec
                                              (hash "Api" deferred) bridge-needs)
                (list (list "Api" '() #t))))

(module+ main
  (test-subtyping-laws)
  (test-identity-interpretation)
  (test-name-table-agreement)
  (test-row-valued-interpretation)
  (test-handler-chains)
  (test-bridge-checker-as-subtyping)
  (displayln "All effect-subtyping handler tests passed."))
