#lang racket

;; Property tests for the effect algebra in racklr/effects/algebra/. Deterministic
;; seeded generators plus fixed batteries: provenance sets, join/sequence/choose/
;; scope laws, row subtyping laws, fixpoint monotonicity/leastness on random DAGs,
;; handler interpretation, and infer-row-union differential vs the old reference.

(require rackunit
         racket/set
         racklr/uir
         (only-in racklr/effects/contract
                  effect-subtype? effect-row-union effect-name-symbol)
         racklr/effects/rows
         racklr/effects/algebra/main)

(random-seed 20261101)
(define trial-count 60)
(define fixpoint-nodes 5)
(define fixpoint-trials 50)
(define effect-names '(db io network process cache llm time email))
(define inference-names (append effect-names '(unknown)))
(define algebra-modalities '(may must unknown)) ; must-not canonicalizes to absence
(define ops-pool '("read" "write" "query" "embed" "send"))
(define scope-pool '(#f "class" "module" "domain"))
(define receivers '(#f QuerySet Client))

(define (pick lst) (list-ref lst (random (length lst))))
(define (subset lst) (filter (λ (x) (zero? (random 2))) lst))
(define (law! label trial ok [detail ""])
  (check-true ok (format "~a (trial ~a)~a" label trial detail)))
;; ── row construction ────────────────────────────────────────────────
(define (ent name modality #:ops [ops '()] #:budget [budget #f] #:scope [scope #f])
  (uir-effect-entry (uir-symbol (symbol->string name)) modality
                    (map uir-symbol ops) budget
                    (if scope (uir-symbol scope) #f)))
(define (random-entry name)
  (ent name (pick algebra-modalities) #:ops (subset ops-pool)
       #:budget (if (zero? (random 3)) #f (random 4)) #:scope (pick scope-pool)))
(define (random-provenance)
  (provenance (for/list ([i (in-range (random 3))]) (format "site~a" i))
              (pick receivers) (subset '("outer" "inner" "root"))))
(define (random-row)
  (for/fold ([r (empty-algebra-row)]) ([name (in-list (subset effect-names))])
    (row-add r name (random-entry name) (random-provenance))))
(define (random-triple) (values (random-row) (random-row) (random-row)))
(define empty (empty-algebra-row))
(define (names-of r) (row-effect-names r))
(define (modality-of r n) (define e (row-lookup r n)) (and e (uir-effect-entry-modality e)))
(define (budget-of r n) (define e (row-lookup r n)) (and e (uir-effect-entry-budget e)))
;; ── provenance ──────────────────────────────────────────────────────
(define (test-provenance-value)
  (define p (provenance '("django/db/models" "objects") 'QuerySet
                        '("view" "get_queryset")))
  (check-true (provenance? p))
  (check-equal? (provenance-tokens p) '("django/db/models" "objects"))
  (check-equal? (provenance-receiver p) 'QuerySet)
  (check-equal? (provenance-chain p) '("view" "get_queryset"))
  (check-true (provenance=? p (provenance '("django/db/models" "objects")
                                          'QuerySet '("view" "get_queryset"))))
  (check-equal? (provenance-chain (provenance-extend-chain p "handler"))
                '("handler" "view" "get_queryset"))
  (check-equal? (provenance->string p)
                "django/db/models.objects<QuerySet> via get_queryset -> view")
  (check-equal? (provenance->string (provenance '() #f '())) "?"))

(define (test-provenance-is-a-set)
  ;; Repeated evidence collapses; one row keeps the canonical evidence set.
  (define p1 (provenance '("a") 'C1 '()))
  (define p2 (provenance '("b") #f '("inner")))
  (define r (row-add (row-add (row-add (empty-algebra-row) 'db (ent 'db 'may) p1)
                              'db (ent 'db 'may) p1)
                     'db (ent 'db 'may) p2))
  (check-equal? (row-provenance r 'db) (provenance-sort (remove-duplicates
                                                         (list p1 p2))))
  ;; Property: join keeps both operands' evidence per name and drops duplicates.
  (for ([trial (in-range trial-count)])
    (define a (random-row))
    (define b (random-row))
    (define joined (join a b))
    (for ([n (in-list (remove-duplicates (append (names-of a) (names-of b))))])
      (define expected (provenance-sort
                        (remove-duplicates
                         (append (row-provenance a n) (row-provenance b n)))))
      (check-equal? (row-provenance joined n) expected))
    (law! "duplicate evidence collapses" trial (same-row? (join a a) a))))
;; ── join ────────────────────────────────────────────────────────────
(define (test-join-laws)
  (for ([trial (in-range trial-count)])
    (define-values (a b c) (random-triple))
    (law! "join identity (empty row)" trial
          (and (same-row? (join a empty) a) (same-row? (join empty a) a)))
    (law! "join commutative" trial (same-row? (join a b) (join b a)))
    (law! "join associative" trial
          (same-row? (join (join a b) c) (join a (join b c))))
    (law! "join idempotent" trial (same-row? (join a a) a))
    (law! "join is an upper bound (row-leq?)" trial
          (and (row-leq? a (join a b)) (row-leq? b (join a b))))
    (law! "join is an upper bound (row-subtype?)" trial
          (and (row-subtype? a (join a b)) (row-subtype? b (join a b))))))
;; ── sequence ────────────────────────────────────────────────────────
(define (budget-lub a b) (cond [(or (not a) (not b)) #f] [else (max a b)]))
(define (budget-sum a b) (cond [(or (not a) (not b)) #f] [else (+ a b)]))
(define (test-sequence-laws)
  (for ([trial (in-range trial-count)])
    (define-values (a b c) (random-triple))
    (law! "sequence identity (empty row)" trial
          (and (same-row? (sequence a empty) a) (same-row? (sequence empty a) a)))
    (law! "sequence associative" trial
          (same-row? (sequence (sequence a b) c) (sequence a (sequence b c))))
    ;; Budgets sum; #f (unbounded) dominates; a one-sided entry keeps its budget.
    (define s (sequence a b))
    (for ([n (in-list (remove-duplicates (append (names-of a) (names-of b))))])
      (define ba (budget-of a n))
      (define bb (budget-of b n))
      (define expected (cond [(not (row-lookup a n)) bb]
                             [(not (row-lookup b n)) ba]
                             [else (budget-sum ba bb)]))
      (law! "sequence budgets sum" trial (equal? (budget-of s n) expected)))))
;; ── choose ──────────────────────────────────────────────────────────
(define (test-choose-laws)
  (for ([trial (in-range trial-count)])
    (define-values (a b c) (random-triple))
    (law! "choose commutative" trial (same-row? (choose a b) (choose b a)))
    (law! "choose associative" trial
          (same-row? (choose (choose a b) c) (choose a (choose b c))))
    (law! "choose idempotent" trial (same-row? (choose a a) a))
    ;; Budgets take the max; `must` survives only when both branches must.
    (define ch (choose a b))
    (for ([n (in-list (remove-duplicates (append (names-of a) (names-of b))))])
      (define ea (row-lookup a n))
      (define eb (row-lookup b n))
      (define ba (and ea (uir-effect-entry-budget ea)))
      (define bb (and eb (uir-effect-entry-budget eb)))
      (law! "choose budgets take max" trial
            (equal? (budget-of ch n)
                    (cond [(not ea) bb] [(not eb) ba] [else (budget-lub ba bb)])))
      (law! "choose must requires both branches" trial
            (eq? (eq? (modality-of ch n) 'must)
                 (and (eq? (modality-of a n) 'must)
                      (eq? (modality-of b n) 'must)))))))
;; ── scope ───────────────────────────────────────────────────────────
(define (test-scope-laws)
  (for ([trial (in-range trial-count)])
    (define r (random-row))
    (define handled (subset (names-of r)))
    (define-values (remaining discharged) (scope r handled))
    (law! "scope splits remaining/discharged" trial
          (and (null? (filter (λ (n) (memq n handled)) (names-of remaining)))
               (equal? (sort (append (names-of remaining) (names-of discharged))
                             symbol<?)
                       (names-of r)))
          (format " row=~a handled=~s" (row->string r) handled))
    (law! "scope discharges only handled effects" trial
          (equal? (names-of discharged)
                  (filter (λ (n) (memq n handled)) (names-of r))))
    (law! "scope parts join back to the row" trial
          (same-row? (join remaining discharged) r))
    (law! "scope preserves provenance" trial
          (for/and ([n (in-list (names-of r))])
            (equal? (row-provenance (if (memq n handled) discharged remaining) n)
                    (row-provenance r n))))
    (define-values (rest nothing) (scope r '()))
    (law! "scope with no handled names is identity" trial
          (and (same-row? rest r) (same-row? nothing empty)))))
;; ── subtyping ───────────────────────────────────────────────────────
;; Weaken a row inside `c`: drop names, reduce budgets, narrow ops, keep the
;; modality. The result is a subtype of `c` by construction.
(define (sub-budget modality budget)
  (cond [(eq? modality 'must) budget]
        [(not budget) (if (zero? (random 2)) #f (random 5))]
        [else (random (add1 budget))]))
(define (sub-ops ops)
  (cond [(null? ops) '()]
        [else (sort (remove-duplicates (let ([s (subset ops)])
                                         (if (null? s) (list (pick ops)) s)))
                    string<?)]))
(define (weaken-row c)
  (make-algebra-row
   (uir-effect-row
    (for/list ([n (in-list (subset (names-of c)))])
      (define e (row-lookup c n))
      (ent n (uir-effect-entry-modality e)
           #:ops (sub-ops (map uir-symbol-name (uir-effect-entry-ops e)))
           #:budget (sub-budget (uir-effect-entry-modality e)
                                (uir-effect-entry-budget e))
           #:scope (and (uir-effect-entry-scope e)
                        (uir-symbol-name (uir-effect-entry-scope e))))))
   (hash)))
;; `may` and `unknown` are mutually subtype-equivalent in the contract order.
(define (may-unknown-variant r)
  (make-algebra-row
   (uir-effect-row
    (for/list ([e (in-list (uir-effect-row-entries (algebra-row->uir r)))])
      (struct-copy uir-effect-entry e
                   [modality (case (uir-effect-entry-modality e)
                               [(may) 'unknown] [(unknown) 'may]
                               [else (uir-effect-entry-modality e)])])))
   (hash)))

(define (test-subtyping-laws)
  (for ([trial (in-range trial-count)])
    (define c (random-row))
    (define b (weaken-row c))
    (define a (weaken-row b))
    (law! "subtype reflexive" trial (row-subtype? c c))
    (law! "empty row is bottom" trial (row-subtype? empty c))
    ;; Directed transitivity, non-vacuous: a <= b <= c.
    (law! "subtype premises (a<=b<=c)" trial
          (and (row-subtype? a b) (row-subtype? b c)))
    (law! "subtype transitive" trial (row-subtype? a c))
    ;; Join is a least upper bound, non-vacuously: u,v <= c => join u v <= c.
    (define u (weaken-row c))
    (define v (weaken-row c))
    (law! "lub premises (u<=c, v<=c)" trial
          (and (row-subtype? u c) (row-subtype? v c)))
    (law! "join is a least upper bound" trial (row-subtype? (join u v) c)))
  ;; Mutual subtype is an equivalence relation and a congruence for join.
  (for ([trial (in-range trial-count)])
    (define a (random-row))
    (define b (may-unknown-variant a))
    (define c (may-unknown-variant b))
    (define g (random-row))
    (law! "mutual subtype reflexive" trial (row-equivalent? a a))
    (law! "mutual subtype symmetric" trial
          (eq? (row-equivalent? a b) (row-equivalent? b a)))
    (law! "mutual subtype (may~unknown)" trial (row-equivalent? a b))
    (law! "mutual subtype transitive" trial
          (and (row-equivalent? a b) (row-equivalent? b c) (row-equivalent? a c)))
    (law! "mutual subtype is a join congruence" trial
          (row-equivalent? (join a g) (join b g)))))

(define (test-must-not-canonicalization)
  (define prohibited (uir-effect-row (list (ent 'db 'must-not))))
  (define permitted (uir-effect-row (list (ent 'db 'may #:budget 3))))
  (define alg-prohibited (algebra-row-of prohibited))
  (define alg-permitted (algebra-row-of permitted))
  (check-equal? (row-effect-names alg-prohibited) '())
  (check-false (row-lookup alg-prohibited 'db))
  (check-true (same-row? alg-prohibited empty))
  ;; row-subtype? of the algebra rows equals effect-subtype? of the UIR rows.
  (check-equal? (row-subtype? alg-prohibited empty)
                (effect-subtype? prohibited (uir-effect-row '())))
  (check-equal? (row-subtype? empty alg-prohibited)
                (effect-subtype? (uir-effect-row '()) prohibited))
  (check-equal? (row-subtype? alg-permitted alg-prohibited)
                (effect-subtype? permitted prohibited))
  (check-equal? (row-subtype? alg-prohibited alg-permitted)
                (effect-subtype? prohibited permitted)))
;; ── fixpoint ────────────────────────────────────────────────────────
(define (node-keys) (for/list ([i (in-range fixpoint-nodes)]) i))
(define (initial-table direct)
  (for/hash ([k (in-list (node-keys))]) (values k (hash-ref direct k empty))))
(define (random-graph)
  (values (for/hash ([i (in-range fixpoint-nodes)]) (values i (random-row)))
          (for/hash ([i (in-range fixpoint-nodes)])
            (values i (for/list ([j (in-range i)] #:when (zero? (random 2))) j)))))
(define (naive-least direct edges)
  (let loop ([rows (initial-table direct)] [iteration 0])
    (define next (fixpoint-step direct edges rows))
    (cond [(same-rows? rows next) rows]
          [(> iteration 100) (error 'naive-least "did not converge")]
          [else (loop next (add1 iteration))])))
(define (top-row)
  (make-algebra-row
   (uir-effect-row (for/list ([n (in-list effect-names)]) (ent n 'must))) (hash)))
;; Any fixpoint above the direct rows: descend from the all-top table. The
;; first step is below the table, so monotonicity keeps the chain descending.
(define (fixpoint-candidate direct edges)
  (define top (for/fold ([g (top-row)]) ([(k v) (in-hash direct)]) (join g v)))
  (let loop ([rows (for/hash ([k (in-list (node-keys))]) (values k top))]
             [iteration 0])
    (define next (fixpoint-step direct edges rows))
    (cond [(same-rows? rows next) rows]
          [(> iteration 100) (error 'fixpoint-candidate "did not converge")]
          [else (loop next (add1 iteration))])))

(define (test-fixpoint-laws)
  (for ([trial (in-range fixpoint-trials)])
    (define-values (direct edges) (random-graph))
    (define init (initial-table direct))
    (define step1 (fixpoint-step direct edges init))
    (define step2 (fixpoint-step direct edges step1))
    (law! "fixpoint iteration ascends (rows <= step rows)" trial
          (and (rows-leq? init step1) (rows-leq? step1 step2))
          (format " direct=~s edges=~s" (hash->list direct) (hash->list edges)))
    ;; Monotone pointwise: low <= high => step low <= step high.
    (define delta (for/hash ([k (in-list (node-keys))]) (values k (random-row))))
    (define low (for/hash ([k (in-list (node-keys))])
                  (values k (hash-ref direct k empty))))
    (define high (for/hash ([k (in-list (node-keys))])
                   (values k (join (hash-ref low k) (hash-ref delta k)))))
    (law! "monotone premise (low <= high)" trial (rows-leq? low high))
    (law! "fixpoint-step monotone" trial
          (rows-leq? (fixpoint-step direct edges low)
                     (fixpoint-step direct edges high)))
    (define lf (least-fixpoint direct edges))
    (law! "least-fixpoint agrees with naive iteration" trial
          (same-rows? lf (naive-least direct edges)))
    (law! "least-fixpoint is a fixed point" trial
          (same-rows? lf (fixpoint-step direct edges lf)))
    (define candidate (fixpoint-candidate direct edges))
    (law! "candidate contains direct rows" trial (rows-leq? init candidate))
    (law! "candidate is a fixpoint" trial
          (same-rows? candidate (fixpoint-step direct edges candidate)))
    (law! "least-fixpoint is least" trial (rows-leq? lf candidate))))
;; ── handler interpretation ──────────────────────────────────────────
(define interpretations
  (hash "database" '(db) "network" '(network io)
        "cache" '(cache) "llm" '("llm")))
(define (capability-row names)
  (algebra-row-of
   (uir-effect-row
    (for/list ([n (in-list names)])
      (ent n (if (eq? n 'unknown) 'unknown 'may))))))
(define (test-handler-interpretation)
  (define db-network (capability-row '(db network)))
  (check-true (set=? (allowed-effect-names '("database" "network") interpretations)
                     (set "db" "network" "io")))
  (check-equal? (row-uninterpreted-effects db-network '("database") interpretations)
                '(network))
  (check-false (row-interpretable-by? db-network '("database") interpretations))
  (check-equal? (row-uninterpreted-effects db-network
                                          '("database" "network") interpretations)
                '())
  (check-true (row-interpretable-by? db-network
                                    '("database" "network") interpretations))
  ;; `unknown` is the tracked deferral, never an interpreted permission.
  (define deferred (capability-row '(unknown)))
  (define all-needs '("database" "network" "cache" "llm"))
  (check-equal? (row-uninterpreted-effects deferred all-needs interpretations)
                '(unknown))
  (check-false (row-interpretable-by? deferred all-needs interpretations))
  ;; Property: uncovered = the row's names outside the needs' interpreted set.
  (for ([trial (in-range trial-count)])
    (define row (capability-row (subset inference-names)))
    (define needs (subset '("database" "network" "cache" "llm" "time")))
    (define allowed
      (for*/set ([need (in-list needs)]
                 [effect (in-list (hash-ref interpretations need '()))])
        (if (symbol? effect) (symbol->string effect) effect)))
    (define expected
      (for/list ([n (in-list (names-of row))]
                 #:unless (set-member? allowed (symbol->string n)))
        n))
    (law! "allowed-effect-names" trial
          (set=? (allowed-effect-names needs interpretations) allowed))
    (law! "row-uninterpreted-effects" trial
          (equal? (row-uninterpreted-effects row needs interpretations) expected))
    (law! "row-interpretable-by? iff nothing uninterpreted" trial
          (eq? (row-interpretable-by? row needs interpretations)
               (null? expected)))))
;; ── differential: infer-row-union vs pre-refactor reference ──────────
(define (canonical-uir-row r)
  (uir-effect-row
   (sort (uir-effect-row-entries r)
         (λ (a b) (symbol<? (effect-name-symbol a) (effect-name-symbol b))))))
;; Operations are a set: normalize their order (the algebra sorts them; the old
;; reference preserved append order).
(define (normalize-ops-row r)
  (uir-effect-row
   (for/list ([e (in-list (uir-effect-row-entries r))])
     (struct-copy uir-effect-entry e
                  [ops (map uir-symbol
                            (sort (remove-duplicates
                                   (map uir-symbol-name (uir-effect-entry-ops e)))
                                  string<?))]))))
(define (reference-infer-row-union r1 r2)
  (canonical-uir-row
   (uir-effect-row
    (for/list ([e (in-list (uir-effect-row-entries (effect-row-union r1 r2)))])
      (if (eq? (effect-name-symbol e) 'unknown)
          (struct-copy uir-effect-entry e [modality 'unknown])
          e)))))
(define (random-inference-row)
  (uir-effect-row
   (for/list ([n (in-list (subset inference-names))])
     (ent n (if (eq? n 'unknown) 'unknown 'may) #:ops (subset ops-pool)))))
(define (test-infer-row-union-differential)
  (for ([trial (in-range trial-count)])
    (define r1 (random-inference-row))
    (define r2 (random-inference-row))
    (law! "infer-row-union = pre-refactor reference" trial
          (equal? (normalize-ops-row (infer-row-union r1 r2))
                  (normalize-ops-row (reference-infer-row-union r1 r2)))
          (format " r1=~s r2=~s" r1 r2))))
;; ── main ────────────────────────────────────────────────────────────
(module+ main
  (test-provenance-value)
  (test-provenance-is-a-set)
  (test-join-laws)
  (test-sequence-laws)
  (test-choose-laws)
  (test-scope-laws)
  (test-subtyping-laws)
  (test-must-not-canonicalization)
  (test-fixpoint-laws)
  (test-handler-interpretation)
  (test-infer-row-union-differential)
  (displayln "All algebra property tests passed."))
