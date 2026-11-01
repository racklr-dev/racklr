#lang racket

;; racklr/effects/invocation-row.rkt
;;
;; Body → algebra-row evaluation for effect inference, and the call-graph
;; fixpoint that runs it over every named function and method.
;;
;; The effect algebra already knows how modality, budget and scope compose:
;; `sequence` sums budgets and keeps obligations, `choose` takes the per-branch
;; maximum and only keeps `must` when every branch must. This module turns a
;; UIR body into such a row, so a function's inferred row is the composition of
;; its own classified call sites and its callees' rows, not a flat union:
;;
;;   - a direct classified effect is a `must` leaf with budget 1 (the call is
;;     performed when that expression is evaluated) and its grounded scope;
;;   - a call-graph edge inserts the callee's current row;
;;   - statement sequences use `sequence`; `if`/`match` branches use `choose`
;;     (so an effect in only one branch is `may`);
;;   - loop/comprehension bodies, `and`/`or` right operands and handler bodies
;;     are `repeatable` (zero or more executions), loop tests are
;;     `unbounded-budget` (at least once, may repeat);
;;   - a `try` body is `weaken-must`: an exception can abandon it before a
;;     later statement runs, so obligations weaken to permissions;
;;   - a statement that may transfer control (return/raise/break/continue)
;;     makes the rest of the enclosing block conditional (`choose` with the
;;     empty row), and an always-transferring statement ends it.
;;
;; The fixpoint inserts the current callee rows and recomposes until stable.
;; The lattice is finite except for budgets: a call-graph cycle can only repeat
;; itself, so an edge inside a strongly-connected component widens the callee's
;; positive budgets to unbounded (`#f`) before composition. An acyclic call
;; graph then converges because budgets only sum/max along a finite path.
;;
;; Classification is supplied by the caller (`class-of`), so single-file and
;; whole-program inference share this driver and cannot drift apart; the only
;; difference is the call-graph key shape (plain name vs component-qualified
;; pair).

(require racklr/uir
         racklr/effects/shape
         racklr/effects/walk
         racklr/effects/imports
         racklr/effects/lexicon
         racklr/effects/invocation-scope
         (prefix-in alg: racklr/effects/algebra/main))

(provide eval-body-row
         inference-rows
         strongly-connected-components
         widen-budget-row)

;; ── control transfer ────────────────────────────────────────────────
;;
;; `never`  — the construct completes normally on every path;
;; `maybe`  — some path transfers control out of it;
;; `always` — every path transfers control out of it.

(define (branch-exits a b)
  (cond [(and (eq? a 'always) (eq? b 'always)) 'always]
        [(or (eq? a 'always) (eq? b 'always) (eq? a 'maybe) (eq? b 'maybe)) 'maybe]
        [else 'never]))

;; ── body evaluation ─────────────────────────────────────────────────

(define (eval-body-row body class-of lookup tokens known-class?)
  (define empty (alg:empty-algebra-row))
  (define (classify node) (class-of node))
  (define (value-row n)
    (call-with-values (λ () (eval-expr n)) (λ (r _e) r)))

  ;; A statement sequence. An always-transferring statement makes every later
  ;; statement unreachable; a maybe-transferring one makes them conditional.
  ;; Unreachable statements are kept in the row as *conditional* (a `may`
  ;; presence) so inference never silently drops a classified call site — the
  ;; old flat walk counted them, and a `may` entry is a sound over-approximation
  ;; — but they can never make an effect `must`.
  (define (eval-nodes nodes)
    (cond
      [(null? nodes) (values empty 'never)]
      [else
       (define-values (r e) (eval-expr (car nodes)))
       (cond
         [(eq? e 'always)
          (define-values (rest-r _rest-e) (eval-nodes (cdr nodes)))
          (values (alg:sequence r (alg:choose rest-r empty)) 'always)]
         [(eq? e 'maybe)
          (define-values (rest-r _rest-e) (eval-nodes (cdr nodes)))
          (values (alg:sequence r (alg:choose rest-r empty)) 'maybe)]
         [else
          (define-values (rest-r rest-e) (eval-nodes (cdr nodes)))
          (values (alg:sequence r rest-r) rest-e)])]))

  (define (eval-fields fields)
    (for/fold ([r empty]) ([c (in-list fields)]) (alg:sequence r (value-row c))))

  (define (eval-case c)
    ;; (uir-case pattern guard body): the pattern/guard are tested first, and
    ;; the body runs only when the case is selected.
    (define-values (pat-r pat-e) (eval-expr (uir-case-pattern c)))
    (define guard (uir-case-guard c))
    (define guard-row (if guard (value-row guard) empty))
    (define-values (body-r body-e) (eval-expr (uir-case-body c)))
    (values (alg:sequence (alg:sequence pat-r guard-row) body-r)
            (branch-exits pat-e body-e)))

  (define (eval-cases cases)
    (cond
      [(null? cases) (values empty 'never)]
      [else
       (define-values (r e) (eval-case (first cases)))
       (define-values (rest-r rest-e) (eval-cases (cdr cases)))
       (values (alg:choose r rest-r) (branch-exits e rest-e))]))

  (define (eval-try n)
    (define body-r (value-row (uir-try-body n)))
    ;; A handler runs only on the exceptional path, so it is a branch.
    (define catch-r
      (for/fold ([r empty]) ([c (in-list (uir-try-catches n))])
        (if (and (list? c) (>= (length c) 3))
            (alg:choose r (alg:sequence (value-row (first c))
                                        (alg:sequence (value-row (second c))
                                                      (value-row (third c)))))
            r)))
    (define else-r (if (uir-try-else-body n)
                       (alg:choose (value-row (uir-try-else-body n)) empty)
                       empty))
    (define finally-r (if (uir-try-finally-body n)
                          (value-row (uir-try-finally-body n))
                          empty))
    ;; The normal path runs the body (then `else`), the exceptional path runs a
    ;; handler: one of them, not both, so the invocation bound is their max.
    (values (alg:sequence (alg:choose (alg:sequence (alg:weaken-must body-r) else-r)
                                      catch-r)
                          finally-r)
            'never))

  (define (eval-while n)
    (define test-r (value-row (uir-while-test n)))
    (define body-r (alg:repeatable (value-row (uir-while-body n))))
    (define else-r (alg:choose (value-row (uir-while-else-body n)) empty))
    (values (alg:sequence (alg:sequence (alg:unbounded-budget test-r) body-r)
                          else-r)
            'never))

  (define (eval-for-each n)
    (define iter-r (value-row (uir-for-each-iterable n)))
    (define body-r (alg:repeatable (value-row (uir-for-each-body n))))
    (define else-r (alg:choose (value-row (uir-for-each-else-body n)) empty))
    (values (alg:sequence (alg:sequence iter-r body-r) else-r) 'never))

  (define (eval-with n)
    (define items-r
      (for/fold ([r empty]) ([item (in-list (uir-with-items n))])
        (alg:sequence r (value-row (if (pair? item) (first item) item)))))
    (define-values (body-r body-e) (eval-expr (uir-with-body n)))
    (values (alg:sequence items-r body-r) body-e))

  (define (eval-comprehension n)
    ;; (call "list-comp" (list result loop-var iterable filter)): the iterable is
    ;; evaluated once; the element expression and filter run zero or more times.
    (define args (uir-call-args n))
    (define iter-r (value-row (if (>= (length args) 3) (third args) (uir-null))))
    (define result-r (value-row (first args)))
    (define filter-r (if (>= (length args) 4) (value-row (fourth args)) empty))
    (values (alg:sequence iter-r
                          (alg:repeatable (alg:sequence result-r filter-r)))
            'never))

  ;; Name of a callee when it names a control-transfer construct: a bare callee
  ;; name (`raise`) or the final member of a dotted transfer call (`sys.exit`).
  (define (transfer-name callee)
    (define names (tokens-control-transfer-names tokens))
    (define bare (and (uir-symbol? callee) (uir-symbol-name callee)))
    (define op (effect-op-of-callee callee))
    (cond [(and bare (set-member? names bare)) bare]
          [(and (member-access-callee? callee) op (set-member? names op)) op]
          [else #f]))

  (define (eval-call n)
    (define callee (uir-call-callee n))
    (define cname (and (uir-symbol? callee) (uir-symbol-name callee)))
    (define cls (classify n))
    (cond
      ;; A control-transfer callee evaluates its arguments, then transfers.
      [(transfer-name callee)
       (values (alg:sequence (value-row callee)
                             (for/fold ([r empty]) ([a (in-list (uir-call-args n))])
                               (alg:sequence r (value-row a))))
               'always)]
      ;; Comprehension encodings: element expressions run per iteration.
      [(and cname (set-member? (tokens-comprehension-callees tokens) cname)
            (>= (length (uir-call-args n)) 3))
       (eval-comprehension n)]
      ;; Short-circuit boolean operators: the left operand is always evaluated;
      ;; every later operand is conditional on the earlier ones.
      [(and cname (set-member? (tokens-short-circuit-callees tokens) cname))
       (define args (uir-call-args n))
       (define first-r (value-row (first args)))
       (define rest-r
         (for/fold ([r empty]) ([a (in-list (cdr args))])
           (alg:sequence r (value-row a))))
       (values (alg:sequence first-r (alg:choose rest-r empty)) 'never)]
      [else
       ;; Evaluate the callee expression then the arguments, left to right, then
       ;; the call itself (`kwarg` encodings are argument values, not calls).
       (define head-r
         (for/fold ([r empty]) ([part (in-list (cons callee (uir-call-args n)))])
           (alg:sequence r (value-row part))))
       (define leaf
         (case (car cls)
           [(edge) (lookup (cdr cls))]
           [(effect)
            ;; The classifier's effect payload is a typed capability when the
            ;; receiver-grounded path produced one, or a bare domain symbol
            ;; from the name-heuristic tail.
            (define eff (cdr cls))
            (define cap? (effect-cap? eff))
            (define ops (if cap? (effect-cap-ops eff) '()))
            (effect-leaf-row (if cap? (effect-cap-domain eff) eff)
                             (map (λ (s) (uir-symbol (symbol->string s))) ops)
                             (call-scope n ops tokens known-class?))]
           [(pure) empty]
           [else unknown-row]))
       (values (alg:sequence head-r leaf) 'never)]))

  (define (eval-expr n)
    (cond
      [(or (uir-fn? n) (uir-method? n) (uir-class? n)
           (arrow-call? n) (function-expr-call? n))
       (values empty 'never)]
      [(uir-block? n) (eval-nodes (uir-block-stmts n))]
      [(uir-if? n)
       (define-values (test-r test-e) (eval-expr (uir-if-test n)))
       (define-values (then-r then-e) (eval-expr (uir-if-then n)))
       (define-values (else-r else-e) (eval-expr (uir-if-else n)))
       (values (alg:sequence test-r (alg:choose then-r else-r))
               (branch-exits (branch-exits test-e then-e) else-e))]
      [(uir-while? n) (eval-while n)]
      [(uir-for-each? n) (eval-for-each n)]
      [(uir-try? n) (eval-try n)]
      [(uir-with? n) (eval-with n)]
      [(uir-match? n)
       (define-values (subject-r subject-e) (eval-expr (uir-match-subject n)))
       (define-values (cases-r cases-e) (eval-cases (uir-match-cases n)))
       (values (alg:sequence subject-r cases-r) (branch-exits subject-e cases-e))]
      [(uir-return? n) (values (value-row (uir-return-value n)) 'always)]
      [(uir-call? n) (eval-call n)]
      [(uir-symbol? n)
       (if (set-member? (tokens-control-transfer-names tokens) (uir-symbol-name n))
           (values empty 'always)
           (values empty 'never))]
      [(pair? n)
       ;; A proper list is an element sequence; an improper pair is a key/value
       ;; entry (`uir-record` entries and other pair-coded fields), whose two
       ;; halves are both evaluated.
       (if (list? n)
           (eval-nodes n)
           (values (alg:sequence (value-row (car n)) (value-row (cdr n)))
                   'never))]
      [(struct? n)
       (values (eval-fields (cdr (vector->list (struct->vector n)))) 'never)]
      [else (values empty 'never)]))

  (value-row body))

;; ── call graph and fixpoint ─────────────────────────────────────────

;; Every callee key a body mentions through an `edge` classification. Used only
;; for the SCC decomposition, so it does not need conditionality.
(define (body-edges body class-of)
  (define out '())
  (walk-body body
             (λ (n)
               (when (uir-call? n)
                 (define cls (class-of n))
                 (when (eq? (car cls) 'edge)
                   (set! out (cons (cdr cls) out))))))
  (remove-duplicates out))

;; Tarjan's algorithm over the call graph. Returns a hash key → component id.
;; The recursion depth is the call-graph depth, not the node count.
(define (strongly-connected-components keys edge-map)
  (define nodes
    (remove-duplicates
     (append keys (append-map (λ (k) (hash-ref edge-map k '())) keys))))
  (define index (make-hash))
  (define lowlink (make-hash))
  (define on-stack (make-hash))
  (define stack '())
  (define next-index 0)
  (define component (make-hash))
  (define next-component 0)
  (define (strongconnect v)
    (hash-set! index v next-index)
    (hash-set! lowlink v next-index)
    (set! next-index (add1 next-index))
    (set! stack (cons v stack))
    (hash-set! on-stack v #t)
    (for ([w (in-list (hash-ref edge-map v '()))])
      (cond
        [(not (hash-has-key? index w))
         (strongconnect w)
         (hash-set! lowlink v (min (hash-ref lowlink v) (hash-ref lowlink w)))]
        [(hash-ref on-stack w #f)
         (hash-set! lowlink v (min (hash-ref lowlink v) (hash-ref index w)))]))
    (when (= (hash-ref lowlink v) (hash-ref index v))
      (let loop ()
        (define w (car stack))
        (set! stack (cdr stack))
        (hash-remove! on-stack w)
        (hash-set! component w next-component)
        (unless (eq? w v) (loop)))
      (set! next-component (add1 next-component))))
  (for ([v (in-list nodes)])
    (unless (hash-has-key? index v) (strongconnect v)))
  component)

;; A callee row inserted across a call-graph cycle can execute an unbounded
;; number of times, so widening replaces every positive budget with the
;; unbounded marker (`#f`). Modality, ops and scope are unchanged: the claim
;; that the effect happens when the callee runs is still true.
(define (widen-budget-row row)
  (define entries
    (for/list ([e (in-list (uir-effect-row-entries (alg:algebra-row->uir row)))])
      (if (and (uir-effect-entry-budget e)
               (positive? (uir-effect-entry-budget e)))
          (struct-copy uir-effect-entry e [budget #f])
          e)))
  (alg:make-algebra-row (uir-effect-row entries) (alg:algebra-row-provenance row)))

;; Compute every key's row by recomposing its body from the current rows until
;; the table is stable. `body-of : key → body`, `class-of : key → node →
;; classification`. Returns key → algebra-row.
(define (inference-rows keys body-of class-of
                       #:tokens [tokens default-effect-tokens]
                       #:known-class? [known-class? (λ (s) #f)]
                       #:seed [seed (λ (k) (alg:empty-algebra-row))]
                       #:max-rounds [max-rounds 10000])
  (define edge-map
    (for/hash ([k (in-list keys)])
      (values k (body-edges (body-of k) (class-of k)))))
  (define scc (strongly-connected-components keys edge-map))
  (define (compose k rows)
    (define own (hash-ref scc k -1))
    ;; A key's seed row is a declaration asserted beside the inferred body
    ;; (whole-program inference seeds declared method capabilities): it is part
    ;; of every round, so callers inherit it through the fixpoint.
    (alg:join (seed k)
              (eval-body-row (body-of k) (class-of k)
                             (λ (callee)
                               (define r (hash-ref rows callee (alg:empty-algebra-row)))
                               (if (= own (hash-ref scc callee -2))
                                   (widen-budget-row r)
                                   r))
                             tokens known-class?)))
  (let loop ([rows (for/hash ([k (in-list keys)])
                     (values k (seed k)))]
             [round 0])
    (define next-rows
      (for/hash ([k (in-list keys)])
        (values k (compose k rows))))
    (define stable?
      (for/and ([(k r) (in-hash next-rows)])
        (alg:same-row? r (hash-ref rows k (alg:empty-algebra-row)))))
    (cond
      [stable? next-rows]
      [(>= round max-rounds)
       (error 'inference-rows
              "effect inference did not converge after ~a rounds over ~a keys"
              round (length keys))]
      [else (loop next-rows (add1 round))])))
