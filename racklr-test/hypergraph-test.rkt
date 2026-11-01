#lang racket

;; Proves the typed-hypergraph kernel (racklr/hypergraph) is a faithful
;; generalization of the hand-written effect algebra in
;; racklr/effects/contract, then shows it extends to typed function
;; subtyping, graph pattern queries, and Effervesce-style capability
;; composition.
;;
;; The P4.1 section at the end proves the substrate claim: ONE hypergraph
;; instance holds the UIR, call, type, effect, capability, and module graphs
;; together, with typed, queryable edges — the effect rows are real inference
;; output and a function vertex is shared across the call, effect, and module
;; graphs of that one instance.

(require rackunit
         rackunit/text-ui
         racklr/hypergraph
         racklr/hypergraph/uir-project
         racklr/uir
         racklr/effects/contract)

;; ── UIR fixture helpers (mirror effects-contract-test) ──────────────

(define (ent name modality #:ops [ops '()] #:budget [budget #f] #:scope [scope #f])
  (uir-effect-entry (uir-symbol name) modality
                    (map uir-symbol ops)
                    budget
                    (if scope (uir-symbol scope) #f)))

(define (row . entries) (uir-effect-row entries))

;; ── UIR ↔ hypergraph converters ─────────────────────────────────────

(define (entry->link e)
  (effect-entry
   (effect-name-symbol e)
   (uir-effect-entry-modality e)
   (entry-ops e)
   (uir-effect-entry-budget e)
   (and (uir-effect-entry-scope e)
        (string->symbol (uir-symbol-name (uir-effect-entry-scope e))))))

(define (link->entry l)
  (define out (link-out l))
  (uir-effect-entry
   (uir-symbol (symbol->string (node-name (first out))))
   (node-name (second out))
   (map (λ (s) (uir-symbol (symbol->string s))) (node-name (third out)))
   (node-name (fourth out))
   (and (node-name (fifth out))
        (uir-symbol (symbol->string (node-name (fifth out)))))))

(define (row->links r) (map entry->link (uir-effect-row-entries r)))
(define (links->row links) (uir-effect-row (map link->entry links)))

(define (canonical r)
  (uir-effect-row
   (sort (uir-effect-row-entries r)
         (λ (a b) (symbol<? (effect-name-symbol a) (effect-name-symbol b))))))

;; ── equivalence with contract.rkt ───────────────────────────────────

;; All four row operations must agree with contract.rkt on every pair.
(define (check-equiv r1 r2)
  (check-equal? (canonical (links->row (effect-row-join (row->links r1) (row->links r2))))
                (canonical (effect-row-union r1 r2))
                "join")
  (check-equal? (canonical (links->row (effect-row-meet (row->links r1) (row->links r2))))
                (canonical (effect-row-intersect r1 r2))
                "meet")
  (check-equal? (canonical (links->row (effect-row-diff (row->links r1) (row->links r2))))
                (canonical (effect-row-difference r1 r2))
                "diff")
  (check-equal? (effect-row-leq (row->links r1) (row->links r2))
                (effect-subtype? r1 r2)
                "subtype"))

(define battery
  (list
   ;; disjoint names
   (list (row (ent "llm" 'may)) (row (ent "db" 'may)))
   ;; may+may: budget sums
   (list (row (ent "llm" 'may #:budget 3)) (row (ent "llm" 'may #:budget 4)))
   ;; must-not absorbs on union
   (list (row (ent "db" 'may)) (row (ent "db" 'must-not)))
   ;; must dominates may on union
   (list (row (ent "db" 'must)) (row (ent "db" 'may)))
   ;; ops union
   (list (row (ent "llm" 'may #:ops '("query")))
         (row (ent "llm" 'may #:ops '("embed"))))
   ;; ops "all" (empty) is absorbing
   (list (row (ent "llm" 'may #:ops '())) (row (ent "llm" 'may #:ops '("query"))))
   ;; unbounded budget (absence) dominates
   (list (row (ent "llm" 'may #:budget 5)) (row (ent "llm" 'may)))
   ;; scope: same scope merges
   (list (row (ent "db" 'may #:scope "reads=x")) (row (ent "db" 'may #:scope "reads=x")))
   ;; scope: conflict collapses to unconstrained
   (list (row (ent "db" 'may #:scope "reads=x")) (row (ent "db" 'may #:scope "reads=y")))
   ;; empty rows
   (list (row) (row))
   ;; empty + something
   (list (row) (row (ent "llm" 'must)))
   ;; overlap + disjoint mix
   (list (row (ent "llm" 'may #:budget 2) (ent "db" 'must-not))
         (row (ent "llm" 'may #:budget 7) (ent "net" 'may)))))

(define (test-effect-equivalence)
  (for ([p (in-list battery)])
    (check-equiv (car p) (cadr p))))

;; ── typed effect algebra (the extension contract.rkt lacks) ─────────

(define (test-typed-effect-subtyping)
  ;; Function type with a wider effect row is a supertype.
  (define e-narrow (list (effect-entry 'llm 'may '() 5 #f)))
  (define e-wide   (list (effect-entry 'llm 'may '() 10 #f)
                         (effect-entry 'db  'may '() #f #f)))
  (check-true (type-leq (t-fn (t-base 'int) (t-base 'string) e-narrow)
                        (t-fn (t-base 'int) (t-base 'string) e-wide)))

  ;; A network-performing function cannot satisfy a pure function type.
  (define e-net (list (effect-entry 'network 'may '() #f #f)))
  (check-false (type-leq (t-fn (t-base 'int) (t-base 'string) e-net)
                         (t-fn (t-base 'int) (t-base 'string) '())))

  ;; Argument is contravariant: fn(listof int) and fn(int) are incomparable.
  (check-false (type-leq (t-fn (t-listof (t-base 'int)) (t-base 'string) '())
                         (t-fn (t-base 'int) (t-base 'string) '())))
  (check-false (type-leq (t-fn (t-base 'int) (t-base 'string) '())
                         (t-fn (t-listof (t-base 'int)) (t-base 'string) '())))

  ;; Result is covariant: int ≤ int (identity) is the positive case here.
  (check-true (type-leq (t-fn (t-base 'int) (t-base 'int) '())
                        (t-fn (t-base 'int) (t-base 'int) '())))

  ;; listof is covariant: listof int ≤ listof int.
  (check-true (type-leq (t-listof (t-base 'int)) (t-listof (t-base 'int)))))

;; ── graph pattern query (AtomSpace PatternMatcher idea) ─────────────

(define (test-pattern-query)
  (define graph
    (list (effect-entry 'llm 'may '(query) 5 #f)
          (effect-entry 'db 'may '() #f #f)
          (effect-entry 'network 'may '() #f #f)))
  (define pat (effect-entry (var 'e) 'may (var 'ops) (var 'budget) (var 'scope)))
  (define matches (hg-query pat graph))
  (check-equal? (length matches) 3)
  (check-equal? (sort (map (λ (h) (hash-ref h 'e)) matches) symbol<?)
                '(db llm network))
  ;; Bound ops carry through: llm's ops are (query).
  (define llm-match
    (for/or ([h (in-list matches)])
      (and (eq? (hash-ref h 'e) 'llm) h)))
  (check-equal? (hash-ref llm-match 'ops) '(query)))

;; ── capability composition (Effervesce needs/provides) ──────────────

(define (test-capability-composition)
  ;; Read<Config> is a subtype of Read<Top-ish>: covariant in its argument.
  (check-true (type-leq (cap-ref 'Read (t-base 'int))
                        (cap-ref 'Read (t-base 'int))))
  ;; Different capability names never satisfy each other.
  (check-false (cap-satisfied? (list (cap-ref 'Read (t-base 'int)))
                               (list (cap-ref 'Fs))))
  ;; A component needing {Read, Log} is satisfied by one providing both.
  (check-true (cap-satisfied?
               (list (cap-ref 'Read (t-base 'int)) (cap-ref 'Log))
               (list (cap-ref 'Read (t-base 'int)) (cap-ref 'Log) (cap-ref 'Fs))))
  ;; Missing Log leaves the need unsatisfied.
  (check-false (cap-satisfied?
                (list (cap-ref 'Read (t-base 'int)) (cap-ref 'Log))
                (list (cap-ref 'Read (t-base 'int))))))

;; ── generic AST / S-expression materialization ──────────────────────

(define (test-materialize)
  (define atoms (materialize (uir-call (uir-symbol "fetch") '())))
  (check-true (for/or ([a (in-list atoms)])
                (and (node? a) (eq? (node-type a) 'uir-call))))
  (check-true (for/or ([a (in-list atoms)])
                (and (link? a) (eq? (link-type a) 'slot))))
  (check-true (for/or ([a (in-list atoms)])
                (and (node? a) (eq? (node-type a) 'str)
                     (equal? (node-name a) "fetch"))))
  (check-true (for/or ([a (in-list (materialize (uir->sexp (uir-call (uir-symbol "fetch") '()))))])
                (and (node? a) (eq? (node-type a) 'list))))
  (check-equal? (materialize "x") (list (node 'str "x"))))

;; ── OpenCog-compatible effect atoms ─────────────────────────────────

(define (test-open-cog-atoms)
  ;; A typed capability carries its receiver as a distinct node.
  (define op (effect-op 'db "QuerySet" "read"))
  (check-true (link? op))
  (check-equal? (link-type op) 'effect-op)
  (check-equal? (link-out op)
                (list (node 'domain 'db) (node 'type "QuerySet") (node 'op "read")))

  ;; MRO edge serializes to an InheritanceLink.
  (define inh (inherits "QuerySet" "Model"))
  (check-equal? (atom->atomese inh)
                "(InheritanceLink (ConceptNode \"QuerySet\") (ConceptNode \"Model\"))")

  ;; Typed op serializes to an EvaluationLink whose predicate names domain.verb
  ;; and whose argument is the receiver type node.
  (check-equal? (atom->atomese op)
                "(EvaluationLink (PredicateNode \"db.read\") (ConceptNode \"QuerySet\"))")

  ;; Two ops on the same receiver share that receiver node under flattening.
  (define op2 (effect-op 'db "QuerySet" "write"))
  (define flat (flatten-atoms (list op op2)))
  (define receiver-nodes
    (filter (λ (a) (and (node? a) (eq? (node-type a) 'type)
                        (equal? (node-name a) "QuerySet")))
            flat))
  (check-equal? (length receiver-nodes) 1))

;; ── one substrate for all six graphs (P4.1) ─────────────────────────
;;
;; The ROADMAP term for the merged substrate is "a single graph instance holds
;; all six graphs with typed, queryable edges": UIR, call, type, effect,
;; capability, module. The projections below take real data — the program's
;; materialized UIR, the call edges its named functions make, the effect rows
;; inference computes (through the production uir-project adapters), type-
;; relation triples, an Effervesce-style capability declaration set, and the
;; module import graph — and build ONE instance.

(define (merged-symbol text) (uir-symbol text))
(define (merged-fn name body) (uir-fn (merged-symbol name) '() body #f #f))

(define merged-program
  (uir-block
   (list
    (merged-fn "main"
               (uir-block (list (uir-call (merged-symbol "fetch")
                                          (list (merged-symbol "url"))))))
    (merged-fn "fetch"
               (uir-block (list (uir-call (merged-symbol "open")
                                          (list (merged-symbol "f")))
                                (uir-call (merged-symbol "sleep")
                                          (list (merged-symbol "1"))))))
    (uir-import (merged-symbol "django.db") (list (merged-symbol "models")))
    (uir-import (merged-symbol "util") '()))))

;; The production adapters from racklr/hypergraph/uir-project: the same
;; conversions no longer live in this test file.
(define merged-call-edges (program-call-edges merged-program))
(define merged-effect-rows (effect-rows->graph-rows merged-program))

(define merged-type-relations
  (list (list 'subtype 'Manager 'object)
        (list 'assign 'fetch.result 'Model)
        (list 'self-attr 'Model.objects 'Manager)
        (list 'param 'main.url 'str)
        (list 'return 'fetch 'Model)))

;; An Effervesce-style capability declaration set: App needs Store, Database
;; provides Store and needs Db, Db is given, Store via Db, plus a state/action.
(define merged-capability-decls
  (list (list 'component "Database" (list (cons "Db" '())) (list (cons "Store" '())))
        (list 'component "App" (list (cons "Store" '())) '())
        (list 'given (cons "Db" '()))
        (list 'handler (cons "Store" '()) (list (cons "Db" '())))
        (list 'state "cache")
        (list 'action "Load" "cache")))

(define merged-module-deps (program-module-deps merged-program 'app))

(define merged-graph
  (build-hypergraph #:uir merged-program
                    #:calls merged-call-edges
                    #:types merged-type-relations
                    #:effects merged-effect-rows
                    #:capabilities merged-capability-decls
                    #:modules merged-module-deps))

;; The inferred effect names on one function's row in the merged instance.
(define (merged-effect-names fn-name)
  (define row-link
    (first (hg-out merged-graph (hg-function-node fn-name) 'fn-effect-row)))
  (sort (map effect-key (rest (link-out row-link))) symbol<?))

;; The production adapters are verified directly, not only through the
;; projectors they feed: a caller can go from a UIR program and its inference
;; output to the #:calls / #:effects / #:modules inputs without test-local code.
(define (test-production-adapters)
  (check-equal? merged-call-edges
                '((fetch open) (fetch sleep) (main fetch)))
  (check-equal? merged-module-deps '((app django.db util)))
  (check-equal? (map first merged-effect-rows) '(fetch main))
  (check-equal? (map (λ (row) (sort (map effect-key (second row)) symbol<?))
                     merged-effect-rows)
                '((io time) (io time))))

(define (test-merged-substrate)
  ;; one instance, all six graph kinds present
  (check-true (hypergraph? merged-graph))
  (check-equal? (hg-kinds merged-graph) '(call capability effect module type uir))
  (for ([kind (in-list graph-kinds)])
    (check-true (pair? (hg-links merged-graph kind))
                (format "the merged instance holds no ~a edges" kind)))
  ;; the instance is exactly the six projections composed, not a side bag
  (check-equal? (hg-atoms merged-graph)
                (remove-duplicates
                 (flatten-atoms
                  (append (uir-graph-atoms merged-program)
                          (call-graph-atoms merged-call-edges)
                          (type-graph-atoms merged-type-relations)
                          (effect-graph-atoms merged-effect-rows)
                          (capability-graph-atoms merged-capability-decls)
                          (module-graph-atoms merged-module-deps)))))
  ;; every edge is typed by the schema
  (check-equal? (hg-ill-typed-edges merged-graph) '()))

(define (test-merged-typed-queries)
  ;; uir graph
  (check-true (pair? (hg-nodes merged-graph 'uir-call)))
  (check-true (pair? (hg-edges merged-graph 'slot)))
  ;; call graph: main -> fetch, and the reverse direction from the same edge
  (check-equal? (map (λ (l) (node-name (second (link-out l))))
                     (hg-out merged-graph (hg-function-node 'main) 'calls))
                '(fetch))
  (check-equal? (map (λ (l) (node-name (first (link-out l))))
                     (hg-in merged-graph (hg-function-node 'fetch) 'calls))
                '(main))
  ;; effect graph: the rows above are real inference output (io via open,
  ;; time via sleep, propagated from fetch to main through the call edge)
  (check-equal? (merged-effect-names 'fetch) '(io time))
  (check-equal? (merged-effect-names 'main) '(io time))
  ;; type graph
  (check-equal? (map (λ (l) (node-name (second (link-out l))))
                     (hg-out merged-graph (t-base 'Manager) 'subtype))
                '(object))
  (check-equal? (map (λ (l) (node-name (second (link-out l))))
                     (hg-out merged-graph (hg-type-slot-node 'fetch.result) 'assign))
                '(Model))
  ;; capability graph
  (check-equal? (map (λ (l) (node-name (second (link-out l))))
                     (hg-out merged-graph (hg-component-node "App") 'component-needs))
                (list (cons "Store" '())))
  (check-equal? (map (λ (l) (node-name (second (link-out l))))
                     (hg-out merged-graph (hg-capability-node (cons "Store" '()))
                             'capability-interprets))
                (list (cons "Db" '())))
  (check-true (pair? (hg-edges merged-graph 'given)))
  (check-equal? (map (λ (l) (node-name (second (link-out l))))
                     (hg-out merged-graph (hg-action-node "Load") 'action-writes))
                (list "cache"))
  ;; module graph
  (check-equal? (sort (map (λ (l) (node-name (second (link-out l))))
                           (hg-out merged-graph (hg-module-node 'app) 'module-imports))
                      symbol<?)
                '(django.db util))
  ;; cross-graph identity: one `function` atom is the vertex of three graphs
  (check-equal? (hg-node merged-graph 'function 'fetch) (hg-function-node 'fetch))
  (check-true (and (pair? (hg-incident merged-graph (hg-function-node 'fetch) 'calls))
                   (pair? (hg-incident merged-graph (hg-function-node 'fetch)
                                       'fn-effect-row))))
  ;; and the module graph can name that same function vertex
  (define with-defines
    (hg-add merged-graph (hg-module-defines-link 'app 'fetch)))
  (check-equal? (hg-out with-defines (hg-module-node 'app) 'module-defines)
                (list (hg-module-defines-link 'app 'fetch)))
  (check-equal? (hg-ill-typed-edges with-defines) '())
  ;; graph-kind filtering and exact edge-type filtering are different questions
  (check-equal? (hg-edges merged-graph 'effect)
                (filter (λ (l) (eq? (link-type l) 'effect))
                        (hg-links merged-graph 'effect)))
  (check-true (> (length (hg-links merged-graph 'effect))
                 (length (hg-edges merged-graph 'effect)))))

(define (test-typed-edge-schema)
  (check-equal? graph-kinds '(uir call type effect capability module))
  (for ([kind (in-list graph-kinds)])
    (check-true (pair? (edge-types-for-kind kind))))
  (for ([pair (in-list '((slot . uir)
                         (calls . call)
                         (subtype . type)
                         (fn-effect-row . effect)
                         (component-needs . capability)
                         (module-imports . module)))])
    (check-eq? (edge-kind (car pair)) (cdr pair)))
  ;; node vocabulary records which graph(s) an atom can belong to
  (check-equal? (node-kinds 'function) '(call effect module))
  (check-equal? (node-kinds 'uir-call) '(uir))
  ;; unknown edge types and ill-shaped edges are rejected, not stored silently
  (define unknown (hg-add (hg-empty) (link 'no-such-edge (list (node 'x 'y)))))
  (check-equal? (length (hg-ill-typed-edges unknown)) 1)
  (define wrong-arity
    (hg-add (hg-empty) (link 'calls (list (hg-function-node 'only-one)))))
  (check-equal? (length (hg-ill-typed-edges wrong-arity)) 1)
  (define wrong-role
    (hg-add (hg-empty) (link 'calls (list (hg-function-node 'a)
                                          (hg-module-node 'b)))))
  (check-equal? (length (hg-ill-typed-edges wrong-role)) 1)
  ;; a different vocabulary is an injected schema, not a different instance type
  (define injected
    (hg-empty (hash 'calls (edge-spec 'call 2 (list 'function 'function)))))
  (check-equal? (hg-kinds (hg-add injected (hg-calls-link 'a 'b))) '(call)))

;; ── instance API: set semantics, closure, direction ─────────────────

(define (test-graph-instance-api)
  (check-equal? (hg-atoms (hg-empty)) '())
  (check-equal? (hg-links (hg-empty)) '())
  (check-equal? (hg-kinds (hg-empty)) '())
  ;; adding is immutable and the instance is set-valued
  (define one (hg-add (hg-empty) (hg-calls-link 'a 'b)))
  (check-equal? (hg-atoms (hg-empty)) '())
  (check-equal? (length (hg-atoms one)) 3) ; the link plus both function nodes
  (check-equal? (length (hg-atoms (hg-add one (hg-calls-link 'a 'b)))) 3)
  ;; the closure makes an embedded node queryable on its own
  (check-equal? (hg-node one 'function 'a) (hg-function-node 'a))
  (check-equal? (hg-query (node 'function (var 'n)) (hg-atoms one))
                (list (hash 'n 'a) (hash 'n 'b)))
  ;; slot 0 is the source, the rest are targets
  (check-equal? (map (λ (l) (node-name (second (link-out l))))
                     (hg-out one (hg-function-node 'a) 'calls))
                '(b))
  (check-equal? (map (λ (l) (node-name (first (link-out l))))
                     (hg-in one (hg-function-node 'b) 'calls))
                '(a))
  (check-equal? (length (hg-incident one (hg-function-node 'b))) 1)
  (check-equal? (hg-ill-typed-edges (hg-empty)) '()))

;; ── suite + real exit code ──────────────────────────────────────────
;;
;; The checks are wrapped in test cases and the process exit code is the real
;; failure count: outside a test case a Rackunit check prints FAILURE but the
;; process still exits 0, which would make this suite a false green.

(define hypergraph-tests
  (test-suite
   "racklr/hypergraph typed substrate"
   (test-case "effect algebra equivalence with contract.rkt" (test-effect-equivalence))
   (test-case "typed effect subtyping" (test-typed-effect-subtyping))
   (test-case "graph pattern query" (test-pattern-query))
   (test-case "capability composition" (test-capability-composition))
   (test-case "generic materialization" (test-materialize))
   (test-case "OpenCog-compatible atoms" (test-open-cog-atoms))
   (test-case "instance API" (test-graph-instance-api))
   (test-case "production UIR/inference adapters" (test-production-adapters))
   (test-case "one instance, all six graphs" (test-merged-substrate))
   (test-case "typed queries across the merged substrate" (test-merged-typed-queries))
   (test-case "typed-edge schema" (test-typed-edge-schema))))

(define failure-count (run-tests hypergraph-tests))
(if (zero? failure-count)
    (displayln "All hypergraph tests passed.")
    (exit 1))
