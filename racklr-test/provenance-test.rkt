#lang racket

;; P4.2 — provenance edges as first-class.
;;
;; Proves that inference records the evidence for every effect edge (the
;; classifying call site, its tokens, the receiver type, the call chain) and
;; that the typed-graph projection answers "why is this function db" by walking
;; edges:
;;
;;   * a direct effect grounds on its classifying call expression;
;;   * a propagated effect returns the exact caller→callee chain that carried it;
;;   * receiver-type dataflow puts the receiver class on the provenance edge;
;;   * a declared capability grounds at the method definition (tokens = the
;;     module path / class / method the DATA rule was derived from);
;;   * an `unknown` leaf is still provenanced (kind 'unresolved);
;;   * the graph refuses to contain an effect edge with no ground, so "no effect
;;     edge exists without provenance" is checked at construction.

(require rackunit
         racklr/hypergraph
         racklr/uir
         racklr/effects/contract
         racklr/effects/imports
         racklr/effects/infer
         racklr/effects/provenance
         racklr/graph/provenance)

;; ── helpers ─────────────────────────────────────────────────────────

(define (sym s) (uir-symbol s))
(define (v s) (uir-var (uir-symbol s)))
(define (dot b f) (uir-get b (uir-symbol f)))
(define (fn name body #:params [params '()])
  (uir-fn (uir-symbol name) params body #f #f))
(define (prog . stmts) (uir-block stmts))
(define (py-class name methods)
  (uir-class (uir-symbol name) '() '() methods))
(define (py-class/super name super methods)
  (uir-class (uir-symbol name) (list super) '() methods))
(define (py-method name body)
  (uir-method (uir-symbol name) '() body 'public))

;; Infer one program and return (values rows graph).
(define (infer-graph program)
  (define prov-box (box #f))
  (define rows (infer-effects program #:provenance-record prov-box))
  (values rows (provenance-graph rows (unbox prov-box))))

;; Whole-program analogue: (values rows graph).
(define (infer-all-graph comp-programs #:import-env [env #f])
  (define prov-box (box #f))
  (define rows (infer-effects-all comp-programs #:import-env env #:provenance-record prov-box))
  (values rows (provenance-graph rows (unbox prov-box))))

(define (ground-of path) (walk-path-ground path))
(define (hop-sites path)
  (map walk-hop-site (walk-path-hops path)))
(define (chain-of path)
  (define hops (walk-path-hops path))
  (define ground (walk-path-ground path))
  (if (null? hops)
      (list (walk-ground-fn ground))
      (cons (walk-hop-from (first hops)) (map walk-hop-to hops))))

;; ── direct effect: the classifying call site is the ground ──────────

(define (test-direct-effect-ground)
  (define-values (rows graph)
    (infer-graph (prog (fn "f" (uir-call (v "open") (list))))))
  ;; The row carries io, and the graph carries its edge.
  (check-equal? (map effect-name-symbol (uir-effect-row-entries (hash-ref rows 'f)))
                '(io))
  (check-true (link? (effect-edge graph "f" 'io)))
  (check-false (effect-edge graph "f" 'db))
  ;; The provenance query returns the exact ground.
  (define paths (provenance-paths graph "f" 'io))
  (check-equal? (length paths) 1)
  (define ground (ground-of (first paths)))
  (check-equal? (walk-path-hops (first paths)) '())
  (check-equal? (walk-ground-fn ground) "f")
  (check-equal? (walk-ground-site ground) "open")
  (check-equal? (walk-ground-effect ground) 'io)
  (check-equal? (walk-ground-kind ground) 'inferred)
  (check-not-false (member "open" (walk-ground-tokens ground)))
  (check-false (walk-ground-receiver ground))
  (check-true (string-prefix? (walk-path->string (first paths)) "f => open [io")))

;; ── propagated effect: the exact call chain ─────────────────────────

(define (test-propagated-call-chain)
  ;; c calls requests.get (network); b calls c; a calls b.
  (define-values (rows graph)
    (infer-graph
     (prog (fn "a" (uir-call (v "b") (list)))
           (fn "b" (uir-call (v "c") (list)))
           (fn "c" (uir-call (dot (v "requests") "get") (list))))))
  (check-equal? (map effect-name-symbol (uir-effect-row-entries (hash-ref rows 'a)))
                '(network))
  ;; Querying the top of the chain returns the whole path, hop by hop.
  (define paths (provenance-paths graph "a" 'network))
  (check-equal? (length paths) 1)
  (define path (first paths))
  (check-equal? (map walk-hop-from (walk-path-hops path)) '("a" "b"))
  (check-equal? (map walk-hop-to (walk-path-hops path)) '("b" "c"))
  (check-equal? (hop-sites path) '("b" "c"))
  (check-equal? (chain-of path) '("a" "b" "c"))
  (check-equal? (walk-ground-site (ground-of path)) "requests.get")
  (check-not-false (member "requests" (walk-ground-tokens (ground-of path))))
  (check-not-false (member "get" (walk-ground-tokens (ground-of path))))
  ;; Querying the middle of the chain returns the shorter chain, not the
  ;; entry from the root: the path is per effect edge, exact.
  (define b-path (first (provenance-paths graph "b" 'network)))
  (check-equal? (chain-of b-path) '("b" "c"))
  (check-equal? (hop-sites b-path) '("c"))
  ;; A pure function has no effect edge at all.
  (check-equal? (provenance-paths graph "a" 'db) '()))

;; ── the legacy table view stays compatible ──────────────────────────

(define (test-legacy-provenance-table)
  ;; Effervesce's bridge checker still asks for the rendered table
  ;; (name → effect → call-chain strings); the structured record must not
  ;; break it.
  (define p
    (prog (fn "a" (uir-call (v "b") (list)))
          (fn "b" (uir-call (v "c") (list)))
          (fn "c" (uir-call (v "open") (list)))))
  (define table (box #f))
  (infer-effects p #:provenance-table table)
  (check-equal? (hash-ref (hash-ref (unbox table) 'c) 'io) '("open"))
  (check-equal? (hash-ref (hash-ref (unbox table) 'b) 'io) '("c::open"))
  (check-equal? (sort (hash-ref (hash-ref (unbox table) 'a) 'io) string<?)
                '("b::c::open")))

;; ── unknown is a provenanced obligation ─────────────────────────────

(define (test-unknown-is-provenanced)
  (define-values (_rows graph)
    (infer-graph (prog (fn "f" (uir-call (dot (v "arbitrary") "m") (list))))))
  (define paths (provenance-paths graph "f" 'unknown))
  (check-equal? (length paths) 1)
  (define ground (ground-of (first paths)))
  (check-equal? (walk-ground-kind ground) 'unresolved)
  (check-equal? (walk-ground-site ground) "arbitrary.m")
  (check-not-false (member "arbitrary" (walk-ground-tokens ground)))
  (check-not-false (member "m" (walk-ground-tokens ground))))

;; ── receiver-type dataflow on the provenance edge ───────────────────

(define (test-receiver-type-on-edge)
  ;; `qs.filter(...)` where `qs` is imported as the class QuerySet from a
  ;; db-tagged module: the capability is typed db<QuerySet>.read, so the
  ;; provenance edge names the receiver class that made it db.
  (define app
    (prog (fn "run" (uir-call (dot (v "qs") "filter") (list (uir-record '()))))))
  (define env
    (import-env (hash "app.py" "app")
                (hash "app" (hash "qs" (handle 'class "django.db.models" "QuerySet")))
                (hash)))
  (define-values (rows graph)
    (infer-all-graph (list (cons "app.py" app)) #:import-env env))
  (check-equal? (entry-ops (row-lookup (hash-ref rows (cons "app.py" 'run)) 'db))
                '(db<QuerySet>.read))
  (define path (first (provenance-paths graph "app.py::run" 'db)))
  (check-equal? (walk-ground-receiver (ground-of path)) "QuerySet")
  (check-equal? (walk-ground-site (ground-of path)) "qs.filter")
  (check-true (string-contains? (walk-path->string path) "receiver: QuerySet")))

;; ── declared capability grounds at the definition ───────────────────

(define (test-declared-capability-ground)
  ;; A blessed capability (the DATA config asserts Model.save is db<Model>.write)
  ;; is a ground at the method definition, not at a call site. A caller reaches
  ;; it through the call graph, so the chain ends at the declared evidence.
  (define models (prog (py-class "Model" (list (py-method "save" (uir-null))))))
  (define app
    (prog (py-class/super "Sub" (v "Model")
                          (list (py-method "run"
                                           (uir-call (dot (v "self") "save") (list)))))))
  (define env
    (import-env (hash "models.py" "django.db.models" "app.py" "app")
                (hash)
                (hash "Model.save"
                      (effect-cap 'db (list 'db<Model>.write)))))
  (define-values (_rows graph)
    (infer-all-graph (list (cons "models.py" models) (cons "app.py" app))
                     #:import-env env))
  (define paths (provenance-paths graph "app.py::Sub.run" 'db))
  (check-equal? (length paths) 1)
  (define path (first paths))
  (check-equal? (chain-of path) '("app.py::Sub.run" "models.py::Model.save"))
  (define ground (ground-of path))
  (check-equal? (walk-ground-kind ground) 'declared)
  (check-equal? (walk-ground-site ground) "(declared)")
  (check-equal? (walk-ground-receiver ground) "Model")
  (check-equal? (walk-ground-tokens ground)
                (sort '("django.db.models" "Model" "save") string<?)))

;; ── cross-file call chain ───────────────────────────────────────────

(define (test-cross-file-chain)
  ;; b.py::helper calls open (io); a.py::run calls b.py::helper.
  (define b (prog (fn "helper" (uir-call (v "open") (list)))))
  (define a (prog (fn "run" (uir-call (v "helper") (list)))))
  (define-values (rows graph)
    (infer-all-graph (list (cons "a.py" a) (cons "b.py" b))))
  (check-equal? (map effect-name-symbol
                     (uir-effect-row-entries (hash-ref rows (cons "a.py" 'run))))
                '(io))
  (define path (first (provenance-paths graph "a.py::run" 'io)))
  (check-equal? (chain-of path) '("a.py::run" "b.py::helper"))
  (check-equal? (walk-ground-fn (ground-of path)) "b.py::helper")
  (check-equal? (walk-ground-site (ground-of path)) "open"))

;; ── recursion terminates and still finds the ground ─────────────────

(define (test-recursive-chain)
  ;; a calls itself and b; b calls open. The query must terminate (cycle guard)
  ;; and return the chain through b, not loop on a.
  (define p
    (prog (fn "a" (uir-block (list (uir-call (v "a") (list))
                                   (uir-call (v "b") (list)))))
          (fn "b" (uir-call (v "open") (list)))))
  (define-values (_rows graph) (infer-graph p))
  (define paths (provenance-paths graph "a" 'io))
  (check-equal? (length paths) 1)
  (check-equal? (chain-of (first paths)) '("a" "b"))
  (check-equal? (walk-ground-site (ground-of (first paths))) "open"))

;; ── multiple grounds: one path per evidence site ────────────────────

(define (test-multiple-grounds)
  (define p
    (prog (fn "f" (uir-block (list (uir-call (v "open") (list))
                                   (uir-call (dot (v "requests") "get") (list)))))))
  (define-values (_rows graph) (infer-graph p))
  (define io-paths (provenance-paths graph "f" 'io))
  (define network-paths (provenance-paths graph "f" 'network))
  (check-equal? (length io-paths) 1)
  (check-equal? (length network-paths) 1)
  (check-equal? (walk-ground-site (ground-of (first io-paths))) "open")
  (check-equal? (walk-ground-site (ground-of (first network-paths))) "requests.get"))

;; ── "no effect edge exists without provenance" ──────────────────────

(define (test-provenance-is-enforced)
  ;; A row entry with no ground is refused at projection time.
  (define p (prog (fn "f" (uir-call (v "open") (list)))))
  (define prov-box (box #f))
  (define rows (infer-effects p #:provenance-record prov-box))
  (define rows* (hash-copy rows))
  (hash-set! rows* 'g
             (uir-effect-row (list (uir-effect-entry (sym "network") 'may '() #f #f))))
  (check-exn exn:fail?
             (λ () (provenance-graph rows* (unbox prov-box))))
  ;; Projecting without the check still exposes the unprovenanced edge, so a
  ;; consumer can report it rather than silently trusting the row.
  (define graph (provenance-graph rows* (unbox prov-box) #:require-provenance? #f))
  (check-equal? (unprovenanced-edges graph) (list (list "g" 'network)))
  (check-exn exn:fail? (λ () (assert-provenance-complete! graph))))

(define (test-real-graph-is-complete)
  (define p
    (prog (fn "a" (uir-call (v "b") (list)))
          (fn "b" (uir-call (v "open") (list)))))
  (define-values (_rows graph) (infer-graph p))
  (check-equal? (unprovenanced-edges graph) '())
  (check-true (void? (assert-provenance-complete! graph))))

(module+ main
  (test-direct-effect-ground)
  (test-propagated-call-chain)
  (test-unknown-is-provenanced)
  (test-legacy-provenance-table)
  (test-receiver-type-on-edge)
  (test-declared-capability-ground)
  (test-cross-file-chain)
  (test-recursive-chain)
  (test-multiple-grounds)
  (test-provenance-is-enforced)
  (test-real-graph-is-complete)
  (displayln "All provenance tests passed."))
