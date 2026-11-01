#lang racket

;; racklr/graph/provenance.rkt
;;
;; Effect-edge provenance as FIRST-CLASS TYPED GRAPH EDGES (P4.2).
;;
;; The inference pass (racklr/effects/infer) records the grounds of every
;; classification and the inferred call graph; this module projects them into
;; the typed-hypergraph substrate (racklr/hypergraph) so that "why is this
;; function db" is answered by walking edges, not by reading a side comment.
;;
;; An effect edge `(fn-effect f entry)` is PROVEN by a path: start at `f`, follow
;; call-site links, and end at a function with a `grounds` link whose entry has
;; the same effect name. `provenance-paths` returns exactly those paths (one
;; shortest path per ground), so the query result IS the call chain — every hop
;; names the caller, the callee, and the call expression that carried the effect.
;; Each ground also carries the edge's labels: the classification kind (which
;; maps to the confidence label) and the coverage label.
;;
;; `provenance-graph` refuses to project a row with an effect edge that no ground
;; can reach: "no effect edge exists without provenance" is enforced at
;; construction, by the SAME reachability traversal over the projected atoms
;; that the query uses. A projection bug that drops a call-site link therefore
;; fails the construction instead of silently producing an unprovable edge.
;;
;; This module is the projection and the query. The atom vocabulary
;; (provenance-atoms.rkt), the parsed index (provenance-index.rkt), and the
;; completeness check (provenance-check.rkt) live in sibling modules, and are
;; re-exported here so consumers only require racklr/graph/provenance.

(require racklr/uir
         racklr/hypergraph
         racklr/effects/provenance
         data/queue
         "provenance-atoms.rkt"
         "provenance-index.rkt"
         "provenance-check.rkt")

(provide
  ;; projection
  provenance-graph
  ;; queries
  effect-edge
  provenance-paths
  walk-hop walk-hop? walk-hop-from walk-hop-to walk-hop-site
  walk-ground walk-ground? walk-ground-fn walk-ground-site walk-ground-effect
  walk-ground-tokens walk-ground-receiver walk-ground-kind walk-ground-coverage
  walk-ground-entry
  walk-path walk-path? walk-path-hops walk-path-ground
  walk-path->string
  ;; re-exported submodule APIs (kept stable for existing consumers)
  (all-from-out "provenance-atoms.rkt")
  (all-from-out "provenance-index.rkt")
  (all-from-out "provenance-check.rkt"))

;; ── projection ──────────────────────────────────────────────────────

;; Project `rows` (the inference result: call-graph key → uir-effect-row) and
;; `prov` (the provenance record collected by the same run) into the typed graph.
;; Every effect edge must be provable from a ground, else this raises: an
;; unprovenanced effect edge is a bug, not a query result.
(define (provenance-graph rows prov #:require-provenance? [require? #t])
  (define row-map (make-hash))      ;; fq → uir-effect-row
  (for ([(key row) (in-hash rows)])
    (define fq (function-fq key))
    (when (hash-has-key? row-map fq)
      (error 'provenance-graph "duplicate function identity ~a" fq))
    (hash-set! row-map fq row))

  (define atoms '())
  (define (add! a) (set! atoms (cons a atoms)))

  ;; The effect edge of every row entry, plus a per-function lookup of the
  ;; hypergraph entry links so grounds can point at the edge they prove.
  (define entry-by (make-hash))     ;; fq → (hash effect → entry link)
  (for ([(fq row) (in-hash row-map)])
    (define by-effect (make-hash))
    (for ([e (in-list (uir-effect-row-entries row))])
      (define entry (entry->hg-entry e))
      (define name (entry-effect-name e))
      (hash-set! by-effect name entry)
      (add! (fn-effect-link fq entry)))
    (hash-set! entry-by fq by-effect))

  ;; Grounds: the direct evidence for an effect edge, with its tokens, receiver
  ;; type, classification kind, and coverage label.
  (for ([(fq by-effect) (in-hash (provenance-grounds prov))])
    (for ([(effect sites) (in-hash by-effect)])
      (define entry (hash-ref (hash-ref entry-by fq (hash)) effect #f))
      (unless entry
        (error 'provenance-graph
               "ground for ~a at ~a is not in that function's row"
               effect fq))
      (for ([site (in-list sites)])
        (define id (site-id fq (prov-site-site site)))
        (add! (grounds-link fq id entry))
        (add! (site-expression-link id (prov-site-site site)))
        (add! (site-effect-link id effect))
        (for ([token (in-list (prov-site-tokens site))])
          (add! (site-token-link id token)))
        (if (prov-site-receiver site)
            (add! (site-receiver-link id (prov-site-receiver site)))
            (void))
        (add! (site-kind-link id (prov-site-kind site)))
        (if (prov-site-coverage site)
            (add! (site-coverage-link id (prov-site-coverage site)))
            (void)))))

  ;; Call graph: each edge carries the callee expression the classifier saw, so
  ;; a path step names WHY the effect travelled that hop.
  (for ([(caller calls) (in-hash (provenance-calls prov))])
    (for ([call (in-list calls)])
      (define id (site-id caller (prov-call-site call)))
      (add! (call-site-link caller (prov-call-to call) id))
      (add! (site-expression-link id (prov-call-site call)))))

  (define graph (sort (flatten-atoms atoms) atom-less?))
  ;; Enforcement is the atom-level reachability check, not a promise from the
  ;; record tables: the graph the consumer will query is the graph that is
  ;; proven complete.
  (if require?
      (let ([missing (unprovenanced-edges graph)])
        (if (null? missing)
            graph
            (error 'provenance-graph
                   "effect edges without provenance: ~a"
                   (string-join (map (λ (m) (format "~a/~a" (car m) (cadr m))) missing)
                                ", "))))
      graph))

;; ── query ───────────────────────────────────────────────────────────

;; One hop of a provenance path: the caller, the callee, and the call expression
;; that classification saw at that step.
(struct walk-hop (from to site) #:transparent)

;; The grounding site a path ends at: the classifying call expression, the
;; effect it grounded, its tokens, the receiver type (when resolved), the
;; classification kind (confidence evidence), the coverage label, and the effect
;; edge entry it proves.
(struct walk-ground (fn site effect tokens receiver kind coverage entry)
  #:transparent)

;; A complete provenance path for one effect edge: the hops from the queried
;; function toward the grounding function, then the ground itself.
(struct walk-path (hops ground) #:transparent)

;; The effect edge (hypergraph effect-entry link) for `fq` and `effect-name`, or
;; #f when the function carries no such edge.
(define (effect-edge graph fq effect-name)
  (define idx (ensure-index graph))
  (for/or ([entry (in-list (hash-ref (graph-index-edges idx) fq '()))])
    (and (eq? (hg-entry-effect entry) effect-name) entry)))

(define (sort-calls calls)
  (sort calls (λ (a b) (string<? (format "~a" a) (format "~a" b)))))

(define (sort-grounds grounds)
  (sort grounds (λ (a b) (string<? (format "~a" a) (format "~a" b)))))

;; Shortest call-chain paths from `fq` to the grounds of `effect-name`, one per
;; ground. Breadth-first so each returned chain is a shortest witness; the graph
;; is the provenance, so every step is a typed `call-site` link.
(define (provenance-paths graph fq effect-name)
  (define idx (ensure-index graph))
  (define want (if (symbol? effect-name) effect-name (string->symbol (format "~a" effect-name))))
  (define results '())
  (define seen (make-hash))
  (define queue (make-queue))
  (define (push! hops fn) (enqueue! queue (list hops fn)))
  (hash-set! seen fq #t)
  (push! '() fq)
  (let loop ()
    (unless (queue-empty? queue)
      (define item (dequeue! queue))
      (define hops (first item))
      (define fn (second item))
      ;; Every ground at this function proves the edge; the hops are the chain.
      (for ([ground (in-list (sort-grounds (hash-ref (graph-index-grounds idx) fn '())))])
        (define site (first ground))
        (define entry (third ground))
        (if (eq? (hg-entry-effect entry) want)
            (let* ([tokens (sort (hash-ref (graph-index-site-tokens idx) site '()) string<?)]
                   [receiver (hash-ref (graph-index-site-receivers idx) site #f)]
                   [expr (hash-ref (graph-index-site-exprs idx) site site)]
                   [kind (hash-ref (graph-index-site-kinds idx) site 'inferred)]
                   [coverage (hash-ref (graph-index-site-coverages idx) site #f)])
              (set! results
                    (cons (walk-path (reverse hops)
                                     (walk-ground fn expr want tokens receiver
                                                  kind coverage entry))
                          results)))
            (void)))
      ;; Continue along the inferred call graph; the first visit is the shortest.
      (for ([call (in-list (sort-calls (hash-ref (graph-index-calls idx) fn '())))])
        (define callee (first call))
        (unless (hash-ref seen callee #f)
          (hash-set! seen callee #t)
          (define site (hash-ref (graph-index-site-exprs idx) (second call) (second call)))
          (push! (cons (walk-hop fn callee site) hops) callee)))
      (loop)))
  (sort results (λ (a b) (string<? (walk-path->key a) (walk-path->key b)))))

(define (walk-path->key p)
  (format "~a/~a" (walk-ground-fn (walk-path-ground p)) (walk-path->string p)))

;; ── rendering ───────────────────────────────────────────────────────

(define (walk-ground->string g)
  (format "~a [~a; tokens: ~a~a; ~a/~a]"
          (walk-ground-site g)
          (walk-ground-effect g)
          (string-join (walk-ground-tokens g) ",")
          (if (walk-ground-receiver g)
              (format "; receiver: ~a" (walk-ground-receiver g))
              "")
          (walk-ground-kind g)
          (or (walk-ground-coverage g) 'unknown)))

;; Render a path as the exact call chain: `a -[b()]-> b => open [io; tokens: ...]`.
(define (walk-path->string p)
  (define hops (walk-path-hops p))
  (define ground (walk-path-ground p))
  (if (null? hops)
      (format "~a => ~a" (walk-ground-fn ground) (walk-ground->string ground))
      (format "~a ~a => ~a"
              (walk-hop-from (first hops))
              (apply string-append
                     (for/list ([hop (in-list hops)])
                       (format " -[~a]-> ~a" (walk-hop-site hop) (walk-hop-to hop))))
              (walk-ground->string ground))))
