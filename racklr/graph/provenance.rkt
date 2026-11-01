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
;; Atom vocabulary
;; ---------------
;;   (node 'function fq)     a function, fq = "component::name" or "name"
;;   (node 'effect name)     an effect kind (db, io, network, ...)
;;   (node 'site id)         one classifying call occurrence (owner-qualified)
;;   (node 'expression s)    the call expression a site classified, e.g. "open"
;;   (node 'token s)         a source token that grounded the classification
;;   (node 'receiver s)      the receiver class the dataflow resolved
;;   (node 'kind k)          'inferred | 'unresolved | 'declared
;;
;; Link vocabulary
;; ---------------
;;   (call-site caller callee site)          caller's call to callee, carrying
;;                                           the callee expression classified
;;   (site-expression site expression)       the readable site expression
;;   (site-effect site effect)               the effect a site grounded
;;   (site-token site token)                 a grounding token
;;   (site-receiver site receiver)           the receiver type, when resolved
;;   (site-kind site kind)                   how the site was classified
;;   (fn-effect function entry)              an effect edge (the row entry)
;;   (grounds function site entry)           direct evidence for that effect edge
;;
;; An effect edge `(fn-effect f entry)` is PROVEN by a path: start at `f`, follow
;; call-site links, and end at a function with a `grounds` link whose entry has
;; the same effect name. `provenance-paths` returns exactly those paths (one
;; shortest path per ground), so the query result IS the call chain — every hop
;; names the caller, the callee, and the call expression that carried the effect.
;;
;; `provenance-graph` refuses to project a row with an effect edge that no ground
;; can reach: "no effect edge exists without provenance" is enforced at
;; construction, not asserted in prose.

(require racklr/uir
         racklr/hypergraph
         racklr/effects/provenance
         data/queue)

(provide
  ;; projection
  provenance-graph
  ;; atom constructors (for consumers that extend the projection)
  function-node effect-node site-node token-node receiver-node kind-node
  fn-effect-link grounds-link call-site-link site-expression-link
  site-effect-link site-token-link site-receiver-link site-kind-link
  entry->hg-entry
  ;; queries
  effect-edge
  provenance-paths
  walk-hop walk-hop? walk-hop-from walk-hop-to walk-hop-site
  walk-ground walk-ground? walk-ground-fn walk-ground-site walk-ground-effect
  walk-ground-tokens walk-ground-receiver walk-ground-kind walk-ground-entry
  walk-path walk-path? walk-path-hops walk-path-ground
  walk-path->string
  ;; checking
  unprovenanced-edges
  assert-provenance-complete!)

;; ── atom constructors ───────────────────────────────────────────────

(define (function-node fq) (node 'function fq))
(define (effect-node name) (node 'effect name))
(define (site-node id) (node 'site id))
(define (token-node token) (node 'token token))
(define (receiver-node receiver) (node 'receiver receiver))
(define (kind-node kind) (node 'kind kind))

;; A site identity is the classifying function plus the expression, so the same
;; expression classified differently in two functions stays two distinct sites.
(define (site-id owner expression)
  (format "~a::@~a" owner expression))

(define (fn-effect-link fq entry)
  (link 'fn-effect (list (function-node fq) entry)))

(define (grounds-link fq site entry)
  (link 'grounds (list (function-node fq) (site-node site) entry)))

(define (call-site-link caller callee site)
  (link 'call-site (list (function-node caller) (function-node callee) (site-node site))))

(define (site-expression-link site expression)
  (link 'site-expression (list (site-node site) (node 'expression expression))))

(define (site-effect-link site effect)
  (link 'site-effect (list (site-node site) (effect-node effect))))

(define (site-token-link site token)
  (link 'site-token (list (site-node site) (token-node token))))

(define (site-receiver-link site receiver)
  (link 'site-receiver (list (site-node site) (receiver-node receiver))))

(define (site-kind-link site kind)
  (link 'site-kind (list (site-node site) (kind-node kind))))

;; ── UIR effect entry → hypergraph effect-entry hyperedge ─────────────

(define (entry->hg-entry e)
  (effect-entry
   (string->symbol (uir-symbol-name (uir-effect-entry-name e)))
   (uir-effect-entry-modality e)
   (map (λ (o) (string->symbol (uir-symbol-name o))) (uir-effect-entry-ops e))
   (uir-effect-entry-budget e)
   (and (uir-effect-entry-scope e)
        (string->symbol (uir-symbol-name (uir-effect-entry-scope e))))))

;; The effect-name symbol of a hypergraph effect entry link.
(define (hg-entry-effect entry)
  (node-name (first (link-out entry))))

;; The effect-name symbol of a UIR effect entry.
(define (uir-entry-effect e)
  (string->symbol (uir-symbol-name (uir-effect-entry-name e))))

;; Deterministic atom order: sort on the printed struct (type + fields), not on
;; Atomese rendering (two node types can render to the same ConceptNode text).
(define (atom-less? a b)
  (string<? (format "~s" a) (format "~s" b)))

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
      (define name (uir-entry-effect e))
      (hash-set! by-effect name entry)
      (add! (fn-effect-link fq entry)))
    (hash-set! entry-by fq by-effect))

  ;; Grounds: the direct evidence for an effect edge, with its tokens, receiver
  ;; type, and kind.
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
        (add! (site-kind-link id (prov-site-kind site))))))

  ;; Call graph: each edge carries the callee expression the classifier saw, so
  ;; a path step names WHY the effect travelled that hop.
  (for ([(caller calls) (in-hash (provenance-calls prov))])
    (for ([call (in-list calls)])
      (define id (site-id caller (prov-call-site call)))
      (add! (call-site-link caller (prov-call-to call) id))
      (add! (site-expression-link id (prov-call-site call)))))

  (define graph (sort (flatten-atoms atoms) atom-less?))
  (if require?
      (let ([missing (provenance-missing rows prov)])
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
;; classification kind, and the effect edge entry it proves.
(struct walk-ground (fn site effect tokens receiver kind entry) #:transparent)

;; A complete provenance path for one effect edge: the hops from the queried
;; function toward the grounding function, then the ground itself.
(struct walk-path (hops ground) #:transparent)

;; A private parsed view of the graph atoms.
(struct index (calls grounds edges site-exprs site-tokens site-receivers site-kinds)
  #:transparent)

(define (out-name l i) (node-name (list-ref (link-out l) i)))

(define (index-atoms atoms)
  (define calls (make-hash))         ;; fq → (list (list callee-fq site-id))
  (define grounds (make-hash))       ;; fq → (list (list site-id effect entry))
  (define edges (make-hash))         ;; fq → (list entry)
  (define site-exprs (make-hash))    ;; site-id → expression
  (define site-tokens (make-hash))   ;; site-id → (list token)
  (define site-receivers (make-hash));; site-id → receiver
  (define site-kinds (make-hash))    ;; site-id → classification kind
  (for ([a (in-list atoms)])
    (if (link? a)
        (case (link-type a)
          [(site-expression)
           (hash-set! site-exprs (out-name a 0) (node-name (list-ref (link-out a) 1)))]
          [(site-token)
           (hash-update! site-tokens (out-name a 0)
                         (λ (l) (cons (out-name a 1) l)) '())]
          [(site-receiver)
           (hash-set! site-receivers (out-name a 0) (out-name a 1))]
          [(site-kind)
           (hash-set! site-kinds (out-name a 0) (out-name a 1))]
          [(call-site)
           (hash-update! calls (out-name a 0)
                         (λ (l) (cons (list (out-name a 1) (out-name a 2)) l)) '())]
          [(grounds)
           (hash-update! grounds (out-name a 0)
                         (λ (l) (cons (list (out-name a 1)
                                            (node-name (first (link-out (list-ref (link-out a) 2))))
                                            (list-ref (link-out a) 2))
                                      l))
                            '())]
          [(fn-effect)
           (hash-update! edges (out-name a 0)
                         (λ (l) (cons (list-ref (link-out a) 1) l)) '())]
          [else (void)])
        (void)))
  (index calls grounds edges site-exprs site-tokens site-receivers site-kinds))

;; The effect edge (hypergraph effect-entry link) for `fq` and `effect-name`, or
;; #f when the function carries no such edge.
(define (effect-edge atoms fq effect-name)
  (define idx (index-atoms atoms))
  (for/or ([entry (in-list (hash-ref (index-edges idx) fq '()))])
    (and (eq? (hg-entry-effect entry) effect-name) entry)))

(define (sort-calls calls)
  (sort calls (λ (a b) (string<? (format "~a" a) (format "~a" b)))))

(define (sort-grounds grounds)
  (sort grounds (λ (a b) (string<? (format "~a" a) (format "~a" b)))))

;; Shortest call-chain paths from `fq` to the grounds of `effect-name`, one per
;; ground. Breadth-first so each returned chain is a shortest witness; the graph
;; is the provenance, so every step is a typed `call-site` link.
(define (provenance-paths atoms fq effect-name)
  (define idx (index-atoms atoms))
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
      (for ([ground (in-list (sort-grounds (hash-ref (index-grounds idx) fn '())))])
        (define site (first ground))
        (define entry (third ground))
        (if (eq? (hg-entry-effect entry) want)
            (let* ([tokens (sort (hash-ref (index-site-tokens idx) site '()) string<?)]
                   [receiver (hash-ref (index-site-receivers idx) site #f)]
                   [expr (hash-ref (index-site-exprs idx) site site)]
                   [kind (hash-ref (index-site-kinds idx) site 'inferred)])
              (set! results
                    (cons (walk-path (reverse hops)
                                     (walk-ground fn expr want tokens receiver
                                                  kind entry))
                          results)))
            (void)))
      ;; Continue along the inferred call graph; the first visit is the shortest.
      (for ([call (in-list (sort-calls (hash-ref (index-calls idx) fn '())))])
        (define callee (first call))
        (unless (hash-ref seen callee #f)
          (hash-set! seen callee #t)
          (define site (hash-ref (index-site-exprs idx) (second call) (second call)))
          (push! (cons (walk-hop fn callee site) hops) callee)))
      (loop)))
  (sort results (λ (a b) (string<? (walk-path->key a) (walk-path->key b)))))

(define (walk-path->key p)
  (format "~a/~a" (walk-ground-fn (walk-path-ground p)) (walk-path->string p)))

;; ── rendering ───────────────────────────────────────────────────────

(define (walk-ground->string g)
  (format "~a [~a; tokens: ~a~a; ~a]"
          (walk-ground-site g)
          (walk-ground-effect g)
          (string-join (walk-ground-tokens g) ",")
          (if (walk-ground-receiver g)
              (format "; receiver: ~a" (walk-ground-receiver g))
              "")
          (walk-ground-kind g)))

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

;; ── checking ────────────────────────────────────────────────────────

;; Every effect edge in the graph whose effect has no ground reachable from it.
(define (unprovenanced-edges atoms)
  (define idx (index-atoms atoms))
  (sort
   (for*/list ([(fq entries) (in-hash (index-edges idx))]
               [entry (in-list entries)]
               #:unless (pair? (provenance-paths atoms fq (hg-entry-effect entry))))
     (list fq (hg-entry-effect entry)))
   (λ (a b) (string<? (format "~a/~a" (car a) (cadr a))
                      (format "~a/~a" (car b) (cadr b))))))

;; Raise unless every effect edge in the graph is provable.
(define (assert-provenance-complete! atoms)
  (define missing (unprovenanced-edges atoms))
  (if (null? missing)
      (void)
      (error 'assert-provenance-complete!
             "effect edges without provenance: ~a"
             (string-join (map (λ (m) (format "~a/~a" (car m) (cadr m))) missing)
                          ", "))))
