#lang racket

;; racklr/graph/profile.rkt
;;
;; Transitive effect profiles: every function's effect edges, reached through
;; the inferred call graph and labelled by confidence and coverage.
;;
;; The analog is Rainbow's per-theorem axiom profile: a theorem exposes the
;; transitive set of axioms its proof depends on, and its maturity level is set
;; by the weakest link. An effect profile exposes, for one function:
;;
;;   entries      — one entry per effect reachable from the function along the
;;                  inferred call graph, carrying
;;                    * the effect's edge entry (modality / ops / budget / scope),
;;                    * the confidence and coverage labels aggregated over the
;;                      grounds that support it (WEAKEST link, like Rainbow),
;;                    * the provenance paths (call chains + grounding sites)
;;                      that witness it.
;;   obligations  — every unresolved ground reachable from the function, with
;;                  its chain and coverage evidence kind. This is the
;;                  "eliminate-unknown" obligation as DATA: a function is
;;                  complete exactly when this list is empty, and each entry
;;                  names the site that must be eliminated and why it stays
;;                  unresolved (parameter-rooted vs local/literal evidence).
;;
;; A profile is a graph query: the provenance graph (racklr/graph/provenance)
;; already stores grounds, call-site links, and the per-site labels, so the
;; profile is a breadth-first walk that collects the ground sites reachable from
;; the function. One shortest witness chain per ground, as in
;; `provenance-paths`, so the profile can point at the exact call chain.

(require racklr/effects/labels
         racklr/effects/provenance
         racklr/graph/provenance
         data/queue)

(provide
 ;; records
 (struct-out profile-entry)
 (struct-out effect-profile)
 ;; queries
 transitive-profile
 transitive-profiles
 profile-entry-for
 profile->string)

;; One effect in a function's transitive profile.
;;   effect     — the effect-name symbol
;;   entry      — the hypergraph effect-entry of the function's edge
;;   confidence — 'inferred | 'declared | 'unknown, weakest over the grounds
;;   coverage   — 'universal | 'census-param | 'local-literal, weakest over the
;;                grounds
;;   paths      — (listof walk-path): one shortest provenance chain per ground
(struct profile-entry (effect entry confidence coverage paths) #:transparent)

;; A function's transitive effect profile.
;;   entries      — (listof profile-entry), sorted by effect name
;;   obligations  — (listof walk-path) for every unresolved ground reachable
;;                  from the function, sorted
;;   complete?    — (null? obligations): the elimination obligation is data,
;;                  not a flag
(struct effect-profile (fn entries obligations complete?) #:transparent)

;; ── helpers ─────────────────────────────────────────────────────────

(define (sort-by-printed items)
  (sort items (λ (a b) (string<? (format "~s" a) (format "~s" b)))))

(define (site-expr-of idx site)
  (hash-ref (graph-index-site-exprs idx) site site))

;; Every function the provenance graph knows: it carries an effect edge or
;; grounds one, makes a call, or is called. A pure leaf function that only
;; appears as a callee still gets an (empty) complete profile rather than #f.
(define (indexed-functions idx)
  (define fns (make-hash))
  (for ([fq (in-hash-keys (graph-index-edges idx))]) (hash-set! fns fq #t))
  (for ([fq (in-hash-keys (graph-index-grounds idx))]) (hash-set! fns fq #t))
  (for ([(caller calls) (in-hash (graph-index-calls idx))])
    (hash-set! fns caller #t)
    (for ([call (in-list calls)]) (hash-set! fns (first call) #t)))
  (hash-keys fns))

(define (known-function? idx fq)
  (and (member fq (indexed-functions idx)) #t))

;; The walk-path ending at one ground: the reversed hops collected from the
;; queried function, plus the ground's own evidence.
(define (ground-path idx hops fn ground)
  (define site (first ground))
  (define entry (third ground))
  (walk-path
   (reverse hops)
   (walk-ground fn
                (site-expr-of idx site)
                (hg-entry-effect entry)
                (sort (hash-ref (graph-index-site-tokens idx) site '()) string<?)
                (hash-ref (graph-index-site-receivers idx) site #f)
                (hash-ref (graph-index-site-kinds idx) site 'inferred)
                (hash-ref (graph-index-site-coverages idx) site #f)
                entry)))

;; Breadth-first walk from `fq` collecting every reachable ground site. Each
;; visited function keeps the shortest hop list that reached it, so every
;; ground's witness is a shortest call chain from the queried function.
(define (reachable-paths idx fq)
  (define visited (make-hash))        ;; fq → reversed (listof walk-hop)
  (define queue (make-queue))
  (define paths '())
  (hash-set! visited fq '())
  (enqueue! queue fq)
  (let loop ()
    (unless (queue-empty? queue)
      (define fn (dequeue! queue))
      (define hops (hash-ref visited fn))
      (for ([ground (in-list (sort-by-printed (hash-ref (graph-index-grounds idx) fn '())))])
        (set! paths (cons (ground-path idx hops fn ground) paths)))
      (for ([call (in-list (sort-by-printed (hash-ref (graph-index-calls idx) fn '())))])
        (define callee (first call))
        (unless (hash-ref visited callee #f)
          (hash-set! visited callee
                     (cons (walk-hop fn callee (site-expr-of idx (second call))) hops))
          (enqueue! queue callee)))
      (loop)))
  paths)

;; ── profile ──────────────────────────────────────────────────────────

;; Build the profile record from the witness paths reachable from one function.
;; Per-effect labels are the weakest link over the grounds that support that
;; effect (a profile may not claim more than its weakest evidence), and the
;; obligations are exactly the unresolved grounds.
(define (paths->profile fq paths)
  (define by-effect (make-hash))      ;; effect → (listof walk-path)
  (for ([p (in-list paths)])
    (define effect (walk-ground-effect (walk-path-ground p)))
    (hash-update! by-effect effect (λ (l) (cons p l)) '()))
  (define entries
    (sort
     (for/list ([(effect ps) (in-hash by-effect)])
       (define grounds (map walk-path-ground ps))
       (profile-entry
        effect
        (walk-ground-entry (first grounds))
        (apply confidence-weakest
               (map (λ (g) (confidence-of-kind (walk-ground-kind g))) grounds))
        (apply coverage-weakest
               (map (λ (g) (or (walk-ground-coverage g) 'universal)) grounds))
        (sort ps (λ (a b) (string<? (walk-path->string a) (walk-path->string b))))))
     (λ (a b) (string<? (symbol->string (profile-entry-effect a))
                        (symbol->string (profile-entry-effect b))))))
  (define obligations
    (sort (for/list ([p (in-list paths)]
                     #:when (eq? (confidence-of-kind
                                  (walk-ground-kind (walk-path-ground p)))
                                 'unknown))
            p)
          (λ (a b) (string<? (walk-path->string a) (walk-path->string b)))))
  (effect-profile fq entries obligations (null? obligations)))

;; The transitive effect profile of one function: every effect reachable from it
;; along the inferred call graph, labelled by confidence and coverage and
;; witnessed by the exact call chains, plus the data structure of the
;; eliminate-unknown obligation. Returns #f when the function is not in the
;; provenance graph.
(define (transitive-profile graph fq)
  (define idx (ensure-index graph))
  (if (not (known-function? idx fq))
      #f
      (paths->profile fq (sort-by-printed (reachable-paths idx fq)))))

;; Every function's transitive profile, sorted by function identity. The
;; function set is the union of the effect edges, grounds, and call-site links:
;; a function is in the graph when it carries an effect edge, grounds an edge,
;; or calls / is called by another function.
(define (transitive-profiles graph)
  (define idx (ensure-index graph))
  (sort (filter values
                (for/list ([fq (in-list (sort (indexed-functions idx)
                                              (λ (a b) (string<? a b))))])
                  (transitive-profile idx fq)))
        (λ (a b) (string<? (effect-profile-fn a) (effect-profile-fn b)))))

;; The profile entry for one effect, or #f.
(define (profile-entry-for profile effect)
  (for/or ([e (in-list (effect-profile-entries profile))])
    (and (eq? (profile-entry-effect e) effect) e)))

;; Render a profile: one line per effect entry plus the open obligations, each
;; with its witness chain. Deterministic; used by tests and the corpus probe.
(define (profile->string profile)
  (define entries (effect-profile-entries profile))
  (define obligations (effect-profile-obligations profile))
  (string-join
   (append
    (list (format "~a: ~a effect(s), ~a open obligation(s)~a"
                  (effect-profile-fn profile)
                  (length entries)
                  (length obligations)
                  (if (effect-profile-complete? profile) "" " (incomplete)")))
    (for/list ([e (in-list entries)])
      (format "  ~a ~a/~a (~a witness~a)"
              (profile-entry-effect e)
              (profile-entry-confidence e)
              (profile-entry-coverage e)
              (length (profile-entry-paths e))
              (if (= 1 (length (profile-entry-paths e))) "" "es")))
    (for/list ([o (in-list obligations)])
      (format "  ! ~a" (walk-path->string o))))
   "\n"))
