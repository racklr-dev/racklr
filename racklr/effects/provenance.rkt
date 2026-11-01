#lang racket

;; racklr/effects/provenance.rkt
;;
;; First-class provenance for effect edges (P4.2).
;;
;; Inference already knows WHY it classified a call: the classifying call-site
;; expression, the tokens that grounded it (the receiver root and the operation
;; verb), the receiver type when receiver-type dataflow resolved one, and the
;; caller→callee edges the entry propagated along. This module records that as
;; structured data instead of a rendering string, so the graph projection
;; (racklr/graph/provenance.rkt) can carry it as typed graph edges and a
;; provenance query can return the exact call-chain path for an effect edge.
;;
;; Three records:
;;
;;   prov-site — a GROUND: the classifying call site where an effect was
;;               actually detected (or an unresolved `unknown` leaf, or a
;;               declared/blessed capability asserted at a method definition).
;;               Carries the source tokens and the receiver type, so every
;;               effect edge can be pointed at evidence.
;;   prov-call — one caller→callee edge of the inferred call graph, carrying
;;               the callee expression the classification saw. This is the
;;               step a provenance path walks.
;;   provenance — the two tables above, keyed by function identity.
;;
;; Function identity is a fully-qualified name string: "file.py::name" for a
;; component-qualified whole-program key, "name" for a same-file key. That is
;; the same identity the type-graph projection already uses for `function`
;; nodes, so the provenance graph and the type graph agree on what a function
;; is.
;;
;; Completeness is checkable without enumerating paths: if `g`'s row carries
;; effect E and `f` calls `g`, the fixpoint gives `f` effect E, so `f` has an
;; effect edge for E only if it can reach some ground site of E along the
;; recorded call edges. `provenance-missing` computes that reachability and
;; returns every effect edge that has no ground — "no effect edge exists
;; without provenance", as a property rather than a promise.

(require racklr/uir
         racklr/effects/shape
         racklr/effects/imports)

(provide
  ;; function identity
  function-fq
  ;; provenance records
  prov-site prov-site? prov-site-owner prov-site-effect prov-site-site
  prov-site-tokens prov-site-receiver prov-site-kind
  prov-call prov-call? prov-call-from prov-call-to prov-call-site
  provenance provenance? provenance-grounds provenance-calls
  make-provenance
  provenance-add-ground! provenance-add-call!
  ;; capture helpers
  callee-tokens
  effect-cap-receiver
  make-callee-site
  make-declared-site
  ;; effect-name access
  entry-effect-name
  ;; completeness
  provenance-missing
  provenance-complete?
  ;; rendering
  prov-site->string)

;; ── function identity ───────────────────────────────────────────────

;; The canonical fully-qualified identity for a call-graph key: a
;; component-qualified `(cons component name)` pair becomes
;; "component::name"; a bare same-file symbol stays its own name. This is the
;; projection `type-graph.rkt` already uses for `function` nodes.
(define (function-fq key)
  (cond
    [(and (pair? key) (string? (car key)))
     (format "~a::~a" (car key) (cdr key))]
    [(symbol? key) (symbol->string key)]
    [else
     (error 'function-fq
            "not a call-graph key (symbol or (cons string symbol)): ~s"
            key)]))

;; ── records ─────────────────────────────────────────────────────────

;; One ground of an effect edge.
;;   owner    — fq of the function whose body contains the classifying site
;;              (the method itself for a declared capability)
;;   effect   — the effect name symbol the site grounded
;;   site     — the classifying call expression, e.g. "requests.get"
;;              ("(declared)" for a declared/blessed capability)
;;   tokens   — the source tokens that grounded the classification (root and
;;              operation verb, e.g. ("requests" "get"))
;;   receiver — the receiver class name when receiver-type dataflow resolved
;;              one, else #f
;;   kind     — 'inferred (call-site classification), 'unresolved (the
;;              tracked `unknown` obligation), or 'declared (a capability
;;              asserted at the method definition by the import/config data)
(struct prov-site (owner effect site tokens receiver kind) #:transparent)

;; One call-graph edge: `from` calls `to` at classifying expression `site`.
(struct prov-call (from to site) #:transparent)

;; The provenance of one inference run.
;;   grounds — fq → (hash effect-symbol → (listof prov-site))
;;   calls   — fq → (listof prov-call)
(struct provenance (grounds calls) #:transparent)

(define (make-provenance)
  (provenance (make-hash) (make-hash)))

;; Record a ground, dropping duplicates (an expression can classify the same
;; call twice only through a duplicate walk, which is not evidence).
(define (provenance-add-ground! prov site)
  (define h (hash-ref! (provenance-grounds prov) (prov-site-owner site) make-hash))
  (hash-update! h (prov-site-effect site)
                (λ (paths) (if (member site paths) paths (cons site paths)))
                '())
  (void))

;; Record a call-graph edge, dropping duplicates and repeated identical sites.
(define (provenance-add-call! prov call)
  (hash-update! (provenance-calls prov) (prov-call-from call)
                (λ (calls) (if (member call calls) calls (cons call calls)))
                '())
  (void))

;; ── capture helpers ─────────────────────────────────────────────────

;; The source tokens a callee expression grounds an effect in: its root
;; (module / receiver / function name) and the operation verb, deduplicated.
;; `open()` grounds on ("open"); `requests.get` on ("requests" "get").
(define (callee-tokens callee)
  (define root (dotted-root callee))
  (define op (effect-op-of-callee callee))
  (remove-duplicates
   (for/list ([s (in-list (list root op))] #:when s)
     (symbol->string s))))

;; The receiver class of a typed capability (`db<QuerySet>.read`), or #f for a
;; coarse capability / a name-grounded classification. When a capability merges
;; ops on different receivers, no single receiver type is claimed.
(define (effect-cap-receiver cap)
  (define receivers
    (for*/list ([op (in-list (effect-cap-ops cap))]
                [parts (in-value (typed-op-parts op))]
                #:when parts)
      (second parts)))
  (cond
    [(null? receivers) #f]
    [(andmap (λ (r) (equal? r (first receivers))) receivers) (first receivers)]
    [else #f]))

;; A ground for a call-site classification. Receivers are only claimed when the
;; classifier's typed op names exactly one class. `effect` is an effect-name
;; symbol or a structured `effect-cap`, normalized here so the record keys on
;; the domain either way.
(define (make-callee-site owner effect callee #:kind [kind 'inferred])
  (prov-site owner
             (if (effect-cap? effect) (effect-cap-domain effect) effect)
             (callee-site-string callee)
             (callee-tokens callee)
             (and (effect-cap? effect) (effect-cap-receiver effect))
             kind))

;; A ground for a declared capability: the evidence is the DATA rule at the
;; method definition, not a call site, so the site names the definition and the
;; tokens are the module path, class, and method the capability was derived from.
(define (make-declared-site owner effect tokens receiver)
  (prov-site owner effect "(declared)" tokens receiver 'declared))

;; ── completeness: every effect edge has a ground ────────────────────

;; Effect → set of function fqs that can reach a ground of that effect along
;; reverse call edges (one reverse BFS per effect, so O(effects · edges)). A row
;; entry for effect E at `f` is proven exactly when `f` is in this set: the
;; fixpoint only adds E to `f` by a call edge to an E-carrying callee, so the
;; set is the closure of the ground owners.
(define (reachability prov)
  (define grounds (provenance-grounds prov))
  (define calls (provenance-calls prov))
  (define reverse (make-hash))
  (for ([(caller cs) (in-hash calls)])
    (for ([c (in-list cs)])
      (hash-update! reverse (prov-call-to c)
                    (λ (l) (if (member caller l) l (cons caller l))) '())))
  (define effects
    (remove-duplicates
     (for*/list ([(owner by-effect) (in-hash grounds)]
                 [effect (in-hash-keys by-effect)])
       effect)))
  (define out (make-hash))
  (for ([effect (in-list effects)])
    (define owners
      (for/list ([(owner by-effect) (in-hash grounds)]
                 #:when (hash-has-key? by-effect effect))
        owner))
    (define seen (make-hash))
    (for ([o (in-list owners)]) (hash-set! seen o #t))
    (define (walk f)
      (for ([caller (in-list (hash-ref reverse f '()))]
            #:unless (hash-ref seen caller #f))
        (hash-set! seen caller #t)
        (walk caller)))
    (for ([o (in-list owners)]) (walk o))
    (hash-set! out effect seen))
  out)

;; Every effect edge (function fq, effect name) in `rows` that the provenance
;; record does not ground, as a sorted list. A row key is a call-graph key
;; (symbol or `(cons component name)`); the row entries name the effects.
(define (provenance-missing rows prov)
  (define reachable (reachability prov))
  (define missing
    (for*/list ([(key row) (in-hash rows)]
                [entry (in-list (uir-effect-row-entries row))]
                [effect (in-value (entry-effect-name entry))]
                #:unless (hash-ref (hash-ref reachable effect (hash)) (function-fq key) #f))
      (list (function-fq key) effect)))
  (sort missing (λ (a b) (string<? (format "~a/~a" (car a) (cadr a))
                                   (format "~a/~a" (car b) (cadr b))))))

;; The effect-name symbol of a UIR effect entry, without requiring the contract
;; module (this record deliberately does not depend on the algebra). This is the
;; one definition; the graph projection reuses it so the two tables can never
;; disagree about what an entry's effect name is.
(define (entry-effect-name entry)
  (string->symbol (uir-symbol-name (uir-effect-entry-name entry))))

(define (provenance-complete? rows prov)
  (null? (provenance-missing rows prov)))

;; ── rendering ───────────────────────────────────────────────────────

(define (prov-site->string site)
  (format "~a [~a; tokens: ~a~a]"
          (prov-site-site site)
          (prov-site-effect site)
          (string-join (prov-site-tokens site) ",")
          (if (prov-site-receiver site)
              (format "; receiver: ~a" (prov-site-receiver site))
              "")))
