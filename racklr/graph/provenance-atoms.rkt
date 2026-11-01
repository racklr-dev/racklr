#lang racket

;; racklr/graph/provenance-atoms.rkt
;;
;; The atom vocabulary of the effect-edge provenance graph (P4.2): typed node
;; and link constructors, the UIR effect-entry conversion, and the deterministic
;; atom order the projection sorts by.
;;
;; Split out of racklr/graph/provenance.rkt so that the projection, the queries,
;; and the completeness check each stay a separate, small responsibility (no
;; .rkt file over 400 lines). The provenance module re-exports everything here,
;; so consumers keep requiring racklr/graph/provenance.
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
;;   (node 'coverage c)      'universal | 'census-param | 'local-literal
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
;;   (site-coverage site coverage)           how far the claim generalizes
;;   (fn-effect function entry)              an effect edge (the row entry)
;;   (grounds function site entry)           direct evidence for that effect edge
;;
;; The kind/coverage pair is the edge's label: confidence maps from the
;; classification kind (racklr/effects/labels), coverage is recorded by the
;; inference run (racklr/effects/provenance). Keeping both as atoms is what lets
;; a transitive effect profile be a graph query (`racklr/graph/profile.rkt`)
;; rather than a side table.

(require racklr/uir
         racklr/hypergraph)

(provide
 ;; atom constructors (for consumers that extend the projection)
 function-node effect-node site-node token-node receiver-node kind-node
 coverage-node
 fn-effect-link grounds-link call-site-link site-expression-link
 site-effect-link site-token-link site-receiver-link site-kind-link
 site-coverage-link
 site-id
 ;; UIR effect entry → hypergraph effect-entry hyperedge
 entry->hg-entry hg-entry-effect
 ;; deterministic atom order
 atom-less?)

;; ── atom constructors ───────────────────────────────────────────────

(define (function-node fq) (node 'function fq))
(define (effect-node name) (node 'effect name))
(define (site-node id) (node 'site id))
(define (token-node token) (node 'token token))
(define (receiver-node receiver) (node 'receiver receiver))
(define (kind-node kind) (node 'kind kind))
(define (coverage-node coverage) (node 'coverage coverage))

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

(define (site-coverage-link site coverage)
  (link 'site-coverage (list (site-node site) (coverage-node coverage))))

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

;; ── deterministic atom order ────────────────────────────────────────

;; Sort on the printed struct (type + fields), not on Atomese rendering (two
;; node types can render to the same ConceptNode text).
(define (atom-less? a b)
  (string<? (format "~s" a) (format "~s" b)))
