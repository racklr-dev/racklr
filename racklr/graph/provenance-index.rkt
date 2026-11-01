#lang racket

;; racklr/graph/provenance-index.rkt
;;
;; The parsed, reusable view of the effect-edge provenance graph atoms (P4.2).
;;
;; Building the index is O(atoms); the provenance queries and the completeness
;; check accept either the atom list or a prebuilt index, so a consumer that
;; queries many edges (the type-graph projection, the transitive effect
;; profiles) builds it once instead of per query.
;;
;; Split out of racklr/graph/provenance.rkt for the 400-line modularity rule;
;; the provenance module re-exports this API.

(require racklr/hypergraph)

(provide
 graph-index graph-index? make-graph-index
 graph-index-calls graph-index-grounds graph-index-edges
 graph-index-site-exprs graph-index-site-tokens graph-index-site-receivers
 graph-index-site-kinds graph-index-site-coverages
 graph-index-site-files graph-index-site-lines graph-index-site-chains
 graph-index-origins
 ensure-index)

;; calls          fq → (list (list callee-fq site-id))
;; grounds        fq → (list (list site-id effect entry))
;; edges          fq → (list effect-entry)
;; site-exprs     site-id → expression
;; site-tokens    site-id → (list token), newest first
;; site-receivers site-id → receiver class name
;; site-kinds     site-id → classification kind
;; site-coverages site-id → coverage label
;; site-files     site-id → origin file component
;; site-lines     site-id → origin source line (when known)
;; site-chains    site-id → origin receiver chain, dotted
;; origins        (listof string): every distinct site origin id, sorted
(struct graph-index (calls grounds edges site-exprs site-tokens site-receivers
                           site-kinds site-coverages site-files site-lines
                           site-chains origins)
  #:transparent)

(define (out-name l i) (node-name (list-ref (link-out l) i)))

(define (make-graph-index atoms)
  (define calls (make-hash))
  (define grounds (make-hash))
  (define edges (make-hash))
  (define site-exprs (make-hash))
  (define site-tokens (make-hash))
  (define site-receivers (make-hash))
  (define site-kinds (make-hash))
  (define site-coverages (make-hash))
  (define site-files (make-hash))
  (define site-lines (make-hash))
  (define site-chains (make-hash))
  (define origins '())
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
          [(site-coverage)
           (hash-set! site-coverages (out-name a 0) (out-name a 1))]
          [(site-file)
           (hash-set! site-files (out-name a 0) (out-name a 1))]
          [(site-line)
           (hash-set! site-lines (out-name a 0) (out-name a 1))]
          [(site-chain)
           (hash-set! site-chains (out-name a 0) (out-name a 1))]
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
        (when (and (node? a) (eq? (node-type a) 'site))
          (set! origins (cons (node-name a) origins)))))
  (graph-index calls grounds edges site-exprs site-tokens site-receivers
               site-kinds site-coverages site-files site-lines site-chains
               (sort (remove-duplicates origins) string<?)))

;; Queries take atoms or an index; accept both and build the index once.
(define (ensure-index graph)
  (if (graph-index? graph) graph (make-graph-index graph)))
