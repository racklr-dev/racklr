#lang racket

;; racklr/effects/rows.rkt
;;
;; The inference-side effect-row helpers: building rows from classified items,
;; canonical ordering, and the fixpoint union that preserves the deferred
;; `unknown` modality.
;;
;; The row algebra itself is `racklr/effects/algebra/` (Phase 1): a row is an
;; algebra row — the canonical UIR row plus first-class provenance — and the
;; fixpoint operation is `algebra/join`, the least upper bound over rows. This
;; module is the thin inference-facing adapter: classified items in, canonical
;; UIR rows / algebra rows out. Materializing an algebra row to a UIR row
;; preserves every entry field; only the provenance side table, which the UIR
;; row has no slot for, is dropped.

(require racklr/uir
         racklr/effects/contract
         racklr/effects/imports
         (only-in racklr/effects/algebra/main
                  join algebra-row-of algebra-row->uir))

(provide empty-row effects->row effects->algebra-row infer-row-union)

(define (empty-row)
  (uir-effect-row '()))

;; Canonicalize row entry order so equal? is order-insensitive.
(define (canonical-row r)
  (uir-effect-row
   (sort (uir-effect-row-entries r)
         (λ (a b) (symbol<? (effect-name-symbol a) (effect-name-symbol b))))))

;; A list of effect-name symbols and/or effect-caps → a row. 'unknown names get
;; modality 'unknown; everything else is a detected 'may. Caps are merged by
;; domain with their op sets unioned; a coarse entry (empty ops = "all ops")
;; dominates, so it is never narrowed by a precise one.
(define (effects->row items)
  (define (merge-ops a b)
    (cond [(null? a) '()] [(null? b) '()] [else (remove-duplicates (append a b))]))
  (define dom-ops (make-hash))
  (for ([it (in-list items)])
    (define-values (d o)
      (cond
        [(effect-cap? it) (values (effect-cap-domain it) (effect-cap-ops it))]
        [(eq? it 'unknown) (values 'unknown '())]
        [else (values it '())]))
    (hash-update! dom-ops d (λ (prev) (merge-ops prev o)) o))
  (canonical-row
   (uir-effect-row
    (for/list ([(d o) (in-hash dom-ops)])
      (uir-effect-entry
       (uir-symbol (symbol->string d))
       (if (eq? d 'unknown) 'unknown 'may)
       (map (λ (s) (uir-symbol (symbol->string s))) o)
       #f
       #f)))))

;; The same classification as an algebra row, ready for the fixpoint. No
;; provenance is attached here: inference records direct-call evidence in its
;; own provenance table (the `#:provenance-table` box), which is threaded per
;; call site rather than per row entry.
(define (effects->algebra-row items)
  (algebra-row-of (effects->row items)))

;; The fixpoint's accumulation step: join two rows, keeping the deferred
;; 'unknown modality. `join` is the algebra's least upper bound and is
;; idempotent/associative/commutative, so iterating it converges.
(define (infer-row-union r1 r2)
  (algebra-row->uir
   (join (algebra-row-of r1) (algebra-row-of r2))))
