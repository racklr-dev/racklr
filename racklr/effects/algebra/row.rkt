#lang racket

;; racklr/effects/algebra/row.rkt
;;
;; Effect rows as sets of (effect, provenance) pairs — the provenance-carrying
;; counterpart of `racklr/effects/contract.rkt`'s `uir-effect-row`.
;;
;; A row keeps two views of the same classification together:
;;
;;   entries    — the canonical UIR row (one entry per effect name, ordered by
;;                name, with modality / ops / budget / scope merged by the
;;                algebra's join). This is what inference writes back and the
;;                contract checker consumes.
;;   provenance — a table from effect name to the evidence records that
;;                grounded it (see `provenance.rkt`). The table is a set:
;;                duplicates are dropped and order is canonical, so rows
;;                compare with `equal?` and combinators stay associative and
;;                idempotent.
;;
;; The entry struct is not extended with a provenance slot on purpose: a
;; field-count change to `uir-effect-entry` would be a UIR schema change with a
;; migration (P0.1), and provenance is an inference-side fact, not part of the
;; serialized contract form. `same-row?` therefore compares both views; the
;; algebra's lattice order compares kind support only, through `join`.

(require racklr/uir
         (only-in racklr/effects/contract effect-name-symbol)
         "provenance.rkt")

(provide
  ;; row
  (struct-out algebra-row)
  make-algebra-row
  empty-algebra-row
  algebra-row-of
  algebra-row->uir
  row-add
  row-set-provenance
  row-provenance
  row-provenance-table
  row-effect-names
  row-lookup
  same-row?
  row->string)

;; entries: a canonical uir-effect-row (sorted by effect name).
;; provenance: immutable hash effect-name-symbol -> (listof provenance).
(struct algebra-row (entries provenance) #:transparent)

;; ── canonical form ──────────────────────────────────────────────────
;;
;; Two canonicalizations make a row a deterministic set representation:
;;
;;   - entries are ordered by effect name;
;;   - `must-not` entries are dropped: prohibition is encoded by absence
;;     (docs/effects-proposal.md §3.2), and a must-not entry is
;;     subtype-equivalent to an absent entry on either side of `≤`. The algebra
;;     row therefore carries exactly the effects that may, must, or defer —
;;     the full declared-row form (including explicit prohibitions) still lives
;;     in the UIR row.
;;
;; Operations keep the order in which they were accumulated. Operation order
;; is presentation, not semantics: the UIR row that inference materializes
;; preserves the order the old fixpoint produced (so the type-graph baseline
;; does not move), while `same-row?` compares operations as sets.

(define (sorted-ops ops)
  (sort (remove-duplicates ops)
        (λ (a b) (string<? (uir-symbol-name a) (uir-symbol-name b)))))

(define (entry->meaning e)
  ;; A comparison view of one entry: operations as a canonical set, every
  ;; other field as-is. Used only for equality checks.
  (list (effect-name-symbol e)
        (uir-effect-entry-modality e)
        (sorted-ops (uir-effect-entry-ops e))
        (uir-effect-entry-budget e)
        (uir-effect-entry-scope e)))

(define (canonical-entries row)
  (define entries (uir-effect-row-entries row))
  (define banned? (λ (e) (eq? (uir-effect-entry-modality e) 'must-not)))
  (uir-effect-row
   (sort (if (for/or ([e (in-list entries)]) (banned? e))
             (filter (λ (e) (not (banned? e))) entries)
             entries)
         (λ (a b) (symbol<? (effect-name-symbol a) (effect-name-symbol b))))))

(define (canonical-provenance-list provs)
  (provenance-sort (remove-duplicates provs)))

(define (canonical-provenance-table table)
  (make-immutable-hash
   (for/list ([(name provs) (in-hash table)]
              #:unless (null? provs))
     (cons name (canonical-provenance-list provs)))))

;; Build a canonical row: sorted entries, table de-duplicated and sorted.
(define (make-algebra-row entries provenance)
  (algebra-row (canonical-entries entries)
               (canonical-provenance-table provenance)))

(define (empty-algebra-row)
  (make-algebra-row (uir-effect-row '()) (hash)))

;; A plain UIR row as an algebra row with no provenance recorded yet.
(define (algebra-row-of row)
  (make-algebra-row row (hash)))

;; The UIR row an algebra row materializes to: the entries view, canonical.
(define (algebra-row->uir r)
  (algebra-row-entries r))

;; ── evidence ────────────────────────────────────────────────────────

;; Add (or replace) one entry under `name`, recording `prov` as its evidence.
;; The entry's own name is trusted to be `name`; callers pass the pair
;; together so a row can never hold an entry with unattributed evidence.
(define (row-add r name entry prov)
  (define entries
    (uir-effect-row
     (cons entry
           (filter (λ (e) (not (eq? (effect-name-symbol e) name)))
                   (uir-effect-row-entries (algebra-row-entries r))))))
  (make-algebra-row entries
                    (hash-set (algebra-row-provenance r)
                              name
                              (cons prov (row-provenance r name)))))

;; Attach additional evidence to an effect name already present in the row.
(define (row-set-provenance r name provs)
  (make-algebra-row (algebra-row-entries r)
                    (hash-set (algebra-row-provenance r)
                              name
                              (append provs (row-provenance r name)))))

;; The evidence recorded for one effect name, canonical (possibly empty).
(define (row-provenance r name)
  (hash-ref (algebra-row-provenance r) name '()))

;; The whole evidence table, canonical.
(define (row-provenance-table r)
  (algebra-row-provenance r))

;; Effect names carried by the row, in canonical entry order.
(define (row-effect-names r)
  (for/list ([e (in-list (uir-effect-row-entries (algebra-row-entries r)))])
    (effect-name-symbol e)))

;; The canonical entry for `name`, or #f when the row does not carry it.
(define (row-lookup r name)
  (for/or ([e (in-list (uir-effect-row-entries (algebra-row-entries r)))]
           #:when (eq? (effect-name-symbol e) name))
    e))

;; Two rows are the same when they carry the same effects, evidence and slot
;; values; operation order is presentation, so it is compared as a set. The
;; `equal?` fast path keeps the fixpoint's per-node convergence checks cheap
;; for unchanged rows (canonical construction makes them structurally equal).
(define (same-row? a b)
  (or (equal? a b)
      (and (equal? (for/list ([e (in-list (uir-effect-row-entries (algebra-row->uir a)))])
                     (entry->meaning e))
                   (for/list ([e (in-list (uir-effect-row-entries (algebra-row->uir b)))])
                     (entry->meaning e)))
           (equal? (canonical-provenance-table (algebra-row-provenance a))
                   (canonical-provenance-table (algebra-row-provenance b))))))

;; Render a row for diagnostics: "<db [django/db/models...<QuerySet>] | ...>".
(define (row->string r)
  (format "<~a>"
          (string-join
           (for/list ([e (in-list (uir-effect-row-entries (algebra-row->uir r)))])
             (define name (effect-name-symbol e))
             (define provs (row-provenance r name))
             (format "~a [~a]"
                     name
                     (if (null? provs)
                         "no provenance"
                         (string-join (map provenance->string provs) "; "))))
           " | ")))
