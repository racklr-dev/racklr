#lang racket

;; Property/law tests for row holes (effect polymorphism) in the algebra.
;; Pins: the ?-prefix cannot collide with a concrete domain; a hole is a
;; first-class entry that joins with itself and never with a real effect;
;; instantiation is idempotent, selective by id, and grounds the concrete
;; effects with the hole's own provenance; close removes without introducing;
;; and instantiate-then-close eliminates every hole (the materialization
;; obligation that `row-holes-open?` reports).

(require rackunit
         racket/set
         racklr/uir
         racklr/effects/algebra/main)

(define effect-names '(db io network process cache llm time email))
(define (ent name modality #:ops [ops '()] #:budget [budget #f])
  (uir-effect-entry (uir-symbol (if (symbol? name) (symbol->string name) name))
                    modality (map uir-symbol ops) budget #f))
(define (row-of . entries)
  (for/fold ([r (empty-algebra-row)]) ([e (in-list entries)])
    (row-add r (string->symbol (uir-symbol-name (uir-effect-entry-name e)))
             e (provenance '("site") 'Fn '("call")))))
(define (mkdb)
  (row-of (ent "db" 'may #:ops '("read"))))
(define (mkio)
  (row-of (ent "io" 'may #:ops '("write"))))

;; ── identity and non-collision ──────────────────────────────────────
(define h0 (row-hole 'h0 (provenance '("views.py" "dispatch") 'View '("get"))))
(check-true (algebra-row? h0))
(check-equal? (row-holes h0) '(h0))
(check-true (row-holes-open? h0))
(check-false (row-holes-open? (empty-algebra-row)))
(check-true (hole-entry? (hole-entry 'k)))
(check-equal? (hole-entry-id (hole-entry 'k)) 'k)
(check-false (hole-entry? (ent "db" 'may)))
;; A hole name can never be one of the concrete domains, so it can never join
;; with a real effect.
(for ([name (in-list effect-names)])
  (check-false (hole-entry? (ent (symbol->string name) 'may))
               (format "concrete domain ~a must not read as a hole" name)))
(check-true (hole-name?
             (string-append hole-prefix "x")))

;; ── join keeps holes, and holes only join with holes ────────────────
(check-equal? (row-holes (join h0 (mkdb))) '(h0))
(check-equal? (row-holes (join h0 (row-hole 'h0))) '(h0))
;; two different holes coexist
(define h1 (row-hole 'h1))
(check-equal? (sort (row-holes (join h0 h1)) symbol<?) '(h0 h1))

;; ── instantiation ───────────────────────────────────────────────────
(define r-with-hole (join (mkio) h0))
(define inst (instantiate-row-hole r-with-hole 'h0 (mkdb)))
(check-false (row-holes-open? inst))
(check-equal? (sort (row-effect-names inst) symbol<?) '(db io))
(check-not-false (row-lookup inst 'db))
;; Idempotent in id: instantiating again adds nothing (the hole is gone).
(check-true (same-row? inst (instantiate-row-hole inst 'h0 (mkdb))))
;; Selective: instantiating h0 leaves h1 alone.
(define two (join (join (mkio) h0) h1))
(define inst-two (instantiate-row-hole two 'h0 (mkdb)))
(check-equal? (row-holes inst-two) '(h1))
(check-not-false (row-lookup inst-two 'db))

;; ── grounding: the hole's provenance reaches the concrete effects ────
(define site-provenance (provenance '("views.py" "dispatch") 'View '("get")))
(define hg (row-hole 'g site-provenance))
(define grounded (instantiate-row-hole hg 'g (mkdb)))
(define db-provs (row-provenance grounded 'db))
(check-true (pair? db-provs))
(check-true (for/or ([p (in-list db-provs)])
              (provenance=? p site-provenance))
            "the hole's evidence must ground every effect it introduces")

;; ── close ───────────────────────────────────────────────────────────
(define closed (close-row-hole r-with-hole 'h0))
(check-false (row-holes-open? closed))
(check-equal? (row-effect-names closed) '(io))
(check-false (row-lookup closed 'db))
;; close is idempotent too
(check-true (same-row? closed (close-row-hole closed 'h0)))

;; ── materialization obligation: every hole can be discharged ────────
(define (discharge r)
  (for/fold ([r r]) ([id (in-list (row-holes r))])
    (close-row-hole r id)))
(check-false (row-holes-open? (discharge (join (join h0 h1) (mkdb)))))

;; ── materialization: open holes become tracked unknown ─────────────
(define open-row (join (join (mkdb) h0) h1))
(define mat (row-holes->unknown open-row))
(check-false (row-holes-open? mat))
(check-not-false (row-lookup mat 'unknown))
(check-not-false (row-lookup mat 'db))
(check-true (pair? (row-provenance mat 'unknown))
            "open-hole evidence is carried under the tracked unknown")
(check-true (same-row? (mkdb) (row-holes->unknown (mkdb)))
            "a hole-free row materializes unchanged")
;; Instantiation merges by name: a row already carrying `unknown` gains no
;; second unknown entry from the substitution.
(define unk-row (row-of (ent "unknown" 'unknown)))
(define merged (instantiate-row-hole (join h0 unk-row) 'h0 unk-row))
(check-equal? (count (λ (e) (eq? (string->symbol (uir-symbol-name (uir-effect-entry-name e))) 'unknown))
                     (uir-effect-row-entries (algebra-row->uir merged)))
              1)

;; ── materialization: open holes can also become tracked dynamic ───
(define dmat (row-holes->dynamic open-row))
(check-false (row-holes-open? dmat))
(check-not-false (row-lookup dmat 'dynamic))
(check-false (row-lookup dmat 'unknown)
             "dynamic materialization must not also leave an unknown entry")
(check-not-false (row-lookup dmat 'db))
(check-true (pair? (row-provenance dmat 'dynamic))
            "open-hole evidence is carried under the tracked dynamic")
(check-equal? (uir-effect-entry-modality (row-lookup dmat 'dynamic)) 'may
              "dynamic is a may-deferral, not an unknown obligation")
(check-true (same-row? (mkdb) (row-holes->dynamic (mkdb)))
            "a hole-free row materializes unchanged under dynamic too")

(displayln "effects-algebra-holes-test: all laws hold")
