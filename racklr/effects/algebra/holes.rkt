#lang racket

;; racklr/effects/algebra/holes.rkt
;;
;; Row holes: effect polymorphism for the provenance-carrying algebra.
;;
;; A hole is an algebra-row entry whose name is ?-prefixed (e.g. ?h0). The
;; prefix cannot collide with a concrete domain (llm/db/network/cache/io/
;; process/time/email), so a hole never joins with a real effect and never
;; reaches the contract checker or the serialized graph by accident.
;;
;; A hole stands for "the effects of a callback this function invokes":
;; inference creates one at a bare call to a parameter, and a caller
;; instantiates it with the row of the argument it passes. A hole therefore
;; carries provenance like any other entry — the call site that created it —
;; and instantiation attaches that evidence to the concrete effects it
;; introduces, so the grounding invariant (evidence over assertion) survives
;; polymorphism. Pass-through (a caller passing its own callback along) is
;; transitive instantiation of the same id.
;;
;; Obligation: a hole that survives to materialization is a tracked deferral,
;; exactly like `unknown`. Callers must check `row-holes-open?` before
;; `algebra-row->uir` is written back or handed to `row-subtype?`; the property
;; tests pin that instantiate-then-close eliminates every hole.

(require racklr/uir
         (only-in racklr/effects/contract effect-name-symbol)
         "row.rkt")

(provide
  hole-prefix
  hole-name?
  hole-entry
  hole-entry?
  hole-entry-id
  row-hole
  row-holes
  row-holes-open?
  instantiate-row-hole
  close-row-hole)

;; The one-character prefix that marks a hole name. Concrete domain names
;; (llm/db/network/cache/io/process/time/email) never start with it.
(define hole-prefix "?")

(define (hole-name? s)
  (and s (> (string-length s) 0) (char=? (string-ref s 0) (string-ref hole-prefix 0))))

(define (hole-symbol id)
  (string->symbol (format "~a~a" hole-prefix id)))

;; A single hole entry, modality `may` (the callback may perform effects),
;; no ops/budget/scope (those belong to the instantiated row).
(define (hole-entry id)
  (uir-effect-entry (uir-symbol (format "~a~a" hole-prefix id)) 'may '() #f #f))

(define (hole-entry? e)
  (and (uir-effect-entry? e)
       (hole-name? (uir-symbol-name (uir-effect-entry-name e)))))

(define (hole-entry-id e)
  (and (hole-entry? e)
       (string->symbol
        (substring (uir-symbol-name (uir-effect-entry-name e))
                   (string-length hole-prefix)))))

;; A row consisting of one hole, optionally grounded by `prov` (the bare-call
;; site that created it).
(define (row-hole id [prov #f])
  (define e (hole-entry id))
  (make-algebra-row
   (uir-effect-row (list e))
   (if prov (hash (effect-name-symbol e) (list prov)) (hash))))

;; The hole ids a row still carries, in entry order.
(define (row-holes r)
  (for/list ([e (in-list (uir-effect-row-entries (algebra-row->uir r)))]
             #:when (hole-entry? e))
    (hole-entry-id e)))

(define (row-holes-open? r)
  (pair? (row-holes r)))

;; Replace every hole named `id` with the entries of `concrete`, attaching the
;; hole's own provenance (the site that created it) to each effect `concrete`
;; introduces. Other holes and entries are untouched. Idempotent in `id`.
(define (instantiate-row-hole r id concrete)
  (define hole-key (hole-symbol id))
  (define matched? (memq id (row-holes r)))
  (define hole-provs (row-provenance r hole-key))
  (define remaining
    (for/list ([e (in-list (uir-effect-row-entries (algebra-row->uir r)))]
               #:unless (and (hole-entry? e) (eq? (hole-entry-id e) id)))
      e))
  (define entries
    (uir-effect-row
     (append remaining
             (if matched? (uir-effect-row-entries (algebra-row->uir concrete)) '()))))
  (define prov-table (hash-remove (algebra-row-provenance r) hole-key))
  (define prov-table*
    (if matched?
        (for/fold ([t prov-table]) ([(n p) (in-hash (algebra-row-provenance concrete))])
          (hash-set t n (append hole-provs p)))
        prov-table))
  (make-algebra-row entries prov-table*))

;; Close a hole: a handler (or an instantiation that resolves it to nothing)
;; has discharged it, so it leaves the row without introducing effects.
(define (close-row-hole r id)
  (make-algebra-row
   (uir-effect-row
    (for/list ([e (in-list (uir-effect-row-entries (algebra-row->uir r)))]
               #:unless (and (hole-entry? e) (eq? (hole-entry-id e) id)))
      e))
   (hash-remove (algebra-row-provenance r) (hole-symbol id))))
