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
         "row.rkt"
         (only-in "combinators.rkt" join))

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
  close-row-hole
  row-holes->unknown)

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
  ;; The row with the hole removed.
  (define remaining-row
    (make-algebra-row
     (uir-effect-row
      (for/list ([e (in-list (uir-effect-row-entries (algebra-row->uir r)))]
                 #:unless (and (hole-entry? e) (eq? (hole-entry-id e) id)))
        e))
     (hash-remove (algebra-row-provenance r) hole-key)))
  (if (not matched?)
      remaining-row
      ;; Join concrete into the remaining row (join merges same-name entries,
      ;; so a row that already carries `unknown` never gains a second one), with
      ;; the hole's own evidence attached to every effect concrete introduces.
      (join
       remaining-row
       (make-algebra-row
        (algebra-row->uir concrete)
        (for/hash ([(n p) (in-hash (algebra-row-provenance concrete))])
          (values n (append hole-provs p)))))))

;; Close a hole: a handler (or an instantiation that resolves it to nothing)
;; has discharged it, so it leaves the row without introducing effects.
(define (close-row-hole r id)
  (make-algebra-row
   (uir-effect-row
    (for/list ([e (in-list (uir-effect-row-entries (algebra-row->uir r)))]
               #:unless (and (hole-entry? e) (eq? (hole-entry-id e) id)))
      e))
   (hash-remove (algebra-row-provenance r) (hole-symbol id))))

;; Materialize a row for the graph/contract checker: every open hole becomes a
;; tracked `unknown` entry (the honest deferral), and the holes' own evidence
;; is carried under `unknown` so the grounding invariant holds at the boundary.
;; Instantiated/closed holes are already gone. This is the only hole->concrete
;; transition that may run; inference must call it before write-back.
(define (row-holes->unknown r)
  (define entries (uir-effect-row-entries (algebra-row->uir r)))
  (define hole-entries (filter hole-entry? entries))
  (define concrete (filter (λ (e) (not (hole-entry? e))) entries))
  (define hole-prov
    (append-map (λ (e) (row-provenance r (effect-name-symbol e))) hole-entries))
  (define has-unknown?
    (for/or ([e (in-list concrete)]) (eq? (effect-name-symbol e) 'unknown)))
  (define entries*
    (if (and (pair? hole-entries) (not has-unknown?))
        (cons (uir-effect-entry (uir-symbol "unknown") 'unknown '() #f #f) concrete)
        concrete))
  (define prov-table
    (for/fold ([t (algebra-row-provenance r)]) ([e (in-list hole-entries)])
      (hash-remove t (effect-name-symbol e))))
  (define prov-table*
    (if (pair? hole-prov)
        (hash-update prov-table 'unknown (λ (l) (append hole-prov l)) '())
        prov-table))
  (make-algebra-row (uir-effect-row entries*) prov-table*))
