#lang racket

;; racklr/effects/algebra/provenance.rkt
;;
;; Provenance is a first-class field of every effect-row entry: the evidence
;; that grounds a classification (docs/effects-proposal.md §6.1). It records
;; the triple that produced an entry:
;;
;;   tokens   — the source tokens that grounded the classification: the module
;;              root / import handle and the method or attribute names that
;;              matched (e.g. ("django/db/models" "objects" "filter")). The
;;              inferrer's per-call-site provenance uses a single rendered site
;;              string here, so a finding can always name a concrete site.
;;   receiver — the resolved receiver class of the call, when the receiver-type
;;              dataflow named one (e.g. 'QuerySet); #f for a bare/global call.
;;   chain    — the call chain from the row's owner down to the classified
;;              site, outermost caller first (e.g. ("view" "get_queryset")).
;;              Empty means the site is directly in the row's owner.
;;
;; An entry without evidence is not a classification: `unknown` entries carry
;; provenance too, because the obligation to eliminate them needs to name where
;; they came from.

(provide
  (struct-out provenance)
  provenance-extend-chain
  provenance=?
  provenance-sort
  provenance->string)

;; A piece of evidence behind one classification.
;;   tokens:   (listof string) — lexicon tokens, or one rendered call site.
;;   receiver: (or/c symbol #f) — resolved receiver class, when named.
;;   chain:    (listof string) — caller→callee names, outermost first.
(struct provenance (tokens receiver chain) #:transparent)

;; Extend a callee's provenance with the call-graph step that carried the entry
;; upward: prepend the caller->callee edge name. When f inherits g's row, g's
;; provenance is extended with "g", so a chain reads owner -> ... -> site.
(define (provenance-extend-chain p step)
  (provenance (provenance-tokens p)
              (provenance-receiver p)
              (cons step (provenance-chain p))))

;; Structural equality is `equal?` on the transparent struct; this alias makes
;; the intent explicit at call sites and gives property tests a name.
(define (provenance=? a b)
  (equal? a b))

;; Canonical total order on provenance, so a row's evidence set has a
;; deterministic serialization and `equal?` is order-insensitive.
(define (provenance-sort provs)
  (sort provs
        (λ (a b) (string<? (format "~s" a) (format "~s" b)))))

;; One-line rendering of the evidence, for diagnostics and error reports:
;; "django/db/models.objects.filter<QuerySet> via view -> get_queryset".
(define (provenance->string p)
  (define token-string
    (if (null? (provenance-tokens p))
        "?"
        (string-join (provenance-tokens p) ".")))
  (define receiver-string
    (if (provenance-receiver p)
        (format "<~a>" (provenance-receiver p))
        ""))
  (define chain-string
    (if (null? (provenance-chain p))
        ""
        (format " via ~a" (string-join (reverse (provenance-chain p)) " -> "))))
  (format "~a~a~a" token-string receiver-string chain-string))
