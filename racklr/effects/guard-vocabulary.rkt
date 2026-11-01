#lang racket

;; racklr/effects/guard-vocabulary.rkt
;;
;; The scope-enforcement vocabulary for the emitted runtime guards.
;;
;; A guard must decide whether the operation it is about to perform is a read or
;; a write, because a `reads=...` scope forbids writes and a `writes=...` scope
;; forbids reads. That classification must use the SAME DATA verb tables the
;; classifier uses (`racklr/effects/imports/library-config.rkt`), so a verb the
;; classifier starts emitting is enforced by construction and the two cannot
;; drift. The target emitters render these lists into their preambles; they do
;; not carry a verb table of their own.

(require racket/set
         racklr/effects/imports/library-config)

(provide scope-read-verbs
         scope-write-verbs
         guard-modalities)

;; Deterministic (sorted) string list from a set, for stable emitted text.
(define (sorted-strings s)
  (sort (set->list s) string<?))

;; Verbs that read: db reads plus io reads. This is the classifier's read verb
;; vocabulary, not a hand-maintained list.
(define (scope-read-verbs)
  (sorted-strings (set-union db-read-methods io-read-verbs)))

;; Verbs that write: db writes plus io writes.
(define (scope-write-verbs)
  (sorted-strings (set-union db-write-methods io-write-verbs)))

;; The closed modality set the guards validate at construction and at runtime.
(define (guard-modalities)
  '("may" "must" "must-not"))

(module+ main
  (displayln "racklr/effects/guard-vocabulary — scope-enforcement vocabulary loaded."))
