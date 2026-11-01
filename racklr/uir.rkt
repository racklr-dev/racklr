#lang racket

;; racklr/uir.rkt — the UIR facade: node kinds, serialization, versioning.
;;
;; The UIR schema is data (uir/schema-data.rkt), the node kinds are generated
;; from it by the hand-written, versioned kernel (uir/kernel.rkt,
;; uir/schema-io.rkt, uir/schema-check.rkt, uir/schema-macro.rkt), and the
;; schema itself is readable as a UIR value. This module re-exports the public
;; surface and provides the two generic predicates over it.

(require "uir/types.rkt"
         racklr/tree
         "uir/serialize.rkt"
         "uir/version.rkt"
         "uir/kernel.rkt"
         "uir/schema-data.rkt"
         "uir/schema-io.rkt"
         "uir/schema-check.rkt")

(provide
 (all-from-out "uir/types.rkt")
 ;; Predicates over the schema-derived layouts.
 uir? uir-tag
 ;; Serialization
 (all-from-out "uir/serialize.rkt")
 ;; Schema versioning and migration
 (all-from-out "uir/version.rkt")
 ;; The hand-written kernel: layouts, bootstrap vocabulary, generic codecs,
 ;; schema validation and extension. UIR-SCHEMA is the schema value itself.
 (all-from-out "uir/kernel.rkt")
 (all-from-out "uir/schema-io.rkt")
 (all-from-out "uir/schema-check.rkt")
 (all-from-out "uir/schema-data.rkt"))

;; ── Predicates ─────────────────────────────────────────────────────

(define (uir? v [layouts UIR-LAYOUTS])
  (and (struct? v)
       (for/or ([l (in-list layouts)])
         ((uir-layout-pred l) v))
       #t))

(define (uir-tag v [layouts UIR-LAYOUTS])
  (define l (layout-for-value v layouts))
  (if l
      (uir-layout-tag l)
      (error 'uir-tag "not a UIR node: ~e" v)))

(module+ main
  (displayln "racklr/uir — Universal Intermediate Representation loaded."))
