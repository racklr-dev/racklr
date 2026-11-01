#lang racket

;; racklr/effects/algebra/main.rkt
;;
;; Public facade of the effect algebra: first-class provenance, rows as sets of
;; (effect, provenance) pairs, the four combinators, the generic monotone
;; fixpoint driver, and row subtyping / handler interpretation.
;;
;; This is the standalone algebra Phase 1 builds on. It is distinct from
;; inference (`racklr/effects/infer.rkt`), which is expressed in terms of these
;; types and operations rather than the other way round; and it depends on the
;; UIR row vocabulary only through the canonical entry struct, never on a
;; language or library.

(require "provenance.rkt"
         "row.rkt"
         "combinators.rkt"
         "fixpoint.rkt"
         "subtyping.rkt")

(provide (all-from-out "provenance.rkt"
                       "row.rkt"
                       "combinators.rkt"
                       "fixpoint.rkt"
                       "subtyping.rkt"))
