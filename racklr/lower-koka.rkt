#lang racket

;; Koka 3.x source-language lowering: Koka.g4 CST → UIR.
;;
;; Public call shape (unchanged): (lower-koka cst tk-type tk-value) → uir?
;; The parser comes from the tracked grammar (grammars/koka/Koka.g4) through
;; racklr/gen-test's layout-aware loader; the CST conventions are documented in
;; lower-koka/helpers.rkt.
;;
;; The pass is split by responsibility (all files < 400 lines):
;;   helpers.rkt      CST navigation, token text rendering, decoration
;;   type.rkt         type/kind text, result-type + effect-row split
;;   pattern.rkt      patterns and literals
;;   params.rkt       parameters / pattern-parameters / operation parameters
;;   expr.rkt         expressions, applications, operators, if/match, atoms
;;   fun.rkt          function bodies and lambdas
;;   handler.rkt      effect handlers, `with` statements, masks
;;   stmt.rkt         blocks, statements, local declarations
;;   type-decl.rkt    type/struct/effect/alias declarations
;;   extern-decl.rkt  extern declarations and extern imports
;;   decl.rkt         program structure: module, imports, top-level declarations
;;
;; Mapping and loss model: docs/plans/koka-pipeline.md (types travel as
;; normalized text; comments/docs are a declared loss; modifiers and type
;; parameter binders travel on uir-decorated; every construct lowers to a real
;; UIR node — there is no raw-source fallback).

(require racklr/tree
         racklr/uir
         "lower-koka/helpers.rkt"
         "lower-koka/type.rkt"
         "lower-koka/pattern.rkt"
         "lower-koka/params.rkt"
         "lower-koka/hooks.rkt"
         "lower-koka/expr.rkt"
         "lower-koka/fun.rkt"
         "lower-koka/handler.rkt"
         "lower-koka/stmt.rkt"
         "lower-koka/decl.rkt")

(provide lower-koka)

(define (lower-koka cst tk-type tk-value)
  (set-token-accessors! tk-type tk-value)
  (install-expr-hooks!)
  (install-fun-hooks!)
  (install-handler-hooks!)
  (install-stmt-hooks!)
  (lower-file cst))

(module+ main
  (displayln "racklr/lower-koka — Koka CST → UIR lowering loaded."))
