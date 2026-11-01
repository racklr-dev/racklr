#lang racket

;; racklr/lower-rust.rkt — Rust front end: CST → UIR lowering hook.
;;
;; Same call shape as every existing lowerer:
;;   (lower-rust cst tk-type tk-value) → uir?
;; The parser comes from the vendored grammar in grammars/rust/ (see
;; racklr/frontends/rust.rkt); this module is the "lowering hook" half of the
;; front end and is pure addition — no core module is edited to add Rust.
;;
;; Mapping summary:
;;   crate            → uir-block of items
;;   fn               → uir-fn (typed params, return type, effects #f)
;;   struct / tuple   → uir-class (fields as uir-field)
;;   enum             → uir-enum / uir-enum-variant
;;   impl             → methods folded into the target uir-class (second pass)
;;   trait            → uir-interface
;;   use              → uir-import
;;   mod              → uir-module
;;   statements/exprs → uir-block / uir-set! / uir-ann-set! / uir-if /
;;                      uir-while / uir-for-each / uir-match / uir-call / ...
;;
;; Loss model: generics/bounds/lifetimes/visibility/attributes are dropped;
;; qualified paths keep the whole "a::b::c" in one symbol; struct-literal
;; field names are dropped (values stay in written order — UIR has no typed
;; struct-literal node); macro arguments lower from literal tokens only;
;; effects stay #f for the inference pass.

(require racklr/tree
         racklr/uir
         "lower-rust/helpers.rkt"
         "lower-rust/item.rkt"
         "lower-rust/stmt.rkt")

(provide lower-rust)

;; Top-level `item` nodes live in the crate's repetition lists (attributes
;; list and item list).
(define (crate-items crate)
  (for/fold ([items '()]) ([list-child (any-tree-children crate)]
                           #:when (pair? list-child))
    (append items (filter (λ (c) (and (cst-node? c) (eq? (node-tag c) 'item)))
                          list-child))))

(define (lower-rust cst tk-type tk-value)
  (case (node-tag cst)
    [(crate) (uir-block (lower-rust-items (crate-items cst) tk-type tk-value))]
    [else (lower-rust-item cst tk-type tk-value)]))
