#lang racket

;; racklr/frontends/rust.rkt — the Rust front-end spec (DATA).
;;
;; A front end is a grammar plus a lowering hook plus a token lexicon. This
;; module is the whole registration: the generic registry in
;; racklr/frontends.rkt discovers it by listing racklr/frontends/, so adding
;; Rust edited no core module and added no entry to a dispatch table.
;;
;;   grammar → grammars/rust/RustParser.g4 (+ RustLexer.g4 via tokenVocab)
;;   lower   → racklr/lower-rust  (lower-rust cst tk-type tk-value) → uir?
;;   lexicon → racklr/frontends/rust/lexicon (rust-effect-tokens)

(require (rename-in racklr/frontends [front-end make-front-end])
         racklr/lower-rust
         "rust/lexicon.rkt")

(provide front-end)

(define front-end
  (make-front-end 'rust
                  "grammars/rust/RustParser.g4"
                  lower-rust
                  rust-effect-tokens))
