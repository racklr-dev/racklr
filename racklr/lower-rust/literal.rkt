#lang racket

;; racklr/lower-rust/literal.rkt — Rust literal tokens → UIR literal nodes.
;;
;; uir-number keeps the raw lexeme string (so precision survives);
;; uir-string keeps the unescaped content (one pair of string/char quotes is
;; stripped). No cross-language escaping is attempted: the emitter owns that.

(require racklr/uir
         "helpers.rkt")

(provide literal-uir)

(define (literal-uir token tk-type tk-value)
  (case (tk-type token)
    [(INTEGER_LITERAL FLOAT_LITERAL) (uir-number (tk-value token))]
    [(STRING_LITERAL RAW_STRING_LITERAL BYTE_STRING_LITERAL
      RAW_BYTE_STRING_LITERAL CHAR_LITERAL) (uir-string (strip-quotes (tk-value token)))]
    [(KW_TRUE) (uir-bool #t)]
    [(KW_FALSE) (uir-bool #f)]
    [else (uir-symbol (tk-value token))]))
