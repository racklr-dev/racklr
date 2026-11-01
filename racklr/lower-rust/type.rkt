#lang racket

;; racklr/lower-rust/type.rkt — Rust type syntax → UIR type symbol.
;;
;; UIR has no type-expression node; every front end represents source types as
;; `uir-symbol` names (Python annotations, TypeScript type texts). Rust types
;; are rendered here as a normalized Rust-shaped string: path separators are
;; `::`, generics use `<...>`, references `&T`/`&mut T`, tuples `(A, B)`,
;; arrays `[T; N]`, and the unit type is `()`.

(require racklr/tree
         racklr/uir
         "helpers.rkt")

(provide lower-type)

;; Token types whose lexeme participates in a Rust type name.
(define type-word-tokens
  '(NON_KEYWORD_IDENTIFIER IDENTIFIER KW_SELFVALUE KW_SELFTYPE KW_SUPER KW_CRATE
    KW_DYN KW_IMPL KW_MUT KW_FN KW_UNSAFE KW_EXTERN KW_AS))

(define (glue token-type text)
  (case token-type
    [(PATHSEP) "::"]
    [(LT) "<"]
    [(GT) ">"]
    [(COMMA) ", "]
    [(LSQUAREBRACKET) "["]
    [(RSQUAREBRACKET) "]"]
    [(LPAREN) "("]
    [(RPAREN) ")"]
    [(AND) "&"]
    [(STAR) "*"]
    [(SEMI) "; "]
    [(QUESTION) "?"]
    [(NOT) "!"]
    [(DOTDOT) ".."]
    [(DOTDOTEQ) "..="]
    [else text]))

(define (lower-type node tk-type tk-value)
  (define src (if (eq? (any-tree-tag node) 'type_) node (or (find-descendant node 'type_) node)))
  (define tokens (deep-tokens src tk-type tk-value))
  (define words
    (for/list ([t (in-list tokens)]
               #:when (or (memq (tk-type t) type-word-tokens)
                          (memq (tk-type t) '(PATHSEP LT GT COMMA LSQUAREBRACKET
                                              RSQUAREBRACKET LPAREN RPAREN AND STAR
                                              SEMI QUESTION NOT DOTDOT DOTDOTEQ
                                              INTEGER_LITERAL FLOAT_LITERAL))))
      (glue (tk-type t) (tk-value t))))
  (uir-symbol (string-append* words)))
