#lang racket

;; racklr/effects/lexicon.rkt
;;
;; The lexicon facade: injectable host-language data for the effect classifier.
;;
;; A `lexicon` (shape in lexicon-types.rkt) bundles everything the engine needs
;; to classify a host language's calls:
;;   tokens           — effect-tokens: the token tables PLUS the host dialect
;;   import-config    — module-path → effect-domain rules for imported libraries
;;   operation tables — db read/write verbs, io read/write/other verbs
;;   builtin-class-methods — builtin receiver class → pure method names
;;
;; The default is the Python/Django lexicon (lexicons/python.rkt), assembled
;; from the DATA modules (library-tokens.rkt, library-pure-names.rkt,
;; library-dialect.rkt, imports/library-config.rkt). Supplying a different
;; lexicon to a classification entry point (infer-effects, infer-effects-all,
;; classify-call, classify-call-all) swaps the host language with no engine
;; edits; lexicons/javascript.rkt is the non-Python proof.
;;
;; Token-only call sites keep working through the optional
;; `[tokens default-effect-tokens]` shape: the dialect travels inside the token
;; tables, so a token value is already a complete classification lexicon.

(require racklr/effects/lexicon-types
         racklr/effects/library-tokens
         racklr/effects/lexicons/python)

(provide
  ;; the shape (a front end builds its lexicon from these)
  (all-from-out racklr/effects/lexicon-types)
  ;; the token tables + dialect struct and its default instance
  (all-from-out racklr/effects/library-tokens)
  ;; the host dialects
  python-lexicon default-lexicon
  lexicon-dialect
  ;; dialect lookups on a token value, for the token-only call sites
  tokens-dialect tokens-self-names tokens-self-name? tokens-class-receiver-name?
  tokens-exception-suffixes
  tokens-synthetic-callees tokens-comprehension-callees
  tokens-wrapper-callees tokens-runtime-primitives tokens-binding-callees
  tokens-method-receiver-offset tokens-init-method-name
  tokens-keyword-arg-callee tokens-annotation-callee
  tokens-literal-receiver-type
  tokens-control-transfer-names tokens-short-circuit-callees)

;; The dialect travels inside the token tables (effect-tokens), so every
;; existing optional `[tokens default-effect-tokens]` parameter is a complete
;; classification-lexicon injection point, not just a name-table swap.
(define (lexicon-dialect lex)
  (effect-tokens-dialect (lexicon-tokens lex)))

;; Token-value convenience accessors. Engine code reads conventions through
;; these instead of naming a protocol; a host lexicon supplies its own values.
(define (tokens-dialect tokens) (effect-tokens-dialect tokens))
(define (tokens-self-names tokens)
  (dialect-self-names (effect-tokens-dialect tokens)))
;; Is `name` (a symbol) one of the host's implicit receiver names?
(define (tokens-self-name? tokens name)
  (and name
       (set-member? (tokens-self-names tokens) (symbol->string name))))
;; Is `name` (a symbol) the host's implicit CLASS receiver name (`cls`)?
(define (tokens-class-receiver-name? tokens name)
  (and name
       (set-member? (dialect-class-receiver-names (tokens-dialect tokens))
                    (symbol->string name))))
(define (tokens-exception-suffixes tokens)
  (dialect-exception-suffixes (effect-tokens-dialect tokens)))
(define (tokens-synthetic-callees tokens)
  (dialect-synthetic-callees (effect-tokens-dialect tokens)))
(define (tokens-comprehension-callees tokens)
  (dialect-comprehension-callees (effect-tokens-dialect tokens)))
(define (tokens-control-transfer-names tokens)
  (dialect-control-transfer-names (effect-tokens-dialect tokens)))
(define (tokens-short-circuit-callees tokens)
  (dialect-short-circuit-callees (effect-tokens-dialect tokens)))
(define (tokens-wrapper-callees tokens)
  (dialect-wrapper-callees (effect-tokens-dialect tokens)))
(define (tokens-runtime-primitives tokens)
  (dialect-runtime-primitives (effect-tokens-dialect tokens)))
(define (tokens-binding-callees tokens)
  (dialect-binding-callees (effect-tokens-dialect tokens)))
(define (tokens-method-receiver-offset tokens)
  (dialect-method-receiver-offset (effect-tokens-dialect tokens)))
(define (tokens-init-method-name tokens)
  (dialect-init-method-name (effect-tokens-dialect tokens)))
(define (tokens-keyword-arg-callee tokens)
  (dialect-keyword-arg-callee (effect-tokens-dialect tokens)))
(define (tokens-annotation-callee tokens)
  (dialect-annotation-callee (effect-tokens-dialect tokens)))

;; Receiver TYPE name (string) a literal VALUE of `kind` has in this host, or
;; #f. The mapping is dialect DATA (a literal's receiver type is a host
;; convention), so the engine never carries the type-name literals.
(define (tokens-literal-receiver-type tokens kind)
  (hash-ref (dialect-literal-receiver-types (tokens-dialect tokens)) kind #f))
