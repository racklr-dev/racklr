#lang racket

;; racklr/effects/lexicons/python.rkt
;;
;; The Python/Django host lexicon as DATA: the default effect-classification
;; language. The token tables live in racklr/effects/library-tokens.rkt, the
;; pure name tables in library-pure-names.rkt, the dialect conventions in
;; library-dialect.rkt, and the import-tag rules + operation vocabulary in
;; racklr/effects/imports/library-config.rkt — this module only assembles them
;; into one injectable `lexicon` value. A different host assembles its own
;; (see racklr/effects/lexicons/javascript.rkt) without touching engine code.

(require racklr/effects/lexicon-types
         racklr/effects/library-tokens
         racklr/effects/imports/library-config)

(provide python-lexicon default-lexicon)

(define python-lexicon
  (lexicon
   'python
   default-effect-tokens
   effect-tag-config
   db-read-methods
   db-write-methods
   io-read-verbs
   io-write-verbs
   io-other-verbs
   domain-op-vocabulary
   builtin-class-methods))

;; Backward-compatible name for engine defaults: the Python/Django lexicon.
(define default-lexicon python-lexicon)
