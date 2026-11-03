#lang racket

;; racklr/effects/receiver-predicates.rkt
;;
;; Receiver-NAME predicates read from the injected token tables: which names
;; count as a pure stdlib/builtin value type, which as an external module
;; receiver, and which as a tracked receiver type that is not a corpus class
;; definition. They are the shared vocabulary of receiver-type.rkt; split out so
;; that module keeps its per-function environment under the modularity
;; obligation. Every table is host DATA (`library-tokens.rkt` through the
;; lexicon), never a name list in this module.

(require racklr/uir
         racklr/effects/shape
         racklr/effects/library-tokens
         racklr/effects/lexicon)

(provide self-or-cls-var? pure-stdlib-type? module-receiver? tracked-receiver-type?)

(define (self-or-cls-var? v [tokens default-effect-tokens])
  (and (or (uir-var? v) (uir-symbol? v))
       (tokens-self-name? tokens (bare-name-symbol v))))

;; A pure stdlib/builtin value constructor name (`Path`, `md5`, `dict`): pure to
;; construct, and therefore also a usable receiver type for a call result.
(define (pure-stdlib-type? n [tokens default-effect-tokens])
  (and n
       (let ([s (symbol->string n)])
         (or (set-member? (effect-tokens-pure-stdlib-bare-names tokens) s)
             (set-member? (effect-tokens-builtin-type-roots tokens) s)))))

;; A receiver value is a MODULE (not a class) when its name names an external
;; module in the receiver-modules token table (pylibmc / pymemcache / memcache /
;; redis). Module receivers resolve their method calls to the module's effect
;; domain in classify-all, keeping engine code free of library names.
(define (module-receiver? nm [tokens default-effect-tokens])
  (and nm (hash-has-key? (effect-tokens-receiver-modules tokens)
                         (symbol->string nm))))

;; A receiver TYPE name that is not a corpus class definition but still a
;; tracked receiver: a stdlib type with a method table (`open`, `Path`), an
;; external module (pylibmc), or a module-path marker (`django.db`).
(define (tracked-receiver-type? nm [tokens default-effect-tokens])
  (define s (and nm (symbol->string nm)))
  (and s (or (hash-has-key? (effect-tokens-stdlib-type-methods tokens) s)
             (hash-has-key? (effect-tokens-receiver-modules tokens) s)
             (regexp-match? #rx"[.]" s))
       nm))
