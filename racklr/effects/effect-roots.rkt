#lang racket

;; racklr/effects/effect-roots.rkt
;;
;; The "direct effect" classifier: a generic engine that maps a callee to an
;; effect (llm / db / network / cache / io / process / time / email) using the
;; injected token lexicon, plus the pure-stdlib predicate. The token tables
;; themselves live in library-tokens.rkt; this module carries no
;; library-specific names.

(require racklr/uir
         racklr/effects/shape
         racklr/effects/library-tokens)

(provide
  direct-effect-of-callee
  pure-stdlib-callee?
  stdlib-type-method-effect)

;; Is `root` a builtin type or stdlib module name? Used to keep the name-based
;; ORM fallback from claiming dotted calls on non-ORM receivers.
(define (non-orm-root? tokens root)
  (and root
       (or (set-member? (effect-tokens-builtin-type-roots tokens) (symbol->string root))
           (set-member? (effect-tokens-stdlib-module-roots tokens) (symbol->string root)))))

(define (orm-callee? tokens c)
  (and (member-access-callee? c)
       (let* ([op (effect-op-of-callee c)]
              [root (dotted-root c)])
         (and op
              (set-member? (effect-tokens-orm-methods tokens) (symbol->string op))
              (not (non-orm-root? tokens root))))))

;; ── stdlib effect roots ─────────────────────────────────────────────
;;
;; Standard-library / framework calls that perform an effect but are not
;; covered by the primary roots (llm/db/network/cache). These map to the
;; opaque effect names `io`, `process`, `time`, and `email` (plus `network`
;; for raw-socket/HTTP-client modules). Detection is root- or method-name
;; based, and runs after the stronger roots in `direct-effect-of-callee`.

;; Effect name for a stdlib/framework callee, or #f.
(define (stdlib-effect-of tokens c)
  (define root (dotted-root c))
  (define nm (callee-name c))
  (define op (effect-op-of-callee c))
  (cond
    [(and nm (set-member? (effect-tokens-io-builtins tokens) (symbol->string nm))) 'io]
    [(and nm (set-member? (effect-tokens-time-methods tokens) (symbol->string nm))) 'time]
    [(hash-ref (effect-tokens-stdlib-root-effects tokens) root #f) => (λ (e) e)]
    [(and op (member-access-callee? c)
          (set-member? (effect-tokens-io-methods tokens) (symbol->string op))) 'io]
    [(and op (member-access-callee? c)
          (set-member? (effect-tokens-email-methods tokens) (symbol->string op))) 'email]
    [(and op (member-access-callee? c)
          (set-member? (effect-tokens-time-methods tokens) (symbol->string op))) 'time]
    [(and op root
          (let ([m (hash-ref (effect-tokens-root-method-effects tokens) root #f)])
            (and m (hash-ref m (symbol->string op) #f))))]
    [else #f]))

;; ── pure stdlib roots ───────────────────────────────────────────────
;;
;; Common pure (side-effect-free) stdlib/framework dotted calls that would
;; otherwise fall to `unknown`. IMPORTANT: this check runs *after*
;; `direct-effect-of-callee`, so the effect roots win over whole-module purity.

(define (pure-stdlib-callee? c [tokens default-effect-tokens])
  (and (member-access-callee? c)
       (let ([root (dotted-root c)]
             [op (effect-op-of-callee c)])
         (and root op
              (or (set-member? (effect-tokens-pure-stdlib-roots tokens) (symbol->string root))
                  (let ([ms (hash-ref (effect-tokens-pure-stdlib-methods tokens) root #f)])
                    (and ms (set-member? ms (symbol->string op)))))))))

;; Effect (or 'pure) for a method on a stdlib/builtin receiver TYPE, from the
;; receiver-type method table (class -> method -> domain). Returns an effect
;; symbol, 'pure, or #f. This is the stdlib half of receiver-type dataflow:
;; `Path(...)` resolves to receiver type `Path`, and its `.attr` chain is
;; classified through THIS table rather than a bare method-name rule.
(define (stdlib-type-method-effect type-name mname [tokens default-effect-tokens])
  (and type-name mname
       (let ([m (hash-ref (effect-tokens-stdlib-type-methods tokens)
                          (symbol->string type-name) #f)])
         (and m (hash-ref m (symbol->string mname) #f)))))

;; Directly-performed effect for a call's callee, or #f.
(define (direct-effect-of-callee c [tokens default-effect-tokens])
  (define root (dotted-root c))
  (define nm (callee-name c))
  (cond
    [(and root (set-member? (effect-tokens-llm-roots tokens) (symbol->string root))) 'llm]
    [(and root (set-member? (effect-tokens-db-roots tokens) (symbol->string root))) 'db]
    [(and root (set-member? (effect-tokens-network-roots tokens) (symbol->string root))) 'network]
    [(and nm (set-member? (effect-tokens-network-names tokens) (symbol->string nm))) 'network]
    [(and root (set-member? (effect-tokens-cache-roots tokens) (symbol->string root))) 'cache]
    [(and nm (set-member? (effect-tokens-cache-names tokens) (symbol->string nm))) 'cache]
    [(orm-callee? tokens c) 'db]
    [(stdlib-effect-of tokens c) => (λ (e) e)]
    [else #f]))
