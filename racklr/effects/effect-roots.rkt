#lang racket

;; racklr/effects/effect-roots.rkt
;;
;; The "direct effect" classifier: a generic engine that maps a callee to an
;; effect (llm / db / network / cache / io / process / time / email) using the
;; injected token tables, plus the pure-stdlib predicate. The tables
;; themselves live in the DATA modules (library-tokens.rkt and friends); this
;; module carries no library-specific names. Every entry point takes an
;; optional token value, which is a complete host lexicon: it carries both the
;; name tables and the host dialect (naming/encoding conventions).

(require racklr/uir
         racklr/effects/shape
         racklr/effects/library-tokens)

(provide
  direct-effect-of-callee
  pure-stdlib-callee?
  stdlib-type-method-effect)

;; Is `root` a builtin type or stdlib module name? Used to keep the name-only
;; method fallback from claiming dotted calls on non-library receivers
;; (`dict.get` must not classify as a database read).
(define (method-fallback-exempt-root? tokens root)
  (and root
       (or (set-member? (effect-tokens-builtin-type-roots tokens) (symbol->string root))
           (set-member? (effect-tokens-stdlib-module-roots tokens) (symbol->string root)))))

;; Effect of a whole call ROOT (the leftmost name of a dotted callee): the
;; domain whose effect-root table contains it. Root domains are tried in the
;; injected priority order (the strongest root wins).
(define (root-effect tokens root)
  (and root
       (for/or ([domain (in-list (effect-tokens-effect-root-priority tokens))])
         (and (set-member? (hash-ref (effect-tokens-effect-roots tokens) domain (set))
                           (symbol->string root))
              domain))))

;; Effect of a BARE callee name that is an effect on its own (`fetch(...)`,
;; `open(...)`, `sleep(...)`), from the injected name→domain table.
(define (bare-name-effect tokens nm)
  (and nm (hash-ref (effect-tokens-bare-name-effects tokens) (symbol->string nm) #f)))

;; Effect of a dotted METHOD name that is an effect regardless of receiver
;; (`obj.write(...)`, `obj.send_mail(...)`), from the injected name→domain
;; table. `log` is deliberately absent from the default table: it collides
;; with `math.log`; a host whose logger is unambiguous can list it.
(define (method-name-effect tokens op)
  (and op (hash-ref (effect-tokens-method-effects tokens) (symbol->string op) #f)))

;; Name-only method fallback: a method name that implies a domain even when the
;; receiver cannot be resolved. The unambiguous table always applies; the
;; ambiguous table (generic English verbs such as `get`/`update`/`save`) applies
;; only when the host enables it. Exempt roots keep builtin/stdlib receivers out
;; so `dict.get` is not misread as a database read.
(define (fallback-method-effect tokens c)
  (and (member-access-callee? c)
       (let* ([op (effect-op-of-callee c)]
              [root (dotted-root c)])
         (and op
              (not (method-fallback-exempt-root? tokens root))
              (or (hash-ref (effect-tokens-unambiguous-method-effects tokens)
                            (symbol->string op) #f)
                  (and (effect-tokens-ambiguous-method-effects-enabled? tokens)
                       (hash-ref (effect-tokens-ambiguous-method-effects tokens)
                                 (symbol->string op) #f)))))))

;; ── stdlib effect roots ─────────────────────────────────────────────
;;
;; Standard-library / framework calls that perform an effect but are not
;; covered by the primary effect roots: bare builtins with an effect, whole
;; library roots that are always an effect, dotted methods that are an effect
;; regardless of receiver, and root-qualified method tables
;; (`os.remove` → io, `sys.exit` → process). Runs after the stronger roots in
;; `direct-effect-of-callee` and after its name-only fallback, preserving the
;; order the default (Django) corpus was classified with.

;; Effect name for a stdlib/framework callee, or #f.
(define (stdlib-effect-of tokens c)
  (define root (dotted-root c))
  (define nm (callee-name c))
  (define op (effect-op-of-callee c))
  (cond
    [(bare-name-effect tokens nm) => values]
    [(hash-ref (effect-tokens-stdlib-root-effects tokens) root #f) => values]
    [(and (member-access-callee? c) (method-name-effect tokens op)) => values]
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
    [(root-effect tokens root) => values]
    [(bare-name-effect tokens nm) => values]
    [(fallback-method-effect tokens c) => values]
    [(stdlib-effect-of tokens c) => values]
    [else #f]))
