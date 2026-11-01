#lang racket

;; racklr/effects/classify.rkt
;;
;; Callee classification entry points. `classify-call` resolves intra-program
;; (same-file) method edges first and defers to `classify-nonedge` (the
;; name-heuristic tail shared with whole-program inference) otherwise.
;;
;; Both take an optional lexicon: the class-level dispatch reads the host
;; dialect's implicit-receiver names from it, and the heuristic tail forwards
;; its token tables to the pure / effect predicates. The default is the
;; Python/Django lexicon, so existing single-argument calls are unchanged.

(require racklr/uir
         racklr/effects/shape
         racklr/effects/collect
         racklr/effects/effect-roots
         racklr/effects/pure-roots
         racklr/effects/lexicon)

(provide classify-call classify-nonedge)

;; Class-qualified method-name symbols (Class.method) for a method call
;; `base.method`, when `base` is a host implicit receiver (`self` / `cls` in
;; the default Python dialect, read from the injected lexicon) or a known class
;; name (expanded through its own chain). Returns the full ordered list of
;; candidate names (most derived first) so inherited methods resolve; the
;; empty list means the call is unresolvable (an arbitrary `obj.method`).
(define (method-callee-qualified-names c owner class-names supers
                                       [tokens default-effect-tokens])
  (cond
    [(not (member-access-callee? c)) '()]
    [else
     (let* ([mname (member-access-field c)]
            [base (member-access-base c)]
            [base-name (bare-name-symbol base)])
       (cond
         [(and mname (or (super-call? base)
                         ;; JS/TS `super.m()`: the `super` keyword lowers to a
                         ;; bare symbol, unlike the Python `super()` invocation.
                         (eq? base-name 'super)))
          ;; super().m() → the enclosing class's parents, most-derived first.
          (if owner
              (map (λ (cls) (qualified-name-symbol cls mname))
                   (cdr (mro-chain owner supers)))
              '())]
         [(and mname base-name)
          (cond
            ;; The host's implicit receiver name (Python `self`/`cls`, JS
            ;; `this`): dispatch through the enclosing class's MRO.
            [(tokens-self-name? tokens base-name)
             (if owner
                 (map (λ (cls) (qualified-name-symbol cls mname))
                      (mro-chain owner supers))
                 '())]
            [(hash-has-key? class-names base-name)
             (map (λ (cls) (qualified-name-symbol cls mname))
                  (mro-chain base-name supers))]
            [else '()])]
         [else '()]))]))

;; Classify a call callee against the known function names:
;;   (cons 'edge name)     — call to a function in the program
;;   (cons 'pure #f)       — front-end-encoded construct (operator/builtin)
;;   (cons 'effect name)   — directly-detected effect
;;   (cons 'unknown #f)    — unresolvable (higher-order / IIFE)
;;
;; `owner` is the enclosing class name symbol (or #f); `class-names` the set of
;; all class names in the program. A method call on the host implicit receiver
;; (`self.m()` / `cls.m()`, or the injected dialect's name) resolves to the
;; edge `Owner.m`, and `Other.m()` to `Other.m`, so intra-program method calls
;; are call-graph edges instead of `unknown`.
(define (classify-call callee known-names [owner #f] [class-names (hash)]
                       [supers (hash)] #:lexicon [lex default-lexicon])
  (define tokens (lexicon-tokens lex))
  (define nm (callee-name callee))
  (define qnms (method-callee-qualified-names callee owner class-names supers tokens))
  (define qnm (for/or ([q (in-list qnms)] #:when (hash-has-key? known-names q)) q))
  (cond
    [(and qnm (hash-has-key? known-names qnm)) (cons 'edge qnm)]
    [(and nm (hash-has-key? known-names nm)) (cons 'edge nm)]
    [else (classify-nonedge callee tokens)]))

;; Shared tail of `classify-call` and `classify-call-all`: after intra-program
;; edge resolution has failed, classify the callee by name heuristics (front-end
;; constructs, pure builtins, exception constructors, runtime primitives, then
;; direct effect roots and pure stdlib). `tokens` is the injected host lexicon.
(define (classify-nonedge callee [tokens default-effect-tokens])
  (cond
    [(and (uir-call? callee)
          (not (synthetic-callee? (uir-call-callee callee) tokens)))
     ;; Higher-order application `(sync_to_async(fn))(...)`: calling a call
     ;; result inherits the inner call's classification (the wrapper's effect).
     ;; The inner `sync_to_async(fn)` call is itself a separate uir-call node,
     ;; so its effect is also counted there; this clause only stops the outer
     ;; application from falling to `unknown`. Synthetic call encodings
     ;; (`(dot a b)`, `(=> ...)`, `(function ...)`) must NOT recurse here:
     ;; they are member accesses / lambdas whose own classification is what
     ;; the following clauses (and direct-effect-of-callee) compute.
     (classify-nonedge (uir-call-callee callee) tokens)]
    [(synthetic-callee? callee tokens) (cons 'pure #f)]
    [(pure-builtin-callee? callee tokens) (cons 'pure #f)]
    [(pure-stdlib-bare-callee? callee tokens) (cons 'pure #f)]
    [(pure-native-builtin-callee? callee tokens) (cons 'pure #f)]
    [(pure-dunder-callee? callee tokens) (cons 'pure #f)]
    [(pure-op-callee? callee tokens) (cons 'pure #f)]
    [(exception-constructor-callee? callee tokens) (cons 'pure #f)]
    [(pure-constructor-callee? callee tokens) (cons 'pure #f)]
    [(runtime-primitive-callee? callee tokens) (cons 'pure #f)]
    [else
     (define eff (direct-effect-of-callee callee tokens))
     (cond
       [eff (cons 'effect eff)]
       [(pure-stdlib-callee? callee tokens) (cons 'pure #f)]
       [(str-literal-method-callee? callee tokens) (cons 'pure #f)]
       [(pure-method-callee? callee tokens) (cons 'pure #f)]
       [else (cons 'unknown #f)])]))
