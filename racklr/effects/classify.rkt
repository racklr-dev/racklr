#lang racket

;; racklr/effects/classify.rkt
;;
;; Callee classification entry points. `classify-call` resolves intra-program
;; (same-file) method edges first and defers to `classify-nonedge` (the
;; name-heuristic tail shared with whole-program inference) otherwise.

(require racklr/uir
         racklr/effects/shape
         racklr/effects/collect
         racklr/effects/effect-roots
         racklr/effects/pure-roots)

(provide classify-call classify-nonedge)

;; Class-qualified method-name symbols (Class.method) for a Python method call
;; `base.method`, when `base` is `self` / `cls` (the enclosing method's class,
;; expanded through its superclass chain) or a known class name (expanded through
;; its own chain). Returns the full ordered list of candidate names (most
;; derived first) so inherited methods resolve; the empty list means the call is
;; unresolvable (an arbitrary `obj.method`).
(define (method-callee-qualified-names c owner class-names supers)
  (cond
    [(not (uir-get? c)) '()]
    [else
     (let* ([mname (get-field-symbol (uir-get-field c))]
            [base (uir-get-base c)])
       (cond
         [(and mname (super-call? base))
          ;; super().m() → the enclosing class's parents, most-derived first.
          (if owner
              (map (λ (cls) (qualified-name-symbol cls mname))
                   (cdr (mro-chain owner supers)))
              '())]
         [(and mname (uir-var? base))
          (let ([base-name (string->symbol (uir-symbol-name (uir-var-name base)))])
            (cond
              [(memq base-name '(self cls))
               (if owner
                   (map (λ (cls) (qualified-name-symbol cls mname))
                        (mro-chain owner supers))
                   '())]
              [(hash-has-key? class-names base-name)
               (map (λ (cls) (qualified-name-symbol cls mname))
                    (mro-chain base-name supers))]
              [else '()]))]
         [else '()]))]))

;; Classify a call callee against the known function names:
;;   (cons 'edge name)     — call to a function in the program
;;   (cons 'pure #f)       — front-end-encoded construct (operator/builtin)
;;   (cons 'effect name)   — directly-detected effect
;;   (cons 'unknown #f)    — unresolvable (higher-order / IIFE)
;;
;; `owner` is the enclosing class name symbol (or #f); `class-names` the set of
;; all class names in the program. A Python `self.m()` / `cls.m()` resolves to
;; the edge `Owner.m`, and `Other.m()` to `Other.m`, so intra-program method
;; calls are call-graph edges instead of `unknown`.
(define (classify-call callee known-names [owner #f] [class-names (hash)] [supers (hash)])
  (define nm (callee-name callee))
  (define qnms (method-callee-qualified-names callee owner class-names supers))
  (define qnm (for/or ([q (in-list qnms)] #:when (hash-has-key? known-names q)) q))
  (cond
    [(and qnm (hash-has-key? known-names qnm)) (cons 'edge qnm)]
    [(and nm (hash-has-key? known-names nm)) (cons 'edge nm)]
    [else (classify-nonedge callee)]))

;; Shared tail of `classify-call` and `classify-call-all`: after intra-program
;; edge resolution has failed, classify the callee by name heuristics (synthetic
;; constructs, pure builtins, exception constructors, runtime primitives, then
;; direct effect roots and pure stdlib).
(define (classify-nonedge callee)
  (cond
    [(and (uir-call? callee)
          (not (synthetic-callee? (uir-call-callee callee))))
     ;; Higher-order application `(sync_to_async(fn))(...)`: calling a call
     ;; result inherits the inner call's classification (the wrapper's effect).
     ;; The inner `sync_to_async(fn)` call is itself a separate uir-call node,
     ;; so its effect is also counted there; this clause only stops the outer
     ;; application from falling to `unknown`. Synthetic call encodings
     ;; (`(dot a b)`, `(=> ...)`, `(function ...)`) must NOT recurse here:
     ;; they are member accesses / lambdas whose own classification is what
     ;; the following clauses (and direct-effect-of-callee) compute.
     (classify-nonedge (uir-call-callee callee))]
    [(synthetic-callee? callee) (cons 'pure #f)]
    [(pure-builtin-callee? callee) (cons 'pure #f)]
    [(pure-stdlib-bare-callee? callee) (cons 'pure #f)]
    [(pure-dunder-callee? callee) (cons 'pure #f)]
    [(gettext-callee? callee) (cons 'pure #f)]
    [(exception-constructor-callee? callee) (cons 'pure #f)]
    [(framework-exception-callee? callee) (cons 'pure #f)]
    [(declarative-callee? callee) (cons 'pure #f)]
    [(runtime-primitive-callee? callee) (cons 'pure #f)]
    [else
     (define eff (direct-effect-of-callee callee))
     (cond
       [eff (cons 'effect eff)]
       [(pure-stdlib-callee? callee) (cons 'pure #f)]
       [(str-literal-method-callee? callee) (cons 'pure #f)]
       [(pure-container-method-callee? callee) (cons 'pure #f)]
       [(pure-config-method-callee? callee) (cons 'pure #f)]
       [else (cons 'unknown #f)])]))
