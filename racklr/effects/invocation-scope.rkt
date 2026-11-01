#lang racket

;; racklr/effects/invocation-scope.rkt
;;
;; Scope inference and the leaf rows an invocation's classification
;; materializes to. Kept separate from the body evaluator so each stays small;
;; the evaluator (`invocation-row.rkt`) composes these leaves with the algebra
;; combinators.
;;
;; A scope is the resource qualifier the runtime guards understand
;; (`reads=<resource>` / `writes=<resource>`, see
;; `racklr/effects/guard-vocabulary.rkt`). Inference only claims one when the
;; call chain grounds both the access direction and the resource:
;;
;;   - a typed op set (`db<User>.read`, `io<File>.write`) names the receiver
;;     class as the resource and the verb as the direction; every op of the
;;     entry must agree, otherwise the entry is not characterized;
;;   - a coarse entry (empty ops) can still be characterized when the call
;;     chain is rooted at a known class (the resource) and the called method is
;;     a read or a write in the guard vocabulary's DATA verb tables.
;;
;; A `self`/`cls` root is the enclosing instance, not a name, so it never
;; grounds a resource. Anything else leaves scope `#f`: uncharacterized, never
;; guessed.

(require racklr/uir
         racklr/effects/shape
         racklr/effects/imports
         racklr/effects/lexicon
         racklr/effects/guard-vocabulary
         (prefix-in alg: racklr/effects/algebra/main))

(provide effect-leaf-row
         unknown-row
         capability-row
         call-scope
         ops-scope)

;; ── rows ────────────────────────────────────────────────────────────

;; A classified effect that the current expression performs once, when it is
;; evaluated. The modality is `must` because evaluating the expression performs
;; the effect: the enclosing combinators weaken it when the expression is
;; conditional or repeated.
(define (effect-leaf-row domain ops scope)
  (alg:make-algebra-row
   (uir-effect-row
    (list (uir-effect-entry (uir-symbol (symbol->string domain))
                            'must
                            ops
                            1
                            scope)))
   (hash)))

;; An unresolvable call is a tracked deferral, never a silent default: an
;; `unknown` entry with the `unknown` modality. It is not an invocation of a
;; known effect, so it carries no budget or scope; the combinators keep the
;; deferral through branches and loops.
(define unknown-row
  (alg:make-algebra-row
   (uir-effect-row
    (list (uir-effect-entry (uir-symbol "unknown") 'unknown '() #f #f)))
   (hash)))

;; A declared capability at a method definition (whole-program inference):
;; asserted by DATA, so it is a permission (`may`) with no invocation count.
;; Its typed ops still carry the receiver, which can ground a scope.
(define (capability-row cap)
  (define domain (effect-cap-domain cap))
  (define ops (effect-cap-ops cap))
  (alg:make-algebra-row
   (uir-effect-row
    (list (uir-effect-entry (uir-symbol (symbol->string domain))
                            'may
                            (map (λ (s) (uir-symbol (symbol->string s))) ops)
                            #f
                            (and (pair? ops) (ops-scope ops)))))
   (hash)))

;; ── scope inference ─────────────────────────────────────────────────

;; A typed op is `domain<Receiver>.verb`; a read/write verb names the access
;; direction and the receiver class names the resource.
(define (grounded-scope receiver verb)
  (cond
    [(and receiver (string=? verb "read"))
     (uir-symbol (format "reads=~a" receiver))]
    [(and receiver (string=? verb "write"))
     (uir-symbol (format "writes=~a" receiver))]
    [else #f]))

;; Scope from a typed op set: every op must agree on one receiver class and one
;; read/write direction, otherwise the resource is not characterized.
(define (ops-scope ops)
  (define parts (for/list ([op (in-list ops)]) (typed-op-parts op)))
  (define receivers (remove-duplicates (filter values (map second parts))))
  (define verbs (remove-duplicates (filter values (map third parts))))
  (and (= (length receivers) 1)
       (= (length verbs) 1)
       (grounded-scope (first receivers) (first verbs))))

;; Direction of a coarse-classified call from the runtime guard's DATA verb
;; vocabulary (the same tables the classifier and the emitted guards use).
(define (method-scope-verb mname)
  (and mname
       (let* ([name (symbol->string mname)]
              [read? (and (member name (scope-read-verbs)) #t)]
              [write? (and (member name (scope-write-verbs)) #t)])
         (cond [(and read? (not write?)) "read"]
               [(and write? (not read?)) "write"]
               [else #f]))))

;; Scope of one classified call. A precise op set grounds it directly; a coarse
;; entry (empty ops) needs a known-class receiver root and one verb direction.
(define (call-scope call-node ops tokens known-class?)
  (cond
    [(pair? ops) (ops-scope ops)]
    [else
     (define callee (uir-call-callee call-node))
     (define mname (effect-op-of-callee callee))
     (define root (and (member-access-callee? callee) (dotted-root callee)))
     (and root
          (not (tokens-self-name? tokens root))
          (known-class? root)
          (grounded-scope root (or (method-scope-verb mname) "")))]))
