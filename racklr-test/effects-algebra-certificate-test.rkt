#lang racket

;; racklr-test/effects-algebra-certificate-test.rkt
;;
;; Certificate-checker tests for the effect-algebra kernel leaf (LOOP_PLAN P1.6,
;; ROADMAP "The kernel-leaf theory"): the kernel re-verifies a producer's claim
;; against a declared contract and never re-derives. Test 1 (extract the kernel)
;; is the line budget below; test 3 (the mutation test) is the ungrounded-claim
;; rejection — a producer that drops an evidence edge must be refused.

(require rackunit
         racket/runtime-path
         racket/file
         racklr/uir
         racklr/effects/algebra/main)

(random-seed 20261103)
(define trial-count 60)
(define effect-pool '(db io network process cache llm time email))

(define (pick-subset lst)
  (filter (λ (x) (zero? (random 2))) lst))

(define (ent name modality)
  (uir-effect-entry (uir-symbol (symbol->string name)) modality '() #f #f))

(define (prov tokens receiver chain)
  (provenance tokens receiver chain))

;; A row carrying each effect with one piece of evidence (a grounded claim).
(define (grounded-row effects)
  (for/fold ([r (empty-algebra-row)]) ([n (in-list effects)])
    (row-add r n (ent n 'may)
             (prov (list (format "site-~a" n)) #f '()))))

;; ── the kernel line budget (falsifiable test 1) ─────────────────────
;; The trusted leaf is provenance + row + combinators + subtyping + the
;; certificate checker. It must stay under this many source lines.
(define kernel-line-budget 1000)
(define kernel-module-names '("provenance.rkt" "row.rkt" "combinators.rkt"
                              "subtyping.rkt" "certificate.rkt"))
(define-runtime-path kernel-dir "../racklr/effects/algebra")
(define (kernel-line-count)
  (for/sum ([m (in-list kernel-module-names)])
    (length (file->lines (build-path kernel-dir m)))))

(define (test-kernel-line-budget)
  (define n (kernel-line-count))
  (check-true (<= n kernel-line-budget)
              (format "kernel is ~a lines, over the ~a budget"
                      n kernel-line-budget)))

;; ── the decision function ───────────────────────────────────────────

(define (test-accept-grounded-interpretable)
  (define claimed (make-claim (grounded-row '(db))))
  (define needs (handler-row '(db)))
  (define-values (ok? v u) (check-claim claimed needs (identity-interpretation)))
  (check-true ok?)
  (check-equal? v '())
  (check-equal? u '()))

;; Mutation test: the producer asserts `db` but drops the evidence edge. The
;; kernel must refuse the claim even though the contract would interpret it.
(define (test-reject-ungrounded)
  (define row (grounded-row '(db)))
  (define bad (claim row (hash))) ; certificate missing the db edge
  (define needs (handler-row '(db)))
  (define-values (ok? v u) (check-claim bad needs (identity-interpretation)))
  (check-false ok?)
  (check-equal? v '())
  (check-equal? u '(db)))

;; A grounded claim the contract does not interpret is refused with the
;; uncovered effect named (the bridge's violation path).
(define (test-reject-uninterpretable)
  (define claimed (make-claim (grounded-row '(db network))))
  (define needs (handler-row '(db)))
  (define-values (ok? v u) (check-claim claimed needs (identity-interpretation)))
  (check-false ok?)
  (check-equal? v '(network))
  (check-equal? u '()))

;; The kernel's decision agrees with the algebra's interpretation relation on
;; random rows: ok? iff every claimed effect is grounded and interpreted.
(define (test-kernel-agrees-with-relation)
  (for ([trial (in-range trial-count)])
    (define effects (pick-subset effect-pool))
    (define needs-names (pick-subset effect-pool))
    (define row (grounded-row effects))
    (define claimed (make-claim row))
    (define needs (handler-row needs-names))
    (define expected (remove* needs-names effects))
    (define-values (ok? v u) (check-claim claimed needs (identity-interpretation)))
    (check-true (set=? (list->set v) (list->set expected))
                (format "trial ~a: violations ~a vs ~a" trial v expected))
    (check-equal? u '())
    (check-equal? ok? (null? expected)
                 (format "trial ~a: ok? ~a vs expected ~a" trial ok? expected))))

;; ── main ────────────────────────────────────────────────────────────
(module+ main
  (test-kernel-line-budget)
  (test-accept-grounded-interpretable)
  (test-reject-ungrounded)
  (test-reject-uninterpretable)
  (test-kernel-agrees-with-relation)
  (displayln "All algebra certificate tests passed."))
