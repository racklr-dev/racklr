#lang racket

;; tools/certificate-sweep.rkt — the corpus through the kernel certificate
;; checker (ROADMAP "kernel-leaf theory" test 5; LOOP_PLAN P1.6).
;;
;; Usage (from the repository root):
;;   racket tools/certificate-sweep.rkt                 # django/django/core
;;   racket tools/certificate-sweep.rkt django/django   # explicit probe
;;
;; It scans the corpus (reusing tools/provenance-corpus.rkt's whole-program
;; inference), then turns EVERY inferred function row into a CLAIM and runs it
;; through the kernel's certificate checker
;; (racklr/effects/algebra/certificate.rkt): a claim is accepted only when
;; every claimed effect is (a) grounded — it carries an evidence trail naming
;; the classifying site — and (b) interpretable under the declared contract.
;;
;; The corpus has no per-function declared contract, so the sweep declares the
;; honest corpus-wide one: every effect the corpus actually uses is allowed,
;; and `unknown` is NOT. Under that contract the kernel accepts a claim iff it
;; is grounded and unknown-free — the P0.3 bar, now decided by the kernel
;; rather than asserted by the producer. The kernel's grounding verdict is
;; cross-checked against the producer-side `provenance-missing` (call-graph
;; reachability), a second, independent implementation of the same property;
;; the two must agree.
;;
;; Exit 0 = every claim accepted by the kernel and the two grounding checks
;; agree; 1 = a claim refused or a disagreement, with the offender named.

(require racket/file
         racket/list
         racket/string
         racklr/uir
         racklr/effects/provenance
         (prefix-in alg: racklr/effects/algebra/main)
         "provenance-corpus.rkt")

(define default-probe-dirs '("django/django/core"))

(define (repo-root)
  (simplify-path (build-path (simplify-path (path->complete-path
                                             (find-system-path 'run-file)))
                             'up 'up)))

;; ── claim evidence from the first-class provenance record ───────────
;;
;; The record grounds a classification at the site that made it, and records
;; the call graph separately. A function whose row inherits an effect through
;; its callees is grounded by the callee's site reachable along the call graph
;; — the same reachability `provenance-missing` uses. We materialize that into
;; the algebra's evidence shape (effect -> listof provenance) by a BFS over
;; the call graph from each owner.

(define (build-evidence-table rows prov)
  (define grounds (provenance-grounds prov))
  (define calls (provenance-calls prov))
  ;; forward adjacency: owner-fq -> (listof callee-fq)
  (define fwd (make-hash))
  (for ([(caller cs) (in-hash calls)])
    (for ([c (in-list cs)])
      (hash-update! fwd (prov-call-from c)
                    (λ (l) (cons (prov-call-to c) l)) '())))
  ;; ground sites by (owner-fq, effect)
  (define sites-by-owner-eff (make-hash))
  (for ([(owner by-eff) (in-hash grounds)])
    (for ([(eff sites) (in-hash by-eff)])
      (hash-set! sites-by-owner-eff (cons owner eff) sites)))
  ;; evidence for one (owner-fq, effect): every ground site whose owner is the
  ;; owner itself or a callee reachable along the call graph, with the chain of
  ;; caller names from the owner down to the site's owner.
  (define (evidence-for fq eff)
    (define out '())
    (define seen (make-hash))
    (define parent (make-hash))
    (hash-set! seen fq #t)
    (hash-set! parent fq #f)
    (define q (list fq))
    (let loop ()
      (when (pair? q)
        (define n (car q))
        (set! q (cdr q))
        (for ([site (in-list (hash-ref sites-by-owner-eff (cons n eff) '()))])
          (define chain
            (if (equal? fq n)
                '()
                (reverse
                 (let walk ([x n])
                   (if (hash-ref parent x #f)
                       (cons x (walk (hash-ref parent x)))
                       '())))))
          (set! out (cons (alg:provenance (prov-site-tokens site)
                                          (prov-site-receiver site)
                                          chain)
                          out)))
        (for ([to (in-list (hash-ref fwd n '()))]
              #:unless (hash-ref seen to #f))
          (hash-set! seen to #t)
          (hash-set! parent to n)
          (set! q (append q (list to))))
        (loop)))
    out)
  (define table (make-hash))
  (for ([(k row) (in-hash rows)])
    (define fq (function-fq k))
    (define h (make-hash))
    (for ([e (in-list (uir-effect-row-entries row))])
      (define eff (entry-effect-name e))
      (define ev (evidence-for fq eff))
      (when (pair? ev) (hash-set! h eff ev)))
    (hash-set! table fq h))
  table)

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (define dirs (filter (λ (a) (not (string-prefix? a "--"))) args))
  (define probe-dirs (if (null? dirs) default-probe-dirs dirs))
  (define root (repo-root))
  (parameterize ([current-directory root])
    (define-values (rows prov) (corpus-rows root probe-dirs))
    (define effectful
      (for/sum ([(_ row) (in-hash rows)])
        (if (null? (uir-effect-row-entries row)) 0 1)))
    ;; the corpus-wide declared contract: every effect the corpus uses is
    ;; allowed, `unknown` is not.
    (define effect-names
      (remove-duplicates
       (for*/list ([(_ row) (in-hash rows)]
                   [e (in-list (uir-effect-row-entries row))]
                   #:unless (eq? (entry-effect-name e) 'unknown))
         (entry-effect-name e))))
    (define interp
      (alg:interpretation-from-names
       (for/hash ([n (in-list effect-names)]) (values n (list n)))))
    (define needs (alg:handler-row effect-names))
    (define evidence-table (build-evidence-table rows prov))
    (define accepted 0)
    (define refused '())
    (for ([(k row) (in-hash rows)])
      (when (pair? (uir-effect-row-entries row))
        (define fq (function-fq k))
        (define claim
          (alg:make-claim
           (alg:make-algebra-row row (hash-ref evidence-table fq (hash)))))
        (define-values (ok? uncovered ungrounded)
          (alg:check-claim claim needs interp))
        (if ok?
            (set! accepted (add1 accepted))
            (set! refused (cons (list fq uncovered ungrounded) refused)))))
    (define producer-missing (provenance-missing rows prov))
    (printf "named functions:        ~a\n" (hash-count rows))
    (printf "effectful functions:    ~a\n" effectful)
    (printf "kernel-accepted claims: ~a\n" accepted)
    (printf "kernel-refused claims:  ~a\n" (length refused))
    (printf "producer ungrounded (provenance-missing): ~a\n" (length producer-missing))
    (for ([r (in-list (take (reverse refused) (min 20 (length refused))))])
      (printf "  REFUSED ~a\n    uninterpretable: ~a\n    ungrounded: ~a\n"
              (car r) (cadr r) (caddr r)))
    (define ok? (and (null? refused) (null? producer-missing)))
    (if ok?
        (begin
          (printf "certificate OK: the kernel accepts every claim, and the kernel and producer grounding checks agree\n")
          (exit 0))
        (begin
          (printf "certificate FAILED\n")
          (exit 1)))))

(module+ main
  (with-handlers ([exn:fail?
                   (λ (e)
                     (eprintf "certificate-sweep: ~a\n" (exn-message e))
                     (exit 2))])
    (main)))
