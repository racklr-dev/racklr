#lang racket

;; Tests for the declared loss model and the round-trip verifier.
;;
;; The heavy end-to-end check lives in examples-roundtrip-test.rkt (it needs the
;; generated grammars); this suite pins the declaration's integrity and the
;; verifier's classification behavior on synthetic UIR, so a change to the
;; normalization/feature logic fails here first.

(require rackunit
         rackunit/text-ui
         racket/list
         racket/string
         json
         racklr/uir
         racklr/loss-model
         racklr/loss/verify
         racklr/loss/report
         racklr/loss/sexp
         racklr/loss/normalize
         racklr/loss/features
         racklr/loss/effects
         "../tools/loss-artifact.rkt")

;; ── Synthetic UIR ───────────────────────────────────────────────────

(define (fn-node name params body [row #f])
  (uir-fn (uir-symbol name) params body #f row))

(define (simple-param name)
  (uir-typed-param (uir-symbol name) #f #f))

(define (program . stmts)
  (uir-block stmts))

(define io-row
  (uir-effect-row (list (uir-effect-entry (uir-symbol "io") 'may '() #f #f))))

(define uir-sample
  (program (fn-node "add"
                    (list (simple-param "a") (simple-param "b"))
                    (uir-return (uir-call (uir-symbol "+")
                                          (list (uir-var (uir-symbol "a"))
                                                (uir-var (uir-symbol "b"))))))))

(define uir-renamed
  (program (fn-node "plus"
                    (list (simple-param "a") (simple-param "b"))
                    (uir-return (uir-call (uir-symbol "+")
                                          (list (uir-var (uir-symbol "a"))
                                                (uir-var (uir-symbol "b"))))))))

;; ── Suite: the declaration itself ──────────────────────────────────

(define declaration-tests
  (test-suite
   "loss model declaration"
   (test-case "coverage check passes"
     (check-not-exn loss-model-coverage-check))
   (test-case "expected source languages are declared"
     (for ([lang '(python javascript clojure json koka typescript tsx)])
       (check-not-false (loss-model-for-language lang) (format "model for ~a" lang))))
   (test-case "typescript/tsx emit javascript"
     (check-equal? (loss-model-target (loss-model-for 'typescript 'javascript))
                   'javascript)
     (check-equal? (loss-model-target (loss-model-for 'tsx 'javascript))
                   'javascript))
   (test-case "modes are declared per pair"
     (for ([m (in-list supported-loss-models)])
       (check-not-false (memq (loss-model-mode m) '(exact projected))
                        (format "mode for ~a→~a" (loss-model-source m) (loss-model-target m))))
     (check-eq? (loss-model-mode (loss-model-for 'python 'python)) 'exact)
     (check-eq? (loss-model-mode (loss-model-for 'tsx 'javascript)) 'projected))
   (test-case "every model declares an effect-row disposition"
     (for ([m (in-list supported-loss-models)])
       (check-not-false
        (findf (λ (r) (eq? (loss-rule-feature r) 'effect-rows))
               (loss-model-rules m))
        (format "effect-rows rule for ~a" (loss-model-source m)))))
   (test-case "every model declares every feature exactly once"
     (for ([m (in-list supported-loss-models)])
       (check-equal? (missing-features m) '()
                     (format "missing features for ~a→~a"
                             (loss-model-source m) (loss-model-target m)))
       (check-equal? (duplicate-features m) '()
                     (format "duplicate features for ~a→~a"
                             (loss-model-source m) (loss-model-target m)))
       (check-true (loss-model-complete? m))
       (check-equal? (sort (declared-features m) symbol<?)
                     (sort loss-features symbol<?))))
   (test-case "rules use known features and dispositions"
     (for ([m (in-list supported-loss-models)])
       (for ([r (in-list (loss-model-rules m))])
         (check-not-false (memq (loss-rule-feature r) loss-features))
         (check-not-false (memq (loss-rule-disposition r) loss-dispositions))
         (check-true (and (string? (loss-rule-detail r))
                          (not (string=? "" (loss-rule-detail r)))))))
   (test-case "normalizations are from the known set"
     (for ([m (in-list supported-loss-models)])
       (for ([n (in-list (loss-model-normalizations m))])
         (check-not-false (memq n loss-normalizations)))))
   (test-case "unknown pairs are not supported"
     (check-false (supported-pair? 'python 'javascript))
     (check-false (loss-model-for-language 'rust))))))

;; ── Suite: normalizations ──────────────────────────────────────────

(define normalization-tests
  (test-suite
   "normalizations"
   (test-case "generated declarations are dropped, user names are kept"
     (define sexp
       '(block ((fn (symbol "__guard") () (block ()) #f #f)
                (fn (symbol "real") () (block ()) #f #f)
                (import (symbol "inspect") ()))))
     (check-equal? (strip-generated '("inspect") '() sexp)
                   '(block ((fn (symbol "real") () (block ()) #f #f))))
     ;; The source binds `__guard` itself, so it is not emitter scaffolding.
     (check-equal? (strip-generated '() '("__guard") sexp)
                   '(block ((fn (symbol "__guard") () (block ()) #f #f)
                            (fn (symbol "real") () (block ()) #f #f)
                            (import (symbol "inspect") ())))))
   (test-case "effect guards are dropped from structure"
     (define decorator
       (list 'call '(var (symbol "__withEffects"))
             (list (record-sexp
                    (pair-sexp '(string "io")
                               (record-sexp
                                (pair-sexp '(string "modality") '(string "may"))))))))
     (define guarded
       (list 'block
             (list (list 'decorated (list decorator)
                         '(fn (symbol "log") () (block ()) #f (effect-row)))
                   (list 'call '(var (symbol "__perform"))
                         (list '(string "io") '(string "print") ''()
                               '(fn #f () (call (var (symbol "print")) ()) #f #f))))))
     (check-equal? (strip-effect-guards guarded)
                   (list 'block
                         (list '(fn (symbol "log") () (block ()) #f #f)
                               '(call (var (symbol "print")) ())))))
   (test-case "fn effect slots become empty"
     (check-equal? (strip-effect-guards (uir->sexp (fn-node "f" '() (uir-null) io-row)))
                   '(fn (symbol "f") () (null) #f #f)))
   (test-case "parens are not semantic"
     (check-equal? (strip-parens '(paren (paren (number "1")))) '(number "1")))
   (test-case "typescript types are erased"
     (check-equal? (erase-js-types
                    '(fn (symbol "f") ((typed-param (symbol "x") (symbol "number") #f))
                         (block ()) (symbol "int") #f))
                   '(fn (symbol "f") ((typed-param (symbol "x") #f #f))
                        (block ()) #f #f))
     (check-equal? (erase-js-types '(interface (symbol "I") ())) '(null))
     (check-equal? (erase-js-types '(type-alias (symbol "T") () (symbol "int"))) '(null)))
   (test-case "erase-js-types preserves a typed-param kind field"
     ;; Schema v2 appends an optional parameter-kind element. Erasing the type
     ;; must not rebuild the narrower legacy payload and drop the kind.
     (define (erase-params params)
       (call-with-values
           (λ () (fn-node-fields
                  (erase-js-types
                   (list 'fn '(symbol "f") params '(block ()) #f #f))))
         (λ (_name ps _body _ret _effects) ps)))
     (check-equal? (erase-params '((typed-param (symbol "a") (symbol "number") #f kw-only)))
                   '((typed-param (symbol "a") #f #f kw-only)))
     (check-equal? (erase-params '((typed-param (symbol "args") (symbol "int") #f var-positional)))
                   '((typed-param (symbol "args") #f #f var-positional))))
   (test-case "fn rebuild preserves fields appended after the current schema"
     (define fn-with-future-field
       '(fn (symbol "f") () (block ()) #f
            (effect-row (effect-entry (symbol "io") may () #f #f))
            future-field))
     (check-equal? (strip-effect-guards fn-with-future-field)
                   '(fn (symbol "f") () (block ()) #f #f future-field)))
   (test-case "normalize applies only declared transforms"
     (define model (loss-model-for 'python 'python))
     (define sexp (uir->sexp uir-sample))
     (check-equal? (normalize model '() sexp) sexp))))

;; ── Suite: feature projections ────────────────────────────────────

(define feature-tests
  (test-suite
   "feature projections"
   (test-case "definitions are block-scope declarations"
     (define sexp (uir->sexp (program
                             (fn-node "outer" '() (uir-null))
                             (uir-set! (uir-var (uir-symbol "x"))
                                       (uir-call (uir-symbol "f") '())))))
     (check-equal? (feature-projection 'definitions sexp) '("outer" "x")))
   (test-case "signatures carry arity and parameter names"
     (define sexp (uir->sexp (program
                             (fn-node "f" (list (simple-param "a") (simple-param "b"))
                                      (uir-null)))))
     (check-equal? (feature-projection 'signatures sexp) '(("f" 2 ("a" "b")))))
   (test-case "literals are inventoried"
     (define sexp (uir->sexp (program (uir-string "s") (uir-number "3"))))
     (check-equal? (feature-projection 'literals sexp) '("n:3" "s:s")))
   (test-case "control-flow census counts nodes"
     (define sexp (uir->sexp (program (uir-if (uir-bool #t) (uir-null) (uir-null)))))
     (define census (feature-projection 'control-flow sexp))
     (check-equal? (cdr (assq 'if census)) 1))))

;; ── Suite: effect rows in native form ─────────────────────────────

(define (record-sexp . pairs)
  (list* 'record pairs))

(define (pair-sexp key value) (list key value))

(define (emitted-record-row)
  (record-sexp
   (pair-sexp '(string "io")
              (record-sexp
               (pair-sexp '(string "modality") '(string "may"))
               (pair-sexp '(string "ops") '(list (string "print")))))))

(define effect-tests
  (test-suite
   "native effect rows"
   (test-case "emitted record decodes to a canonical row"
     (check-equal? (decode-effect-row (emitted-record-row))
                   (list (list 'effect-entry (list 'symbol "io") 'may
                               (list (list 'symbol "print")) #f #f))))
   (test-case "python decorator is re-absorbed"
     (define emitted
       (list 'decorated
             (list (list 'call '(var (symbol "__withEffects"))
                         (list (record-sexp
                                (pair-sexp '(string "io")
                                           (record-sexp
                                            (pair-sexp '(string "modality") '(string "may"))))))))
             '(fn (symbol "log") () (block ()) #f #f)))
     (define rows (extract-effect-rows emitted))
     (check-equal? (map car rows) '("log"))
     (check-equal? (cdr (car rows))
                   (list (list 'effect-entry (list 'symbol "io") 'may '() #f #f))))
   (test-case "javascript withEffects binding is re-absorbed"
     (define emitted
       (list 'block
             (list (list 'call '(symbol "const")
                         (list (list 'set! '(symbol "f")
                                     (list 'call '(var (symbol "__withEffects"))
                                           (list (record-sexp
                                                  (pair-sexp '(string "io")
                                                             (record-sexp
                                                              (pair-sexp '(string "modality") '(string "may")))))
                                                 '(fn #f () (block ()) #f #f)))))))))
     (define rows (extract-effect-rows emitted))
     (check-equal? (map car rows) '("f")))
   (test-case "function slot row is preserved (koka-style)"
     (define emitted (uir->sexp (program (fn-node "f" '() (uir-null) io-row))))
     (check-equal? (map car (extract-effect-rows emitted)) '("f")))
   (test-case "missing row is reported as changed"
     (define rule (findf (λ (r) (eq? (loss-rule-feature r) 'effect-rows))
                         (loss-model-rules (loss-model-for 'python 'python))))
     (define u1 (uir->sexp (program (fn-node "log" '() (uir-null) io-row))))
     (define u2 (uir->sexp (program (fn-node "log" '() (uir-null)))))
     (define obs (observe-effect-rows rule u1 u2))
     (check-eq? (loss-observation-observed obs) 'changed))
   (test-case "matching rows are preserved"
     (define rule (findf (λ (r) (eq? (loss-rule-feature r) 'effect-rows))
                         (loss-model-rules (loss-model-for 'koka 'koka))))
     (define s (uir->sexp (program (fn-node "f" '() (uir-null) io-row))))
     (check-eq? (loss-observation-observed (observe-effect-rows rule s s)) 'preserved))))

;; ── Suite: verification classification ────────────────────────────

(define (report-for from to u1 u2)
  (verify-round-trip (loss-model-for from to) u1 u2))

(define verification-tests
  (test-suite
   "round-trip verification"
   (test-case "identical UIR is within the exact model"
     (define r (report-for 'python 'python uir-sample uir-sample))
     (check-true (report-within-model? r))
     (check-equal? (loss-report-violations r) '())
     (check-true (pair? (loss-report-observations r))))
   (test-case "a renamed function is an undeclared structural loss"
     (define r (report-for 'python 'python uir-sample uir-renamed))
     (check-false (report-within-model? r))
     (check-not-false (memq 'structure (map loss-violation-feature
                                            (loss-report-violations r))))
     (check-not-false (pair? (loss-report-evidence r))))
   (test-case "projected mode tolerates re-shaped structure"
     (define u1 (program
                 (uir-call (uir-symbol "const")
                           (list (uir-set! (uir-var (uir-symbol "C"))
                                           (uir-call (uir-symbol "=>")
                                                     (list (uir-list '()) (uir-null))))))))
     (define u2 (program
                 (uir-call (uir-symbol "const")
                           (list (uir-set! (uir-var (uir-symbol "C"))
                                           (uir-call (uir-symbol "=>")
                                                     (list (uir-list '())
                                                           (uir-string "re-shaped"))))))))
     (define r (report-for 'tsx 'javascript u1 u2))
     (check-true (report-within-model? r)))
   (test-case "projected mode still requires preserved definitions"
     (define u1 (program
                 (uir-call (uir-symbol "const")
                           (list (uir-set! (uir-var (uir-symbol "C"))
                                           (uir-call (uir-symbol "=>")
                                                     (list (uir-list '()) (uir-null))))))))
     (define u2 (program
                 (uir-call (uir-symbol "const")
                           (list (uir-set! (uir-var (uir-symbol "D"))
                                           (uir-call (uir-symbol "=>")
                                                     (list (uir-list '()) (uir-null))))))))
     (define r (report-for 'tsx 'javascript u1 u2))
     (check-false (report-within-model? r))
     (check-not-false (member 'definitions (map loss-violation-feature
                                                (loss-report-violations r)))))
   (test-case "an incomplete model is rejected, not silently accepted"
     (define incomplete
       (loss-model 'python 'python 'projected '() '()
                   (list (preserved 'definitions "only one rule declared"))))
     (check-exn exn:fail?
                (λ () (verify-round-trip incomplete uir-sample uir-sample)))
     (check-equal? (missing-features incomplete)
                   (remove* '(definitions) loss-features)))
   (test-case "violations serialize with evidence"
     (define r (report-for 'python 'python uir-sample uir-renamed))
     (define js (report->jsexpr r))
     (check-true (hash? js))
     (check-equal? (hash-ref js 'within_model) #f)
     (check-true (pair? (hash-ref js 'violations)))
     (check-true (string? (jsexpr->string (jsexpr-sanitize js)))))))

;; ── Suite: artifact and digest ────────────────────────────────────

(define artifact-tests
  (test-suite
   "loss artifact"
   (test-case "model digest is stable and content-addressed"
     (define d1 (loss-model-digest))
     (define d2 (loss-model-digest))
     (check-equal? d1 d2)
     (check-equal? (string-length d1) 71)
     (check-true (and (string-prefix? d1 "sha256:")
                      (regexp-match? #rx"^[0-9a-f]+$" (substring d1 7)))))
   (test-case "artifact writer produces JSON"
     (define tmp (make-temporary-file "loss-artifact-~a.json"))
     (define payload (hasheq 'tool "test"
                             'model_digest (loss-model-digest)
                             'summary (hasheq 'examples 0 'passed 0 'failed 0)
                             'files '()))
     (write-loss-artifact! tmp payload)
     (define parsed (with-input-from-file tmp read-json))
     (check-equal? (hash-ref parsed 'tool) "test")
     (check-equal? (hash-ref parsed 'model_digest) (loss-model-digest))
     (delete-file tmp))))

;; ── Run ────────────────────────────────────────────────────────────

(define suite
  (test-suite
   "loss model"
   declaration-tests
   normalization-tests
   feature-tests
   effect-tests
   verification-tests
   artifact-tests))

(exit (run-tests suite))
