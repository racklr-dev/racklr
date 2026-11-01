#lang racket

;; tools/self-emit/differential.rkt — the UIR-plus-row differential runner.
;;
;; An emitted tower is any executable that prints the canonical UIR-plus-row
;; document (tools/self-emit/canonical.rkt) for a source file:
;;
;;     <tower> dump-uir <language> <source-file>
;;
;; This module produces the original side of that comparison, runs the emitted
;; side, and names the first difference on a mismatch. It also contains the
;; checker's self-test: a matching tower is accepted, a tower that mutates one
;; field is caught at the exact JSON path, and an absent tower is unrunnable.

(require json
         racket/file
         racket/list
         racket/path
         racket/string
         racket/system
         "canonical.rkt"
         "oracle.rkt")

(provide language-of-path
         default-fixtures
         original-document
         verify-translations
         print-verify-report
         run-selftest)

;; ── fixtures ────────────────────────────────────────────────────────

;; Fixtures exercise the two front ends whose grammars are tracked in the
;; repository. `effects.py` is deliberately effectful so a comparison covers
;; row inference, not only tree shape.
(define (default-fixtures)
  (define root (repo-root))
  (filter file-exists?
          (list (build-path root "tools" "self-emit" "fixtures" "effects.py")
                (build-path root "tools" "self-emit" "fixtures" "data.json")
                (build-path root "docs" "examples" "python" "calc.py"))))

(define (language-of-path path)
  (define name (path->string path))
  (cond [(regexp-match? #rx"[.]py$" name) 'python]
        [(regexp-match? #rx"[.]json$" name) 'json]
        [else #f]))

;; ── the original side ───────────────────────────────────────────────

;; The canonical document the self-emitted tower must reproduce.
(define (original-document language path)
  (define-values (uir rows) (translate-file language path))
  (translation->jsexpr language uir rows))

;; ── differential ────────────────────────────────────────────────────

;; Run one executable translation and compare it with the original. Returns
;;   (hash 'path 'language 'status 'match? 'difference 'output)
;; where status is 'match, 'mismatch, or 'unrunnable.
(define (compare-one tower language path)
  (define original (original-document language path))
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-output-port out] [current-error-port err])
      (system*/exit-code tower "dump-uir" (symbol->string language)
                         (path->string (path->complete-path path)))))
  (define stdout (get-output-string out))
  (define stderr (get-output-string err))
  (cond
    [(not (zero? code))
     (hash 'path (path->string path) 'language language 'status 'unrunnable
           'match? #f 'difference #f
           'output (string-append
                    (format "emitted tower exited ~a\n" code)
                    (if (string=? "" (string-trim stderr))
                        ""
                        (format "--- stderr ---\n~a" stderr))))]
    [else
     (define parsed
       (with-handlers ([exn:fail? (λ (e) e)])
         (string->jsexpr stdout)))
     (cond
       [(exn:fail? parsed)
        (hash 'path (path->string path) 'language language 'status 'unrunnable
              'match? #f 'difference #f
              'output (format "emitted tower did not print JSON: ~a" (exn-message parsed)))]
       [else
        (define diff (jsexpr-difference original parsed))
        (hash 'path (path->string path) 'language language
              'status (if diff 'mismatch 'match)
              'match? (not diff) 'difference diff 'output "")])]))

;; Compare every fixture; returns (hash 'ok? 'unrunnable? 'results 'failed).
(define (verify-translations tower files)
  (define results
    (for/list ([path (in-list files)])
      (define language (language-of-path path))
      (unless language
        (error 'verify "cannot infer a supported language from the file extension: ~a" path))
      (compare-one tower language path)))
  (define unrunnable? (for/or ([r (in-list results)]) (eq? (hash-ref r 'status) 'unrunnable)))
  (define failed (for/list ([r (in-list results)] #:unless (hash-ref r 'match?)) r))
  (hash 'ok? (and (not unrunnable?) (null? failed))
        'unrunnable? unrunnable?
        'results results
        'failed failed))

(define (print-verify-report report)
  (for ([r (in-list (hash-ref report 'results))])
    (cond
      [(eq? (hash-ref r 'status) 'match)
       (printf "MATCH     ~a (~a)\n" (hash-ref r 'path) (hash-ref r 'language))]
      [(eq? (hash-ref r 'status) 'mismatch)
       (printf "MISMATCH  ~a (~a)\n  first difference: ~a\n"
               (hash-ref r 'path) (hash-ref r 'language) (hash-ref r 'difference))]
      [else
       (printf "UNRUNNABLE ~a (~a)\n  ~a\n"
               (hash-ref r 'path) (hash-ref r 'language) (hash-ref r 'output))]))
  (printf "\n~a/~a translations match\n"
          (for/sum ([r (in-list (hash-ref report 'results))] #:when (hash-ref r 'match?)) 1)
          (length (hash-ref report 'results))))

;; ── self-test ───────────────────────────────────────────────────────

(define (print-verify-report-text report)
  (define out (open-output-string))
  (parameterize ([current-output-port out]) (print-verify-report report))
  (string-trim (get-output-string out)))

;; A named boolean check with a detail string.
(define (check name thunk)
  (define-values (ok? detail) (thunk))
  (list ok? name detail))

;; Quote a string for /bin/sh.
(define (shell-quote s)
  (string-append "'" (string-replace s "'" "'\\''") "'"))

;; Prove the checker itself: determinism, a matching tower, a mutating tower,
;; and an absent tower. The fake towers delegate to the original tower's own
;; oracle, so a "match" here validates the comparison pipeline; the mutating
;; one changes the language field so a mismatch cannot be missed.
(define (run-selftest)
  (define root (repo-root))
  (define temp (make-temporary-file "self-emit-selftest-~a" 'directory))
  (define racket-bin (or (find-executable-path "racket") (find-system-path 'exec-file)))
  (define fixture (build-path temp "selftest.py"))
  (display-to-file
   (string-join
    '("import requests" "" "def fetch(url):" "    return requests.get(url)" ""
      "def main():" "    return fetch(\"http://example.com\")" "")
    "\n")
   fixture #:exists 'replace)
  (define (write-tower name mutate?)
    (define path (build-path temp name))
    (define script
      (string-append
       "#!/bin/sh\n"
       "# fake self-emitted tower for the checker self-test (delegates to the oracle)\n"
       "shift\n"
       "REPO=" (shell-quote (path->string root)) "\n"
       "export PLTCOLLECTS=\"$REPO:${PLTCOLLECTS:-}\"\n"
       (format "exec ~a \"$REPO/tools/self-emit.rkt\" oracle \"$1\" \"$2\""
               (shell-quote (path->string racket-bin)))
       (if mutate?
           " | sed 's/\"language\":\"python\"/\"language\":\"mutated\"/'\n"
           "\n")))
    (display-to-file script path #:exists 'replace)
    (file-or-directory-permissions path #o755)
    path)
  (define good-tower (write-tower "tower-good" #f))
  (define bad-tower (write-tower "tower-bad" #t))
  (define checks
    (list
     (check "original output is deterministic"
            (λ ()
              (define a (canonical-json-string (original-document 'python fixture)))
              (define b (canonical-json-string (original-document 'python fixture)))
              (values (string=? a b) (if (string=? a b) "identical canonical JSON" "differs"))))
     (check "a matching tower is accepted"
            (λ ()
              (define r (verify-translations good-tower (list fixture)))
              (values (hash-ref r 'ok?) (print-verify-report-text r))))
     (check "a mutating tower is detected"
            (λ ()
              (define r (verify-translations bad-tower (list fixture)))
              (values (not (hash-ref r 'ok?))
                      (if (hash-ref r 'unrunnable?)
                          "reported unrunnable instead of mismatch"
                          (format "detected mismatch at ~a"
                                  (hash-ref (first (hash-ref r 'failed)) 'difference))))))
     (check "an absent tower is unrunnable"
            (λ ()
              (define r (verify-translations (build-path temp "does-not-exist")
                                             (list fixture)))
              (values (hash-ref r 'unrunnable?) "reported unrunnable")))))
  (for ([c (in-list checks)])
    (printf "~a  ~a: ~a\n"
            (if (car c) "PASS" "FAIL")
            (cadr c)
            (caddr c)))
  (if (for/and ([c (in-list checks)]) (car c)) 0 1))
