#lang racket

;; Tests for the P6.3 self-emission check tooling:
;;   tools/self-emit/canonical.rkt — the UIR-plus-row comparison document
;;   tools/self-emit/oracle.rkt    — the original tower's translation output
;;
;; The emitted tower itself is supplied by P6.2; these tests pin the *checker*
;; so that when it arrives, a mismatch means a real semantic difference and a
;; match means the canonical forms agree. `racket tools/self-emit.rkt selftest`
;; exercises the end-to-end differential with fake towers.

(require rackunit
         rackunit/text-ui
         json
         racklr/uir
         racklr/effects/contract
         "../tools/self-emit/canonical.rkt"
         "../tools/self-emit/oracle.rkt"
         "../tools/self-emit/tower.rkt")

;; ── helpers ─────────────────────────────────────────────────────────

(define (sym s) (uir-symbol s))

(define (entry name modality ops)
  (uir-effect-entry (sym name) modality (map sym ops) #f #f))

(define (row . entries) (uir-effect-row entries))

(define (rows-of . pairs)
  ;; Insert in reverse so hash iteration order is not the list order.
  (define h (make-hash))
  (for ([p (in-list (reverse pairs))])
    (hash-set! h (car p) (cdr p)))
  h)

(define python-source
  (string-append "import requests\n"
                 "\n"
                 "def fetch(url):\n"
                 "    return requests.get(url)\n"))

;; ── canonical rows ──────────────────────────────────────────────────

(define (test-rows-sorted-by-function)
  (define doc (translation->jsexpr 'python (uir-null)
                                   (rows-of (cons 'zeta (row))
                                            (cons 'alpha (row)))))
  (check-equal? (map (λ (r) (hash-ref r 'fn)) (hash-ref doc 'rows))
                '("alpha" "zeta")))

(define (test-rows-canonicalize-hash-order)
  (define a (translation->jsexpr 'python (uir-null)
                                 (rows-of (cons 'f (row (entry "network" 'may '())))
                                          (cons 'g (row (entry "io" 'may '()))))))
  (define b (translation->jsexpr 'python (uir-null)
                                 (rows-of (cons 'g (row (entry "io" 'may '())))
                                          (cons 'f (row (entry "network" 'may '()))))))
  (check-equal? (canonical-json-string a) (canonical-json-string b)))

(define (test-entries-sorted-by-effect)
  (define doc (translation->jsexpr 'python (uir-null)
                                   (rows-of (cons 'f (row (entry "network" 'may '())
                                                          (entry "io" 'may '()))))))
  (define entries (hash-ref (first (hash-ref doc 'rows)) 'entries))
  (check-equal? (map (λ (e) (hash-ref e 'effect)) entries) '("io" "network")))

(define (test-ops-sorted)
  (define doc (translation->jsexpr 'python (uir-null)
                                   (rows-of (cons 'f (row (entry "db" 'may '("write" "read")))))))
  (define e (first (hash-ref (first (hash-ref doc 'rows)) 'entries)))
  (check-equal? (hash-ref e 'ops) '("read" "write")))

;; ── canonical document ──────────────────────────────────────────────

(define (test-document-carries-schema-and-language)
  (define doc (translation->jsexpr 'python (uir-null) (make-hash)))
  (check-equal? (hash-ref doc 'format) "racklr-self-emit/1")
  (check-equal? (hash-ref doc 'language) "python")
  (check-true (exact-nonnegative-integer? (hash-ref doc 'schema))))

(define (test-uir-is-versioned)
  (define doc (translation->jsexpr 'python (uir-null) (make-hash)))
  (define uir (hash-ref doc 'uir))
  (check-true (list? uir))
  (check-equal? (first uir) "uir")
  (check-true (exact-nonnegative-integer? (second uir))))

(define (test-canonical-text-is-valid-json)
  (define text (canonical-json-string (translation->jsexpr 'json (uir-null) (make-hash))))
  (define parsed (string->jsexpr text))
  (check-equal? (hash-ref parsed 'format) "racklr-self-emit/1"))

;; ── difference reporting ────────────────────────────────────────────

(define (test-difference-equal-is-false)
  (define doc (translation->jsexpr 'python (uir-null) (rows-of (cons 'f (row)))))
  (check-false (jsexpr-difference doc doc)))

(define (test-difference-names-the-path)
  (define a (translation->jsexpr 'python (uir-null) (rows-of (cons 'f (row)))))
  (define b (translation->jsexpr 'python (uir-null) (rows-of (cons 'f (row (entry "io" 'may '()))))))
  (define diff (jsexpr-difference a b))
  (check-not-false diff)
  (check-true (string? diff))
  (check-true (regexp-match? #rx"rows" diff))
  (check-true (regexp-match? #rx"entries" diff)))

(define (test-difference-detects-language-drift)
  (define a (translation->jsexpr 'python (uir-null) (make-hash)))
  (define b (translation->jsexpr 'json (uir-null) (make-hash)))
  (check-not-false (jsexpr-difference a b)))

;; ── oracle ──────────────────────────────────────────────────────────

(define (test-oracle-python-rows)
  (define-values (uir rows) (translate-source 'python python-source))
  (check-true (uir? uir))
  (check-true (hash-has-key? rows 'fetch))
  (define effects
    (for/list ([e (in-list (uir-effect-row-entries (hash-ref rows 'fetch)))])
      (effect-name-symbol e)))
  (check-not-false (memq 'network effects)))

(define (test-oracle-json)
  (define-values (uir rows) (translate-source 'json "{\"ok\": true}"))
  (check-true (uir? uir))
  (check-true (hash-empty? rows))
  (check-equal? (canonical-json-string (translation->jsexpr 'json uir rows))
                (canonical-json-string (translation->jsexpr 'json uir rows))))

(define (test-oracle-unsupported-language)
  (check-exn exn:fail?
             (λ () (translate-source 'cobol "IDENTIFICATION DIVISION."))))

(define (test-oracle-deterministic-canonical-text)
  (define-values (uir a) (translate-source 'python python-source))
  (define-values (_ b) (translate-source 'python python-source))
  (check-equal? (canonical-json-string (translation->jsexpr 'python uir a))
                (canonical-json-string (translation->jsexpr 'python uir b))))

;; ── the P6.2 seam ───────────────────────────────────────────────────

(define (test-extract-emitted-rows)
  (define rows
    (extract-emitted-rows
     (string-join
      (list "@__withEffects({\"io\": {\"modality\": \"may\"}, \"time\": {\"modality\": \"may\", \"max\": 2}})"
            "def main():"
            "    pass"
            "@__withEffects({})"
            "def pure_stage():"
            "    pass")
      "\n")))
  (check-equal? (sort (hash-keys rows) symbol<?) '(main pure_stage))
  (check-equal? (length (hash-ref rows 'main)) 2)
  (define io (findf (λ (e) (equal? (hash-ref e 'effect) "io")) (hash-ref rows 'main)))
  (check-not-false io)
  (check-equal? (hash-ref io 'modality) "may")
  (define time (findf (λ (e) (equal? (hash-ref e 'effect) "time")) (hash-ref rows 'main)))
  (check-equal? (hash-ref time 'budget) 2)
  (check-equal? (hash-ref rows 'pure_stage) '()))

(define (test-tower-availability-report)
  (define availability (self-image-availability))
  (check-true (hash? availability))
  (check-equal? (sort (hash-keys availability) symbol<?) '(seed self-emit))
  (for ([(module present?) (in-hash availability)])
    (check-true (boolean? present?) (format "~a availability is a boolean" module))))

;; The differential is the P6.3 acceptance run. When P6.2's modules are present
;; this exercises the real emitted compiler; when they are not, the report must
;; name the missing prerequisite instead of silently passing.
(define (test-tower-differential)
  (define report (run-tower-differential))
  (cond
    [(hash-ref report 'available?)
     (check-true (hash-ref report 'rows-native?) "rows are native in the emitted source")
     (check-true (hash-ref (hash-ref report 'absorb) 'ok?)
                 "every named function of the emitted compiler carries a row")
     (for ([c (in-list (hash-ref report 'cases))])
       (check-true (hash-ref c 'match-compiled?)
                   (format "translation of ~s matches the original contract; got ~s"
                           (hash-ref c 'source) (hash-ref c 'output)))
       (check-true (hash-ref c 'match-result?)
                   (format "compiled program for ~s matches the reference" (hash-ref c 'source))))
     (check-true (hash-ref report 'ok?) "the emitted compiler passes the P6.3 differential")]
    [else
     (check-false (hash-ref report 'available?))
     (check-true (hash-has-key? report 'availability)
                 "an unavailable P6.2 seam names the missing modules")]))

;; ── run ─────────────────────────────────────────────────────────────

(define self-emit-tests
  (test-suite
   "self-emit"
   (test-case "rows sorted by function" (test-rows-sorted-by-function))
   (test-case "rows canonicalize hash order" (test-rows-canonicalize-hash-order))
   (test-case "entries sorted by effect" (test-entries-sorted-by-effect))
   (test-case "ops sorted" (test-ops-sorted))
   (test-case "document carries schema and language" (test-document-carries-schema-and-language))
   (test-case "UIR is versioned" (test-uir-is-versioned))
   (test-case "canonical text is valid JSON" (test-canonical-text-is-valid-json))
   (test-case "difference of equal documents is #f" (test-difference-equal-is-false))
   (test-case "difference names the path" (test-difference-names-the-path))
   (test-case "difference detects language drift" (test-difference-detects-language-drift))
   (test-case "oracle: python rows" (test-oracle-python-rows))
   (test-case "oracle: json" (test-oracle-json))
   (test-case "oracle: unsupported language" (test-oracle-unsupported-language))
   (test-case "oracle: deterministic canonical text" (test-oracle-deterministic-canonical-text))
   (test-case "tower: emitted rows are read back from guards" (test-extract-emitted-rows))
   (test-case "tower: availability report" (test-tower-availability-report))
   (test-case "tower: P6.3 differential" (test-tower-differential))))

(module+ main
  (define failures (run-tests self-emit-tests))
  (cleanup-oracle)
  (when (zero? failures)
    (printf "All self-emit tests passed.\n"))
  (exit (if (zero? failures) 0 1)))
