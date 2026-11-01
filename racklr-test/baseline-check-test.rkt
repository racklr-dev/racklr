#lang racket

;; racklr-test/baseline-check-test.rkt — the gate for the baseline's own
;; machinery.
;;
;; Run from racklr-test/:  racket baseline-check-test.rkt
;;
;; Covers the pieces the content-addressed baseline rests on: SHA-256 against
;; the FIPS vectors, the canonical snapshot document (summary extraction, path
;; rewriting, deterministic ordering, parsing) and the exact graph diff. The
;; full end-to-end check (running type-graph over Django) is the shared
;; baseline gate itself: `racket tools/baseline-check.rkt`.

(require rackunit
         "../tools/sha256.rkt"
         "../tools/baseline-snapshot.rkt")

;; ── fixtures ────────────────────────────────────────────────────────

(define sample-report
  (string-join
   '("=== Django type-effect catalogue ==="
     "files scanned:  112"
     "files parsed:   112"
     "files failed:   0"
     "named functions: 979"
     "effectful functions: 740"
     "Effect algebra (name -> distinct entries):"
     "  db  (functions-touching: 169, files-touching: 54)")
   "\n"))

(define sample-root "/tmp/racklr-checkout")

(define (atom-lines-for-root root)
  (list
   "(EffectLink (ConceptNode db) (ConceptNode may) (ConceptNode ()) (ConceptNode #f) (ConceptNode #f))"
   (string-append
    "(Fn-effect-rowLink (ConceptNode \""
    root
    "/django/core/a.py::f\") (EffectLink (ConceptNode db) (ConceptNode may)"
    " (ConceptNode ()) (ConceptNode #f) (ConceptNode #f)))")
   "(InheritanceLink (ConceptNode \"Sub\") (ConceptNode \"Base\"))"))

(define (build-sample-snapshot #:report [report sample-report]
                               #:atom-lines [atom-lines (atom-lines-for-root sample-root)]
                               #:root [root sample-root])
  (build-snapshot report atom-lines root "django/django/core"))

;; ── sha256 ──────────────────────────────────────────────────────────

(define (test-sha256-fips-vectors)
  (check-equal? (sha256-hex #"")
                "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
  (check-equal? (sha256-hex #"abc")
                "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
  (check-equal? (sha256-hex #"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
                "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")
  ;; Multi-block message: 1,000,000 'a's.
  (check-equal? (sha256-hex (make-bytes 1000000 97))
                "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0"))

(define (test-bytes->hex-pads-single-digits)
  (check-equal? (bytes->hex #"\x00\x01\x0f\x10\xff") "00010f10ff"))

;; ── summary extraction ──────────────────────────────────────────────

(define (test-summary-extraction)
  (check-equal?
   (extract-summary sample-report)
   '(("files scanned" . "112") ("files parsed" . "112")
     ("files failed" . "0") ("named functions" . "979")
     ("effectful functions" . "740"))))

(define (test-summary-missing-counter-fails-closed)
  (check-exn exn:fail?
             (λ () (extract-summary "=== catalogue ===\nfiles parsed: 112"))))

;; ── canonical atom lines ────────────────────────────────────────────

(define (test-normalize-rewrites-root-and-sorts)
  (define normalized (normalize-atom-lines (atom-lines-for-root sample-root)
                                           sample-root))
  (check-equal? normalized (sort normalized string<?))
  (check-false (findf (λ (l) (string-contains? l sample-root)) normalized))
  (check-not-false (findf (λ (l) (string-contains? l "django/core/a.py::f")) normalized)))

(define (test-normalize-accepts-trailing-slash-root)
  (check-equal?
   (normalize-atom-lines (atom-lines-for-root sample-root)
                         (string-append sample-root "/"))
   (normalize-atom-lines (atom-lines-for-root sample-root) sample-root)))

(define (test-normalize-rejects-outside-repo-path)
  (check-exn exn:fail?
             (λ ()
               (normalize-atom-lines
                (list "(Fn-effect-rowLink (ConceptNode \"/elsewhere/a.py::f\"))")
                sample-root))))

;; ── snapshot document ───────────────────────────────────────────────

(define (test-snapshot-is-path-portable)
  (define first-root "/tmp/checkout-one")
  (define second-root "/home/someone/else/checkout-two")
  (check-equal?
   (snapshot-content-id (build-sample-snapshot #:root first-root
                                               #:atom-lines (atom-lines-for-root first-root)))
   (snapshot-content-id (build-sample-snapshot #:root second-root
                                               #:atom-lines (atom-lines-for-root second-root)))))

(define (test-snapshot-is-order-insensitive)
  (define atom-lines (atom-lines-for-root sample-root))
  (check-equal?
   (snapshot-content-id (build-sample-snapshot #:atom-lines atom-lines))
   (snapshot-content-id (build-sample-snapshot #:atom-lines (reverse atom-lines)))))

(define (test-snapshot-roundtrip)
  (define text (build-sample-snapshot))
  (define-values (header body) (parse-snapshot text))
  (check-equal? (assoc "files parsed" header) '("files parsed" . "112"))
  (check-equal? (assoc "graph atoms" header) '("graph atoms" . "3"))
  (check-equal? (length body) 3)
  (check-true (string-prefix? text "# racklr type-graph baseline snapshot")))

(define (test-snapshot-hash-format)
  (check-regexp-match #px"^sha256:[0-9a-f]{64}$"
                      (snapshot-content-id (build-sample-snapshot))))

;; Format freeze: the canonical document layout and hash. If this changes on
;; purpose, bump snapshot-format-version and re-pin the real baseline.
(define (test-snapshot-format-freeze)
  (check-equal? (snapshot-content-id (build-sample-snapshot))
                "sha256:61eac0ce81dedc2253c4c7c3e3e4b8090cc86bca7004add29c27d851f2726d12"))

(define (test-parse-without-header-marker-fails)
  (check-exn exn:fail?
             (λ () (parse-snapshot "# racklr type-graph baseline snapshot\nfiles parsed: 112"))))

;; ── exact graph diff ────────────────────────────────────────────────

(define (test-diff-of-identical-snapshots-is-empty)
  (define text (build-sample-snapshot))
  (define-values (changes removed added) (diff-snapshots text text))
  (check-equal? changes '())
  (check-equal? removed '())
  (check-equal? added '()))

(define (test-diff-reports-summary-and-body-changes)
  (define pinned (build-sample-snapshot))
  (define current
    (build-sample-snapshot
     #:report (string-replace sample-report "files failed:   0" "files failed:   1")
     #:atom-lines
     (append (drop-right (atom-lines-for-root sample-root) 1)
             (list "(InheritanceLink (ConceptNode \"Sub\") (ConceptNode \"ExtraBase\"))"))))
  (define-values (changes removed added) (diff-snapshots pinned current))
  (check-equal? changes '(("files failed" "0" "1")))
  (check-equal? (length removed) 1)
  (check-equal? (length added) 1)
  (check-true (string-contains? (first removed) "Base"))
  (check-true (string-contains? (first added) "ExtraBase")))

(module+ main
  (test-sha256-fips-vectors)
  (test-bytes->hex-pads-single-digits)
  (test-summary-extraction)
  (test-summary-missing-counter-fails-closed)
  (test-normalize-rewrites-root-and-sorts)
  (test-normalize-accepts-trailing-slash-root)
  (test-normalize-rejects-outside-repo-path)
  (test-snapshot-is-path-portable)
  (test-snapshot-is-order-insensitive)
  (test-snapshot-roundtrip)
  (test-snapshot-hash-format)
  (test-snapshot-format-freeze)
  (test-parse-without-header-marker-fails)
  (test-diff-of-identical-snapshots-is-empty)
  (test-diff-reports-summary-and-body-changes)
  (displayln "All baseline-check tests passed."))
