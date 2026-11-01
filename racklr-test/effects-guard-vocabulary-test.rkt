#lang racket

;; The emitted runtime guards' vocabulary must match the classifier's lexicon.
;;
;; A scope guard decides read vs write from DATA verb tables; the emitters render
;; those tables into their preambles. This suite keeps the two sides in sync: the
;; rendered Python/JS lists equal the classifier's db+io verb vocabulary, so a
;; lexicon change gains enforcement automatically instead of drifting from a
;; hand-written copy in engine code.

(require rackunit
         racket/set
         racklr/uir
         racklr/emit-python
         racklr/emit-javascript
         racklr/effects/guard-vocabulary
         racklr/effects/imports/library-config)

(define (sym s) (uir-symbol s))

(define (row)
  (uir-effect-row
   (list (uir-effect-entry (sym "llm") 'may '() 5 #f))))

;; A program with a row forces each emitter to prepend its guard preamble.
(define (python-preamble)
  (emit-python
   (uir-block (list (uir-fn (sym "f") '() (uir-block (list (uir-null)))
                            #f (row))))))

(define (javascript-preamble)
  (emit-javascript
   (uir-block (list (uir-set! (sym "f")
                              (uir-fn #f '() (uir-block (list (uir-null)))
                                      #f (row)))))))

;; ── extraction ──────────────────────────────────────────────────────

;; regexp-match* returns whole matches, so trim the decoration around each word.
(define (quoted-words rx text start-trim end-trim)
  (for/list ([m (in-list (regexp-match* rx text))])
    (substring m start-trim (- (string-length m) end-trim))))

(define (python-tuple-strings text name)
  (define m (regexp-match (pregexp (format "~a = \\(([^)]*)\\)" name)) text))
  (and m (quoted-words #px"\"[^\"]+\"" (second m) 1 1)))

(define (js-lookup-keys text name)
  (define m (regexp-match (pregexp (format "var ~a = \\{([^}]*)\\};" name)) text))
  (and m (quoted-words #px"\"[^\"]+\":1" (second m) 1 3)))

(define (js-array-strings text name)
  (define m (regexp-match (pregexp (format "var ~a = \\[([^]]*)\\];" name)) text))
  (and m (quoted-words #px"\"[^\"]+\"" (second m) 1 1)))

;; ── the lexicon is the single source ────────────────────────────────

(define (classifier-read-verbs)
  (sort (set->list (set-union db-read-methods io-read-verbs)) string<?))

(define (classifier-write-verbs)
  (sort (set->list (set-union db-write-methods io-write-verbs)) string<?))

(check-equal? (scope-read-verbs) (classifier-read-verbs)
              "the guard's read vocabulary is the classifier's read verbs")
(check-equal? (scope-write-verbs) (classifier-write-verbs)
              "the guard's write vocabulary is the classifier's write verbs")

;; ── both emitted preambles render exactly that vocabulary ───────────

(define py (python-preamble))
(define js (javascript-preamble))

(check-equal? (python-tuple-strings py "__write_ops") (scope-write-verbs)
              "the Python preamble embeds the write vocabulary")
(check-equal? (python-tuple-strings py "__read_ops") (scope-read-verbs)
              "the Python preamble embeds the read vocabulary")
(check-equal? (js-lookup-keys js "__writeOps") (scope-write-verbs)
              "the JS preamble embeds the write vocabulary")
(check-equal? (js-lookup-keys js "__readOps") (scope-read-verbs)
              "the JS preamble embeds the read vocabulary")

;; The same vocabulary in both targets, so the two guards enforce one contract.
(check-equal? (python-tuple-strings py "__write_ops")
              (js-lookup-keys js "__writeOps")
              "Python and JS agree on the write vocabulary")
(check-equal? (python-tuple-strings py "__read_ops")
              (js-lookup-keys js "__readOps")
              "Python and JS agree on the read vocabulary")

;; A verb the classifier emits but the old hand-maintained lists missed is now
;; enforced everywhere (this is the drift the shared vocabulary prevents).
(check-not-false (member "fetchall" (scope-read-verbs)) "lexicon read verbs are included")
(check-not-false (member "bulk_update" (scope-write-verbs)) "lexicon write verbs are included")

;; ── modality set, likewise ──────────────────────────────────────────

(check-equal? (python-tuple-strings py "_valid_modalities") (guard-modalities)
              "the Python preamble validates the closed modality set")
(check-equal? (js-array-strings js "_valid") (guard-modalities)
              "the JS preamble validates the closed modality set")

(displayln "All guard-vocabulary tests passed.")
