#lang racket

;; tools/loss-artifact.rkt — write the round-trip verification artifact.
;;
;; The round-trip harness verifies the docs/examples corpus within the declared
;; loss model (racklr/loss-model.rkt) and writes every result here, including
;; undeclared losses, as one machine-readable JSON artifact. A violation is
;; therefore a durable record with evidence, not a line that scrolls past.
;;
;; Usage (from the repository root):
;;
;;   racket tools/loss-artifact.rkt          # print the declared model digest
;;
;; The harness requires this module; the digest pins the exact model the
;; artifact was produced against, so a model edit and a result cannot be
;; confused.

(require racket/file
         racket/list
         racket/string
         json
         racklr/loss-model
         "sha256.rkt")

(provide loss-model-digest
         artifact-path
         write-loss-artifact!
         jsexpr-sanitize)

(define (loss-model-digest)
  (string-append
   "sha256:"
   (sha256-hex (string->bytes/utf-8 (loss-model-canonical-string)))))

;; The artifact lives beside the baseline for easy collection by CI; it is
;; build output, not a source file (see .gitignore).
(define (artifact-path repo-root)
  (build-path repo-root "loss-artifacts" "round-trip.json"))

;; jsexpr->string accepts symbols for keys but their values are written as
;; strings only when they are symbols with a printable form; be explicit and
;; convert every symbol to its name so the artifact is unambiguous.
(define (jsexpr-sanitize v)
  (cond [(symbol? v) (symbol->string v)]
        [(hash? v) (for/hash ([(k x) (in-hash v)]) (values k (jsexpr-sanitize x)))]
        [(list? v) (map jsexpr-sanitize v)]
        [(vector? v) (vector-map jsexpr-sanitize v)]
        [else v]))

(define (write-loss-artifact! path payload)
  (define out (jsexpr-sanitize payload))
  (make-directory* (path-only path))
  (call-with-output-file path
    (λ (port) (write-json out port) (newline port))
    #:exists 'replace)
  path)

(module+ main
  (define run-file (find-system-path 'run-file))
  (define root (if (path? run-file)
                   (simplify-path (build-path run-file 'up 'up))
                   (current-directory)))
  (printf "loss model digest: ~a\n" (loss-model-digest))
  (printf "artifact path: ~a\n" (artifact-path root)))
