#lang racket

;; tools/frontend-path-additions-check.rkt — mechanical path check for P2.3.
;;
;; The P2.3 acceptance claim is that adding a Rust front end touches zero core
;; files: grammar plus lowering hook plus token lexicon only.
;;
;; SCOPE: this tool checks PATH NOVELTY only. Against a base ref (default
;; `origin/main`) it asserts:
;;
;;   * every path changed between merge-base(base, HEAD) and HEAD is ADDED;
;;   * each added path is inside the declared new-front-end surface.
;;
;; It does NOT prove the semantic claim: a dispatch branch added inside a *new*
;; file would pass here. That "no engine edit in substance" property still
;; needs review (the diff is small enough to read). The name reflects the
;; narrower mechanical guarantee.
;;
;; Usage (from anywhere inside the checkout):
;;   racket tools/frontend-path-additions-check.rkt [base-ref]
;;
;; Exit 0 prints "frontend-path-additions OK: path novelty only: ..." and the
;; added paths; exit 1 prints the offending status lines.

(require racket/file
         racket/list
         racket/port
         racket/string)

;; Run from anywhere inside the checkout; the check walks up to `.git`.
(define (repository-root)
  (let loop ([p (path->complete-path (current-directory))])
    (cond [(file-exists? (build-path p ".git")) p]
          [(equal? p (path-only p)) (error 'repository-root "not inside a git checkout")]
          [else (loop (path-only p))])))

(define root (repository-root))

(define git-executable
  (or (find-executable-path "git")
      (error 'frontend-path-additions-check "git not found on PATH")))

(define (git . args)
  (with-output-to-string
    (λ ()
      (parameterize ([current-directory root])
        (apply system* git-executable args)))))

(define (git-status-lines base)
  ;; Three-dot diff: changes introduced on HEAD since the merge base.
  (git "diff" "--name-status" (format "~a...HEAD" base)))

(define base-arg
  (let ([args (vector->list (current-command-line-arguments))])
    (if (pair? args) (car args) "origin/main")))

(define merge-base
  (string-trim (git "merge-base" base-arg "HEAD")))

;; Declared surface of the Rust front end (all new files).
(define allowed-prefixes
  '("grammars/rust/"
    "racklr/lower-rust"
    "racklr/frontends.rkt"
    "racklr/frontends/"
    "racklr-test/lower-rust-test.rkt"
    "racklr-test/rust-lexicon-test.rkt"
    "tools/frontend-path-additions-check.rkt"))

(define (allowed? path)
  (for/or ([prefix (in-list allowed-prefixes)])
    (string-prefix? path prefix)))

(define lines
  (filter (λ (l) (not (string=? l "")))
          (string-split (git-status-lines merge-base) "\n")))

(define entries
  (for/list ([line (in-list lines)])
    (define parts (string-split line "\t"))
    (cons (first parts) (second parts))))

(define modified
  (for/list ([e (in-list entries)] #:unless (string=? (car e) "A"))
    e))

(define added
  (for/list ([e (in-list entries)] #:when (string=? (car e) "A"))
    (cdr e)))

(define outside-surface (filter (λ (p) (not (allowed? p))) added))

(cond
  [(and (null? modified) (null? outside-surface))
   (printf "frontend-path-additions OK: path novelty only: base ~a (merge-base ~a)\n" base-arg
           (substring merge-base 0 12))
   (for ([p (in-list (sort added string<?))])
     (printf "  added  ~a\n" p))
   (exit 0)]
  [else
   (eprintf "frontend-path-additions FAILED: changed paths are not pure additions\n")
   (for ([e (in-list modified)])
     (eprintf "  modified/deleted: ~a ~a\n" (car e) (cdr e)))
   (for ([p (in-list outside-surface)])
     (eprintf "  added outside the declared front-end surface: ~a\n" p))
   (exit 1)])
