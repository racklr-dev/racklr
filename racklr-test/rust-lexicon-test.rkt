#lang racket

;; racklr-test/rust-lexicon-test.rkt — the Rust token lexicon (P2.2/P2.3).
;;
;; Acceptance claim: a non-Python host language supplies its own token lexicon
;; as DATA and injects it into the language-agnostic classifier, with zero
;; edits to engine code. This suite proves:
;;   * the default (Python/Django) lexicon does not classify Rust callees;
;;   * the injected Rust lexicon classifies io / network / process / time;
;;   * the Rust lexicon is complete — Python tokens do not leak into it;
;;   * rows attached through the lexicon are preserved by the engine's
;;     annotate-effects pass (the zero-edit write-back path);
;;   * classification engine files contain no Rust literals (grep proof).

(require racket/file
         rackunit
         rackunit/text-ui
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/frontends
         racklr/frontends/rust/lexicon
         racklr/effects/effect-roots
         racklr/effects/pure-roots
         racklr/effects/library-tokens
         racklr/effects/infer
         racklr/effects/contract)

(define (callee path) (uir-var (uir-symbol path)))

;; ── Injection: default vs Rust lexicon ──────────────────────────────

(define rust-callee-paths
  '("std::fs::read_to_string" "std::fs::write" "std::io::stdin"
    "std::net::TcpStream::connect" "std::net::UdpSocket::bind"
    "reqwest::get" "std::process::Command::new" "std::time::Instant::now"
    "std::env::var"))

(define suite
  (test-suite
   "Rust token lexicon injection"

   (test-case "default Python lexicon does not classify Rust callees"
     (for ([path (in-list rust-callee-paths)])
       (check-false (direct-effect-of-callee (callee path) default-effect-tokens)
                    (format "default lexicon must not classify ~a" path))))

   (test-case "injected Rust lexicon classifies Rust callees"
     (check-equal? (direct-effect-of-callee (callee "std::fs::read_to_string")
                                            rust-effect-tokens)
                   'io)
     (check-equal? (direct-effect-of-callee (callee "std::fs::write") rust-effect-tokens) 'io)
     (check-equal? (direct-effect-of-callee (callee "std::net::TcpStream::connect")
                                            rust-effect-tokens)
                   'network)
     (check-equal? (direct-effect-of-callee (callee "reqwest::get") rust-effect-tokens) 'network)
     (check-equal? (direct-effect-of-callee (callee "std::process::Command::new")
                                            rust-effect-tokens)
                   'process)
     (check-equal? (direct-effect-of-callee (callee "std::time::Instant::now")
                                            rust-effect-tokens)
                   'time)
     (check-equal? (direct-effect-of-callee (callee "println!") rust-effect-tokens) 'io)
     (check-equal? (rust-direct-effect (callee "std::env::var")) 'io))

   (test-case "the Rust lexicon is complete: no Python tokens leak in"
     ;; These are Python/Django lexicon entries; a Rust host program must not
     ;; inherit them.
     (check-false (direct-effect-of-callee
                   (uir-get (uir-var (uir-symbol "llm")) (uir-string "query"))
                   rust-effect-tokens))
     (check-false (direct-effect-of-callee
                   (uir-get (uir-var (uir-symbol "requests")) (uir-string "get"))
                   rust-effect-tokens))
     (check-false (direct-effect-of-callee (callee "open") rust-effect-tokens)))

   (test-case "pure Rust helpers classify as pure, not unknown"
     (check-true (pure-stdlib-bare-callee? (callee "format!") rust-effect-tokens))
     (check-false (pure-stdlib-bare-callee? (callee "format!") default-effect-tokens))
     (check-equal? (rust-classify-nonedge (callee "format!")) '(pure . #f))
     (check-equal? (rust-classify-nonedge (callee "std::mem::swap")) '(pure . #f))
     (check-equal? (rust-classify-nonedge (callee "std::fs::read_to_string")) '(effect . io))
     (check-equal? (rust-classify-nonedge (callee "my_local_function")) '(unknown . #f)))

   (test-case "end to end: lower Rust, attach rows through the injected lexicon"
     (define spec
       (for/first ([s (discover-front-ends)] #:when (eq? (front-end-name s) 'rust)) s))
     (define-values (parse tokenize tok-type tok-value)
       (gen-and-load (front-end-grammar-file spec)))
     (define program
       ((front-end-lower spec)
        (parse (string-append
                "fn read_it() { let s = std::fs::read_to_string(\"f\"); }\n"
                "fn greet() { println!(\"hi\"); }\n"))
        tok-type tok-value))
     (define (rows-of uir name)
       (for/first ([fn (in-list (filter uir-fn? (uir-block-stmts uir)))]
                   #:when (equal? (uir-symbol-name (uir-fn-name fn)) name))
         (and (uir-fn-effects fn)
              (map effect-name-symbol
                   (uir-effect-row-entries (uir-fn-effects fn))))))
     ;; Before injection the functions carry no row (effects stay #f at lowering).
     (check-false (rows-of program "read_it"))
     (check-false (rows-of program "greet"))
     ;; Attaching through the injected lexicon produces the expected rows.
     (define annotated (rust-attach-effects program))
     (check-equal? (rows-of annotated "read_it") '(io))
     (check-equal? (rows-of annotated "greet") '(io))
     ;; The engine's annotate-effects preserves a hand-written row.
     (check-equal? (rows-of (annotate-effects annotated) "read_it") '(io))
     (cleanup))

   (test-case "no Rust literals in language-agnostic engine code"
     ;; The zero-core-coupling half of the proof: the engine files that classify
     ;; and lower carry no Rust tokens. Adding a language did not edit them.
     (define engine-files
       '("racklr/effects/classify.rkt"
         "racklr/effects/classify-all.rkt"
         "racklr/effects/effect-roots.rkt"
         "racklr/effects/pure-roots.rkt"
         "racklr/effects/shape.rkt"
         "racklr/effects/receiver-type.rkt"
         "racklr/effects/infer.rkt"
         "racklr/effects/rows.rkt"
         "racklr/effects/imports.rkt"
         "racklr/effects/imports/effect-tag.rkt"
         "racklr/effects/imports/library-config.rkt"
         "racklr/effects/library-tokens.rkt"
         "racklr/uir/serialize.rkt"
         "racklr/uir/version.rkt"
         "racklr/gend-parser.rkt"
         "racklr/g4-parse.rkt"
         "racklr/tree.rkt"
         "racklr/frontends.rkt"))
     (define rust-token-rx #rx"\\bRust\\b|\\brust\\b|std::|TcpStream|UdpSocket|println!|read_to_string")
     (for ([rel (in-list engine-files)])
       (define path (build-path (repository-root) rel))
       (check-true (file-exists? path) (format "engine file exists: ~a" rel))
       (define text (file->string path))
       (check-false (regexp-match rust-token-rx text)
                    (format "engine file carries no Rust literal: ~a" rel))))))

(exit (run-tests suite))
