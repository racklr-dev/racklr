#lang racket

;; racklr/frontends/rust/lexicon.rkt — the Rust token lexicon (DATA) plus the
;; small front-end-owned adapter that classifies Rust callees.
;;
;; This is the "token lexicon" half of the Rust front end: concrete Rust
;; module/method/builtin paths mapped to the language-neutral effect domains.
;; It is injected into the existing language-agnostic classifier through its
;; documented optional `[tokens default-effect-tokens]` arguments — no engine
;; file is edited, and the default Python/Django lexicon is never consulted
;; for a Rust host program.
;;
;; Encoding: the Rust lowering keeps a qualified path (`std::fs::read_to_string`)
;; inside ONE `uir-var`/`uir-symbol` name with `::` preserved. shape.rkt's
;; `dotted-root` does not split on "::", so the whole path arrives as the
;; `stdlib-root-effects` key — which is how a multi-segment module path is
;; expressed without adding a new lexicon field.

(require racket/hash
         racket/set
         racklr/uir
         racklr/effects/shape
         racklr/effects/library-tokens
         racklr/effects/effect-roots
         racklr/effects/pure-roots
         racklr/effects/rows)

(provide rust-effect-tokens
         rust-callee-path
         rust-direct-effect
         rust-classify-nonedge
         rust-attach-effects)

;; ── Data: Rust path → effect domain ─────────────────────────────────

(define rust-path-effects
  (hash '|std::fs::read_to_string| 'io
        '|std::fs::read| 'io
        '|std::fs::write| 'io
        '|std::fs::read_dir| 'io
        '|std::fs::remove_file| 'io
        '|std::fs::metadata| 'io
        '|std::fs::File::open| 'io
        '|std::fs::File::create| 'io
        '|std::io::stdin| 'io
        '|std::io::stdout| 'io
        '|std::io::stderr| 'io
        '|std::net::TcpStream::connect| 'network
        '|std::net::TcpListener::bind| 'network
        '|std::net::UdpSocket::bind| 'network
        '|std::net::UdpSocket::connect| 'network
        '|reqwest::get| 'network
        '|reqwest::blocking::get| 'network
        '|std::process::Command::new| 'process
        '|std::process::exit| 'process
        '|std::process::abort| 'process
        '|std::process::id| 'process
        '|std::time::Instant::now| 'time
        '|std::time::SystemTime::now| 'time
        '|std::thread::sleep| 'time
        '|std::env::var| 'io
        '|std::env::args| 'io))

(define rust-pure-paths
  (set '|std::time::Duration::from_secs|
       '|std::time::Duration::from_millis|
       '|std::mem::swap| '|std::mem::take| '|std::mem::replace|
       '|std::cmp::max| '|std::cmp::min|
       '|std::str::from_utf8| '|std::ptr::null|))

(define rust-io-methods
  (set "read_to_string" "read_exact" "read_line" "read_to_end"
       "write_all" "write_fmt" "flush" "sync_all" "recv" "send"))

(define rust-time-methods
  (set "elapsed" "as_secs" "as_millis" "as_micros" "as_nanos"
       "checked_add" "checked_sub" "checked_duration_since"))

(define rust-io-builtins
  (set "println!" "print!" "eprintln!" "eprint!" "stdin" "stdout" "stderr"))

(define rust-pure-bare-names
  (set "format!" "vec!" "assert!" "assert_eq!" "assert_ne!"
       "debug_assert!" "write!" "todo!" "unreachable!" "matches!"
       "include_str!" "include_bytes!" "concat!" "stringify!"))

;; A COMPLETE lexicon: no Python/Django token leaks into a Rust host program.
(define rust-effect-tokens
  (effect-tokens
   (set) (set)                                  ; llm-roots db-roots
   (set "reqwest" "hyper" "tungstenite")         ; network-roots
   (set)                                         ; network-names
   (set) (set)                                   ; cache-roots cache-names
   (set) (set) #f                                ; orm-methods generic-by-name?
   (set) (set)                                   ; builtin-type-roots module-roots
   rust-io-builtins                              ; io-builtins
   rust-path-effects                             ; stdlib-root-effects (exact paths)
   rust-io-methods                               ; io-methods
   (set)                                         ; email-methods
   rust-time-methods                             ; time-methods
   (hash)                                        ; root-method-effects
   (hash)                                        ; stdlib-type-methods
   (set)                                         ; pure-stdlib-roots
   (hash)                                        ; pure-stdlib-methods
   (set) (set)                                   ; framework-exception declarative
   rust-pure-bare-names                          ; pure-stdlib-bare-names
   (set) (set)                                   ; gettext pure-config
   (set) (set) (set)                             ; python-builtins dunders str
   (set)                                         ; pure-container-methods
   (hash) (hash)                                 ; module-result-types receiver-modules
   (set)))                                       ; racket-builtins (host-neutral: none)

;; ── Adapter (front-end-owned; no engine edits) ──────────────────────

(define (field-name f)
  (cond [(uir-symbol? f) (uir-symbol-name f)]
        [(uir-string? f) (uir-string-value f)]
        [else #f]))

;; Reconstruct a full Rust path from either a single `a::b::c` symbol or a
;; uir-get chain (both are supported so the lowering may evolve).
(define (rust-callee-path c)
  (let loop ([c c])
    (cond
      [(uir-get? c)
       (define base (loop (uir-get-base c)))
       (define f (field-name (uir-get-field c)))
       (and base f (format "~a::~a" base f))]
      [(uir-var? c) (uir-symbol-name (uir-var-name c))]
      [(uir-symbol? c) (uir-symbol-name c)]
      [else #f])))

;; Exact full-path lookup first, then the existing language-agnostic engine
;; with the injected lexicon.
(define (rust-direct-effect c [tokens rust-effect-tokens])
  (define path (rust-callee-path c))
  (or (and path
           (hash-ref (effect-tokens-stdlib-root-effects tokens)
                     (string->symbol path) #f))
      (direct-effect-of-callee c tokens)))

;; `(effect . name)`, `(pure . #f)` or `(unknown . #f)` for one callee.
(define (rust-classify-nonedge c [tokens rust-effect-tokens])
  (define eff (rust-direct-effect c tokens))
  (define path (rust-callee-path c))
  (cond
    [eff (cons 'effect eff)]
    [(or (pure-stdlib-callee? c tokens)
         (pure-stdlib-bare-callee? c tokens)
         (and path (set-member? rust-pure-paths (string->symbol path))))
     (cons 'pure #f)]
    [else (cons 'unknown #f)]))

;; Attach rows to named functions from the calls in their bodies. A row written
;; here is preserved by the engine's annotate-effects (hand-written rows win),
;; which is the zero-edit write-back path into the shared pipeline.
(define (rust-attach-effects program [tokens rust-effect-tokens])
  (define (callee-effects node)
    (cond
      [(uir-call? node)
       (define eff (rust-direct-effect (uir-call-callee node) tokens))
       (define nested (append* (map callee-effects (uir-call-args node))))
       (append (if eff (list eff) '()) nested)]
      [(uir-block? node) (append* (map callee-effects (uir-block-stmts node)))]
      [(uir-list? node) (append* (map callee-effects (uir-list-items node)))]
      [(uir-record? node) (append* (map (λ (e) (callee-effects (cdr e)))
                                        (uir-record-entries node)))]
      [(uir-get? node) (callee-effects (uir-get-base node))]
      [(uir-set!? node) (append (callee-effects (uir-set!-name node))
                                (callee-effects (uir-set!-value node)))]
      [(uir-ann-set!? node) (append (callee-effects (uir-ann-set!-lhs node))
                                    (callee-effects (uir-ann-set!-value node)))]
      [(uir-new? node) (append* (map callee-effects (uir-new-args node)))]
      [(uir-if? node) (append (callee-effects (uir-if-test node))
                              (callee-effects (uir-if-then node))
                              (callee-effects (uir-if-else node)))]
      [(uir-return? node) (callee-effects (uir-return-value node))]
      [(uir-while? node) (append (callee-effects (uir-while-test node))
                                 (callee-effects (uir-while-body node)))]
      [(uir-for-each? node) (append (callee-effects (uir-for-each-iterable node))
                                    (callee-effects (uir-for-each-body node)))]
      [(uir-fn? node) (callee-effects (uir-fn-body node))]
      [else '()]))
  (define (walk n)
    (cond
      [(uir-fn? n)
       (define effs (callee-effects (uir-fn-body n)))
       (define n* (if (null? effs) n
                      (struct-copy uir-fn n [effects (effects->row effs)])))
       (struct-copy uir-fn n* [body (walk (uir-fn-body n*))])]
      [(uir-block? n) (struct-copy uir-block n [stmts (map walk (uir-block-stmts n))])]
      [(uir-class? n) (struct-copy uir-class n
                                   [methods (map (λ (m)
                                                   (struct-copy uir-method m
                                                                [body (walk (uir-method-body m))]))
                                                 (uir-class-methods n))])]
      [(uir-module? n) (struct-copy uir-module n [body (walk (uir-module-body n))])]
      [else n]))
  (walk program))
