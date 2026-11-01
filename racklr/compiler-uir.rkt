#lang racket

;; racklr/compiler-uir.rkt
;;
;; The compiler as a UIR program: discover the compiler's own Racket modules,
;; lower them through racklr/lower-racket, run the core whole-program effect
;; inference over that UIR, and expose the annotated programs and report behind
;; the P6.1 Definition of Done — every compiler module lowers to a uir-module,
;; every named uir-fn carries an inferred effect row, and infer.rkt runs over
;; the compiler's own UIR with no second inference engine.
;;
;; The repo root is auto-discovered from this module's runtime path (its parent
;; contains racklr/ and effervesce/); `#:root DIR` overrides it. Discovery is
;; data, not a file list: every *.rkt under racklr/ and effervesce/ is a
;; compiler module except build output (compiled/), vendored grammars
;; (grammars-v4/), docs/examples trees, and package `info.rkt` files.

(require racket/list
         racket/path
         racket/runtime-path
         racklr/uir
         racklr/lower-racket
         racklr/effects/walk
         racklr/effects/infer
         racklr/effects/contract)

(provide repo-root
         compiler-source-paths
         compiler-programs
         compiler-effect-rows
         compiler-annotated-programs
         compiler-report
         compiler-report->string
         program-named-functions
         row->string)
;; ── repo root ──────────────────────────────────────────────────────

(define-runtime-path this-file "compiler-uir.rkt")
;; The checkout root: the parent of the racklr/ directory that holds this file.
(define (repo-root)
  (simplify-path (build-path (path-only this-file) 'up)))

;; Resolve (and validate) the root every public entry point works against.
(define (root-of maybe-root)
  (define root (simplify-path (path->complete-path (or maybe-root (repo-root)))))
  (if (and (directory-exists? (build-path root "racklr"))
           (directory-exists? (build-path root "effervesce")))
      root
      (error 'compiler-uir
             (string-append "not a racklr checkout root (expected racklr/ and "
                            "effervesce/ under ~a); run from the checkout with "
                            "PLTCOLLECTS=\"$(pwd):\" or pass #:root")
             root)))
;; ── source discovery ───────────────────────────────────────────────

;; Directory names that are never part of the compiler corpus: build output,
;; vendored grammars, docs, and example trees.
(define skipped-dir-names '("compiled" "grammars-v4" "docs" "examples"))

(define (rkt-source? entry)
  (and (regexp-match? #rx"[.]rkt$" (path->string entry))
       (not (equal? (path->string entry) "info.rkt"))))

(define (compiler-source-paths #:root [maybe-root #f])
  (define root (root-of maybe-root))
  (define found '())
  (define (visit-dir! dir)
    (for ([entry (in-list (directory-list dir))])
      (define p (build-path dir entry))
      (if (directory-exists? p)
          (if (member (path->string entry) skipped-dir-names) (void) (visit-dir! p))
          (if (rkt-source? entry)
              (set! found (cons (simplify-path p) found))
              (void)))))
  (for ([sub (in-list '("racklr" "effervesce"))])
    (visit-dir! (build-path root sub)))
  (sort found (λ (a b) (string<? (path->string a) (path->string b)))))
;; ── lowering and inference ────────────────────────────────────────

;; Component id: source path relative to the root without .rkt (e.g.
;; 'racklr/effects/infer). Top-level names are mangled "<component>::<name>"
;; against exactly this id, which lets the core fixpoint resolve cross-file edges.
(define (path->component root source-path)
  (string->symbol
   (regexp-replace #rx"[.]rkt$"
                   (path->string (find-relative-path root source-path))
                   "")))

(define (compiler-programs #:root [maybe-root #f])
  (define root (root-of maybe-root))
  (lower-racket-modules (compiler-source-paths #:root root) #:root root))

(define (compiler-effect-rows #:root [maybe-root #f]
                              #:programs [provided #f]
                              #:unknown-tally [tally #f])
  (define programs (or provided (compiler-programs #:root (root-of maybe-root))))
  (if tally
      (infer-effects-all programs #:unknown-tally tally)
      (infer-effects-all programs)))
;; ── named functions and annotation ────────────────────────────────

;; Every named uir-fn node reachable from a program, in traversal order; nested
;; functions keep bare names while top-level names are "<component>::<name>".
(define (program-named-fn-nodes program)
  (define nodes '())
  (walk-uir program
            (λ (n)
              (if (and (uir-fn? n) (uir-symbol? (uir-fn-name n)))
                  (set! nodes (cons n nodes))
                  (void))))
  (reverse nodes))

;; Every named function's symbol, in traversal order.
(define (program-named-functions program)
  (map (λ (n) (string->symbol (uir-symbol-name (uir-fn-name n))))
       (program-named-fn-nodes program)))

;; Attach the inferred row to every named uir-fn's effects slot, recursing
;; through the node kinds the Racket front end emits (fn/call/block/if/set!/
;; class/field/list/literal/module, plus plain Racket lists for module bodies).
;; struct-copy preserves every other field, so the attached row is the exact
;; inferred row (tracked `unknown` entries included) and no empty row is ever
;; invented for an uninferred name.
(define (annotate-nodes nodes comp rows)
  (map (λ (n) (annotate-node n comp rows)) nodes))

(define (annotate-fn n comp rows)
  (define name (uir-fn-name n))
  (define nm (and (uir-symbol? name) (string->symbol (uir-symbol-name name))))
  (define row (and nm (hash-ref rows (cons comp nm) #f)))
  (struct-copy uir-fn n
    [params (annotate-nodes (uir-fn-params n) comp rows)]
    [body (annotate-node (uir-fn-body n) comp rows)]
    [return-type (annotate-node (uir-fn-return-type n) comp rows)]
    [effects (if row row (annotate-node (uir-fn-effects n) comp rows))]))

(define (annotate-node n comp rows)
  (cond
    ;; A module body is a plain Racket list in the Racket front end's UIR.
    [(list? n) (annotate-nodes n comp rows)]
    [(uir-fn? n) (annotate-fn n comp rows)]
    [(uir-typed-param? n)
     (struct-copy uir-typed-param n
                  [type (annotate-node (uir-typed-param-type n) comp rows)]
                  [default (annotate-node (uir-typed-param-default n) comp rows)])]
    [(uir-module? n)
     (struct-copy uir-module n
                  [imports (annotate-nodes (uir-module-imports n) comp rows)]
                  [exports (annotate-nodes (uir-module-exports n) comp rows)]
                  [body (annotate-node (uir-module-body n) comp rows)])]
    [(uir-block? n)
     (struct-copy uir-block n [stmts (annotate-nodes (uir-block-stmts n) comp rows)])]
    [(uir-call? n)
     (struct-copy uir-call n
                  [callee (annotate-node (uir-call-callee n) comp rows)]
                  [args (annotate-nodes (uir-call-args n) comp rows)])]
    [(uir-if? n)
     (struct-copy uir-if n
                  [test (annotate-node (uir-if-test n) comp rows)]
                  [then (annotate-node (uir-if-then n) comp rows)]
                  [else (annotate-node (uir-if-else n) comp rows)])]
    [(uir-let? n)
     (struct-copy uir-let n
                  [value (annotate-node (uir-let-value n) comp rows)]
                  [body (annotate-node (uir-let-body n) comp rows)])]
    [(uir-set!? n)
     (struct-copy uir-set! n [value (annotate-node (uir-set!-value n) comp rows)])]
    [(uir-class? n)
     (struct-copy uir-class n
                  [super (annotate-nodes (uir-class-super n) comp rows)]
                  [fields (annotate-nodes (uir-class-fields n) comp rows)]
                  [methods (annotate-nodes (uir-class-methods n) comp rows)])]
    [(uir-field? n)
     (struct-copy uir-field n
                  [type (annotate-node (uir-field-type n) comp rows)]
                  [init (annotate-node (uir-field-init n) comp rows)])]
    [(uir-list? n)
     (struct-copy uir-list n [items (annotate-nodes (uir-list-items n) comp rows)])]
    [(uir-import? n)
     (struct-copy uir-import n [source (annotate-node (uir-import-source n) comp rows)])]
    [else n]))

(define (compiler-annotated-programs #:root [maybe-root #f]
                                     #:programs [provided #f]
                                     #:rows [rows #f])
  (define programs (or provided (compiler-programs #:root (root-of maybe-root))))
  (define inferred (or rows (compiler-effect-rows #:programs programs)))
  (for/list ([cp (in-list programs)])
    (cons (car cp) (annotate-node (cdr cp) (car cp) inferred))))
;; ── report ────────────────────────────────────────────────────────

(define (named-keys programs)
  (append*
   (for/list ([cp (in-list programs)])
     (for/list ([nm (in-list (program-named-functions (cdr cp)))])
       (cons (car cp) nm)))))

(define (has-unknown-entry? row)
  (for/or ([e (in-list (uir-effect-row-entries row))]) (eq? (effect-name-symbol e) 'unknown)))

(define (compiler-report #:root [maybe-root #f]
                         #:programs [provided #f])
  (define root (root-of maybe-root))
  (define programs (or provided (compiler-programs #:root root)))
  (define tally (box '()))
  (define rows (infer-effects-all programs #:unknown-tally tally))
  (define named (named-keys programs))
  (define missing (filter (λ (k) (not (hash-has-key? rows k))) named))
  (define histogram (make-hash))
  (define modalities (make-hash))
  (define unknown-fns
    (for/sum ([(k row) (in-hash rows)])
      (for ([e (in-list (uir-effect-row-entries row))])
        (hash-update! histogram (effect-name-symbol e) add1 0)
        (hash-update! modalities (uir-effect-entry-modality e) add1 0))
      (if (has-unknown-entry? row) 1 0)))
  (define per-component (make-hash))
  (for ([cp (in-list programs)])
    (hash-set! per-component
               (car cp)
               (length (program-named-functions (cdr cp)))))
  ;; Count named functions whose effects slot actually holds a row after
  ;; write-back (the report's functions-with-row).
  (define annotated (compiler-annotated-programs #:programs programs #:rows rows))
  (define with-row
    (for*/sum ([cp (in-list annotated)]
               [n (in-list (program-named-fn-nodes (cdr cp)))])
      (if (uir-effect-row? (uir-fn-effects n)) 1 0)))
  (define row-count (hash-count rows))
  (hash 'root (path->string root)
        'modules (length programs)
        'functions (length named)
        'rows row-count
        'functions-with-row with-row
        'missing-row-count (length missing)
        'unknown-function-count unknown-fns
        'unknown-function-ratio (if (zero? row-count) 0.0 (/ unknown-fns row-count))
        'unknown-site-count (length (unbox tally))
        'effect-histogram histogram
        'entry-modality-histogram modalities
        'per-component per-component))
;; ── rendering ─────────────────────────────────────────────────────

;; A row as one line: effect names in canonical order; a tracked modality other
;; than 'may is shown explicitly so `unknown` never looks like a real effect.
(define (row->string row)
  (cond
    [(not (uir-effect-row? row)) "(no row)"]
    [(null? (uir-effect-row-entries row)) "pure"]
    [else
     (string-join
      (for/list ([e (in-list (uir-effect-row-entries row))])
        (define nm (symbol->string (effect-name-symbol e)))
        (define m (uir-effect-entry-modality e))
        (if (eq? m 'may) nm (format "~a[~a]" nm m)))
      ", ")]))

(define (symbol<?* a b) (string<? (symbol->string a) (symbol->string b)))

(define (histogram->string h)
  (if (hash-empty? h)
      "(empty)"
      (string-join
       (for/list ([p (in-list
                       (sort (hash->list h)
                             (λ (a b)
                               (if (= (cdr a) (cdr b))
                                   (symbol<?* (car a) (car b))
                                   (> (cdr a) (cdr b))))))])
         (format "~a:~a" (car p) (cdr p)))
       " ")))

(define (ratio->percent r)
  (if (and (real? r) (not (infinite? r)) (not (nan? r)))
      (format "~a%" (real->decimal-string (* 100.0 r) 1))
      "?"))

(define (compiler-report->string report)
  (define (get k default) (hash-ref report k default))
  (define components
    (sort (hash->list (get 'per-component (hash)))
          (λ (a b) (symbol<?* (car a) (car b)))))
  (string-append
   "compiler as UIR — inferred effect rows\n"
   (format "  root:                  ~a\n" (get 'root "?"))
   (format "  modules:               ~a   named functions: ~a   inferred rows: ~a\n"
           (get 'modules 0) (get 'functions 0) (get 'rows 0))
   (format "  functions with row:    ~a\n" (get 'functions-with-row 0))
   (format "  functions missing row: ~a\n" (get 'missing-row-count 0))
   (format "  unknown functions:     ~a (~a of rows)\n"
           (get 'unknown-function-count 0)
           (ratio->percent (get 'unknown-function-ratio 0.0)))
   (format "  unknown call sites:    ~a\n" (get 'unknown-site-count 0))
   (format "  effect histogram:      ~a\n" (histogram->string (get 'effect-histogram (hash))))
   (format "  entry modalities:      ~a\n" (histogram->string (get 'entry-modality-histogram (hash))))
   "  functions per component:\n"
   (string-join (for/list ([p (in-list components)])
                  (format "    ~a: ~a\n" (car p) (cdr p)))
                "")))
