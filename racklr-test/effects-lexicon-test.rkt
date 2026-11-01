#lang racket

;; racklr-test/effects-lexicon-test.rkt
;;
;; The injectable-lexicon / language-knowledge-as-data gate for P2.2.
;;
;;   1. The lexicon SHAPE is host-neutral: the default is the Python/Django
;;      lexicon; a non-Python host supplies its own value as data.
;;   2. Injection works end to end: `infer-effects` / `infer-effects-all` /
;;      `classify-call` accept `#:lexicon`, and a JavaScript lexicon classifies
;;      JavaScript calls with zero engine edits, while the default Python
;;      lexicon leaves the same calls `unknown`.
;;   3. The host dialect travels in the lexicon: a JS `this` receiver resolves
;;      through the same receiver-type/edge machinery that resolves `self`.
;;   4. The grep guard: the core classification engine carries no
;;      library-specific name literals; they live only in the lexicon DATA
;;      modules. (UIR encoding names — `dot`, `=>`, `async-fn`, ... — are
;;      front-end construct encodings shared by the lowerers, handled by the
;;      synthetic-callee set, and are deliberately not "library knowledge".)

(require rackunit
         (rename-in rackunit/text-ui [run-tests run-test-suite])
         racket/file
         racket/path
         racket/list
         racklr/uir
         racklr/effects/infer
         racklr/effects/classify
         racklr/effects/contract
         racklr/effects/lexicon
         racklr/effects/library-tokens
         racklr/effects/imports
         racklr/effects/lexicons/python
         racklr/effects/lexicons/javascript)

;; ── helpers ─────────────────────────────────────────────────────────

(define (v s) (uir-var (uir-symbol s)))
(define (sym s) (uir-symbol s))
(define (dot b f) (uir-get b (sym f)))
(define (jsdot b f) (uir-call (sym "dot") (list b (sym f))))
(define (fn name body) (uir-fn (sym name) '() body #f #f))
(define (prog . stmts) (uir-block stmts))
(define (class-of name methods) (uir-class (sym name) '() '() methods))
(define (method-of name body) (uir-method (sym name) '() body 'public))

(define (row-names row)
  (sort (map (λ (e) (string->symbol (uir-symbol-name (uir-effect-entry-name e))))
             (uir-effect-row-entries row))
        symbol<?))

(define (js-call b f) (uir-call (jsdot (v b) f) '()))

;; ── 1. the lexicon shape and the two hosts ──────────────────────────

(define (test-default-lexicon-is-python)
  (check-true (lexicon? default-lexicon))
  (check-eq? (lexicon-name default-lexicon) 'python)
  (check-eq? (lexicon-name python-lexicon) 'python)
  (check-equal? (lexicon-tokens default-lexicon) default-effect-tokens)
  (check-true (set-member? (tokens-self-names (lexicon-tokens default-lexicon)) "self"))
  (check-true (tokens-class-receiver-name? (lexicon-tokens default-lexicon) 'cls))
  (check-equal? (tokens-method-receiver-offset (lexicon-tokens default-lexicon)) 1))

(define (test-javascript-lexicon-is-a-data-swap)
  (check-true (lexicon? javascript-lexicon))
  (check-eq? (lexicon-name javascript-lexicon) 'javascript)
  (check-true (set-member? (tokens-self-names (lexicon-tokens javascript-lexicon)) "this"))
  (check-false (set-member? (tokens-self-names (lexicon-tokens javascript-lexicon)) "self"))
  (check-equal? (tokens-method-receiver-offset (lexicon-tokens javascript-lexicon)) 0)
  ;; The two hosts share no token tables: this is a value swap, not a mode flag.
  (check-false (equal? (lexicon-tokens javascript-lexicon)
                       (lexicon-tokens python-lexicon))))

;; ── 2. injection through classify-call ──────────────────────────────

(define (test-classify-call-honors-injected-lexicon)
  ;; JavaScript `console.log(...)` → io under the JS lexicon; the Python
  ;; lexicon does not know `console` or a name-only `log` (deliberately absent
  ;; because `math.log` collides), so it stays unknown.
  (check-equal? (classify-call (jsdot (v "console") "log") (hash)
                               #:lexicon javascript-lexicon)
                '(effect . io))
  (check-equal? (classify-call (jsdot (v "console") "log") (hash))
                '(unknown . #f))
  ;; `JSON.parse(...)` is a pure whole-module root in the JS lexicon.
  (check-equal? (classify-call (jsdot (v "JSON") "parse") (hash)
                               #:lexicon javascript-lexicon)
                '(pure . #f))
  (check-equal? (classify-call (jsdot (v "JSON") "parse") (hash))
                '(unknown . #f))
  ;; A dotted JS method-name effect (`fs.readFile`).
  (check-equal? (classify-call (jsdot (v "fs") "readFile") (hash)
                               #:lexicon javascript-lexicon)
                '(effect . io))
  (check-equal? (classify-call (jsdot (v "fs") "readFile") (hash))
                '(unknown . #f)))

;; ── 3. injection through infer-effects / infer-effects-all ──────────

(define (test-infer-effects-honors-injected-lexicon)
  (define p (prog (fn "f" (js-call "console" "log"))))
  (check-equal? (row-names (hash-ref (infer-effects p #:lexicon javascript-lexicon) 'f))
                '(io))
  (check-equal? (row-names (hash-ref (infer-effects p) 'f)) '(unknown))
  ;; Cross-file fixpoint, same program as a component.
  (define rows (infer-effects-all (list (cons "app.js" p))
                                  #:lexicon javascript-lexicon))
  (check-equal? (row-names (hash-ref rows (cons "app.js" 'f))) '(io))
  (define python-rows (infer-effects-all (list (cons "app.js" p))))
  (check-equal? (row-names (hash-ref python-rows (cons "app.js" 'f))) '(unknown)))

;; ── 3b. receiver-type domain tables are lexicon data ────────────────

(define (test-injected-receiver-type-tables)
  ;; `fetch(...).json()`: the JS lexicon's stdlib-type-methods table maps the
  ;; call-result receiver TYPE `fetch` to its pure methods, so the chain
  ;; classifies from the receiver type rather than a bare method name. The
  ;; Python lexicon has no `fetch` receiver type and no `json` method rule.
  (define p (prog (fn "f" (uir-call (dot (uir-call (v "fetch") '()) "json") '()))))
  (define js-row (hash-ref (infer-effects-all (list (cons "app.js" p))
                                              #:lexicon javascript-lexicon)
                           (cons "app.js" 'f)))
  (check-equal? (row-names js-row) '(network))
  (define py-row (hash-ref (infer-effects-all (list (cons "app.js" p)))
                           (cons "app.js" 'f)))
  (check-not-false (row-lookup py-row 'unknown))
  ;; A tracked MODULE receiver (`x = fs` -> `x.readFile()`): the lexicon's
  ;; receiver-modules domain table resolves the method through the receiver
  ;; type, not through a name-only fallback.
  (define q (prog (fn "g" (uir-block (list (uir-set! (v "x") (v "fs"))
                                           (uir-call (dot (v "x") "readFile") '()))))))
  (define js-row2 (hash-ref (infer-effects-all (list (cons "app.js" q))
                                               #:lexicon javascript-lexicon)
                            (cons "app.js" 'g)))
  (check-equal? (row-names js-row2) '(io))
  (define py-row2 (hash-ref (infer-effects-all (list (cons "app.js" q)))
                            (cons "app.js" 'g)))
  (check-not-false (row-lookup py-row2 'unknown)))

;; ── 3c. import-tag rules and operation vocabulary are lexicon data ──

(define (test-injected-import-config-and-verbs)
  ;; `ConnectionRouter(...)` / `router.route(...)` where the class lives in a
  ;; module tagged by the lexicon's import rules, and the method's typed op is
  ;; derived from the lexicon's operation vocabulary. Both are data.
  (define mods
    (build-module-table
     (list (module-spec "app" (prog (class-of "Router" (list (method-of "route" (uir-null))))) '()))))
  (define js-config (lexicon-import-config javascript-lexicon))
  (define caps (derive-class-method-effects
                (hash-set mods "app.js"
                          (harvest-module "app.js"
                                          (prog (class-of "Router"
                                                          (list (method-of "route" (uir-null)))))
                                          '()))
                js-config
                javascript-lexicon))
  ;; The lexicon's db verbs decompose `route`? No: `route` is unclassified in
  ;; the JS vocabulary, so the capability keeps the coarse () ops.
  (check-equal? (hash-ref caps "Router.route") (effect-cap 'io '()))
  ;; The Python lexicon's config knows nothing about the JS app module.
  (check-false (module-effect-tag "app" (lexicon-import-config python-lexicon))))

;; ── 4. the dialect travels with the lexicon ─────────────────────────

(define (test-injected-dialect-implicit-receiver)
  ;; class Client { helper() { console.log() }  run() { this.helper() } }
  ;; Under the JS lexicon, `this` is the implicit receiver, so run() resolves
  ;; Client.helper and inherits its io effect. Under the Python lexicon there
  ;; is no `this` receiver and the call stays unknown.
  (define p
    (prog (class-of "Client"
                    (list (method-of "helper" (js-call "console" "log"))
                          (method-of "run" (uir-call (dot (v "this") "helper") '()))))))
  (define js-rows (infer-effects p #:lexicon javascript-lexicon))
  (check-equal? (row-names (hash-ref js-rows 'Client.run)) '(io))
  (define python-rows (infer-effects p))
  (check-not-false (row-lookup (hash-ref python-rows 'Client.run) 'unknown))
  (check-false (row-lookup (hash-ref js-rows 'Client.run) 'unknown)))

;; ── 5. grep guard: no library knowledge in core engine code ─────────
;;
;; The scan target is DERIVED from the effects directory: every module there is
;; either a CORE engine module (scanned) or an explicitly exempt DATA / per-host
;; front-end module. A new module therefore fails the completeness check below
;; until it is classified, so a module split can never silently shrink the
;; scanned surface.

(define (repo-root)
  (simplify-path (build-path (current-directory) 'up)))

;; Modules under racklr/effects that are deliberately NOT scanned as engine
;; code: data tables (the lexicon itself) and per-host front-end hooks.
(define effects-exempt-files
  '("library-tokens.rkt"         ; Python/Django token tables (DATA)
    "library-pure-names.rkt"     ; Python/Django pure-name tables (DATA)
    "library-stdlib-types.rkt"   ; receiver-type domain tables (DATA)
    "library-dialect.rkt"        ; host naming/encoding conventions (DATA)
    "library-racket-tokens.rkt"  ; Racket stdlib pure/effect names (DATA)
    "annotation-hooks.rkt"       ; front-end annotation hook binding
    "python-annot.rkt"           ; Python annotation front end
    "clojure-annot.rkt"          ; Clojure annotation front end
    "ts-annot.rkt"               ; TS/JS annotation front end
    "jsdoc-annot.rkt"            ; JSDoc annotation front end
    "lexicons/python.rkt"        ; host lexicon (DATA)
    "lexicons/javascript.rkt"    ; host lexicon (DATA)
    "imports/import-extract.rkt" ; Python import front end (token scanner)
    "imports/resolve.rkt"        ; Python import front end (module paths)
    "imports/library-config.rkt" ; Python/Django import rules (DATA)
    "imports.rkt"))              ; facade

(define (rkt-file? p)
  (and (file-exists? p) (string-suffix? (path->string p) ".rkt")))

;; Repo-relative paths of every .rkt under racklr/effects.
(define (all-effects-files)
  (define root (build-path (repo-root) "racklr/effects"))
  (sort
   (for/list ([p (in-directory root)] #:when (rkt-file? p))
     (path->string (find-relative-path root p)))
   string<?))

(define core-engine-files
  (let ([exempt (list->set effects-exempt-files)])
    (for/list ([f (in-list (all-effects-files))] #:unless (set-member? exempt f))
      f)))

;; Every library / receiver / convention NAME a host lexicon supplies. These
;; must appear only in DATA modules. Front-end construct encodings emitted by
;; the lowerers (`dot`, `=>`, `async-fn`, `gen-fn`, comprehension tags, ...)
;; are UIR-level encoding knowledge, not library knowledge, and are excluded.
(define (lexicon-name-literals tokens)
  (define (hash-keys* h) (if (hash? h) (hash-keys h) '()))
  (define (hash-values* h) (if (hash? h) (hash-values h) '()))
  (append
   (append-map (λ (roots) (if (set? roots) (set->list roots) '()))
               (hash-values* (effect-tokens-effect-roots tokens)))
   (hash-keys* (effect-tokens-bare-name-effects tokens))
   (hash-keys* (effect-tokens-method-effects tokens))
   (hash-keys* (effect-tokens-unambiguous-method-effects tokens))
   (hash-keys* (effect-tokens-ambiguous-method-effects tokens))
   (append-map (λ (m) (hash-keys* m))
               (hash-values* (effect-tokens-root-method-effects tokens)))
   (set->list (effect-tokens-builtin-type-roots tokens))
   (set->list (effect-tokens-stdlib-module-roots tokens))
   (hash-keys* (effect-tokens-stdlib-type-methods tokens))
   (append-map (λ (m) (hash-keys* m))
               (hash-values* (effect-tokens-stdlib-type-methods tokens)))
   (set->list (effect-tokens-pure-stdlib-roots tokens))
   (set->list (effect-tokens-pure-stdlib-bare-names tokens))
   (set->list (effect-tokens-pure-native-builtin-names tokens))
   (append-map (λ (m) (if (set? m) (set->list m) '()))
               (hash-values* (effect-tokens-pure-stdlib-methods tokens)))
   (set->list (effect-tokens-pure-op-names tokens))
   (set->list (effect-tokens-pure-constructor-names tokens))
   (set->list (effect-tokens-pure-builtin-names tokens))
   (set->list (effect-tokens-protocol-method-names tokens))
   (set->list (effect-tokens-string-method-names tokens))
   (set->list (effect-tokens-pure-method-names tokens))
   (hash-keys* (effect-tokens-module-result-types tokens))
   (hash-keys* (effect-tokens-receiver-modules tokens))
   ;; host naming conventions
   (list (dialect-init-method-name (effect-tokens-dialect tokens))
         (dialect-keyword-arg-callee (effect-tokens-dialect tokens))
         (dialect-annotation-callee (effect-tokens-dialect tokens)))
   (set->list (dialect-self-names (effect-tokens-dialect tokens)))
   (set->list (dialect-class-receiver-names (effect-tokens-dialect tokens)))
   (set->list (dialect-exception-suffixes (effect-tokens-dialect tokens)))))

;; Effect-algebra vocabulary the engine legitimately holds as quoted symbols:
;; the domains and the domain values they appear as (`'db`, `'io`, `'pure`, ...).
(define (tokens-algebra-names tokens)
  (define (values* h) (if (hash? h) (hash-values h) '()))
  (append
   (map symbol->string (hash-keys (effect-tokens-effect-roots tokens)))
   (map symbol->string
        (append
         (values* (effect-tokens-bare-name-effects tokens))
         (values* (effect-tokens-method-effects tokens))
         (values* (effect-tokens-unambiguous-method-effects tokens))
         (values* (effect-tokens-ambiguous-method-effects tokens))
         (values* (effect-tokens-stdlib-root-effects tokens))
         (append-map values* (values* (effect-tokens-root-method-effects tokens)))))
   '("pure")))

;; Effect-algebra operation atoms the engine builds typed capabilities with
;; (`(typed-op domain cname "read")` -> `db<X>.read`): canonical algebra
;; vocabulary, not library-specific names.
(define effect-algebra-strings (list->set '("read" "write")))

;; Racket format-template directives are data atoms (they appear in the native
;; pure-name table because `format` is pure), not callable library procedures;
;; engine format strings legitimately contain them.
(define format-directive-atoms (list->set '("~a" "~s" "~v" "~e" "~%")))

;; Engine-internal diagnostic tags that happen to collide with lexicon names:
;; the `#:unknown-tally` binding-kind buckets report an aliased binding as
;; `'alias` and an exception binding as `'exception`. These are quoted symbols
;; of local diagnostic vocabulary, not library knowledge.
(define engine-local-symbols (list->set '("alias" "exception")))

;; UIR construct-encoding vocabulary the lowerers/emitters introduce as quoted
;; symbols (`'super`). These are front-end encodings, not library knowledge.
(define (tokens-encoding-names tokens)
  (define d (effect-tokens-dialect tokens))
  (append (set->list (dialect-synthetic-callees d))
          (set->list (dialect-comprehension-callees d))
          (set->list (dialect-wrapper-callees d))
          (set->list (dialect-binding-callees d))
          (set->list (dialect-runtime-primitives d))))

;; The literal NAME surface of a module: string literals anywhere (including
;; `(string->symbol "x")` arguments) and symbols inside quoted data (`'openai`,
;; `'(openai ...)`). Plain identifiers are code, not names, and are skipped;
;; comments are absent from the read syntax, so no comment stripping is needed.
;; Returns (values strings quoted-symbol-names).
(define (literal-surface-of-datum datum)
  (define strings '())
  (define quoted-symbols '())
  (define (walk v quoted?)
    (cond
      [(string? v) (set! strings (cons v strings))]
      [(symbol? v)
       (when quoted? (set! quoted-symbols (cons (symbol->string v) quoted-symbols)))]
      [(pair? v)
       (cond
         [(and (eq? (car v) 'quote) (pair? (cdr v)) (null? (cddr v)))
          (walk (cadr v) #t)]
         [else (walk (car v) quoted?) (walk (cdr v) quoted?)])]
      [(vector? v) (for ([x (in-vector v)]) (walk x quoted?))]
      [(box? v) (walk (unbox v) quoted?)]
      [else (void)]))
  (walk datum #f)
  (values (remove-duplicates strings string=?)
          (remove-duplicates quoted-symbols string=?)))

(define (literal-surface path)
  (define stx
    (parameterize ([read-accept-lang #t] [read-accept-reader #t])
      (with-input-from-file path (λ () (read-syntax path)))))
  (literal-surface-of-datum (syntax->datum stx)))

(define (test-grep-scan-covers-every-module)
  (define all (all-effects-files))
  (define accounted
    (sort (remove-duplicates (append core-engine-files effects-exempt-files) string=?)
          string<?))
  (check-equal? accounted all
                "every racklr/effects module is either scanned or exempt by design")
  (check-not-false (member "classify.rkt" core-engine-files))
  (check-not-false (member "classify-all.rkt" core-engine-files))
  (check-not-false (member "receiver-resolve.rkt" core-engine-files))
  (check-not-false (member "infer.rkt" core-engine-files))
  (check-not-false (member "whole-program.rkt" core-engine-files))
  ;; The two guards must scan a non-trivial surface (a cwd mistake would shrink
  ;; them toward empty, and this would catch it).
  (check-true (> (length core-engine-files) 10))
  (check-true (> (length shared-lowering-files) 3)))

(define (test-guard-covers-symbol-and-string-surfaces)
  ;; A library name hardcoded as a quoted symbol or quoted datum would slip a
  ;; string-only scan; the literal surface must include both. `(string->symbol
  ;; "openai")` is covered by the string surface.
  (define-values (strings1 symbols1) (literal-surface-of-datum ''openai))
  (check-equal? symbols1 '("openai"))
  (check-equal? strings1 '())
  (define-values (strings2 symbols2) (literal-surface-of-datum ''(openai 'console)))
  (check-equal? (sort symbols2 string<?) '("console" "openai"))
  (define-values (strings3 _) (literal-surface-of-datum '(string->symbol "openai")))
  (check-equal? strings3 '("openai"))
  ;; Identifiers are code, not names, and are not collected.
  (define-values (strings4 symbols4)
    (literal-surface-of-datum '(define (openai helper) (helper openai))))
  (check-equal? strings4 '())
  (check-equal? symbols4 '()))

(define (test-grep-no-library-literals-in-core-engine)
  ;; The guard is derived from the DATA: the union of the Python and
  ;; JavaScript lexicons' names.
  (define name-literals
    (let ([py (lexicon-name-literals (lexicon-tokens python-lexicon))]
          [js (lexicon-name-literals (lexicon-tokens javascript-lexicon))])
      (list->set (filter (λ (s) (and (string? s) (not (string=? s ""))))
                         (append py js)))))
  ;; Quoted symbols may name effect-algebra atoms or UIR construct encodings;
  ;; those are engine vocabulary, not library knowledge.
  (define exempt-symbol-names
    (let ([py (lexicon-tokens python-lexicon)]
          [js (lexicon-tokens javascript-lexicon)])
      (list->set (append (tokens-algebra-names py)
                         (tokens-algebra-names js)
                         (tokens-encoding-names py)
                         (tokens-encoding-names js)))))
  (check-true (> (set-count name-literals) 100)
              "the guard is derived from a non-trivial name set")
  (check-true (set-member? name-literals "openai")
              "a Python/Django library root is in the guarded set")
  (check-true (set-member? name-literals "console")
              "a JavaScript library root is in the guarded set")
  (define root (repo-root))
  (for ([rel (in-list core-engine-files)])
    (define path (build-path root "racklr/effects" rel))
    (define-values (strings quoted-symbols) (literal-surface path))
    (define offending
      (append
       (for/list ([s (in-list strings)]
                  #:when (and (set-member? name-literals s)
                              (not (set-member? effect-algebra-strings s))
                              (not (set-member? format-directive-atoms s))))
         s)
       (for/list ([s (in-list quoted-symbols)]
                  #:when (and (set-member? name-literals s)
                              (not (set-member? exempt-symbol-names s))
                              (not (set-member? engine-local-symbols s))))
         s)))
    (check-equal? offending '()
                  (format "racklr/effects/~a carries no library-specific name literals: ~a"
                          rel offending)))
  ;; Core inference is host-neutral: language annotation surfaces arrive as an
  ;; injected hook (annotation-hooks.rkt), not a require of one host's module.
  (for ([rel (in-list core-engine-files)])
    (define text (file->string (build-path root "racklr/effects" rel)))
    (define offending
      (for/list ([needle (in-list '("ts-annot" "python-annot" "clojure-annot"
                                    "jsdoc-annot"))]
                 #:when (string-contains? text needle))
        needle))
    (check-equal? offending '()
                  (format "racklr/effects/~a requires no language annotation module: ~a"
                          rel offending))))

;; ── 6. grep guard: shared parser / lowering infrastructure ──────────
;;
;; The shared absorb-layer generator (grammar CST -> parser module source) is
;; core: a new front end must supply a grammar + a language policy, not edit
;; these engines. Host grammar knowledge (the line-break token, the layout
;; model, template-literal rule names, bracket token names, the entry rule
;; name) lives in racklr/gend-parser/language-policy.rkt as DATA.

;; Derived, not hand-listed: every .rkt file of the parser generator and the
;; .g4 front end is scanned; language-policy.rkt is the DATA module.
(define shared-lowering-files
  (let* ([root (repo-root)]
         [gend (build-path root "racklr/gend-parser")]
         [gend-files
          (for/list ([p (in-directory gend)] #:when (rkt-file? p))
            (path->string (find-relative-path root p)))])
    (sort (append (list "racklr/gend-parser.rkt"
                        "racklr/g4-lex.rkt"
                        "racklr/g4-parse.rkt")
                  (remove "racklr/gend-parser/language-policy.rkt" gend-files))
          string<?)))

(define host-grammar-policy-literals
  (list->set
   '("NEWLINE" "TEMPLATE" "BackTick" "BackTickInside"
     "TemplateStringStartExpression" "TemplateCloseBrace" "CloseBrace"
     "OpenBrace" "OPEN_PAREN" "OPEN_BRACK" "OPEN_BRACE"
     "CLOSE_PAREN" "CLOSE_BRACK" "CLOSE_BRACE" "program")))

(define (test-grep-no-host-literals-in-shared-lowering-infra)
  (define root (repo-root))
  (for ([rel (in-list shared-lowering-files)])
    (define path (build-path root rel))
    (check-true (file-exists? path) (format "~a exists" rel))
    (define-values (strings quoted-symbols) (literal-surface path))
    (define offending
      (for/list ([s (in-list (append strings quoted-symbols))]
                 #:when (set-member? host-grammar-policy-literals s))
        s))
    (check-equal? offending '()
                  (format "~a carries no host grammar-policy literals: ~a"
                          rel offending)))
  ;; The policy DATA module is where the host grammar knowledge lives, and the
  ;; derived scan target covers the whole parser-generator directory.
  (define policy-file (build-path root "racklr/gend-parser/language-policy.rkt"))
  (define policy-text (file->string policy-file))
  (for ([needle (in-list '("NEWLINE" "TEMPLATE" "BackTick" "OPEN_PAREN"
                           "CloseBrace" "program"))])
    (check-true (string-contains? policy-text needle)
                (format "the language policy data names ~a" needle)))
  (define gend (build-path root "racklr/gend-parser"))
  (define gend-files
    (for/list ([p (in-directory gend)] #:when (rkt-file? p))
      (path->string (find-relative-path root p))))
  (define scanned-parser-gen
    (sort (cons "racklr/gend-parser.rkt"
                (remove "racklr/gend-parser/language-policy.rkt" gend-files))
          string<?))
  (check-equal? scanned-parser-gen
                (sort (remove* (list "racklr/g4-lex.rkt" "racklr/g4-parse.rkt")
                               shared-lowering-files)
                      string<?)
                "every parser-generator module is either scanned or the policy data")
  (check-not-false (member "racklr/g4-lex.rkt" shared-lowering-files))
  (check-not-false (member "racklr/g4-parse.rkt" shared-lowering-files)))

;; ── run ─────────────────────────────────────────────────────────────

(define (main-tests)
  (define failures
    (run-test-suite
     (test-suite
      "effects lexicon (P2.2)"
      (test-case "default lexicon is Python" (test-default-lexicon-is-python))
      (test-case "JavaScript lexicon is a data swap"
        (test-javascript-lexicon-is-a-data-swap))
      (test-case "classify-call honors an injected lexicon"
        (test-classify-call-honors-injected-lexicon))
      (test-case "infer-effects honors an injected lexicon"
        (test-infer-effects-honors-injected-lexicon))
      (test-case "receiver-type domain tables are lexicon data"
        (test-injected-receiver-type-tables))
      (test-case "import-tag rules and operation vocabulary are lexicon data"
        (test-injected-import-config-and-verbs))
      (test-case "injected dialect implicit receiver"
        (test-injected-dialect-implicit-receiver))
      (test-case "grep scan covers every effects module"
        (test-grep-scan-covers-every-module))
      (test-case "guard covers quoted-symbol and string literal surfaces"
        (test-guard-covers-symbol-and-string-surfaces))
      (test-case "grep guard: no library literals in the core engine"
        (test-grep-no-library-literals-in-core-engine))
      (test-case "grep guard: no host literals in shared lowering infra"
        (test-grep-no-host-literals-in-shared-lowering-infra)))))
  (if (zero? failures)
      (printf "All effects-lexicon tests passed\n")
      (exit 1)))

(module+ main
  (main-tests))
