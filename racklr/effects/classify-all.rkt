#lang racket

;; racklr/effects/classify-all.rkt
;;
;; Whole-program callee classification: after intra-program edge resolution has
;; failed, classify a call callee structurally — imported-name receivers, the
;; receiver-type dataflow cascade, stdlib/framework parent fallbacks, manager
;; attributes — before deferring to the name heuristics in classify.rkt. The
;; per-receiver effect-resolution helpers live in receiver-resolve.rkt so this
;; module keeps only the classification cascade and the diagnostics shape.
;;
;; The host language arrives as an injected lexicon (tokens + import config +
;; operation vocabulary); the default is the Python/Django lexicon, and every
;; library name is read from it, never from engine code.

(require racklr/uir
         racklr/effects/imports
         racklr/effects/shape
         racklr/effects/class-resolve
         racklr/effects/classify
         racklr/effects/receiver-type
         racklr/effects/receiver-chain
         racklr/effects/receiver-resolve
         racklr/effects/effect-roots
         racklr/effects/library-tokens
         racklr/effects/lexicon)

(provide classify-call-all method-cap receiver-shape)

;; Is the call's receiver the enclosing instance (`self`/`cls`, or the host
;; dialect's name such as JS `this`)? The name set is host lexicon data, not an
;; engine table; such a receiver is the owner class, but its class->module->
;; domain fallback is applied only at the `unknown` tail below, so a more
;; precise rule (edge, stdlib table, parent) still wins.
(define (self-receiver-callee? callee tokens)
  (and (member-access-callee? callee)
       (tokens-self-name? tokens (bare-name-symbol (member-access-base callee)))))

(define (classify-call-all callee known methods class-defs fn-defs comp-supers
                           class-attrs comp owner [import-env #f] [rt #f]
                           [return-types #f] [cov-box #f]
                           #:lexicon [lex default-lexicon])
  (define tokens (lexicon-tokens lex))
  (define nm (callee-name callee))
  (define keys (method-callee-keys callee comp owner methods class-defs comp-supers tokens))
  (define key (for/or ([k (in-list keys)] #:when (hash-has-key? known k)) k))
  ;; Side channel for the coverage label (racklr/effects/labels): what evidence
  ;; classified this call. 'dataflow means the receiver class came from
  ;; receiver-type dataflow (locals, self-attributes, chains, call results);
  ;; 'structural means a DATA rule, import handle, class attribute, or MRO rule
  ;; classified it. It is reset per call, so a caller that reuses one box never
  ;; reads a previous call's evidence. The classification result is unchanged.
  (define (note-evidence! kind) (when cov-box (set-box! cov-box kind)))
  (note-evidence! 'structural)
  (cond
    [(and key (hash-has-key? known key)) (cons 'edge key)]
    [(bare-function-key nm comp fn-defs known) => (λ (k) (cons 'edge k))]
    [else
     ;; A locally-tracked receiver (x = KnownClass(...)) shadows any import of
     ;; the same name and, more importantly, wins over the constructor
     ;; heuristic below: `loader.get_migration_by_prefix(...)` is a METHOD on a
     ;; tracked MigrationLoader, not a construction of the (pure-bare-named)
     ;; `get_migration_by_prefix` class.
     ;; The receiver is read through the member-access encoding the front end
     ;; uses — Python uir-get or JS/TS dot call — so `obj.m()`, `this.m()` and
     ;; the lowered `x?.m()` chain all enter the same dataflow.
     (define rcls (and rt (member-access-callee? callee)
                        (receiver-chain-class rt (member-access-base callee)
                                              comp class-defs class-attrs comp-supers
                                              owner return-types methods fn-defs known
                                              import-env tokens)))
     (define mname (and (member-access-callee? callee) (member-access-field callee)))
     ;; A call to a known class name is a constructor (pure instantiation), not
     ;; an effectful call — this also seeds the receiver-type dataflow without
     ;; polluting the enclosing function with `unknown`. A class imported from
     ;; an unscanned module (`from django.db.migrations.loader import
     ;; MigrationLoader`) is equally pure to construct: the import graph knows
     ;; it is a class, even though its body is outside the corpus.
     (define ctor (and (not rcls)
                       (or (constructor-class callee class-defs tokens)
                           (imported-module-class-ref callee comp import-env tokens)
                           (and nm comp import-env
                                (imported-class-ref nm comp import-env)))))
     (cond
       [ctor (cons 'pure #f)]
       ;; `cls(...)` in a classmethod constructs the enclosing class — the same
       ;; pure construction as `KnownClass(...)`, but `cls` names the class
       ;; dynamically (its value is the first parameter), so the constructor
       ;; heuristic above cannot see it. `self(...)` is deliberately not
       ;; included: calling an instance dispatches to its __call__.
       [(and owner (tokens-class-receiver-name? tokens nm)) (cons 'pure #f)]
       ;; A tracked class-valued callee (`loader_class()` / `qs_class()` where
       ;; the local was bound to a class): construction is pure, mirroring
       ;; `KnownClass(...)`.
       [(and rt (receiver-class-of rt callee tokens)) (cons 'pure #f)]
       [else
        (define re (and (not (self-receiver-callee? callee tokens))
                        (receiver-domain-effect comp rcls mname class-defs import-env lex)))
        (cond
          [(eq? re 'pure) (cons 'pure #f)]
          [(effect-cap? re)
           ;; A module-receiver / module-path rule is structural; a
           ;; class-domain receiver effect needed the resolved receiver class.
           (note-evidence!
            (if (or (module-receiver-effect rcls tokens)
                    (module-path-marker-effect rcls mname import-env lex))
                'structural
                'dataflow))
           (cons 'effect re)]
          [else
           ;; stdlib/builtin receiver type: a call-result chain
           ;; (`Path(...).is_absolute`) or a tracked local (`x = open(...)`)
           ;; whose type has a method-table entry.
           (define sre (and rcls mname (stdlib-type-method-effect rcls mname tokens)))
           (cond
             [(eq? sre 'pure) (cons 'pure #f)]
             [sre
              ;; A stdlib receiver method needed the resolved receiver type.
              (note-evidence! 'dataflow)
              (cons 'effect sre)]
             [else
              ;; `super().m()` with no corpus edge: parent-type classification.
              (define sfe (super-stdlib-fallback callee comp owner comp-supers
                                                 class-defs import-env lex))
              (cond
                [(eq? sfe 'pure) (cons 'pure #f)]
                [sfe (cons 'effect sfe)]
                [else
                 ;; `self.m()` inherited from a NON-corpus parent
                 ;; (`unittest.TestCase.fail`): classify through its stdlib table.
                 (define sse (self-stdlib-fallback callee comp owner comp-supers
                                                   import-env lex))
                 (cond
                   [(eq? sse 'pure) (cons 'pure #f)]
                   [sse (cons 'effect sse)]
                   [else
                    (define me (manager-effect callee comp class-defs import-env lex))
                    (cond
                      [(eq? me 'pure) (cons 'pure #f)]
                      [(effect-cap? me) (cons 'effect me)]
                      [else
                       (define ie (imported-effect callee comp import-env lex))
                       (cond
                         [(eq? ie 'pure) (cons 'pure #f)]
                         [(effect-cap? ie) (cons 'effect ie)]
                         [else
                          ;; A tracked receiver whose class is a corpus class
                          ;; in an untagged module has no library domain. Only
                          ;; upgrade a name-heuristic `unknown`: a known effect
                          ;; or pure stays (the name tables are still the
                          ;; authority for effect names on arbitrary
                          ;; receivers). This turns `build_file.cleanup()` into
                          ;; an edge to `BuildFile.cleanup` without overriding
                          ;; a real email/db/io classification.
                          (define bfi (and (not (uir-get? callee)) nm comp
                                           (imported-fn-effect nm comp import-env lex)))
                          (define nn (cond
                                       [(eq? bfi 'pure) (cons 'pure #f)]
                                       [(effect-cap? bfi) (cons 'effect bfi)]
                                       [else (classify-nonedge callee tokens)]))
                          (cond
                            [(eq? (car nn) 'unknown)
                             (define rc (and rcls mname
                                             (resolve-class-component rcls comp class-defs)))
                             (define rkey
                               (and rc (for/or ([k (in-list (mro-method-keys rc rcls mname methods
                                                                          class-defs comp-supers))]
                                                #:when (hash-has-key? known k))
                                               k)))
                             (cond
                               [(and rkey (hash-has-key? known rkey)) (cons 'edge rkey)]
                               [else
                                ;; `self.m()` / `this.m()` that no edge, stdlib
                                ;; table or parent rule resolved: the enclosing
                                ;; class's home module still names the effect
                                ;; domain (class -> module -> domain). Only a
                                ;; concrete domain capability is used — a
                                ;; pure-tagged module does not make an unknown
                                ;; callable attribute pure, because the attribute
                                ;; may hold user code.
                                (define sde (and (self-receiver-callee? callee tokens)
                                                 (receiver-domain-effect comp rcls mname
                                                                         class-defs import-env lex)))
                                (cond
                                  [(effect-cap? sde)
                                   (note-evidence! 'dataflow)
                                   (cons 'effect sde)]
                                  [else nn])])]
                            [else nn])])])])])])])])]))

;; Syntactic shape of a call's receiver, used by diagnostics to bucket `unknown`
;; sites by the lever that resolves them. `param-names` (when given) is the
;; enclosing function's parameter-name list, so a bare-variable receiver is
;; reported as 'param (needs parameter dataflow) vs 'local (already tracked by
;; local constructor assignments). Returns a symbol:
;;   'bare-call    — foo(...)
;;   'self-method  — implicit-receiver method call (self.m() / cls.m())
;;   'self-attr    — self.attr.m() / cls.attr.m()
;;   'call-result  — f(...).m()
;;   'chained      — a.b.m() (non-self root)
;;   'param        — x.m() where x is a parameter
;;   'local        — x.m() where x is a local (not a parameter)
;;   'symbol       — Name.m() (bare symbol receiver)
;;   'other
(define (receiver-shape callee [param-names #f] [tokens default-effect-tokens])
  (cond
    [(not (member-access-callee? callee)) 'bare-call]
    [else
     (define base (member-access-base callee))
     (define base-name (bare-name-symbol base))
     (cond
       [(tokens-self-name? tokens base-name) 'self-method]
       [(uir-call? base) 'call-result]
       [(member-access-callee? base)
        (define root (dotted-root base))
        (if (and root (tokens-self-name? tokens root)) 'self-attr 'chained)]
       [(uir-symbol? base) 'symbol]
       [base-name
        (cond
          [(and param-names (memq base-name param-names)) 'param]
          [param-names 'local]
          [else 'var])]
       [else 'other])]))

;; The derived capability for a call-graph method key `(component . Class.method)`,
;; resolved COMPONENT-AWARE: the component's own module path supplies the domain,
;; so same-named classes in different modules do not collide (unlike the bare-key
;; caps table). Returns an effect-cap or #f. This is the "blessed summary" from
;; the per-library config — a typed op like `db<Model>.write` is asserted at the
;; method's own definition site, so it flows through the fixpoint to every
;; caller even when the method body descends into unclassified plumbing.
(define (method-cap k import-env [lex default-lexicon])
  (and import-env
       (let* ([comp (car k)]
              [modpath (hash-ref (import-env-comp-modpath import-env) comp #f)])
         (and modpath
              (let* ([s (symbol->string (cdr k))]
                     [cls-str (regexp-replace #rx"[.].*$" s "")]
                     [m-str (regexp-replace #rx"^[^.]*[.]" s "")])
                (and (not (string=? cls-str s))
                     (let ([domain (handle-effect-domain modpath cls-str m-str
                                                         (lexicon-import-config lex))])
                       (and domain
                            (not (eq? domain 'pure))
                            (effect-cap domain (method-ops domain modpath cls-str m-str lex))))))))))
