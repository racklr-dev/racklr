#lang racket

;; racklr/effects/imports/effect-tag.rkt
;;
;; Generic module-path → effect-domain tagging: a class inherits its module's
;; domain from the injected lexicon's import rules, and its method's primitive
;; ops are derived from the method name via the lexicon's per-domain verb
;; tables. The capability model structs live in effect-tag-types.rkt; the
;; Python/Django data lives in library-config.rkt and is assembled into the
;; default lexicon.
;;
;; Every lookup takes an optional injected lexicon: the import-tag rule config
;; and the operation vocabulary are host data, so a non-Python host's lexicon
;; decomposes its own methods into typed ops without editing this engine.

(require racklr/effects/imports/import-extract
         racklr/effects/imports/handle
         racklr/effects/imports/effect-tag-types
         racklr/effects/lexicon)

(provide
  (all-from-out racklr/effects/imports/effect-tag-types)
  effect-tag-config
  db-read-methods db-write-methods io-read-verbs io-write-verbs io-other-verbs
  domain-op-vocabulary builtin-class-methods
  resolve-effect-tag
  module-effect-tag
  handle-effect-domain
  clear-effect-tag-cache!
  class-effect-tag
  method-ops
  typed-op-parts
  derive-class-method-effects
  derive-class-supers)

;; Backward-compatible default data bindings: the Python/Django lexicon's
;; import rules and operation vocabulary. Kept exported because the public
;; contract (and existing callers) name them directly.
(define effect-tag-config (lexicon-import-config default-lexicon))
(define db-read-methods (lexicon-db-read-methods default-lexicon))
(define db-write-methods (lexicon-db-write-methods default-lexicon))
(define io-read-verbs (lexicon-io-read-verbs default-lexicon))
(define io-write-verbs (lexicon-io-write-verbs default-lexicon))
(define io-other-verbs (lexicon-io-other-verbs default-lexicon))
(define domain-op-vocabulary (lexicon-domain-op-vocabulary default-lexicon))
(define builtin-class-methods (lexicon-builtin-class-methods default-lexicon))

;; An effect is a structured capability, not a bare category atom. It is a
;; pair (domain . ops): `domain` is the effect root the effect-row algebra
;; keys on (db/io/network/cache/email/process/llm/time), and `ops` is the set
;; of primitive operations the capability covers (() = "all ops" = the coarse
;; category). A method's capability is derived from WHERE its class lives
;; (domain, from the config rules) and WHAT the method does (ops, from the
;; method name via the lexicon's per-domain verb table). `pure` is not an
;; effect — it is the absence of one (the empty effect row), so pure methods
;; get no entry.
;;
;; The lookup is cached per library (nested hash: library-name → module-path →
;; domain), so after the first resolution of a module path the rest are O(1).

;; Per-library lookup cache: library-name → (hash module-path → domain-or-#f).
(define effect-tag-cache (make-hash))

(define (clear-effect-tag-cache!)
  (set! effect-tag-cache (make-hash)))

;; Generic, uncached: the first matching rule in `config` for `path`, else #f.
(define (resolve-effect-tag config path)
  (define lib-name (first-component path))
  (for/or ([l (in-list config)]
           #:when (string=? (effect-library-name l) lib-name))
    (for/or ([r (in-list (effect-library-rules l))])
      (and (string-prefix? path (effect-rule-prefix r))
           (effect-rule-domain r)))))

;; Cached facade over a config (the default unless one is supplied). A non-default
;; config bypasses the cache, since the cache is keyed by module path alone.
(define (module-effect-tag path [config effect-tag-config])
  (if (equal? config effect-tag-config)
      (let* ([lib-name (first-component path)]
             [lib-cache (hash-ref! effect-tag-cache lib-name make-hash)])
        (hash-ref! lib-cache path (λ () (resolve-effect-tag config path))))
      (resolve-effect-tag config path)))

(define (class-effect-tag path cname [config effect-tag-config])
  ;; Seam for per-class overrides; the default is the module's domain.
  (module-effect-tag path config))

;; Dotted candidates for a call resolved through an import handle, most-specific
;; first: the imported object's own path (package.obj), the called member's path
;; (package.member — for module handles), and the bare package path. This is the
;; re-export follow: `from django.utils import autoreload` synthesizes a handle
;; whose path is the package (`django.utils`) but whose object actually lives in
;; `django.utils.autoreload`; trying each candidate against the ordered DATA
;; rules lets a specific submodule rule win without the engine knowing any name.
(define (dotted-candidates path name mname)
  (filter (λ (s) (and s (not (string=? s ""))))
          (list (and path name mname (string-append path "." name "." mname))
                (and path name (string-append path "." name))
                (and path mname (string-append path "." mname))
                path)))

;; Resolve a handle-shaped call to an effect domain (or 'pure / #f) by trying
;; the candidate dotted paths in order against the config rules.
(define (handle-effect-domain path name mname [config effect-tag-config])
  (for/or ([cand (in-list (dotted-candidates path name mname))])
    (module-effect-tag cand config)))

;; A typed operation symbol: domain<ReceiverType>.verb (e.g. db<QuerySet>.read,
;; io<File>.write). The receiver TYPE is the concrete class that performs the
;; operation — a db.read on a QuerySet is a different capability from one on a
;; backend cursor, and the type keeps them distinct atoms in the effect graph.
;; An unclassified method keeps () — the coarse "any op in the domain" — never
;; a receiver-less domain.verb: that middle tier is unsound because it conflates
;; every receiver that happens to share a verb.
(define (typed-op domain cname verb)
  (string->symbol (format "~a<~a>.~a" domain cname verb)))

;; Primitive ops for a method of a class in a module, given the domain. Ops are
;; TYPED by the receiver class, so the I/O stream (file/http/stdio/socket) is
;; implicit in the class name — no separate resource table. The verb tables and
;; the builtin receiver method table come from the injected lexicon.
(define (method-ops domain path cname mname [lex default-lexicon])
  (case domain
    [(db) (cond [(set-member? (lexicon-db-write-methods lex) mname)
                 (list (typed-op domain cname "write"))]
                [(set-member? (lexicon-db-read-methods lex) mname)
                 (list (typed-op domain cname "read"))]
                [else '()])]
    [(io) (cond
            [(set-member? (lexicon-io-write-verbs lex) mname)
             (list (typed-op domain cname "write"))]
            [(set-member? (lexicon-io-read-verbs lex) mname)
             (list (typed-op domain cname "read"))]
            [(set-member? (lexicon-io-other-verbs lex) mname)
             (list (typed-op domain cname mname))]
            [else '()])]
    ;; Any other domain is typed from the injected operation vocabulary (the
    ;; network/process/cache/... verb tables), so a framework method resolves to
    ;; a family-typed op instead of staying coarse. A domain with no vocabulary
    ;; keeps the coarse () capability.
    [else
     (define vocab (hash-ref (lexicon-domain-op-vocabulary lex) domain #f))
     (define op (and vocab (hash-ref vocab mname #f)))
     (if op (list (typed-op domain cname op)) '())]))

;; Derive Class.method → effect-cap for every class in a tagged module. Pure
;; modules (domain 'pure) and untagged modules contribute nothing; methods
;; with no classified op keep () ops (the coarse category within the domain).
;; `config` is the import-tag rules; `lex` supplies the operation vocabulary and
;; the builtin receiver method table.
(define (derive-class-method-effects mods [config effect-tag-config]
                                    [lex default-lexicon])
  (define out (make-hash))
  (for ([(path mi) (in-hash mods)])
    (define domain (module-effect-tag path config))
    (when (and domain (not (eq? domain 'pure)))
      (for ([(cname ci) (in-hash (module-info-classes mi))])
        (for ([mname (in-hash-keys (class-info-methods ci))])
          (hash-set! out (format "~a.~a" cname mname)
                     (effect-cap domain (method-ops domain path cname mname lex)))))))
  out)

;; Parse a typed operation symbol back into its parts: `db<QuerySet>.read` →
;; '(db "QuerySet" "read"); a non-typed symbol → #f. The inverse of `typed-op`.
;; The receiver name is a *string* so callers can look it up in the class/super
;; tables without re-parsing.
(define (typed-op-parts op)
  (define s (symbol->string op))
  (define m (regexp-match #rx"^([a-z]+)<([^>]+)>[.](.+)$" s))
  (and m
       (list (string->symbol (list-ref m 1))
             (list-ref m 2)
             (list-ref m 3))))

;; Class → list of direct super names, harvested from the class-info super
;; field. This is the MRO *surface*: a per-class chain, not a linearized list,
;; because linearization needs cross-module resolution (super names may live in
;; imported modules) which the caller owns. Keys are bare class names — the
;; same granularity as `derive-class-method-effects`.
(define (derive-class-supers mods)
  (define out (make-hash))
  (for ([(path mi) (in-hash mods)])
    (for ([(cname ci) (in-hash (module-info-classes mi))])
      (define supers (class-info-super ci))
      (when (pair? supers) (hash-set! out cname supers))))
  out)
