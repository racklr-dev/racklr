#lang racket

;; racklr/effects/origin.rkt
;;
;; Stable site identities for effect-edge provenance (nc-4A19).
;;
;; Provenance answers "why is this function db", so the identity of a ground
;; must be the SITE that produced the evidence — never a value inference
;; generated (the effect name, the resolved receiver type, a rendered call
;; expression) and never a particular corpus example. A value-dependent
;; identity partitions evidence that belongs to one site and makes graph drift
;; across runs undiagnosable.
;;
;; A `prov-origin` has the components a stable site identity needs:
;;
;;   file      the component / source file the site lives in, when the
;;             inference mode knows it: the component key for whole-program
;;             inference, or the optional `#:source-file` of a per-file run.
;;             #f when the caller supplied no file.
;;   line      the source line, when the front end records positions. UIR
;;             carries no source positions today, so this is #f; when the UIR
;;             gains them the line joins the identity.
;;   owner     the enclosing function / method name: the name part of the
;;             call-graph key ("Class.method" for a method), without the file
;;             component (file is its own field).
;;   position  the 0-based ordinal of the classifying call among the call
;;             nodes of the owner body's pre-order walk. This is the
;;             positional discriminator that stands in for a line number: it
;;             is derived from source structure only, so two identical
;;             expressions at different sites stay distinct, duplicate walks
;;             of one site agree, and two runs over the same source produce
;;             the same ordinal.
;;   chain     the receiver chain, taken structurally from the UIR callee:
;;             the dotted source names the callee reads through
;;             ("self.save" -> ("self" "save"), "open" -> ("open"),
;;             "super().__init__" -> ("super" "__init__")). Never parsed back
;;             out of a rendered expression.
;;
;; The origin deliberately excludes the effect, the tokens, the resolved
;; receiver class, and the match that produced the classification: those are
;; inference results, so keying identity on them makes an edge whose effect
;; changes look like a different site.
;;
;; `prov-origin->string` is the canonical identity used wherever a single
;; value names a site (the graph `site` atom). It is total and deterministic:
;; identity is a function of the origin fields only.

(require racklr/uir
         racklr/effects/shape)

(provide
  ;; record + accessors
  (struct-out prov-origin)
  ;; constructors
  make-call-origin
  make-declared-origin
  ;; structural receiver chain
  callee-chain
  ;; identity
  prov-origin->string
  ;; fq split helper (file + name), used by callers and tests
  function-identity-parts)

;; ── record ──────────────────────────────────────────────────────────

(struct prov-origin (file line owner position chain) #:transparent)

;; Split a fully-qualified function identity ("component/file::name") into
;; its file and name parts. A same-file key has no "::" and no file.
(define (function-identity-parts fq)
  (define m (regexp-match-positions #rx"::" fq))
  (if m
      (let ([start (car (car m))] [end (cdr (car m))])
        (values (substring fq 0 start) (substring fq end)))
      (values #f fq)))

;; ── structural receiver chain ───────────────────────────────────────

;; The dotted source names a callee reads through, taken from the UIR shape.
;; A `uir-get` (`a.b`), a JS/TS `(call "dot" (a b))`, and a call used as a
;; receiver (`super().__init__`) extend the chain; anything else contributes
;; no name. Best-effort by construction: an unnameable shape contributes
;; nothing, and two sites are still distinguished by `position`.
(define (chain-field s)
  (define f (and s (get-field-symbol s)))
  (and f (symbol->string f)))

(define (callee-chain callee)
  (let loop ([c callee])
    (cond
      [(uir-get? c)
       (append (loop (uir-get-base c))
               (list (or (chain-field (uir-get-field c)) "?")))]
      [(dot-call? c)
       (append (loop (first (uir-call-args c)))
               (list (or (chain-field (second (uir-call-args c))) "?")))]
      [(uir-var? c) (list (uir-symbol-name (uir-var-name c)))]
      [(uir-symbol? c) (list (uir-symbol-name c))]
      [(uir-paren? c) (loop (uir-paren-inner c))]
      ;; A call used as a receiver chain base: name the invoked callee, e.g.
      ;; `super()...` contributes "super". The invocation itself is structural,
      ;; so this stays source-derived, not value-derived.
      [(uir-call? c) (loop (uir-call-callee c))]
      [else '()])))

;; ── constructors ────────────────────────────────────────────────────

;; The origin of a call-site ground. `owner-fq` is the function identity the
;; site lives in; `#:file` overrides the file component the identity would
;; otherwise take from `owner-fq` (per-file inference passes its source file).
(define (make-call-origin owner-fq callee
                          #:file [file #f]
                          #:line [line #f]
                          #:position [position #f])
  (define-values (fq-file owner) (function-identity-parts owner-fq))
  (prov-origin (or file fq-file) line owner position (callee-chain callee)))

;; The origin of a declared capability: the method definition that the DATA
;; rule asserts the capability at. There is no call site, so there is no
;; position; the receiver chain is the module path / class / method the rule
;; was derived from.
(define (make-declared-origin owner-fq chain
                              #:file [file #f]
                              #:line [line #f])
  (define-values (fq-file owner) (function-identity-parts owner-fq))
  (prov-origin (or file fq-file) line owner #f (filter values chain)))

;; ── canonical identity ──────────────────────────────────────────────

;; `file::owner@position:chain` (or `owner@...` without a file;
;; `@declared` for a definition site). The chain keeps its dotted reading so a
;; drift diff names the receiver chain directly. When the UIR gains source
;; positions, a call site's line enters the positional part as `line:position`
;; — a deliberate identity-format change, documented with the field. A
;; definition site has no call position, so its identity stays `declared`.
(define (prov-origin->string origin)
  (define owner-part
    (if (prov-origin-file origin)
        (format "~a::~a" (prov-origin-file origin) (prov-origin-owner origin))
        (prov-origin-owner origin)))
  (define positional
    (cond
      [(not (prov-origin-position origin)) "declared"]
      [(prov-origin-line origin)
       (format "~a:~a" (prov-origin-line origin) (prov-origin-position origin))]
      [else (format "~a" (prov-origin-position origin))]))
  (format "~a@~a:~a"
          owner-part
          positional
          (string-join (prov-origin-chain origin) ".")))
