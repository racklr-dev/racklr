#lang racket

;; racklr-test/uir-koka-test.rkt — Koka 3.x additive UIR node kinds.
;;
;; One additive kind per real modern-Koka construct whose syntax has no
;; existing UIR representation. Each is checked for construction, tag,
;; accessors, and both serialization directions (the codec is schema-driven, so
;; this also guards each kind's schema entry). The Koka cases live here rather
;; than in uir-test.rkt so the general UIR test stays at its pre-Koka size.

(require rackunit
         racklr/uir)

;; ── Koka 3.x source node kinds ──────────────────────────────────────
;;
;; One additive kind per real modern-Koka construct whose syntax has no
;; existing UIR representation. Each is checked for construction, tag,
;; accessors, and both serialization directions (the codec is schema-driven, so
;; this also guards each kind's schema entry).

(define (test-koka-struct)
  (define u (uir-struct (uir-symbol "timezone-info")
                        (list (uir-symbol "a"))
                        (list (uir-typed-param (uir-symbol "name")
                                               (uir-symbol "string")
                                               #f #f))
                        'public))
  (check-true (uir? u) "uir-struct is a UIR node")
  (check-equal? (uir-tag u) 'struct "uir-struct tag")
  (check-equal? (uir-symbol-name (uir-struct-name u)) "timezone-info")
  (check-equal? (length (uir-struct-fields u)) 1 "uir-struct field count")
  (check-equal? (uir-struct-visibility u) 'public "uir-struct visibility")
  (check-equal? (sexp->uir (uir->sexp u)) u "uir-struct sexp round-trip")
  (check-equal? (json->uir (uir->json u)) u "uir-struct json round-trip"))

(define (test-koka-fixity)
  (define u (uir-fixity 'infixl 70 (list (uir-symbol "(*)") (uir-symbol "(/)"))))
  (check-true (uir? u) "uir-fixity is a UIR node")
  (check-equal? (uir-tag u) 'fixity "uir-fixity tag")
  (check-equal? (uir-fixity-precedence u) 70 "uir-fixity precedence")
  (check-equal? (length (uir-fixity-names u)) 2 "uir-fixity names")
  (check-equal? (sexp->uir (uir->sexp u)) u "uir-fixity sexp round-trip")
  (check-equal? (json->uir (uir->json u)) u "uir-fixity json round-trip"))

(define (test-koka-extern)
  (define ev (uir-extern-value
              (uir-symbol "atom-create")
              (uir-symbol "( s : string ) -> int32")
              (list (cons (uir-symbol "cs") (uir-string "Atom.Create")))
              'private '()))
  (check-true (uir-extern-value? ev) "uir-extern-value is a UIR node")
  (check-equal? (uir-tag ev) 'extern-value "uir-extern-value tag")
  (check-equal? (length (uir-extern-value-entries ev)) 1 "extern-value entries")
  (check-equal? (sexp->uir (uir->sexp ev)) ev "uir-extern-value sexp round-trip")
  (check-equal? (json->uir (uir->json ev)) ev "uir-extern-value json round-trip")
  (define ef (uir-extern-fun
              (uir-symbol "main-console")
              (list (uir-symbol "a") (uir-symbol "e"))
              (list (uir-typed-param (uir-symbol "main") (uir-symbol "() -> e a") #f #f))
              (uir-symbol "e a")
              '() 'public (list (uir-symbol "inline"))))
  (check-true (uir-extern-fun? ef) "uir-extern-fun is a UIR node")
  (check-equal? (uir-tag ef) 'extern-fun "uir-extern-fun tag")
  (check-equal? (uir-symbol-name (uir-extern-fun-result ef)) "e a")
  (check-equal? (sexp->uir (uir->sexp ef)) ef "uir-extern-fun sexp round-trip")
  (check-equal? (json->uir (uir->json ef)) ef "uir-extern-fun json round-trip")
  (define ei (uir-extern-import
              (uir-symbol "js")
              (list (cons (uir-symbol "file") (uir-string "timezone-inline.js")))))
  (check-true (uir-extern-import? ei) "uir-extern-import is a UIR node")
  (check-equal? (uir-tag ei) 'extern-import "uir-extern-import tag")
  (check-equal? (sexp->uir (uir->sexp ei)) ei "uir-extern-import sexp round-trip")
  (check-equal? (json->uir (uir->json ei)) ei "uir-extern-import json round-trip"))

(define (test-koka-handler)
  (define op (uir-handler-op 'fun
                             (uir-symbol "get")
                             (uir-symbol "int")
                             '()
                             (uir-number "42")))
  (check-true (uir-handler-op? op) "uir-handler-op is a UIR node")
  (check-equal? (uir-handler-op-kind op) 'fun "handler-op kind")
  (check-true (uir? (uir-handler-op-body op)) "handler-op body")
  (define h (uir-handler 'with
                         (uir-symbol "async-await")
                         #f
                         '()
                         (list op)
                         (uir-call (uir-var (uir-symbol "action")) '())))
  (check-true (uir-handler? h) "uir-handler is a UIR node")
  (check-equal? (uir-tag h) 'handler "uir-handler tag")
  (check-equal? (uir-handler-sort h) 'with "handler sort")
  (check-equal? (length (uir-handler-ops h)) 1 "handler op count")
  (check-true (uir? (uir-handler-body h)) "handler body")
  (check-equal? (sexp->uir (uir->sexp op)) op "uir-handler-op sexp round-trip")
  (check-equal? (json->uir (uir->json op)) op "uir-handler-op json round-trip")
  (check-equal? (sexp->uir (uir->sexp h)) h "uir-handler sexp round-trip")
  (check-equal? (json->uir (uir->json h)) h "uir-handler json round-trip"))

(define (test-koka-mask)
  (define u (uir-mask (list (uir-symbol "strands") (uir-symbol "ndet"))
                      #t 'apply (uir-var (uir-symbol "f"))))
  (check-true (uir? u) "uir-mask is a UIR node")
  (check-equal? (uir-tag u) 'mask "uir-mask tag")
  (check-equal? (length (uir-mask-effects u)) 2 "uir-mask effects")
  (check-true (uir-mask-behind u) "uir-mask behind")
  (check-equal? (sexp->uir (uir->sexp u)) u "uir-mask sexp round-trip")
  (check-equal? (json->uir (uir->json u)) u "uir-mask json round-trip"))

(define (test-koka-misc-kinds)
  ;; named argument
  (define na (uir-named-arg (uir-symbol "download-delay") (uir-var (uir-symbol "d"))))
  (check-equal? (uir-tag na) 'named-arg "uir-named-arg tag")
  (check-equal? (sexp->uir (uir->sexp na)) na "uir-named-arg sexp round-trip")
  (check-equal? (json->uir (uir->json na)) na "uir-named-arg json round-trip")
  ;; character literal keeps its quoted source text
  (define ch (uir-char "'a'"))
  (check-equal? (uir-tag ch) 'char "uir-char tag")
  (check-equal? (uir-char-value ch) "'a'" "uir-char value")
  (check-equal? (sexp->uir (uir->sexp ch)) ch "uir-char sexp round-trip")
  (check-equal? (json->uir (uir->json ch)) ch "uir-char json round-trip")
  ;; declaration type-parameter binder (a declaration decorator)
  (define tp (uir-type-params (list (uir-symbol "a") (uir-symbol "e :: E"))))
  (check-equal? (uir-tag tp) 'type-params "uir-type-params tag")
  (check-equal? (length (uir-type-params-params tp)) 2 "type-params binders")
  (check-equal? (sexp->uir (uir->sexp tp)) tp "uir-type-params sexp round-trip")
  (check-equal? (json->uir (uir->json tp)) tp "uir-type-params json round-trip")
  ;; import alias (`import elem = sys/dom/html/htmlElement`)
  (define ia (uir-import-alias (uir-symbol "elem")
                               (uir-symbol "sys/dom/html/htmlElement")
                               '()))
  (check-equal? (uir-tag ia) 'import-alias "uir-import-alias tag")
  (check-equal? (sexp->uir (uir->sexp ia)) ia "uir-import-alias sexp round-trip")
  (check-equal? (json->uir (uir->json ia)) ia "uir-import-alias json round-trip")
  ;; expression ascription
  (define asc (uir-ascribe (uir-list (list (uir-number "1"))) (uir-symbol "list<int>")))
  (check-equal? (uir-tag asc) 'ascribe "uir-ascribe tag")
  (check-equal? (sexp->uir (uir->sexp asc)) asc "uir-ascribe sexp round-trip")
  (check-equal? (json->uir (uir->json asc)) asc "uir-ascribe json round-trip")
  ;; implicit reference (`?show` / `?(==)`)
  (define im (uir-implicit-ref (uir-symbol "show")))
  (check-equal? (uir-tag im) 'implicit-ref "uir-implicit-ref tag")
  (check-equal? (uir-symbol-name (uir-implicit-ref-name im)) "show")
  (check-equal? (sexp->uir (uir->sexp im)) im "uir-implicit-ref sexp round-trip")
  (check-equal? (json->uir (uir->json im)) im "uir-implicit-ref json round-trip")
  ;; index expression
  (define ix (uir-index (uir-var (uir-symbol "xs")) (uir-var (uir-symbol "i"))))
  (check-equal? (uir-tag ix) 'index "uir-index tag")
  (check-equal? (sexp->uir (uir->sexp ix)) ix "uir-index sexp round-trip")
  (check-equal? (json->uir (uir->json ix)) ix "uir-index json round-trip"))

(module+ main
  (test-koka-struct)
  (test-koka-fixity)
  (test-koka-extern)
  (test-koka-handler)
  (test-koka-mask)
  (test-koka-misc-kinds)
  (displayln "All Koka UIR tests passed."))
