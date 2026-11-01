#lang racket

;; tools/self-emit/canonical.rkt — the canonical form of a translation result.
;;
;; P6.3 asks one comparison question: does a translation through the
;; self-emitted tower produce the same UIR-plus-row output as the original?
;; The emitted tower runs in another language, so the answer travels through
;; JSON, and this module fixes the JSON document that both sides produce:
;;
;;   { "format":   "racklr-self-emit/1",
;;     "language": "<source language>",
;;     "schema":   <UIR schema version>,
;;     "uir":      <versioned UIR JSON, as racklr's uir->json writes it>,
;;     "rows":     [ { "fn": "<name>",
;;                     "entries": [ { "effect": "<kind>",
;;                                    "modality": "may|must|must-not|unknown",
;;                                    "ops": ["<op>", ...],
;;                                    "budget": <number>|null,
;;                                    "scope": "<scope>"|null } ] } ] }
;;
;; Everything that a hash table could order arbitrarily is canonicalized:
;; object keys sorted, rows sorted by function name, entries sorted by effect
;; name, ops sorted. Two translations with the same UIR and the same rows
;; therefore produce byte-identical canonical JSON regardless of the hash
;; iteration order in either implementation.

(require json
         racklr/uir
         racklr/uir/version)

(provide FORMAT
         translation->jsexpr
         row->jsexpr
         rows->jsexpr
         canonical-json-string
         jsexpr-difference)

;; The document format identifier. Bump only with a deliberate change to the
;; comparison shape; the emitted tower must reproduce the current format.
(define FORMAT "racklr-self-emit/1")

;; ── rows ────────────────────────────────────────────────────────────

;; One uir-effect-entry as JSON. `effect` is the effect kind, `ops` the typed
;; op set when the classification knows it, `budget`/`scope` the optional
;; constraints the contract layer may carry.
(define (entry->jsexpr e)
  (hasheq 'effect (uir-symbol-name (uir-effect-entry-name e))
          'modality (symbol->string (uir-effect-entry-modality e))
          'ops (sort (map uir-symbol-name (uir-effect-entry-ops e)) string<?)
          'budget (let ([b (uir-effect-entry-budget e)]) (if b b 'null))
          'scope (let ([s (uir-effect-entry-scope e)])
                   (if s (uir-symbol-name s) 'null))))

(define (effect-name-of-entry-jsexpr e) (hash-ref e 'effect))
(define (fn-of-row-jsexpr r) (hash-ref r 'fn))

;; One uir-effect-row as a sorted JSON array of entry objects.
(define (row->jsexpr row)
  (sort (map entry->jsexpr (uir-effect-row-entries row))
        string<?
        #:key effect-name-of-entry-jsexpr))

;; (hashof symbol uir-effect-row) → sorted JSON array of row objects.
(define (rows->jsexpr rows)
  (define items
    (for/list ([(fn row) (in-hash rows)])
      (hasheq 'fn (symbol->string fn)
              'entries (row->jsexpr row))))
  (sort items string<? #:key fn-of-row-jsexpr))

;; ── the whole document ──────────────────────────────────────────────

;; A translation's canonical document. `uir` is serialized through the
;; versioned `uir->json` writer, so the UIR schema version travels with the
;; document instead of being guessed by the reader.
(define (translation->jsexpr language uir rows)
  (unless (memq language '(python json))
    (error 'translation->jsexpr "unsupported source language: ~a" language))
  (hasheq 'format FORMAT
          'language (symbol->string language)
          'schema UIR-SCHEMA-VERSION
          'uir (string->jsexpr (uir->json uir))
          'rows (rows->jsexpr rows)))

;; ── canonical JSON text ─────────────────────────────────────────────

(define (key->string k)
  (cond [(symbol? k) (symbol->string k)]
        [(string? k) k]
        [else (error 'canonical-json-string "object key is not a symbol or string: ~e" k)]))

;; Write `v` as JSON with object keys in sorted order. Arrays keep their
;; order: row and entry arrays are already sorted by the builder above, and
;; UIR arrays are ordered by construction (source order is semantic).
(define (write-canonical! v out)
  (cond
    [(hash? v)
     (define pairs
       (sort (for/list ([(k val) (in-hash v)]) (cons (key->string k) val))
             string<?
             #:key car))
     (display "{" out)
     (for ([i (in-naturals)] [pair (in-list pairs)])
       (when (> i 0) (display "," out))
       (display (jsexpr->string (car pair)) out)
       (display ":" out)
       (write-canonical! (cdr pair) out))
     (display "}" out)]
    [(or (list? v) (vector? v))
     (define items (if (vector? v) (vector->list v) v))
     (display "[" out)
     (for ([i (in-naturals)] [item (in-list items)])
       (when (> i 0) (display "," out))
       (write-canonical! item out))
     (display "]" out)]
    [else (display (jsexpr->string v) out)]))

;; Canonical JSON text. Two documents that are `equal?` as jsexpr values
;; always produce the same text through this writer.
(define (canonical-json-string jsexpr)
  (define out (open-output-string))
  (write-canonical! jsexpr out)
  (get-output-string out))

;; ── difference reporting ────────────────────────────────────────────

;; The JSON path of the first structural difference between `a` and `b`, or
;; #f when they are equal. Used to make a mismatch actionable: "rows/0/entries
;; differ" is a review, "documents differ" is not.
(define (jsexpr-difference a b [path "$"])
  (cond
    [(and (hash? a) (hash? b))
     (define keys
       (sort (remove-duplicates
              (append (for/list ([(k _) (in-hash a)]) (key->string k))
                      (for/list ([(k _) (in-hash b)]) (key->string k))))
             string<?))
     (for/or ([k (in-list keys)])
       (define av (hash-ref a (string->symbol k) #f))
       (define bv (hash-ref b (string->symbol k) #f))
       (cond [(not (hash-has-key? a (string->symbol k))) (format "~a/~a (missing on left)" path k)]
             [(not (hash-has-key? b (string->symbol k))) (format "~a/~a (missing on right)" path k)]
             [else (jsexpr-difference av bv (format "~a/~a" path k))]))]
    [(or (list? a) (list? b))
     (define av (if (list? a) a '()))
     (define bv (if (list? b) b '()))
     (cond
       [(not (and (list? a) (list? b))) (format "~a (array vs non-array)" path)]
       [else
        (define n (max (length av) (length bv)))
        (for/or ([i (in-range n)])
          (cond [(>= i (length av)) (format "~a/~a (missing on left)" path i)]
                [(>= i (length bv)) (format "~a/~a (missing on right)" path i)]
                [else (jsexpr-difference (list-ref av i) (list-ref bv i)
                                         (format "~a/~a" path i))]))])]
    [(and (or (list? a) (vector? a) (hash? a)) (or (list? b) (vector? b) (hash? b)))
     (format "~a (shape differs)" path)]
    [(equal? a b) #f]
    [else (format "~a: ~s vs ~s" path a b)]))
