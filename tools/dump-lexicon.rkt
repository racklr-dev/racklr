#lang racket

;; tools/dump-lexicon.rkt — dump the default (Python/Django) lexicon's
;; name->domain DATA as JSON for the off-corpus calibration harness. Each entry
;; carries a kind: bare (bare call), method (dotted method name), module
;; (effect-rule prefix). pure means the empty row.

(require racket/string
         racklr/effects/library-tokens
         racklr/effects/imports/library-config
         (only-in racklr/effects/imports/effect-tag-types
                  effect-rule-prefix effect-rule-domain effect-library-rules))

(define (jstr s) (format "\"~a\"" s))

(define (entry kind name dom)
  (format "{\"name\":\"~a\",\"domain\":\"~a\",\"kind\":\"~a\"}"
          name (if (symbol? dom) (symbol->string dom) dom) kind))

(define (main)
  (define tokens default-effect-tokens)
  (define out '())
  ;; bare names: effect table wins over pure (pure is the absence).
  (define bare-fx (effect-tokens-bare-name-effects tokens))
  (define bare-pure (effect-tokens-pure-stdlib-bare-names tokens))
  (define bare (make-hash))
  (for ([name (in-set bare-pure)]) (hash-set! bare name "pure"))
  (for ([(name dom) (in-hash bare-fx)])
    (hash-set! bare name (symbol->string dom)))
  (for ([(name dom) (in-hash bare)])
    (set! out (cons (entry "bare" name dom) out)))
  ;; method effects (unambiguous table; the ambiguous table is gated, so it is
  ;; not part of the default's name-only surface).
  (define mfx (effect-tokens-unambiguous-method-effects tokens))
  (for ([(name dom) (in-hash mfx)])
    (set! out (cons (entry "method" name (symbol->string dom)) out)))
  ;; module effect rules (flatten effect-libraries).
  (for ([lib (in-list effect-tag-config)])
    (for ([rule (in-list (effect-library-rules lib))])
      (set! out (cons (entry "module" (effect-rule-prefix rule)
                             (symbol->string (effect-rule-domain rule)))
                      out))))
  (display (string-append "[" (string-join (reverse out) ",") "]")))

(module+ main (main))
