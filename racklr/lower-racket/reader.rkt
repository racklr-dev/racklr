#lang racket

;; racklr/lower-racket/reader.rkt
;;
;; Racket source reader for the self-host front end. A Racket module starts
;; with `#lang <language>` and is then a sequence of datums that share the
;; language's reader. We only need the datum stream: the leading `#lang` line
;; is stripped and the remainder is read with `read`.
;;
;; This is the front end's "parser": Racket's own reader is the grammar, and
;; the lowering hook in lower-racket/expr.rkt turns the datum stream into UIR.
;; Reader failures are reported with the file path so a broken module names
;; its source.

(provide read-source-datums
         read-file-datums)

;; The first line of a Racket module is its `#lang ...` (or `#! ...`) line.
;; Reading it as a datum requires the #lang reader, which is unavailable from
;; inside a module body; stripping it is enough because the language only adds
;; a module wrapper we re-create ourselves when lowering.
(define lang-line-rx #rx"^[ \t]*#lang[^\n]*\n?")
(define shebang-line-rx #rx"^[ \t]*#![^\n]*\n")

(define (strip-language-line text)
  (define text* (regexp-replace shebang-line-rx text ""))
  (if (regexp-match? lang-line-rx text*)
      (regexp-replace lang-line-rx text* "")
      text*))

;; source-text string → (listof datum). Any `read` failure is re-raised as an
;; exn:fail naming `name` (normally a file path).
(define (read-source-datums text [name "<source>"])
  (define body (strip-language-line text))
  (with-handlers ([(λ (e) (exn:fail? e))
                   (λ (e)
                     (raise (exn:fail
                             (format "racklr/lower-racket: cannot read ~a: ~a"
                                     name (exn-message e))
                             (exn-continuation-marks e))))])
    (with-input-from-string body
      (λ ()
        (let loop ([acc '()])
          (define d (read))
          (if (eof-object? d)
              (reverse acc)
              (loop (cons d acc))))))))

;; A module file path → (listof datum).
(define (read-file-datums path)
  (define path* (if (path? path) path (string->path path)))
  (read-source-datums (file->string path*) (path->string path*)))
