#lang racket

(require racket/string
         "helpers.rkt")

(provide preprocess-imports)

;; NOTE: the regex-based `preprocess-hooks` was removed here (B71). Hooks are
;; lowered structurally by lower-tsx/hook-lower.rkt (B35); this module now only
;; handles import preprocessing and type/interface member-separator insertion.

;; ── Brace-matching helper (needed by insert-type-separators) ─────────

(define (find-matching-brace s open-pos)
  (define len (string-length s))
  (unless (and (< open-pos len) (char=? (string-ref s open-pos) #\{))
    (error 'find-matching-brace "expected { at ~a" open-pos))
  (let loop ([pos (+ open-pos 1)] [depth 1])
    (cond [(= depth 0) (- pos 1)]
          [(>= pos len) #f]
          [(or (char=? (string-ref s pos) #\')
               (char=? (string-ref s pos) #\"))
           (loop (advance-past-string s pos (string-ref s pos)) depth)]
          [(char=? (string-ref s pos) #\{) (loop (+ pos 1) (+ depth 1))]
          [(char=? (string-ref s pos) #\}) (loop (+ pos 1) (- depth 1))]
          [else (loop (+ pos 1) depth)])))

;; ── B56/B63: Insert ; between type/interface members separated by newlines ─
;; The ANTLR TS grammar requires `;` or `,` between members of `type X = {}`
;; and `interface X {}` blocks; standard TS allows bare newlines. Semicolons
;; are accepted by the grammar, so we *add* them rather than strip them.

(define (insert-type-separators source)
  ;; Scan for `type X = {` or `interface X ... {` blocks, and within each,
  ;; add `;` at the end of member lines that lack a separator.
  ;; `[^\n{]` keeps the `{` on the same line as the keyword (newline is excluded
  ;; from the match), so prose like "import the interface below do:" cannot
  ;; reach the `{` of a commented-out import on a later line (which corrupted
  ;; `import { User }` into `import { User ;}`).
  (define rx-type-start #px"(?:type|interface)\\s+\\w+[^\n{]*?\\{")
  (let loop ([s source] [start 0])
    (define m (regexp-match-positions rx-type-start s start (string-length s)))
    (if (not m)
        s
        (let* ([match-end (cdar m)]
               [brace-pos (- match-end 1)]  ;; position of the {
               [after-brace (substring s brace-pos)]
               [close-pos (find-matching-brace after-brace 0)])
          (if (not close-pos)
              ;; Unmatched brace — skip past the { and continue
              (loop s (+ brace-pos 1))
              (let* ([block-content (substring after-brace 1 close-pos)]
                     [fixed-block (fix-type-member-lines block-content)]
                     [before (substring s 0 brace-pos)]
                     [close-abs (+ brace-pos close-pos)]
                     [after (substring s (add1 close-abs))]
                     ;; The grammar's `typeAliasDeclaration` ends in `eos`
                     ;; (SemiColon | EOF); a bare newline before the next
                     ;; statement does not terminate a `type X = {...}`. Add a
                     ;; `;` after the closing `}` when the next non-space char
                     ;; isn't already `;` (interfaces accept it, so this is
                     ;; harmless there too).
                     [needs-semi (not (regexp-match? #px"^[[:space:]]*;" after))])
                (define new-s (string-append before "{" fixed-block "}"
                                             (if needs-semi ";" "") after))
                (loop new-s (+ (string-length before) 1
                               (string-length fixed-block) 1
                               (if needs-semi 1 0)))))))))

(define (fix-type-member-lines block-str)
  ;; For each line in the block: if it's non-empty and doesn't end with
  ;; {, }, ,, or ;, add ; at the end. #:trim? #f preserves the leading
  ;; and trailing newlines of the block.
  (define lines (string-split block-str "\n" #:trim? #f))
  (string-join
   (for/list ([line (in-list lines)])
     (define trimmed (string-trim line))
     (cond [(equal? trimmed "") line]
            ;; A trailing `}` closes a nested object type (e.g. `ogImage: {…}`);
            ;; it still needs a member separator. Only `{`, `,`, `;` are already
            ;; terminated (or open a nested block).
            [(regexp-match #rx"[{,;]\\s*$" trimmed) line]
           [else (string-append line ";")]))
   "\n"))

;; ── Ambient module declarations (`.d.ts` bodies) ─────────────────────
;; The vendored ANTLR TS grammar has no `moduleDeclaration` rule, so an ambient
;; `declare module 'name'` — the body of a `.d.ts` file — cannot be parsed at
;; all. Ambient declarations have no runtime form, so strip them before
;; parsing; surrounding runtime declarations still lower normally.
;;
;; Only the quoted-string name form is matched. Upstream TypeScript also allows
;; a dotted identifier name (`declare module a.b.c { ... }`, ModuleDeclaration
;; in the TS spec); that form is not handled here, so a file using it still
;; fails to parse. Extend this strip point if such `.d.ts` files appear in a
;; corpus. (The regex-plus-state-machine approach is a source-level patch; the
;; durable fix is folding declaration erasure into the lexer pass.)

(define rx-ambient-module
  #px"(^|[;{}\n])[[:space:]]*declare[[:space:]]+module[[:space:]]+(['\"])([^'\"]*)\\2")

(define (skip-whitespace s pos)
  (define len (string-length s))
  (let loop ([p pos])
    (if (and (< p len) (char-whitespace? (string-ref s p))) (loop (+ p 1)) p)))

;; A declaration ends at its `;`, at the matching `}` of its body, or (no body,
;; no semicolon) at the next non-space token.
(define (ambient-module-end s body-start)
  (define len (string-length s))
  (cond
    [(>= body-start len) body-start]
    [(char=? (string-ref s body-start) #\;) (+ body-start 1)]
    [(char=? (string-ref s body-start) #\{)
     (define close (find-matching-brace s body-start))
     (if close (+ close 1) body-start)]
    [else body-start]))

;; True when pos is ordinary code, i.e. not inside a string literal or comment.
;; The boundary characters the regex accepts can also occur inside those, so a
;; match is only a declaration when it starts in code.
(define (code-position? s pos)
  (define len (string-length s))
  (let loop ([i 0] [state 'code])
    (cond
      [(>= i pos) (eq? state 'code)]
      [else
       (define c (string-ref s i))
       (define next (and (< (+ i 1) len) (string-ref s (+ i 1))))
       (case state
         [(code)
          (cond [(char=? c #\\) (loop (+ i 1) 'code)]
                [(char=? c #\') (loop (+ i 1) 'single)]
                [(char=? c #\") (loop (+ i 1) 'double)]
                [(char=? c #\`) (loop (+ i 1) 'template)]
                [(and (char=? c #\/) next (char=? next #\/)) (loop (+ i 2) 'line-comment)]
                [(and (char=? c #\/) next (char=? next #\*)) (loop (+ i 2) 'block-comment)]
                [else (loop (+ i 1) 'code)])]
         [(single)
          (cond [(char=? c #\\) (loop (+ i 2) state)]
                [(char=? c #\') (loop (+ i 1) 'code)]
                [else (loop (+ i 1) state)])]
         [(double)
          (cond [(char=? c #\\) (loop (+ i 2) state)]
                [(char=? c #\") (loop (+ i 1) 'code)]
                [else (loop (+ i 1) state)])]
         [(template)
          (cond [(char=? c #\\) (loop (+ i 2) state)]
                [(char=? c #\`) (loop (+ i 1) 'code)]
                [else (loop (+ i 1) state)])]
         [(line-comment)
          (if (char=? c #\newline) (loop (+ i 1) 'code) (loop (+ i 1) state))]
         [(block-comment)
          (if (and (char=? c #\*) next (char=? next #\/))
              (loop (+ i 2) 'code)
              (loop (+ i 1) state))])])))

;; Remove every ambient module declaration. The leading boundary character
;; (start of file, `;`, `}`, or newline) is kept so the remaining statements
;; stay separated. Matches that only occur in strings/comments are left alone.
(define (strip-ambient-modules source)
  (define len (string-length source))
  (let loop ([cursor 0] [search 0] [pieces '()])
    (define m (regexp-match-positions rx-ambient-module source search len))
    (cond
      [(not m)
       (string-append* (reverse (cons (substring source cursor len) pieces)))]
      [else
       (define whole (car m))
       (define delete-start (cdr (second m)))
       (cond
         [(not (code-position? source delete-start))
          ;; Not a declaration (text inside a string/comment); resume searching
          ;; one character past this match's start without consuming anything.
          (loop cursor (+ (car whole) 1) pieces)]
         [else
          (define delete-end
            (ambient-module-end source (skip-whitespace source (cdr whole))))
          (loop delete-end delete-end
                (cons (substring source cursor delete-start) pieces))])])))

;; ── Import stripping (regex — keeps source valid for TS parser) ─────

(define (preprocess-imports source)
  ;; Step 0: erase ambient declarations the grammar cannot parse
  (define s-ambient (strip-ambient-modules source))

  ;; Step 1: Remove react imports
  (define rx-import-braces #px"import[[:space:]]+\\{[^}]*\\}[[:space:]]+from[[:space:]]+[\"']react[\"'][[:space:]]*;?[[:space:]]*\n?")
  (define rx-import-combo  #px"import[[:space:]]+[[:word:]]+[[:space:]]*,[[:space:]]*\\{[^}]*\\}[[:space:]]+from[[:space:]]+[\"']react[\"'][[:space:]]*;?[[:space:]]*\n?")
  (define rx-import-default #px"import[[:space:]]+[[:word:]]+[[:space:]]+from[[:space:]]+[\"']react[\"'][[:space:]]*;?[[:space:]]*\n?")
  
  (define s0 (regexp-replace* rx-import-braces s-ambient ""))
  (define s1 (regexp-replace* rx-import-combo s0 ""))
  (define s2 (regexp-replace* rx-import-default s1 ""))

  ;; Step 1.5: Remove CSS module imports (B28)
  (define rx-css-module #px"import[[:space:]]+[[:word:]]+[[:space:]]+from[[:space:]]+[\"'][^\"']*\\.module\\.css[\"'][[:space:]]*;?[[:space:]]*\n?")
  (define s3 (regexp-replace* rx-css-module s2 ""))

  ;; Step 1.5b: Remove npm polyfilled imports (B65) — classnames, date-fns
  (define (strip-npm-import source pkg)
    (define rx-braces   (pregexp (string-append "import[[:space:]]+\\{[^}]*\\}[[:space:]]+from[[:space:]]+[\"']" pkg "[\"'][[:space:]]*;?[[:space:]]*\n?")))
    (define rx-combo    (pregexp (string-append "import[[:space:]]+[[:word:]]+[[:space:]]*,[[:space:]]*\\{[^}]*\\}[[:space:]]+from[[:space:]]+[\"']" pkg "[\"'][[:space:]]*;?[[:space:]]*\n?")))
    (define rx-default  (pregexp (string-append "import[[:space:]]+[[:word:]]+[[:space:]]+from[[:space:]]+[\"']" pkg "[\"'][[:space:]]*;?[[:space:]]*\n?")))
    (regexp-replace* rx-braces
      (regexp-replace* rx-combo
        (regexp-replace* rx-default source "") "") ""))
  (define s3b (foldl (lambda (pkg src) (strip-npm-import src pkg)) s3 '("classnames" "date-fns")))

  ;; Step 1.6: Normalize double-spaces after { (only literal spaces — do not
  ;; collapse `{\n`, which would join the opening brace onto the first member)
  (define s4 (regexp-replace* #px"\\{ {2,}" s3b "{ "))

  ;; Step 1.7: Insert ; between type/interface members separated by newlines
  (insert-type-separators s4))
