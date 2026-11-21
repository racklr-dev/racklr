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

(define (line-comment-or-blank? trimmed)
  (or (equal? trimmed "")
      (regexp-match #rx"^//" trimmed)
      (regexp-match #rx"^/\\*" trimmed)
      (regexp-match #rx"^\\*" trimmed)))

(define (strip-type-only-imports source)
  ;; TypeScript 4.5 `import type ...` / `export type { ... } from ...` have no
  ;; runtime form and the vendored TS grammar has no `import type` production.
  ;; The lowering erases type-only declarations anyway, so drop these
  ;; statements before parsing. Only single-line statements are matched.
  (define lines (string-split source "\n" #:trim? #f))
  (define in-block-comment? #f)
  (string-join
   (for/list ([line (in-list lines)])
     (define trimmed (string-trim line))
     (cond
       [in-block-comment?
        (define closes? (regexp-match? #rx"\\*/" line))
        (set! in-block-comment? (not closes?))
        line]
       [(regexp-match #rx"^/\\*" trimmed)
        (unless (regexp-match? #rx"\\*/" trimmed) (set! in-block-comment? #t))
        line]
       [(regexp-match #rx"^//" trimmed) line]
       [(regexp-match #px"^[[:blank:]]*import[[:blank:]]+type[[:blank:]]+.*from[[:blank:]]+['\"][^'\"]*['\"][[:blank:]]*;?[[:blank:]]*$" line)
        ""]
       [(regexp-match #px"^[[:blank:]]*export[[:blank:]]+type[[:blank:]]+\\{[^}]*\\}[[:blank:]]*from[[:blank:]]+['\"][^'\"]*['\"][[:blank:]]*;?[[:blank:]]*$" line)
        ""]
       [else line]))
   "\n"))

(define (balanced-single-line? line)
  ;; Net bracket depth change of one line (strings/comments ignored), used to
  ;; avoid treating the first line of a multi-line import as a complete one.
  (call-with-values (λ () (line-bracket-delta line 'code)) (λ (d _) (= d 0))))

(define (insert-import-semicolons source)
  ;; The grammar's `importStatement` / `exportStatement` end in `eos`
  ;; (SemiColon | EOF); standard TypeScript relies on ASI, so single-line
  ;; imports without a semicolon fail to parse as an import statement and get
  ;; misparsed as expressions. Add the `;` the grammar wants.
  (define lines (string-split source "\n" #:trim? #f))
  (define in-block-comment? #f)
  (string-join
   (for/list ([line (in-list lines)])
     (define trimmed (string-trim line))
     (cond
       [in-block-comment?
        (define closes? (regexp-match? #rx"\\*/" line))
        (set! in-block-comment? (not closes?))
        line]
       [(regexp-match #rx"^/\\*" trimmed)
        (unless (regexp-match? #rx"\\*/" trimmed) (set! in-block-comment? #t))
        line]
       [(line-comment-or-blank? trimmed) line]
       [(and (regexp-match #px"^[[:blank:]]*import[[:blank:]]" line)
             (not (regexp-match? #px";[[:blank:]]*$" line))
             (balanced-single-line? line)
             (or (regexp-match #px"['\"][^'\"]*['\"][[:blank:]]*$" line)
                 (regexp-match #px"\\)[[:blank:]]*$" line)))
        (string-append line ";")]
       [(and (regexp-match #px"^[[:blank:]]*export[[:blank:]]+(\\*|\\{)" line)
             (not (regexp-match? #px";[[:blank:]]*$" line))
             (balanced-single-line? line)
             (regexp-match #px"['\"][^'\"]*['\"][[:blank:]]*$" line))
        (string-append line ";")]
       [else line]))
   "\n"))

;; ── B56/B63: Insert ; between type/interface members separated by newlines ─
;; The ANTLR TS grammar requires `;` or `,` between members of `type X = {}`
;; and `interface X {}` blocks; standard TS allows bare newlines. Semicolons
;; are accepted by the grammar, so we *add* them rather than strip them.

(define (insert-type-separators source)
  ;; Scan for `type X = {` or `interface X ... {` blocks, and within each,
  ;; add `;` at the end of member lines that lack a separator.
  ;; The match must land on the brace that opens the declaration's *body*:
  ;; for a type alias that is the `{` immediately after the `=`, and for an
  ;; interface it is a `{` on the same line as the header. A looser
  ;; `type X ... {` match also catches an object type or object literal on the
  ;; declaration's right-hand side, and the old pattern rewrote the `{}` of
  ;; `type T = A | (string & {})` into `(string & {};)`, corrupting the file.
  ;; The match must also start in code, not a comment or string: prose like
  ;; "... the request data type" followed by a line containing a `{` was
  ;; treated as a declaration and got a `;` inserted into the next annotation.
  (define rx-type-start
    #px"\\b(?:type[[:blank:]]+\\w+(?:[[:blank:]]*<[^>\n]*>)?[[:blank:]]*=[[:space:]]*\\{|interface[[:blank:]]+\\w+(?:[[:blank:]]*<[^>\n]*>)?[^\n{]*?\\{)")
  (let loop ([s source] [start 0])
    (define m (regexp-match-positions rx-type-start s start (string-length s)))
    (cond
      [(not m) s]
      [(not (code-position? s (caar m)))
       (loop s (add1 (caar m)))]
      [else
       (define brace-pos (sub1 (cdar m)))
       (define after-brace (substring s brace-pos))
       (define close-pos (find-matching-brace after-brace 0))
       (cond
         [(not close-pos)
          ;; Unmatched brace — skip past the { and continue
          (loop s (add1 brace-pos))]
         [else
          (define block-content (substring after-brace 1 close-pos))
          (define fixed-block (fix-type-member-lines block-content))
          (define before (substring s 0 brace-pos))
          (define close-abs (+ brace-pos close-pos))
          (define after (substring s (add1 close-abs)))
          ;; The grammar's `typeAliasDeclaration` ends in `eos`
          ;; (SemiColon | EOF); a bare newline before the next statement does
          ;; not terminate a `type X = {...}`. Add a `;` after the closing `}`
          ;; when the declaration continues into a new statement. A type
          ;; expression can continue past the block (`{...} & {...}`,
          ;; `{...}[]`, `{...} | T`), so a continuation token means the block is
          ;; mid-expression: inserting `;` there splits the type and makes the
          ;; source unparseable.
          (define next-non-space
            (let loop ([i 0])
              (cond [(>= i (string-length after)) #f]
                    [(char-whitespace? (string-ref after i)) (loop (add1 i))]
                    [else (string-ref after i)])))
          (define type-continuation-chars (string->list "&|[,.)]}>?:;=<>("))
          (define needs-semi
            ;; EOF also needs the `;`: the grammar's `typeAliasDeclaration` ends
            ;; in `eos` (`SemiColon | EOF`), and the historical contract of this
            ;; pass is to terminate the alias with `;` even at end of file.
            (not (and next-non-space
                      (memv next-non-space type-continuation-chars))))
          (define new-s (string-append before "{" fixed-block "}"
                                       (if needs-semi ";" "") after))
          (loop new-s (+ (string-length before) 1
                         (string-length fixed-block) 1
                         (if needs-semi 1 0)))])])))

(define (line-bracket-delta line state)
  ;; Net bracket depth change of one line, ignoring brackets inside strings and
  ;; comments. `state` carries an open block comment / string across lines.
  (define len (string-length line))
  (let loop ([i 0] [state state] [delta 0])
    (cond
      [(>= i len) (values delta (if (eq? state 'line-comment) 'code state))]
      [else
       (define c (string-ref line i))
       (define next (and (< (+ i 1) len) (string-ref line (+ i 1))))
       (case state
         [(code)
          (cond [(char=? c #\\) (loop (+ i 2) state delta)]
                [(char=? c #\') (loop (+ i 1) 'single delta)]
                [(char=? c #\") (loop (+ i 1) 'double delta)]
                [(char=? c #\`) (loop (+ i 1) 'template delta)]
                [(and (char=? c #\/) next (char=? next #\/))
                 (loop (+ i 2) 'line-comment delta)]
                [(and (char=? c #\/) next (char=? next #\*))
                 (loop (+ i 2) 'block-comment delta)]
                [(or (char=? c #\() (char=? c #\[) (char=? c #\{))
                 (loop (+ i 1) state (+ delta 1))]
                [(or (char=? c #\)) (char=? c #\]) (char=? c #\}))
                 (loop (+ i 1) state (- delta 1))]
                [else (loop (+ i 1) state delta)])]
         [(single)
          (cond [(char=? c #\\) (loop (+ i 2) state delta)]
                [(char=? c #\') (loop (+ i 1) 'code delta)]
                [else (loop (+ i 1) state delta)])]
         [(double)
          (cond [(char=? c #\\) (loop (+ i 2) state delta)]
                [(char=? c #\") (loop (+ i 1) 'code delta)]
                [else (loop (+ i 1) state delta)])]
         [(template)
          (cond [(char=? c #\\) (loop (+ i 2) state delta)]
                [(char=? c #\`) (loop (+ i 1) 'code delta)]
                [else (loop (+ i 1) state delta)])]
         [(line-comment) (loop (+ i 1) state delta)]
         [(block-comment)
          (if (and (char=? c #\*) next (char=? next #\/))
              (loop (+ i 2) 'code delta)
              (loop (+ i 1) state delta))])])))

(define (fix-type-member-lines block-str)
  ;; Add `;` after a top-level (bracket depth 0) member that lacks a
  ;; separator, but only when the member really ends on this line: the next
  ;; non-empty, non-comment line must not continue the same type (a leading
  ;; `|`, `&`, `)`, `]`, `>`, `.`, `?` or `:` starts a continuation). Line
  ;; endings alone are not enough — a multi-line union arm ends in `)` or `>`
  ;; mid-member, and a bare method signature ends in `void`.
  ;; `#:trim? #f` preserves the leading and trailing newlines of the block.
  (define lines (string-split block-str "\n" #:trim? #f))
  (define n (length lines))
  (define (comment-line? trimmed)
    (or (regexp-match #rx"^//" trimmed)
        (regexp-match #rx"^/\\*" trimmed)
        (regexp-match #rx"^\\*" trimmed)))
  (define (next-content i)
    (let loop ([j (add1 i)])
      (cond [(>= j n) #f]
            [else
             (define t (string-trim (list-ref lines j)))
             (cond [(equal? t "") (loop (add1 j))]
                   [(comment-line? t) (loop (add1 j))]
                   [else t])])))
  (define (continuation-start? t)
    (and t (pair? (string->list t))
         (memv (first (string->list t)) (string->list "|&)]>.?:"))))
  (define depth 0)
  (define state 'code)
  (string-join
   (for/list ([line (in-list lines)] [i (in-naturals)])
     (define trimmed (string-trim line))
     (define-values (delta next-state) (line-bracket-delta line state))
     (set! state next-state)
     (set! depth (max 0 (+ depth delta)))
     (cond [(equal? trimmed "") line]
           [(comment-line? trimmed) line]
           [(and (= depth 0)
                 (not (continuation-start? (next-content i)))
                 (not (regexp-match #rx"[{(,;|&=<>?:]\\s*$" trimmed)))
            (string-append line ";")]
           [else line]))
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
  (define s5 (insert-type-separators s4))
  ;; Step 1.8: Erase type-only import/export statements the grammar cannot parse
  (define s6 (strip-type-only-imports s5))
  ;; Step 1.9: ASI for import/export statements (grammar requires `eos`)
  (insert-import-semicolons s6))
