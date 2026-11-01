#lang racket

(require racklr/tree
         racklr/g4-parse
         "language-policy.rkt")

(provide extract-option
         load-lexer-grammar-rules
         lexer-has-newline-rule?
         lexer-rule-hidden?
         collect-parser-literals
         build-implicit-lexer-rules
         build-mode-map
         gen-header
         gen-match-helpers
         gen-lexer-matchers
         gen-tokenizer
         unescape-g4-literal)

;; ── Helpers for tokenVocab ─────────────────────────────────────────

(define (extract-option options-node key)
  (for/or ([opt (any-tree-children options-node)]
           #:when (string=? (any-tree-text (first (any-tree-children opt))) key))
    (any-tree-text (second (any-tree-children opt)))))

(define (load-lexer-grammar-rules lexer-path)
  (define lexer-cst (parse-g4-file lexer-path))
  (define rules-node (second (any-tree-children lexer-cst)))
  (define rules (any-tree-children rules-node))
  (values (filter (lambda (r) (eq? (any-tree-tag r) 'lexer-rule)) rules)
          (filter (lambda (r) (eq? (any-tree-tag r) 'fragment-rule)) rules)
          (filter (lambda (r) (eq? (any-tree-tag r) 'mode)) rules)))

(define (lexer-has-newline-rule? lexer-rules [policy default-language-policy])
  (define newline-name (language-policy-newline-token-name policy))
  (and newline-name
       (for/or ([r lexer-rules])
         (and (eq? (any-tree-tag r) 'lexer-rule)
              (string=? (any-tree-text (first (any-tree-children r))) newline-name)))))

;; Is this lexer rule hidden from the parser (channel(HIDDEN) / channel(1))?
;; With comment preservation these tokens are emitted (not skipped) so they can
;; be inspected, then filtered out of the token list the parser matches.
(define (lexer-rule-hidden? rule)
  (define children (any-tree-children rule))
  (and (> (length children) 2)
       (for/or ([cmd (drop children 2)])
         (define txt (any-tree-text cmd))
         (and (string-prefix? txt "channel(")
              (let ([ch (substring txt 8 (sub1 (string-length txt)))])
                (or (string=? ch "HIDDEN") (string=? ch "1")))))))
;; ── Implicit Lexer Rules for Parser Literals ─────────────────────
;; In ANTLR4, string literals like '{' or 'true' used directly in parser
;; rules are implicitly tokenized. We extract them and generate lexer rules.

(define (collect-parser-literals parser-rules)
  (define literals '())
  (define (walk node)
    (when node
      (when (and (any-tree? node) (eq? (any-tree-tag node) 'literal))
        (define txt (any-tree-text node))
        (set! literals (cons txt literals)))
      (when (cst-node? node)
        (for ([ch (cst-node-children node)])
          (walk ch)))))
  (for ([r parser-rules])
    (walk r))
  (remove-duplicates literals))

(define (build-implicit-lexer-rules literals existing-lexer-rules)
  ;; Collect names of existing lexer rules (non-fragment ones)
  (define existing-names
    (for/list ([r existing-lexer-rules]
                #:when (eq? (any-tree-tag r) 'lexer-rule))
      (any-tree-text (first (any-tree-children r)))))
  ;; Build a synthetic rule CST for each literal not already covered
  ;; A lexer rule CST: (node 'lexer-rule ((leaf 'name ...) (node 'alternatives ((node 'alternative ((leaf 'literal ...)))))))
  ;; For simplicity, we just create minimal structures the generator can walk
  (for/list ([lit literals]
              #:unless (member lit existing-names))
    ;; Only handle simple single-char or keyword literals
    ;; Skip literals that would conflict with existing rules
    (node 'lexer-rule
          (list (leaf 'name lit #:start (pos 0 0 0) #:end (pos 0 0 0))
                (node 'alternatives
                      (list (node 'alternative
                                  (list (leaf 'literal lit #:start (pos 0 0 0) #:end (pos 0 0 0))))))))))

;; ── Sections ──────────────────────────────────────────────────────

;; ── Source-position helpers (embedded in generated lexer) ─────────
;; A lexer token can span several lines: a multi-line triple-quoted string, a
;; NEWLINE token carrying the next line's indentation, a multi-line comment or
;; a template literal. The position after such a token must account for the
;; newlines inside it; otherwise every following token is reported on the
;; token's opening line.
(define (gen-position-helper)
  "
;; Advance the (line, column) of a matched token's text by one token.
;; \\r\\n counts as a single line break; lone \\n and \\r also break the line.
;; (A form feed is whitespace that the tokenizer skips before any rule matches
;; it, so it only ever appears inside a token's text and is not a line break.
;; This matches Python's tokenizer; do not turn it into a line break.)
(define (advance-line-col l c text)
  (define len (string-length text))
  (let loop ([i 0] [l l] [c c])
    (cond
      [(>= i len) (values l c)]
      [(char=? (string-ref text i) #\\newline)
       (loop (+ i 1) (+ l 1) 1)]
      [(char=? (string-ref text i) #\\return)
       (if (and (< (+ i 1) len) (char=? (string-ref text (+ i 1)) #\\newline))
           (loop (+ i 2) (+ l 1) 1)
           (loop (+ i 1) (+ l 1) 1))]
      [else (loop (+ i 1) l (+ c 1))])))
"
  )

(define (gen-header #:indent-tokens? [indent-tokens? #f]
                     #:policy [policy default-language-policy])
  (string-append
   "#lang racket
(require racklr/tree)
(provide token token? token-type token-value token-start token-end tokenize parse)
(struct token (type value start end) #:transparent)"
   (gen-position-helper)
   (let ([layout (language-policy-layout-helper-source policy)])
     (if (and indent-tokens? layout)
         layout
         ""))))

;; Mangle a string into a valid Racket identifier
(define (mangle str)
  (if (regexp-match? #rx"^[a-zA-Z_][a-zA-Z0-9_]*$" str)
      str
      (string-join
       (for/list ([c str])
         (cond [(char-alphabetic? c) (string c)]
               [(char-numeric? c) (string c)]
               [(char=? c #\_) "_"]
               [else (format "_x~a_" (number->string (char->integer c) 16))]))
       "")))

;; ── ANTLR4 escape sequences for char classes ──────────────────────
;; Convert ANTLR4 escape sequences in character class strings to actual chars.
;; Handles: \\t \\n \\r \\uXXXX (Unicode code points)
(define (unescape-char-class s)
  ;; Pre-process: strip surrogate code point ranges (\\uD800-\\uDFFF) from
  ;; the character class. In Racket's code-point model, surrogates don't exist
  ;; as characters, so ranges involving them match nothing. We remove them
  ;; from the pattern to prevent spurious matches in cc-match.
  ;;
  ;; Three patterns to handle, in order:
  ;;   \\uXXXX-\\uYYYY where both are surrogates → remove entire range
  ;;   \\uXXXX- at start of range (lower bound surrogate) → remove \\uXXXX-
  ;;   -\\uXXXX at end of range (upper bound surrogate) → remove -\\uXXXX
  ;;   Standalone surrogate \\uXXXX → remove it
  ;;
  ;; Surrogate hex: D800-DFFF. High surrogates: D[89AB]xxx. Low: D[CDEF]xxx.
  
  (define s0
    (let loop ([s s])
      (cond
        ;; Entire surrogate-surrogate range (high-high, low-low, or high-low)
        [(regexp-match-positions #px"\\\\uD[89AB][0-9A-Fa-f]{2}-\\\\uD[89AB][0-9A-Fa-f]{2}" s)
         => (lambda (m)
              (define i (caar m))
              (define j (cdar m))
              (loop (string-append (substring s 0 i) (substring s j))))]
        [(regexp-match-positions #px"\\\\uD[CDEF][0-9A-Fa-f]{2}-\\\\uD[CDEF][0-9A-Fa-f]{2}" s)
         => (lambda (m)
              (define i (caar m))
              (define j (cdar m))
              (loop (string-append (substring s 0 i) (substring s j))))]
        ;; Surrogate at end of range: X-\\uXXXX
        [(regexp-match-positions #px"-\\\\uD[89AB][0-9A-Fa-f]{2}" s)
         => (lambda (m)
              (loop (string-append (substring s 0 (caar m)) (substring s (cdar m)))))]
        [(regexp-match-positions #px"-\\\\uD[CDEF][0-9A-Fa-f]{2}" s)
         => (lambda (m)
              (loop (string-append (substring s 0 (caar m)) (substring s (cdar m)))))]
        ;; Surrogate at start of range: \\uXXXX-
        [(regexp-match-positions #px"\\\\uD[89AB][0-9A-Fa-f]{2}-" s)
         => (lambda (m)
              (loop (string-append (substring s 0 (caar m)) (substring s (cdar m)))))]
        [(regexp-match-positions #px"\\\\uD[CDEF][0-9A-Fa-f]{2}-" s)
         => (lambda (m)
              (loop (string-append (substring s 0 (caar m)) (substring s (cdar m)))))]
        ;; Standalone surrogate: just remove it
        [(regexp-match-positions #px"\\\\uD[89AB][0-9A-Fa-f]{2}" s)
         => (lambda (m)
              (loop (string-append (substring s 0 (caar m)) (substring s (cdar m)))))]
        [(regexp-match-positions #px"\\\\uD[CDEF][0-9A-Fa-f]{2}" s)
         => (lambda (m)
              (loop (string-append (substring s 0 (caar m)) (substring s (cdar m)))))]
        [else s])))
  
  ;; Now convert remaining (non-surrogate) \\uXXXX to actual characters
  ;; Use pregexp (not regexp) because regexp doesn't support {n} quantifiers
  ;; regexp-replace* with pregexp passes (matched . groups) to the lambda
  (define s1
    (regexp-replace* #px"\\\\u([0-9A-Fa-f]{4})" s0
      (lambda (matched hex-str)
        (define code (string->number hex-str 16))
        (string (integer->char code)))))
  ;; Then handle \\t \\n \\r \\f
  (define s2 (regexp-replace* #rx"\\\\t"
               (regexp-replace* #rx"\\\\n"
                (regexp-replace* #rx"\\\\r"
                 (regexp-replace* #rx"\\\\f" s1 "\f")
                 "\r")
                "\n")
               "\t"))
  ;; Convert \\- (escaped hyphen) to a bare - at start of class (right after [).
  ;; In Racket cc-match, - in the middle of a class is a range operator.
  ;; By moving to position 1, it is always literal.
  (define has-hyphen? (regexp-match? #rx"\\\\-" s2))
  (define no-hyphens (regexp-replace* #rx"\\\\-" s2 ""))
  (define s3
    (if (and has-hyphen? (>= (string-length no-hyphens) 1) (char=? (string-ref no-hyphens 0) #\[))
        (string-append "[" (if (and (>= (string-length no-hyphens) 2) (char=? (string-ref no-hyphens 1) #\^))
                               (string-append "^" "-" (substring no-hyphens 2))
                               (string-append "-" (substring no-hyphens 1))))
        no-hyphens))
  s3)

;; Convert ANTLR4 escape sequences in literal strings to actual chars.
;; E.g. "\\n" -> newline, "\\\\" -> backslash, "\\'" -> "'", "\\u0041" -> "A".
;; A left-to-right scan (rather than nested regexp replacements) is what makes
;; \\uXXXX safe: an escaped backslash before a `u` must not be read as a
;; Unicode escape.
(define (hex-digit? c)
  (or (char-numeric? c) (char<=? #\a c #\f) (char<=? #\A c #\F)))

(define (unescape-g4-literal s)
  (define n (string-length s))
  (define out (open-output-string))
  (let loop ([i 0])
    (cond
      [(>= i n) (get-output-string out)]
      [(and (char=? (string-ref s i) #\\) (< (add1 i) n))
       (define c (string-ref s (add1 i)))
       (cond
         [(and (char=? c #\u)
               (<= (+ i 6) n)
               (for/and ([k (in-range (+ i 2) (+ i 6))])
                 (hex-digit? (string-ref s k))))
          (write-char (integer->char (string->number (substring s (+ i 2) (+ i 6)) 16)) out)
          (loop (+ i 6))]
         [else
          (write-char (case c
                        [(#\n) #\newline]
                        [(#\r) #\return]
                        [(#\t) #\tab]
                        [(#\f) #\page]
                        [else c])
                      out)
          (loop (+ i 2))])]
      [else
       (write-char (string-ref s i) out)
       (loop (add1 i))])))

(define (gen-match-helpers)
  "
(define (mlit s p in)
  (define sl (string-length s))
  (if (and (<= (+ p sl) (string-length in))
           (string=? (substring in p (+ p sl)) s))
      (list (+ p sl))
      '()))

(define (mrange lo hi p in)
  (if (>= p (string-length in)) '()
      (let ([c (string-ref in p)])
        (if (char<=? lo c hi) (list (+ p 1)) '()))))

(define (mcclass pat p in)
  (if (>= p (string-length in)) '()
      (let ([c (string-ref in p)])
        (if (cc-match c pat) (list (+ p 1)) '()))))

(define (mstar f p in)
  (let loop ([pp p])
    (let ([rs (f pp in)])
      (if (null? rs)
          (list pp)
          (append (append-map (lambda (r) (loop r)) rs)
                  (list pp))))))

(define (mstar-ng f p in)
  (let loop ([pp p])
    (let ([rs (f pp in)])
      (cons pp
            (if (null? rs)
                '()
                (append-map (lambda (r) (loop r)) rs))))))

(define (mplus f p in)
  (let ([rs (f p in)])
    (if (null? rs) '()
        (append-map (lambda (r) (mstar f r in)) rs))))

(define (mplus-ng f p in)
  (let ([rs (f p in)])
    (if (null? rs) '()
        (append-map (lambda (r) (mstar-ng f r in)) rs))))

(define (mopt f p in)
  (let ([rs (f p in)])
    (if (null? rs) (list p)
        (append rs (list p)))))

(define (mopt-ng f p in)
  (let ([rs (f p in)])
    (if (null? rs) (list p)
        (cons p rs))))

(define (mnot f p in)
  (if (>= p (string-length in)) '()
      (let ([rs (f p in)])
        (if (null? rs) (list (+ p 1)) '()))))

(define (malt fs p in)
  ;; Maximal munch across alternatives: order each alternative's results by
  ;; its preferred (first) match length, longest first, ties to the earlier
  ;; alternative. Each alternative's internal order is preserved (greedy =
  ;; longest first, non-greedy = shortest first), so a non-greedy loop like
  ;; LONG_STRING_ITEM*? keeps its shortest-match preference while a longer
  ;; alternative still wins over a shorter one. A first-match malt blocked the
  ;; longer [rR][fF] prefix behind [rR] and POINT_FLOAT behind INT_PART
  ;; (1.5e10); a flat union re-ordered non-greedy results so the longest —
  ;; not the preferred shortest — match won.
  (define indexed
    (for/list ([f fs] [i (in-naturals)])
      (cons i (f p in))))
  (define sorted
    (sort indexed
          (lambda (a b)
            (define ra (cdr a)) (define rb (cdr b))
            (cond [(null? ra) (and (null? rb) (< (car a) (car b)))]
                  [(null? rb) #t]
                  [else (let ([pa (car ra)] [pb (car rb)])
                          (if (= pa pb)
                              (< (car a) (car b))
                              (> pa pb)))]))))
  (append-map cdr sorted))

(define (mseq fs p in)
  (let loop ([xs fs] [acc (list p)])
    (if (null? xs) acc
        (loop (cdr xs)
              (for*/list ([pos (in-list acc)]
                          [r (in-list ((car xs) pos in))])
                r)))))

(define (cc-match c pat)
   (define pl (string-length pat))
   (let loop ([i 1])
     (cond [(>= i (- pl 1)) #f]
           ;; Handle \\p{XX} and \\P{XX} Unicode property escapes
           [(and (char=? (string-ref pat i) #\\\\) (member (string-ref pat (+ i 1)) '(#\\p #\\P)))
            (define is-negated (char=? (string-ref pat (+ i 1)) #\\P))
            (define start (+ i 3)) ;; skip \\p{ or \\P{
            (let find-end ([j start])
              (if (char=? (string-ref pat j) #\\})
                  (let ([prop (substring pat start j)])
                    (define cat (char-general-category c))
                    (define cat-str (symbol->string cat))
                    (define matches?
                      (cond [(string=? prop \"L\") (char-ci=? (string-ref cat-str 0) #\\L)]
                            [(string=? prop \"Nl\") (eq? cat 'nl)]
                            [(string=? prop \"Mn\") (eq? cat 'mn)]
                            [(string=? prop \"Mc\") (eq? cat 'mc)]
                            [(string=? prop \"Nd\") (eq? cat 'nd)]
                            [(string=? prop \"Pc\") (eq? cat 'pc)]
                            [else (eprintf \"Warning: unhandled Unicode property ~s~n\" prop) #f]))
                    (and (if is-negated (not matches?) matches?) #t))
                  (find-end (+ j 1))))]
           ;; Handle character range: a-z
           [(and (< (+ i 2) pl) (char=? (string-ref pat (+ i 1)) #\\-)
                 (char<=? (string-ref pat i) c (string-ref pat (+ i 2)))) #t]
           [(char=? (string-ref pat i) c) #t]
           [(and (< (+ i 2) pl) (char=? (string-ref pat (+ i 1)) #\\-)) (loop (+ i 3))]
           [else (loop (+ i 1))])))")

(define (gen-lexer-matchers all-lexer)
  (define seen (make-hash))
  (string-join
   (for/list ([rule all-lexer]
              #:when (let ([name (mangle (any-tree-text (first (any-tree-children rule))))])
                       (and (not (hash-has-key? seen name))
                            (begin (hash-set! seen name #t) #t))))
     (gen-one-lexer-matcher rule))
   "\n"))

(define (gen-one-lexer-matcher rule)
  (define raw-name (any-tree-text (first (any-tree-children rule))))
  (define name (mangle raw-name))
  (define alts-node (second (any-tree-children rule)))
  (define alts (any-tree-children alts-node))
  (format "(define (~a-match p in) ~a)"
          name
          (gen-match-alt alts)))

(define (gen-match-alt alts)
  (if (= (length alts) 1)
      (gen-match-seq (any-tree-children (car alts)))
      (format "(malt (list ~a) p in)"
              (string-join (map (lambda (a) (gen-match-seq-fn (any-tree-children a))) alts) " "))))

(define (gen-match-seq-fn elems)
  (format "(lambda (p in) ~a)" (gen-match-seq elems)))

(define (gen-match-seq elems)
  ;; Filter out action elements (embedded code blocks)
  (define real-elems (filter (lambda (e) (not (eq? (any-tree-tag e) 'action))) elems))
  (if (= (length real-elems) 1)
      (format "(~a p in)" (gen-match-elem (car real-elems)))
      (format "(mseq (list ~a) p in)"
              (string-join (map gen-match-elem real-elems) " "))))

;; Convert ANTLR4 character representation to Racket character literal text.
;; Examples: "a" → "#\\a", "\\n" → "#\\newline", "\\u1885" → "#\\u1885"
(define (g4-char->racket ch-str)
  (define racket-escapes
    '(("\\n" . "newline") ("\\r" . "return") ("\\t" . "tab")
      ("\\f" . "page") ("\\\\" . "\\\\") ("\\'" . "'") ("\\\"" . "\"")))
  (cond
    [(assoc ch-str racket-escapes) => (lambda (pair) (format "#\\~a" (cdr pair)))]
    [(and (>= (string-length ch-str) 2) (string=? (substring ch-str 0 2) "\\u"))
     (format "#\\u~a" (substring ch-str 2))]
    [else (format "#\\~a" ch-str)]))

(define (gen-match-elem elem)
  (define tag (any-tree-tag elem))
  (cond
    [(eq? tag 'literal)  (format "(lambda (p i) (mlit ~s p i))" (unescape-g4-literal (any-tree-text elem)))]
    [(eq? tag 'char-class) (format "(lambda (p i) (mcclass ~s p i))" (unescape-char-class (any-tree-text elem)))]
    [(eq? tag 'range)
      (define lo (any-tree-text (first (any-tree-children elem))))
      (define hi (any-tree-text (second (any-tree-children elem))))
      (format "(lambda (p i) (mrange ~a ~a p i))" (g4-char->racket lo) (g4-char->racket hi))]
    [(eq? tag 'token-ref)
      (define ref-name (any-tree-text elem))
      (if (string=? ref-name "EOF")
          "(lambda (p i) (if (>= p (string-length i)) (list p) '()))"
          (format "~a-match" (mangle ref-name)))]
    [(eq? tag 'star)    (format "(lambda (p i) (mstar ~a p i))" (gen-match-elem (first (any-tree-children elem))))]
    [(eq? tag 'plus)    (format "(lambda (p i) (mplus ~a p i))" (gen-match-elem (first (any-tree-children elem))))]
    [(eq? tag 'optional) (format "(lambda (p i) (mopt ~a p i))" (gen-match-elem (first (any-tree-children elem))))]
    [(eq? tag 'star-ng) (format "(lambda (p i) (mstar-ng ~a p i))" (gen-match-elem (first (any-tree-children elem))))]
    [(eq? tag 'plus-ng) (format "(lambda (p i) (mplus-ng ~a p i))" (gen-match-elem (first (any-tree-children elem))))]
    [(eq? tag 'optional-ng) (format "(lambda (p i) (mopt-ng ~a p i))" (gen-match-elem (first (any-tree-children elem))))]
    [(eq? tag 'negated)
     (define child (first (any-tree-children elem)))
     ;; If negation wraps a quantifier, swap: ~X+ => (~X)+
     (define child-tag (any-tree-tag child))
     (if (memq child-tag '(star plus optional star-ng plus-ng optional-ng))
         (gen-match-elem (node child-tag (list (node 'negated (any-tree-children child)))))
         (format "(lambda (p i) (mnot ~a p i))" (gen-match-elem child)))]
    [(eq? tag 'group)
      (define sub-alts (any-tree-children elem))
      (format "(lambda (p i) ~a)" (gen-match-alt sub-alts))]
    [(eq? tag 'action) (format "(lambda (p i) (list p))")] ;; action/predicate matches empty
    [(eq? tag 'any) (format "(lambda (p i) (mnot (lambda (p2 i2) '()) p i))")] ;; . matches any char
    [else (error "unknown match elem tag:" tag)]))

(define (build-mode-map lexer-rules mode-nodes implicit-rules)
  (define mode-entries
    (for/list ([mn mode-nodes])
      (define children (any-tree-children mn))
      (define mode-name (any-tree-text (first children)))
      (define mode-rules (rest children))
      (cons mode-name mode-rules)))
  (cons (cons 'default (append lexer-rules implicit-rules))
        mode-entries))

(define (gen-tokenizer mode-map #:has-newline? [has-newline? #f] #:indent-tokens? [indent-tokens? #f]
                       #:preserve-comments? [preserve-comments? #f]
                       #:hidden-names [hidden-names '()]
                       #:policy [policy default-language-policy])
  (define mode-clauses
    (string-join
     (for/list ([(mode-name rules) (in-dict mode-map)])
       (gen-mode-section mode-name rules preserve-comments? policy))
     "\n"))
  (define template-mode (language-policy-template-mode-name policy))
  (define whitespace-handler
    (cond
      [has-newline?
       ;; Skip non-newline whitespace only; let newlines be matched by the
       ;; host policy's line-break token rule.
       "            [(and (char-whitespace? ch) (not (char=? ch #\\newline)))
              (loop (+ p 1) l (+ c 1) (+ o 1) tks mode mstack pending)]\n"]
      [template-mode
       ;; Skip all whitespace including newlines, except in the policy's
       ;; template-literal mode (its text is matched by the grammar).
       (format "            [(and (char-whitespace? ch) (not (eq? mode '~a)))
               (if (char=? ch #\\newline)
                   (loop (+ p 1) (+ l 1) 1 (+ o 1) tks mode mstack pending)
                   (loop (+ p 1) l (+ c 1) (+ o 1) tks mode mstack pending))]\n"
               template-mode)]
      [else
       ;; No template modes: skip all whitespace including newlines.
       "            [(char-whitespace? ch)
               (if (char=? ch #\\newline)
                   (loop (+ p 1) (+ l 1) 1 (+ o 1) tks mode mstack pending)
                   (loop (+ p 1) l (+ c 1) (+ o 1) tks mode mstack pending))]\n"]))
  (string-append
   (format "(define (~a in)~n" (if indent-tokens? "tokenize-raw" "tokenize"))
    "   (define il (string-length in))
    (define template-depth (box 0))
    (define brace-depth (box 0))
    (let loop ([p 0] [l 1] [c 1] [o 0] [tks '()] [mode 'default] [mstack '()] [pending #f])
     (if (>= p il)
         (let ([final-tks (if pending
                             (cons (token 'UNKNOWN pending (pos l c o) (pos l c o)) tks)
                             tks)])
           (reverse (cons (token 'EOF \"\" (pos l c o) (pos l c o)) final-tks)))
         (let ([ch (string-ref in p)])
           (cond\n"
   whitespace-handler
   mode-clauses
   "\n            [else (error 'tokenize \"unexpected char ~a in mode ~a at ~a:~a\" ch mode l c)])))))"
    (if indent-tokens?
        "\n(define (tokenize in) (insert-indents (tokenize-raw in)))"
        "")
    (if preserve-comments?
        (format "\n(define hidden-token-types (list ~a))\n(define (hidden-token? t) (member (token-type t) hidden-token-types))"
                (string-join
                 (map (λ (n) (format "(string->symbol ~s)" n)) hidden-names)
                 " "))
        "")
    "\n"))

(define (gen-mode-section mode-name rules preserve-comments?
                          [policy default-language-policy])
  ;; Generate a longest-match loop: try all matchers at position p,
  ;; pick the one that advances furthest. If tie, first defined wins.
  ;; This implements ANTLR4's maximal munch lexer strategy.
  (define clauses-str
    (string-join (map (λ (rule) (gen-token-clause rule preserve-comments? policy)) rules) "\n"))
  (string-append
   (format "            [(eq? mode '~a)\n             (let __mloop ([__rules (list\n" mode-name)
   clauses-str
    ")]\n                                      [__best-np p] [__best-handle #f])\n"
    "               (if (null? __rules)\n"
    "                   (if __best-handle\n"
    "                       (__best-handle __best-np (substring in p __best-np))\n"
     (format "                       (error 'tokenize \"no matching rule in mode ~a at ~~a:~~a\" l c))\n" mode-name)
   "                   (let ([__r ((caar __rules) p in)])\n"
   "                     (if (and (pair? __r) (> (car __r) __best-np))\n"
   "                         (__mloop (cdr __rules) (car __r) (cdar __rules))\n"
   "                         (__mloop (cdr __rules) __best-np __best-handle)))))]"))

(define (gen-token-clause rule preserve-comments? [policy default-language-policy])
  (define children (any-tree-children rule))
  (define raw-name (any-tree-text (first children)))
  (define mangled (mangle raw-name))
  (define commands
    (if (> (length children) 2)
        (drop children 2)
        '()))
  (define token-expr
    (if (regexp-match? #rx"^[a-zA-Z_][a-zA-Z0-9_]*$" raw-name)
        (format "'~a" raw-name)
        (format "(string->symbol ~s)" raw-name)))
  (define cmd-body (gen-command-body commands token-expr raw-name preserve-comments? policy))
  (format "               (cons ~a-match (lambda (np v) ~a))"
          mangled cmd-body))

;; Resume the scanner after an accepted token. advance-line-col updates the
;; line/column across the token's own line breaks (a multi-line string or a
;; NEWLINE token), while np is the absolute offset after the token.
(define (gen-token-emit token-expr mode mstack pending)
  (format "(define-values (nl nc) (advance-line-col l c v)) (define tk (token ~a v (pos l c o) (pos nl nc np))) (loop np nl nc np (cons tk tks) ~a ~a ~a)"
          token-expr mode mstack pending))

;; Resume the scanner after a token that is not emitted (skip / hidden
;; channel). Skipped text can still contain line breaks (e.g. a line-joining
;; continuation), so the position must advance the same way.
(define (gen-token-skip mode mstack pending)
  (format "(define-values (nl nc) (advance-line-col l c v)) (loop np nl nc np tks ~a ~a ~a)"
          mode mstack pending))

(define (gen-command-body commands token-expr raw-name preserve-comments?
                          [policy default-language-policy])
  (define inner-body
    (if (null? commands)
        (gen-token-emit token-expr "mode" "mstack" "pending")
        (let loop ([cmds commands]
                   [skip? #f]
                   [more? #f]
                   [emit-type token-expr]
                   [next-mode "mode"]
                   [next-mstack "mstack"]
                   [next-pending "pending"])
          (if (null? cmds)
              (cond [skip? (gen-token-skip next-mode next-mstack next-pending)]
                    [more? (format "(loop np l c o tks ~a ~a ~a)" next-mode next-mstack next-pending)]
                    [else (gen-token-emit emit-type next-mode next-mstack next-pending)])
              (let ([cmd (car cmds)])
                (define txt (any-tree-text cmd))
                (cond [(string=? txt "skip")
                       (loop (cdr cmds) #t more? emit-type next-mode next-mstack next-pending)]
                    [(string-prefix? txt "channel(")
                     ;; channel(HIDDEN) (== channel(1)) tokens are invisible to the
                     ;; parser. By default we have no channels, so treat them like
                     ;; `skip`. With comment preservation they are emitted (so the
                     ;; caller can inspect them) and filtered out at parse time.
                     (define ch (substring txt 8 (sub1 (string-length txt))))
                     (define hidden? (or (string=? ch "HIDDEN") (string=? ch "1")))
                     (define skip-token? (and hidden? (not preserve-comments?)))
                     (loop (cdr cmds) (or skip? skip-token?) more? emit-type next-mode next-mstack next-pending)]
                    [(string-prefix? txt "pushMode(")
                     (define raw-mname (substring txt 9 (sub1 (string-length txt))))
                     (define mname (if (string=? raw-mname "DEFAULT_MODE") "default" raw-mname))
                     (loop (cdr cmds) skip? more? emit-type (format "'~a" mname) (format "(cons mode ~a)" next-mstack) next-pending)]
                      [(string=? txt "popMode")
                       (loop (cdr cmds) skip? more? emit-type
                             "(if (null? mstack) 'default (car mstack))"
                             "(if (null? mstack) '() (cdr mstack))"
                             next-pending)]
                      [(string=? txt "more")
                       (loop (cdr cmds) skip? #t emit-type next-mode next-mstack
                             "(if pending (string-append pending v) v)")]
                      [(string-prefix? txt "type(")
                       (define tname (substring txt 5 (sub1 (string-length txt))))
                       (define texpr
                         (if (regexp-match? #rx"^[a-zA-Z_][a-zA-Z0-9_]*$" tname)
                             (format "'~a" tname)
                             (format "(string->symbol ~s)" tname)))
                       (loop (cdr cmds) skip? more? texpr next-mode next-mstack next-pending)]
                      [else (loop (cdr cmds) skip? more? emit-type next-mode next-mstack next-pending)]))))))
  ;; Host-specific lexer-rule bookkeeping (e.g. ANTLR template-literal rule
  ;; names) is a policy hook; this engine only calls it.
  (define post ((language-policy-rule-post-process policy) raw-name inner-body token-expr))
  (or post inner-body))
