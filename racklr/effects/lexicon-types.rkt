#lang racket

;; racklr/effects/lexicon-types.rkt
;;
;; The host-language lexicon SHAPE. This module carries no language-specific
;; names; it defines the two records a front end supplies through data:
;;
;;   dialect — the host's naming / encoding conventions the classifier needs
;;             (implicit receiver names, exception-name suffixes, the callee
;;             encodings its lowerer emits for front-end constructs, the
;;             method/constructor conventions of its object model).
;;   lexicon — the whole injected data bundle: the token tables (effect-tokens,
;;             which itself carries its dialect), the import-tag rule config,
;;             the per-domain operation vocabulary, and the builtin receiver
;;             method table.
;;
;; A new host language is a lexicon value: its tokens + its dialect + its
;; import rules + its operation verbs. Core modules take a lexicon (or its
;; token tables) as an optional argument defaulting to the Python/Django one in
;; lexicon.rkt, so supplying another host edits no engine code.

(provide
 ;; dialect
 dialect? dialect
 dialect-self-names dialect-class-receiver-names
 dialect-exception-suffixes dialect-synthetic-callees
 dialect-comprehension-callees dialect-wrapper-callees
 dialect-runtime-primitives dialect-binding-callees
 dialect-method-receiver-offset dialect-init-method-name
 dialect-keyword-arg-callee dialect-annotation-callee
 dialect-literal-receiver-types
 dialect-control-transfer-names dialect-short-circuit-callees
 ;; lexicon
 lexicon? lexicon
 lexicon-name lexicon-tokens lexicon-import-config
 lexicon-db-read-methods lexicon-db-write-methods
 lexicon-io-read-verbs lexicon-io-write-verbs lexicon-io-other-verbs
 lexicon-domain-op-vocabulary
 lexicon-builtin-class-methods)

;; Naming/encoding conventions of a host language. Sets are sets of strings
;; (names as the front end writes them); empty sets mean "this host has no such
;; convention".
(struct dialect
  (self-names             ; set: implicit receiver parameter names (`self`, `this`)
   class-receiver-names   ; set: implicit CLASS receiver names (`cls`)
   exception-suffixes     ; set: suffixes of pure exception-constructor names
   synthetic-callees      ; set: front-end construct callee names (operators, markers)
   comprehension-callees  ; set: comprehension encoding callee names
   wrapper-callees        ; set: named-function wrapper encoding callee names
   runtime-primitives     ; set: emitter-injected runtime primitive callee names
   binding-callees        ; set: binding-form encoding callee names (`var`, `let`)
   method-receiver-offset ; exact-nonnegative-integer: implicit param(s) a method takes
   init-method-name       ; string or #f: constructor method name (`__init__`)
   keyword-arg-callee     ; string or #f: keyword-argument encoding callee (`kwarg`)
   annotation-callee      ; string or #f: effect-row annotation wrapper callee
   ;; node-kind symbol -> receiver TYPE name for a literal VALUE (`string` →
   ;; "str", `record` → "dict"). A literal's receiver type is a host-language
   ;; convention, so it is DATA here rather than a name literal in the engine.
   literal-receiver-types   ; hash: symbol -> string
   ;; Callee/statement names that always transfer control out of the enclosing
   ;; block (`raise`, JS `throw`, `sys.exit`, `break`, `continue`). The effect
   ;; inference's must analysis reads this set, so the transfer vocabulary is
   ;; host DATA, never a name literal in engine code.
   control-transfer-names   ; set: names as strings
   ;; Callee names whose right operands are evaluated only conditionally
   ;; (short-circuit boolean operators): `and`/`or`, JS `&&`/`||`/`??`.
   short-circuit-callees)   ; set: names as strings
  #:transparent)

;; The whole host lexicon: token tables, their dialect, import-tag rules, the
;; operation vocabulary, and the builtin receiver method table.
(struct lexicon
  (name                  ; symbol: host language name (`python`, `javascript`, ...)
   tokens                ; effect-tokens: the token name tables + dialect
   import-config         ; (listof effect-library): module path -> effect domain
   db-read-methods       ; set: method-name verbs that read the db
   db-write-methods      ; set: method-name verbs that write the db
   io-read-verbs         ; set: method-name verbs that read a stream
   io-write-verbs        ; set: method-name verbs that write a stream
   io-other-verbs        ; set: method-name verbs that manage a stream
   ;; The other effect domains' primitive-operation vocabulary, so a host (or a
   ;; library family within a host) types more than db/io. A hash from domain
   ;; symbol to a hash from method-name string to the typed-op operation
   ;; segment: `network<Session>.read` ← 'network + "get" → "read". A domain
   ;; absent from this table keeps the coarse `ops=()` capability.
   domain-op-vocabulary  ; hash: domain -> (hash method-name -> op-name)
   builtin-class-methods) ; hash: class-name -> set of pure method names
  #:transparent)

;; The dialect travels inside the token tables (effect-tokens), so every
;; existing optional `[tokens default-effect-tokens]` argument is a complete
;; lexicon-injection point. `lexicon-dialect` lives in lexicon.rkt, which can
;; require the token module without a cycle.
