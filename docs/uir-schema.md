# UIR (Universal Intermediate Representation) Schema

The UIR is a three-tier intermediate representation for cross-language transpilation.
Tier 0 (Core) captures computational semantics. Tier 1 (OOP/Module) adds object-oriented
and module-system constructs. Tier 2 (UI Component) models declarative component trees.

## Schema as data

The schema is written in UIR itself. `UIR-SCHEMA` (`racklr/uir/schema-data.rkt`) is
a `uir-schema` node holding one `uir-node-kind` per node type; every node kind
records its tag, its Racket constructor name, and one `uir-schema-field` per
field. A field records its name and its **codec** — the field's type, one of
`UIR-FIELD-CODECS` in `racklr/uir/kernel.rkt` (`uir`, `list-uir`, `spliced-uir`,
`optional-uir`, `spliced-pair-uir`, `catch-list`, `with-item-list`,
`handler-op-list`, `eff-op-list`, `text`, `raw`, `bool`, `string`, `symbol`,
`name`, `param-kind`, `int`, and the optional/marker variants).

The bootstrap is a fixed point, not a special case:

- The **hand-written, versioned kernel** owns the vocabulary the schema is
  written in: the three bootstrap node kinds `schema`, `node-kind`, and
  `schema-field`; the codecs; and the generic reader/writer. It lives in
  `racklr/uir/kernel.rkt` (vocabulary and layouts), `schema-io.rkt` (generic
  serialization), `schema-check.rkt` (validation), and `schema-macro.rkt` (the
  code generator). `UIR-KERNEL-VERSION` versions it.
- The code generator (`schema-macro.rkt`) synthesises, from the schema value,
  every other struct, constructor, predicate, accessor, and serialization
  layout. `racklr/uir/types.rkt` is only "load schema, validate, generate".
- `check-schema!` validates the schema value **under the declarations it itself
  contains**, and checks the kernel-owned declarations against the hand-written
  `KERNEL-LAYOUTS`. The schema value also round-trips through its own
  serialization using only the kernel layouts, which is the fixed point: the
  trust root reads and writes the schema without the generated vocabulary.
- Adding a node kind is an addition to the schema value only. Append a
  `(uir-node-kind ...)` declaration (or pass an extended schema value to the
  generator) and the constructor, predicate, accessors, and serialization come
  with it. `racklr-test/uir-test.rkt` proves this with a `demo` node kind added
  to an extended schema value, no core struct definition touched. The
  generator is a macro, so after editing the schema value rebuild with
  `raco make` (or `raco setup`); a stale `.zo` would keep the old vocabulary.

The schema value declares the payload `version` (re-exported as
`UIR-SCHEMA-VERSION`, used by the migration path below) and the
`kernel-version` it requires; `check-schema!` refuses a kernel mismatch.
Version 2 added the optional trailing `typed-param` parameter-kind slot; the
v1 -> v2 migration is the identity because the reader fills the missing slot
with `#f`.

### Fields default to payload compatibility, not constructor arity

An `uir-schema-field` is `(name codec default-marker)`. The marker is
`required` or `false` (the field's default value is Racket `#f`). Only a
**trailing suffix** of fields may be defaulted. The serialized payload omits
trailing fields that hold their default value, and the reader fills them back
in, so older payloads stay readable (`typed-param`'s `kind` slot is the first
use: a legacy three-element payload reads as kind `#f`).

Every kind stays a flat, transparent Racket struct and its generated
constructor is **exact-arity** — one positional argument per field. A default
marker never makes a constructor argument optional; call sites pass every
field explicitly. Adding a kind with a defaulted field or a field of a new
codec is still a schema-data edit for the kind itself; new *codecs* are kernel
work and bump `UIR-KERNEL-VERSION`.

## Node types

Every UIR node type is a transparent (`#:transparent`) Racket struct. The
structs, constructors, predicates, and accessors are generated from the schema
value, so the tables below describe `UIR-SCHEMA` as well as the public
procedures. All nodes support round-trip serialization via `uir->sexp` /
`sexp->uir` and `uir->json` / `json->uir`.

### `uir-null`

Represents the null value.

| Field     | Type | Description |
|-----------|------|-------------|
| _(none)_  | —    | Unit struct, no fields |

**Struct**: `(struct uir-null () #:transparent)`

**Example** — lowering JSON `null`:

- CST: `(cst-node 'value (list (token 'null "null" ...)))`
- UIR: `(uir-null)`

### `uir-bool`

Represents a boolean literal.

| Field   | Type      | Description |
|---------|-----------|-------------|
| `value` | `boolean?` | `#t` or `#f` |

**Struct**: `(struct uir-bool (value) #:transparent)`

**Example** — lowering JSON `true`:

- CST: `(cst-node 'value (list (token 'true "true" ...)))`
- UIR: `(uir-bool #t)`

### `uir-number`

Represents a numeric literal. Value is stored as a string to preserve
precision (important for big integers, exact decimals, etc.).

| Field   | Type     | Description |
|---------|----------|-------------|
| `value` | `string?` | String representation of the number |

**Struct**: `(struct uir-number (value) #:transparent)`

**Example** — lowering JSON `3.14`:

- CST: `(cst-node 'value (list (token 'NUMBER "3.14" ...)))`
- UIR: `(uir-number "3.14")`

### `uir-string`

Represents a string literal. The value is the *unescaped* content (without
surrounding quotes).

| Field   | Type     | Description |
|---------|----------|-------------|
| `value` | `string?` | The string content |

**Struct**: `(struct uir-string (value) #:transparent)`

**Example** — lowering JSON `"hello"`:

- CST: `(cst-node 'value (list (token 'STRING "\"hello\"" ...)))`
- UIR: `(uir-string "hello")`

### `uir-list`

Represents an ordered sequence / array of UIR values.

| Field   | Type         | Description |
|---------|--------------|-------------|
| `items` | `(listof uir?)` | The elements in order |

**Struct**: `(struct uir-list (items) #:transparent)`

**Example** — lowering JSON `[1, true]`:

- CST: `(cst-node 'arr (list [ ... (cst-node 'value (token 'NUMBER "1")) ...
                               (group , (cst-node 'value (token 'true "true"))) ... ]))`
- UIR: `(uir-list (list (uir-number "1") (uir-bool #t)))`

### `uir-record`

Represents a key-value mapping (object / struct / dictionary). Keys are
always `uir-string`; values are arbitrary UIR nodes. Entries are stored
as a list of `(cons uir-string uir?)` in insertion order.

| Field     | Type                          | Description |
|-----------|-------------------------------|-------------|
| `entries` | `(listof (cons uir-string uir?))` | Key-value pairs |

**Struct**: `(struct uir-record (entries) #:transparent)`

**Example** — lowering JSON `{"a": 1}`:

- CST: `(cst-node 'obj (list { (cst-node 'pair (token 'STRING "\"a\"" ...) : (cst-node 'value (token 'NUMBER "1")))) }))`
- UIR: `(uir-record (list (cons (uir-string "a") (uir-number "1"))))`

### `uir-symbol`

Represents an identifier (variable name, function name, field name).
Used for name references in later UIR constructs (lambda parameters,
variable references, etc.).

| Field  | Type     | Description |
|--------|----------|-------------|
| `name` | `string?` | The identifier name |

**Struct**: `(struct uir-symbol (name) #:transparent)`

**Example** — not used in JSON lowering, but would represent a bound name:

- UIR: `(uir-symbol "myVar")`

## Predicates

- `(uir? v)` — returns `#t` if `v` is any UIR node type
- `(uir-tag v)` — returns a symbol: `'null`, `'bool`, `'number`, `'string`, `'list`, `'record`, or `'symbol`

## Serialization

### S-expression format

```
(null)
(bool #t|#f)
(number "string-value")
(string "string-value")
(symbol "name")
(list item...)
(record (key value)...)
```

### JSON format

Uses the same structure with symbols replaced by strings:

```json
["null"]
["bool", true]
["number", "3.14"]
["string", "hello"]
["symbol", "myVar"]
["list", ["null"], ["bool", false]]
["record", [["string", "a"], ["number", "1"]]]
```

Round-trip: `(equal? (json->uir (uir->json u)) u)` holds for all UIR nodes.

## Planned (not yet implemented)

These are part of Tier 0 but not needed for JSON:

| Node type     | Fields                          | Purpose |
|---------------|---------------------------------|---------|
| `uir-fn`      | `params body`                   | Lambda / function expression |
| `uir-call`    | `callee args`                   | Function application |
| `uir-let`     | `name value body`               | Variable binding |
| `uir-var`     | `name`                          | Variable reference (uses `uir-symbol`) |
| `uir-set!`    | `name value`                    | Mutation / assignment |
| `uir-walrus`  | `name value`                    | Assignment expression (Python `:=`) |
| `uir-type-alias` | `name type-params value`     | Type alias declaration (Python 3.12 `type`) |
| `uir-if`      | `test then else`                | Conditional |
| `uir-block`   | `stmts`                         | Sequence of expressions |
| `uir-return`  | `value`                         | Return from function |
