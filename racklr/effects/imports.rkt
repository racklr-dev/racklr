#lang racket

;; Static import/export resolution for Python modules (Pass 2.3).
;;
;; Resolves `import` / `from ... import` statements WITHOUT executing module
;; bodies: the CST→UIR lowering flattens import statements into single dotted
;; names and drops aliases, so this pass reads the raw token stream directly.
;; It produces the `import-env` that the effect fixpoint (Pass 2.4) consumes:
;; for each module, a map of exported name → abstract-domain `handle`, and a
;; table of class-qualified effect capabilities derived from the module path.
;;
;; This module is the public facade. The machinery lives in sibling modules:
;;   import-extract.rkt — token-level import scanner + path helpers
;;   handle.rkt        — handle / class-info / module-info / module-spec + harvest
;;   resolve.rkt       — path→module-path, module resolution, export fixpoint,
;;                       import-env
;;   effect-tag.rkt    — effect-cap model, module tagging, typed-op derivation

(require racklr/effects/imports/import-extract
         racklr/effects/imports/handle
         racklr/effects/imports/resolve
         racklr/effects/imports/effect-tag)

(provide
  ;; import extraction + path helpers
  import-record? import-record-form import-record-source import-record-bindings
  import-record
  extract-imports
  first-component last-component parent-path
  path->module-path
  ;; handle model + harvest
  handle? handle handle-kind handle-path handle-name
  class-info? class-info-name class-info-super class-info-methods
  module-info? module-info-path module-info-classes module-info-functions module-info-imports
  module-spec? module-spec-path module-spec-uir module-spec-imports
  module-spec
  build-module-table
  harvest-module
  ;; resolution
  resolve-exports resolve-module-path
  import-env? import-env import-env-comp-modpath import-env-exports import-env-caps
  ;; effect-cap model + tagging
  effect-cap? effect-cap effect-cap-domain effect-cap-ops
  effect-rule? effect-rule effect-rule-prefix effect-rule-domain
  effect-library? effect-library effect-library-name effect-library-rules
  effect-tag-config
  resolve-effect-tag
  module-effect-tag
  handle-effect-domain
  clear-effect-tag-cache!
  class-effect-tag
  method-ops
  typed-op-parts
  derive-class-method-effects
  derive-class-supers
  builtin-class-methods)

(module+ main
  (displayln "racklr/effects/imports — import resolution loaded."))
