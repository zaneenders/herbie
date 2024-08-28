#lang racket

(require "syntax/types.rkt"
         "utils/errors.rkt"
         (submod "syntax/types.rkt" internals)
         (submod "syntax/syntax.rkt" internals)
         (submod "syntax/platform.rkt" internals)
         (submod "core/rules.rkt" internals))

(provide define-type
         ; representations
         define-representation
         (struct-out representation)
         get-representation
         make-representation
         rename-representation
         ; operators
         define-operator
         register-operator!
         ; operator implementations
         define-operator-impl
         make-operator-impl
         register-conversion-generator!
         ; rulesets
         define-ruleset
         define-ruleset*
         register-ruleset!
         ; platform
         define-platform
         get-platform
         register-platform!
         platform-union
         platform-intersect
         platform-subtract
         platform-filter
         ; misc
         warn)
