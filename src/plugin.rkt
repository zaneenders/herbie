#lang racket

(require "syntax/types.rkt"
         "utils/errors.rkt"
         (submod "syntax/types.rkt" internals)
         (submod "syntax/syntax.rkt" internals)
         (submod "syntax/platform.rkt" internals)
         (submod "core/rules.rkt" internals)
         ; make sure they're in the dependency graph
         ; when importing `herbie/plugin`
         "plugin/bool.rkt"
         "plugin/binary64.rkt"
         "plugin/binary32.rkt"
         "plugin/fallback.rkt")

(provide define-type
         define-representation
         define-operator-impl
         define-operator
         define-ruleset
         define-ruleset*
         register-ruleset!
         register-conversion-generator!
         register-operator!
         (struct-out representation)
         make-representation
         rename-representation
         get-representation
         warn
         define-platform
         get-platform
         register-platform!
         platform-union
         platform-intersect
         platform-subtract
         platform-filter)
