#lang racket/base

(require fmt/conventions)

(provide the-formatter-map)

(define (the-formatter-map s)
  (case s
    [("define-operators") (standard-formatter-map "begin")]
    [("define-operator-impl")
     (format-uniform-body/helper 2 #:body-formatter (format-clause-2/indirect) #:require-body? #f)]
    [("define-libm-impls/binary64") (standard-formatter-map "begin")]
    [else #f]))
