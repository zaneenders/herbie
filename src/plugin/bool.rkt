#lang racket

(require (submod "../syntax/syntax.rkt" internals)
         (submod "../syntax/types.rkt" internals))

(provide bool
         not
         and
         or)

(define-representation bool
                       (bool bool boolean?)
                       identity
                       identity
                       (λ (x) (= x 0))
                       (λ (x) (if x 1 0))
                       1
                       (const #f))

(define-operator-impl (not [x : bool]) bool #:spec (not x) #:fpcore (! (not x)) #:fl not)

(define-operator-impl (and [x : bool] [y : bool]) bool #:spec (and x y) #:fl (lambda (x y) (and x y)))

(define-operator-impl (or [x : bool] [y : bool]) bool #:spec (or x y) #:fl (lambda (x y) (or x y)))
