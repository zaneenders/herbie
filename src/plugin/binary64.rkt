#lang racket

(require math/flonum
         math/bigfloat)

(require (submod "../syntax/syntax.rkt" internals)
         (submod "../syntax/types.rkt" internals)
         "utils.rkt")

(provide binary64
         neg.f64
         +.f64
         -.f64
         *.f64
         /.f64)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Representation

(define-representation binary64
                       (binary64 real flonum?)
                       bigfloat->flonum
                       bf
                       (shift 63 ordinal->flonum)
                       (unshift 63 flonum->ordinal)
                       64
                       (conjoin number? nan?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementations

(define-operator-impl (neg.f64 [x : binary64]) binary64
  #:spec (neg x)
  #:fpcore (! :precision binary64 (- x))
  #:fl -)

(define-operator-impl (+.f64 [x : binary64] [y : binary64]) binary64
  #:spec (+ x y)
  #:fl +)

(define-operator-impl (-.f64 [x : binary64] [y : binary64]) binary64
  #:spec (- x y)
  #:fl -)

(define-operator-impl (*.f64 [x : binary64] [y : binary64]) binary64
  #:spec (* x y)
  #:fl *)

(define-operator-impl (/.f64 [x : binary64] [y : binary64]) binary64
  #:spec (/ x y)
  #:fl /)
