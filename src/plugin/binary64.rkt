#lang racket

(require math/flonum
         math/bigfloat)

(require (submod "../syntax/syntax.rkt" internals)
         (submod "../syntax/types.rkt" internals)
         "bool.rkt"
         "ffi.rkt"
         "utils.rkt")

(provide binary64
         neg.f64
         +.f64
         -.f64
         *.f64
         /.f64
         ==.f64
         !=.f64
         <.f64
         >.f64
         <=.f64
         >=.f64)

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

  (define-comparator-impls binary64
                         [==.f64 == =]
                         [!=.f64 != (negate =)]
                         [<.f64 < <]
                         [>.f64 > >]
                         [<=.f64 <= <=]
                         [>=.f64 >= >=])

(define-syntax (define-libm-impl/binary64 stx)
  (syntax-case stx ()
    [(_ name (op var ...) #:spec spec)
     (let ([vars (syntax->list #'(var ...))])
       (with-syntax ([cname #'op]
                     [(var ...) vars]
                     [(itype ...) (map (lambda (_) 'binary64) vars)]
                     [(citype ...) (map (lambda (_) '_double) vars)])
         #'(begin
             (provide name)
             (define-operator-impl (name [var : itype] ...) binary64
               #:spec spec
               #:fl (ffi/proc cname #f : citype ... -> _double #:fail (const #f))
               #:optional))))]
    [(_ name (op var ...)) #'(define-libm-impl/binary64 name (op var ...) #:spec (op var ...))]))

(define-syntax-rule (define-libm-impls/binary64
                      [name spec ...] ...)
  (begin
    (define-libm-impl/binary64 name spec ...) ...))

(define-libm-impls/binary64
  [acos.f64 (acos x)]
  [acosh.f64 (acosh x)]
  [asin.f64 (asin x)]
  [asinh.f64 (asinh x)]
  [atan.f64 (atan x)]
  [atanh.f64 (atanh x)]
  [cbrt.f64 (cbrt x)]
  [ceil.f64 (ceil x)]
  [cos.f64 (cos x)]
  [cosh.f64 (cosh x)]
  [erf.f64 (erf x)]
  [exp.f64 (exp x)]
  [exp2.f64 (exp2 x)]
  [fabs.f64 (fabs x)]
  [floor.f64 (floor x)]
  [lgamma.f64 (lgamma x)]
  [log.f64 (log x)]
  [log10.f64 (log10 x)]
  [log2.f64 (log2 x)]
  [logb.f64 (logb x)]
  [rint.f64 (rint x)]
  [round.f64 (round x)]
  [sin.f64 (sin x)]
  [sinh.f64 (sinh x)]
  [sqrt.f64 (sqrt x)]
  [tan.f64 (tan x)]
  [tanh.f64 (tanh x)]
  [tgamma.f64 (tgamma x)]
  [trunc.f64 (trunc x)])
