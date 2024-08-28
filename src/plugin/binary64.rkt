#lang racket

(require math/flonum
         math/bigfloat)

(require (submod "../syntax/syntax.rkt" internals)
         (submod "../syntax/types.rkt" internals)
         "bool.rkt"
         "ffi.rkt"
         "utils.rkt")

(provide binary64
         PI.f64
         E.f64
         INFINITY.f64
         NAN.f64
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

(define-constant-impls binary64
  [PI.f64 PI pi]
  [E.f64 E (exp 1.0)]
  [INFINITY.f64 INFINITY +inf.0]
  [NAN.f64 NAN +nan.0])

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
  [atan2.f64 (atan2 x y)]
  [atanh.f64 (atanh x)]
  [cbrt.f64 (cbrt x)]
  [ceil.f64 (ceil x)]
  [copysign.f64 (copysign x y)]
  [cos.f64 (cos x)]
  [cosh.f64 (cosh x)]
  [erf.f64 (erf x)]
  [erfc.f64 (erfc x) #:spec (- 1 (erf x))]
  [exp.f64 (exp x)]
  [exp2.f64 (exp2 x)]
  [expm1.f64 (expm1 x) #:spec (- (exp x) 1)]
  [fabs.f64 (fabs x)]
  [fdim.f64 (fdim x y)]
  [floor.f64 (floor x)]
  [fma.f64 (fma x y z) #:spec (+ (* x y) z)]
  [fmax.f64 (fmax x y)]
  [fmin.f64 (fmin x y)]
  [fmod.f64 (fmod x y)]
  [hypot.f64 (hypot x y) #:spec (+ (* x x) (* y y))]
  [lgamma.f64 (lgamma x)]
  [log.f64 (log x)]
  [log10.f64 (log10 x)]
  [log1p.f64 (log1p x) #:spec (log (+ 1 x))]
  [log2.f64 (log2 x)]
  [logb.f64 (logb x)]
  [pow.f64 (pow x y)]
  [remainder.f64 (remainder x y)]
  [rint.f64 (rint x)]
  [round.f64 (round x)]
  [sin.f64 (sin x)]
  [sinh.f64 (sinh x)]
  [sqrt.f64 (sqrt x)]
  [tan.f64 (tan x)]
  [tanh.f64 (tanh x)]
  [tgamma.f64 (tgamma x)]
  [trunc.f64 (trunc x)])
