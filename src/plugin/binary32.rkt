#lang racket

(require math/bigfloat)

(require (submod "../syntax/syntax.rkt" internals)
         (submod "../syntax/types.rkt" internals)
         "bool.rkt"
         "binary64.rkt"
         "ffi.rkt"
         "float32.rkt"
         "utils.rkt")

(provide binary32
         PI.f32
         E.f32
         INFINITY.f32
         NAN.f32
         neg.f32
         +.f32
         -.f32
         *.f32
         /.f32
         ==.f32
         !=.f32
         <.f32
         >.f32
         <=.f32
         >=.f32
         binary32->binary64
         binary64->binary32)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Representation

(define-representation binary32
                       (binary32 real float32?)
                       bigfloat->float32
                       bf
                       (shift 31 ordinal->float32)
                       (unshift 31 float32->ordinal)
                       32
                       (conjoin number? nan?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementations

(define-constant-impls binary32
  [PI.f32 PI pi]
  [E.f32 E (exp 1.0)]
  [INFINITY.f32 INFINITY +inf.0]
  [NAN.f32 NAN +nan.0])

(define-operator-impl (neg.f32 [x : binary32]) binary32
  #:spec (neg x)
  #:fpcore (! :precision binary32 (- x))
  #:fl -)

(define-operator-impl (+.f32 [x : binary32] [y : binary32]) binary32
  #:spec (+ x y)
  #:fl +)

(define-operator-impl (-.f32 [x : binary32] [y : binary32]) binary32
  #:spec (- x y)
  #:fl -)

(define-operator-impl (*.f32 [x : binary32] [y : binary32]) binary32
  #:spec (* x y)
  #:fl *)

(define-operator-impl (/.f32 [x : binary32] [y : binary32]) binary32
  #:spec (/ x y)
  #:fl /)

(define-comparator-impls binary32
  [==.f32 == =]
  [!=.f32 != (negate =)]
  [<.f32 < <]
  [>.f32 > >]
  [<=.f32 <= <=]
  [>=.f32 >= >=])

(define-syntax (define-libm-impl/binary32 stx)
  (syntax-case stx ()
    [(_ name (op var ...) #:spec spec)
     (let ([vars (syntax->list #'(var ...))])
       (with-syntax ([cname (string->symbol (format "~af" (syntax->datum #'op)))]
                     [(var ...) vars]
                     [(itype ...) (map (lambda (_) 'binary32) vars)]
                     [(citype ...) (map (lambda (_) '_float) vars)])
         #'(begin
             (provide name)
             (define-operator-impl (name [var : itype] ...) binary32
               #:spec spec
               #:fpcore (! :precision binary32 (op var ...))
               #:fl (ffi/proc cname #f : citype ... -> _float #:fail (const #f))
               #:optional))))]
    [(_ name (op var ...)) #'(define-libm-impl/binary32 name (op var ...) #:spec (op var ...))]))

(define-syntax-rule (define-libm-impls/binary32
                      [name spec ...] ...)
  (begin
    (define-libm-impl/binary32 name spec ...) ...))

(define-libm-impls/binary32
  [acos.f32 (acos x)]
  [acosh.f32 (acosh x)]
  [asin.f32 (asin x)]
  [asinh.f32 (asinh x)]
  [atan.f32 (atan x)]
  [atan2.f32 (atan2 x y)]
  [atanh.f32 (atanh x)]
  [cbrt.f32 (cbrt x)]
  [ceil.f32 (ceil x)]
  [copysign.f32 (copysign x y)]
  [cos.f32 (cos x)]
  [cosh.f32 (cosh x)]
  [erf.f32 (erf x)]
  [erfc.f32 (erfc x) #:spec (- 1 (erf x))]
  [exp.f32 (exp x)]
  [exp2.f32 (exp2 x)]
  [expm1.f32 (expm1 x) #:spec (- (exp x) 1)]
  [fabs.f32 (fabs x)]
  [fdim.f32 (fdim x y)]
  [floor.f32 (floor x)]
  [fma.f32 (fma x y z) #:spec (+ (* x y) z)]
  [fmax.f32 (fmax x y)]
  [fmin.f32 (fmin x y)]
  [fmod.f32 (fmod x y)]
  [hypot.f32 (hypot x y) #:spec (+ (* x x) (* y y))]
  [lgamma.f32 (lgamma x)]
  [log.f32 (log x)]
  [log10.f32 (log10 x)]
  [log1p.f32 (log1p x) #:spec (log (+ 1 x))]
  [log2.f32 (log2 x)]
  [logb.f32 (logb x)]
  [pow.f32 (pow x y)]
  [remainder.f32 (remainder x y)]
  [rint.f32 (rint x)]
  [round.f32 (round x)]
  [sin.f32 (sin x)]
  [sinh.f32 (sinh x)]
  [sqrt.f32 (sqrt x)]
  [tan.f32 (tan x)]
  [tanh.f32 (tanh x)]
  [tgamma.f32 (tgamma x)]
  [trunc.f32 (trunc x)])

(define-operator-impl (binary64->binary32 [x : binary64]) binary32
  #:spec x
  #:fpcore (! :precision binary32 (cast x))
  #:fl (curryr ->float32))

(define-operator-impl (binary32->binary64 [x : binary32]) binary64
  #:spec x
  #:fpcore (! :precision binary64 (cast x))
  #:fl identity)
