#lang racket

;; Builtin fallback plugin (:precision racket)

(require math/base
         math/bigfloat
         math/special-functions)

(require (submod "../syntax/syntax.rkt" internals)
         "bool.rkt"
         "binary64.rkt"
         "utils.rkt")

(provide PI.rkt
         E.rkt
         INFINITY.rkt
         NAN.rkt
         ==.rkt
         !=.rkt
         <.rkt
         >.rkt
         <=.rkt
         >=.rkt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant-impls binary64
  [PI.rkt PI pi]
  [E.rkt E (exp 1.0)]
  [INFINITY.rkt INFINITY +inf.0]
  [NAN.rkt NAN +nan.0])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-comparator-impls binary64
  [==.rkt == =]
  [!=.rkt != (negate =)]
  [<.rkt < <]
  [>.rkt > >]
  [<=.rkt <= <=]
  [>=.rkt >= >=])

(define-syntax (define-fallback-impl stx)
  (syntax-case stx ()
    [(_ name (op var ...) fl-impl #:spec spec)
     (let ([vars (syntax->list #'(var ...))])
       (with-syntax ([(var ...) vars]
                     [(itype ...) (map (lambda (_) 'binary64) vars)])
         #'(begin
             (provide name)
             (define-operator-impl (name [var : itype] ...) binary64
               #:spec spec
               #:fpcore (! :precision binary64 :math-library racket (op var ...))
               #:fl fl-impl
               #:optional))))]
    [(_ name (op var ...) fl-impl)
     #'(define-fallback-impl name (op var ...) fl-impl #:spec (op var ...))]))

(define-syntax-rule (define-fallback-impls
                      [name spec ...] ...)
  (begin
    (define-fallback-impl name spec ...) ...))

(define (no-complex fun)
  (λ xs
    (define res (apply fun xs))
    (if (real? res) res +nan.0)))

(define (from-bigfloat bff)
  (λ args (bigfloat->flonum (apply bff (map bf args)))))

(define (bffmod x mod)
  (bf- x (bf* (bftruncate (bf/ x mod)) mod)))

(define (bffma x y z)
  (bf+ (bf* x y) z))

(define-fallback-impls
  [+.rkt (+ x y) +]
  [-.rkt (- x y) -]
  [*.rkt (* x y) *]
  [/.rkt (/ x y) /]
  [neg.rkt (neg x) -]
  [acos.rkt (acos x) (no-complex acos)]
  [acosh.rkt (acosh x) (no-complex acosh)]
  [asin.rkt (asin x) (no-complex asin)]
  [asinh.rkt (asinh x) (no-complex asinh)]
  [atan.rkt (atan x) (no-complex atan)]
  [atan2.rkt (atan2 x y) (no-complex atan)]
  [atanh.rkt (atanh x) (no-complex atanh)]
  [cbrt.rkt (cbrt x) (no-complex (λ (x) (expt x 1/3)))]
  [ceil.rkt (ceil x) ceiling]
  [copysign.rkt (copysign x y) (λ (x y) (if (>= y 0) (abs x) (- (abs x))))]
  [cos.rkt (cos x) cos]
  [cosh.rkt (cosh x) cosh]
  [erf.rkt (erf x) (no-complex erf)]
  [erfc.rkt (erfc x) erfc #:spec (- 1 (erf x))]
  [exp.rkt (exp x) exp]
  [exp2.rkt (exp2 x) (no-complex (λ (x) (expt 2 x)))]
  [expm1.rkt (expm1 x) (from-bigfloat bfexpm1) #:spec (- (exp x) 1)]
  [fabs.rkt (fabs x) abs]
  [fdim.rkt (fdim x y) (λ (x y) (max (- x y) 0))]
  [floor.rkt (floor x) floor]
  [fma.rkt (fma x y z) (from-bigfloat bfhypot) #:spec (sqrt (+ (* x x) (* y y)))]
  [fmax.rkt
   (fmax x y)
   (λ (x y)
     (cond
       [(nan? x) y]
       [(nan? y) x]
       [else (max x y)]))]
  [fmin.rkt
   (fmin x y)
   (λ (x y)
     (cond
       [(nan? x) y]
       [(nan? y) x]
       [else (min x y)]))]
  [fmod.rkt (fmod x y) (from-bigfloat bffmod)]
  [hypot.rkt (hypot x y) (from-bigfloat bfhypot) #:spec (sqrt (+ (* x x) (* y y)))]
  [lgamma.rkt (lgamma x) log-gamma]
  [log.rkt (log x) (no-complex log)]
  [log10.rkt (log10 x) (no-complex (λ (x) (log x 10)))]
  [log1p.rkt (log1p x) (from-bigfloat bflog1p) #:spec (log (+ 1 x))]
  [log2.rkt (log2 x) (from-bigfloat bflog2)]
  [logb.rkt (logb x) (λ (x) (floor (bigfloat->flonum (bflog2 (bf (abs x))))))]
  [pow.rkt (pow x y) (no-complex expt)]
  [remainder.rkt (remainder x y) remainder]
  [rint.rkt (rint x) round]
  [round.rkt (round x) round]
  [sin.rkt (sin x) sin]
  [sinh.rkt (sinh x) sinh]
  [sqrt.rkt (sqrt x) (no-complex sqrt)]
  [tan.rkt (tan x) tan]
  [tanh.rkt (tanh x) tanh]
  [tgamma.rkt (tgamma x) gamma]
  [trunc.rkt (trunc x) truncate])
