#lang racket

(require herbie/plugin math/bigfloat math/base generic-flonum)
(require
  "../syntax/types.rkt"
  "runtime/utils.rkt")
(eprintf "Loading generic float support...\n")


(define ((comparator test) . args)
  (for/and ([left args] [right (cdr args)])
    (test left right)))

(define (from-bigfloat bff)
  (λ args (bigfloat->flonum (apply bff (map bf args)))))

(define (sym-append . args)
  (string->symbol (apply string-append (map ~s args))))

(define-syntax-rule (gfl-op es nbits fun)
  (λ args
    (parameterize ([gfl-exponent es] [gfl-bits nbits])
      (apply fun args))))

(define-syntax-rule (gfl-const es nbits cnst)
  (parameterize ([gfl-exponent es] [gfl-bits nbits])
    cnst))

(define-platform operators)
(define move-cost 1)

;; Generator for floating-point representations
(define (generate-floating-point* name)

  (match name
    [(list 'float es nbits)

     ; Representation
     (register-representation! name 'real gfl?
                               (gfl-op es nbits bigfloat->gfl)
                               gfl->bigfloat
                               (gfl-op es nbits ordinal->gfl)
                               (gfl-op es nbits gfl->ordinal)
                               nbits
                               (disjoin gflinfinite? gflnan?))

     (define repr (get-representation name))

     ; Operator registration
     (define (register-fl-operator! op-name argc fl-impl spec vars #:otype [otype #f])
       (define op-name* (sym-append op-name '.fl es '- nbits))
       (define orepr (if otype (get-representation otype) repr))
       (define ctx
         (context vars
                  orepr
                  (make-list argc (get-representation name))))

       (register-operator-impl! op-name* ctx spec #:fl fl-impl)
       (define move-cost 1)
       (define fl-move-cost (* move-cost 1))
       (define-platform newop
         #:literal [,name fl-move-cost]
         #:default-cost fl-move-cost
         [,op-name* 10])
       (define operators* (platform-union operators newop))
       (set! operators operators*)
       )

     ;DO CONSTANTS

     (register-fl-operator! 'neg 1 (gfl-op es nbits gfl-) '(neg x) '(x))
     (register-fl-operator! '+ 2 (gfl-op es nbits gfl+) '(+ x y) '(x y))
     (register-fl-operator! '- 2 (gfl-op es nbits gfl-) '(- x y) '(x y))
     (register-fl-operator! '* 2 (gfl-op es nbits gfl*) '(* x y) '(x y))
     (register-fl-operator! '/ 2 (gfl-op es nbits gfl/) '(/ x y) '(x y))

     (register-fl-operator! 'sqrt 1 (gfl-op es nbits gflsqrt) '(sqrt x) '(x))
     (register-fl-operator! 'cbrt 1 (gfl-op es nbits gflcbrt) '(cbrt x) '(x))
     (register-fl-operator! 'fabs 1 (gfl-op es nbits gflabs) '(fabs x) '(x))

     (register-fl-operator! 'log 1 (gfl-op es nbits gfllog) '(log x) '(x))
     (register-fl-operator! 'log2 1 (gfl-op es nbits gfllog2) '(log2 x) '(x))
     (register-fl-operator! 'log10 1 (gfl-op es nbits gfllog10) '(log10 x) '(x))
     ;(register-fl-operator! 'log1p 1 (gfl-op es nbits gfllog1p) '(log1p x) '(x))

     (register-fl-operator! 'exp 1 (gfl-op es nbits gflexp) '(exp x) '(x))
     (register-fl-operator! 'exp2 1 (gfl-op es nbits gflexp2) '(exp2 x) '(x))
     ;(register-fl-operator! 'expm1 1 (gfl-op es nbits gflexpm1) '(expm1 x) '(x))
     (register-fl-operator! 'pow 2 (gfl-op es nbits gflexpt) '(pow x y) '(x y))

     (register-fl-operator! 'sin 1 (gfl-op es nbits gflsin) '(sin x) '(x))
     (register-fl-operator! 'cos 1 (gfl-op es nbits gflcos) '(cos x) '(x))
     (register-fl-operator! 'tan 1 (gfl-op es nbits gfltan) '(tan x) '(x))
     (register-fl-operator! 'asin 1 (gfl-op es nbits gflasin) '(asin x) '(x))
     (register-fl-operator! 'acos 1 (gfl-op es nbits gflacos) '(acos x) '(x))
     (register-fl-operator! 'atan 1 (gfl-op es nbits gflatan) '(atan x) '(x))

     (register-fl-operator! 'sinh 1 (gfl-op es nbits gflsinh) '(sinh x) '(x))
     (register-fl-operator! 'cosh 1 (gfl-op es nbits gflcosh) '(cosh x) '(x))
     (register-fl-operator! 'tanh 1 (gfl-op es nbits gfltanh) '(tanh x) '(x))
     (register-fl-operator! 'asinh 1 (gfl-op es nbits gflasinh) '(asinh x) '(x))
     (register-fl-operator! 'acosh 1 (gfl-op es nbits gflacosh) '(acosh x) '(x))
     (register-fl-operator! 'atanh 1 (gfl-op es nbits gflatanh) '(atanh x) '(x))

     (register-fl-operator! 'fmin 2 (gfl-op es nbits gflmin) '(fmin x y) '(x y))
     (register-fl-operator! 'fmax 2 (gfl-op es nbits gflmax) '(fmax x y) '(x y))

     (register-fl-operator! '== 2 (comparator gfl=) '(== x y) '(x y) #:otype 'bool)
     (register-fl-operator! '!= 2 (negate (comparator gfl=))  '(!= x y) '(x y) #:otype 'bool)
     (register-fl-operator! '< 2 (comparator gfl<) '(< x y) '(x y) #:otype 'bool)
     (register-fl-operator! '> 2 (comparator gfl>) '(> x y) '(x y) #:otype 'bool)
     (register-fl-operator! '<= 2 (comparator gfl<=) '(<= x y) '(x y) #:otype 'bool)
     (register-fl-operator! '>= 2 (comparator gfl>=) '(>= x y) '(x y) #:otype 'bool)
     repr]
    [_ #f]))

(define (generate-floating-point name)
  (match name
    ; This plugin is slow!! Use Herbie's built-in equivalents if possible.
    [(list 'float 11 64)
     (define repr (get-representation 'binary64))
     (register-representation-alias! name repr)
     repr]
    [(list 'float 8 32)
     (define repr (get-representation 'binary32))
     (register-representation-alias! name repr)
     repr]
    ; Helpful aliases
    ['binary256
     (define repr (generate-floating-point* '(float 19 256)))
     (register-representation-alias! name repr)
     repr]
    ['binary128
     (define repr (generate-floating-point* '(float 15 128)))
     (register-representation-alias! name repr)
     repr]
    ['binary80
     (define repr (generate-floating-point* '(float 15 80)))
     (register-representation-alias! name repr)
     repr]
    ['pxr24
     (define repr (generate-floating-point* '(float 8 24)))
     (register-representation-alias! name repr)
     repr]
    ['fp24
     (define repr (generate-floating-point* '(float 7 24)))
     (register-representation-alias! name repr)
     repr]
    ['tensorfloat
     (define repr (generate-floating-point* '(float 8 19)))
     (register-representation-alias! name repr)
     repr]
    ['bfloat16
     (define repr (generate-floating-point* '(float 8 16)))
     (register-representation-alias! name repr)
     repr]
    ['binary16
     (define repr (generate-floating-point* '(float 5 16)))
     (register-representation-alias! name repr)
     repr]
    ; Default
    [_ (generate-floating-point* name)]))

(register-generator! generate-floating-point)


(generate-floating-point* '(float 5 16))
(generate-floating-point* '(float 10 32))
;(generate-conversions '(float 5 16) '(float 10 32) 5 16 10 32)


;;; (define-operator-impl (float5-16->float10-32 [x : (float 5 16)]) (float 10 32)
;;;   #:spec x
;;;   #:fpcore (! :precision (float 10 32) (cast x))
;;;   )

;;; (define-operator-impl (float10-32->float5-16 [x : (float 10 32)]) (float 5 6)
;;;   #:spec x
;;;   #:fpcore (! :precision (float 5 16) (cast x))
;;;   )

(define-operator-impl (+.mixed [x : (float 5 16)] [y : (float 10 32)]) (float 10 32)
  #:spec (+ x y))

(define-operator-impl (+.mixed2 [x : (float 5 16)] [y : (float 10 32)]) (float 5 16)
  #:spec (+ x y))


(define-platform boolean-platform
  #:literal [bool move-cost]
  #:default-cost move-cost
  #:if-cost move-cost
  TRUE
  FALSE
  not
  and
  or)

(define-platform more-ops
  #:literal [(float 5 16) 1]
  #:default-cost 1
  +.mixed
  +.mixed2)

(register-platform! 'flonum (platform-union more-ops(platform-union boolean-platform operators)))
