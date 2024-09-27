#lang racket
(require ffi/unsafe
         ffi/unsafe/define
         ffi/vector
         rackunit)

(provide (all-defined-out))

(define qd-lib (ffi-lib "../../QD/src/.libs/libqd"))

(define log2-hi 6.931471805599452862e-01)
(define log2-lo 2.319046813846299558e-17)

(define-ffi-definer define-qd qd-lib)

(define-syntax define-dd-binary
  (syntax-rules ()
    [(_ name ...)
     (begin
       (define-qd name
         (_fun _pointer _pointer _pointer -> _void))
       ...)]))

(define-syntax define-dd-unary
  (syntax-rules ()
    [(_ name ...)
     (begin
       (define-qd name
         (_fun _pointer _pointer -> _void))
       ...)]))

; Example usage:
(define-dd-binary
  c_dd_add
  c_dd_sub
  c_dd_mul
  c_dd_div
  c_dd_npwr
  c_dd_nroot)

(define-dd-unary
  c_dd_sqrt
  c_dd_sqr
  c_dd_abs
  c_dd_exp
  c_dd_log
  c_dd_log10
  c_dd_sin
  c_dd_cos
  c_dd_tan)

(define-syntax define-dd-binary-fn
  (syntax-rules ()
    [(_ [name1 name2] ...)
     (begin
       (define (name1 x1 x2 y1 y2)
         (define a (list->f64vector (list x1 x2)))
         (define b (list->f64vector (list y1 y2)))
         (define c (make-f64vector 2))
         (name2 (f64vector->cpointer a)
                (f64vector->cpointer b)
                (f64vector->cpointer c))
         (apply values (f64vector->list c)))
       ...)]))

(define-syntax define-dd-unary-fn
  (syntax-rules ()
    [(_ [name1 name2] ...)
     (begin
       (define (name1 x1 x2)
         (define a (list->f64vector (list x1 x2)))
         (define b (make-f64vector 2))
         (name2 (f64vector->cpointer a)
                (f64vector->cpointer b))
         (apply values (f64vector->list b)))
       ...)]))

(define-dd-binary-fn
  [dd+ c_dd_add]
  [dd- c_dd_sub]
  [dd* c_dd_mul]
  [dd/ c_dd_div]
  [ddnpow c_dd_npwr]
  [ddnrooth c_dd_nroot])

(define-dd-unary-fn
  [ddsqrt c_dd_sqrt]
  [ddsqr c_dd_sqr]
  [ddabs c_dd_abs]
  [ddexp c_dd_exp]
  [ddlog c_dd_log]
  [ddlog10 c_dd_log10]
  [ddsin c_dd_sin]
  [ddcos c_dd_cos]
  [ddtan c_dd_tan])


(define (ddlog2 x1 x2)
  (let*-values
      ([(a1 a2) (ddlog x1 x2)])
    (dd/ a1 a2 log2-hi log2-lo)))

(define (ddexp2 x1 x2)
  (let*-values
       ([(e1 e2) (dd* x1 x2 log2-hi log2-lo)])
    (ddexp e1 e2)))


(define (ddpi)
  (values 3.141592653589793116e+00 1.224646799147353207e-16))
