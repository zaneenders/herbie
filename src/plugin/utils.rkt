#lang racket

(require (submod "../syntax/syntax.rkt" internals)
         (submod "../syntax/types.rkt" internals))

(provide shift
         unshift
         define-constants)

(define (shift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (fn (- x shift-val))))

(define (unshift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (+ (fn x) shift-val)))

(define-syntax-rule (define-constants repr [impl-name spec-name value] ...)
  (begin
    (define-operator-impl (impl-name) repr
      #:spec (spec-name)
      #:fl (const value)) ...))
