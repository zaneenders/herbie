#lang racket

(require (submod "../syntax/syntax.rkt" internals))

(provide shift
         unshift
         define-constant-impls)

(define (shift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (fn (- x shift-val))))

(define (unshift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (+ (fn x) shift-val)))

(define-syntax-rule (define-constant-impls repr
                      [impl-name spec-name value] ...)
  (begin
    (define-operator-impl (impl-name) repr
      #:spec (spec-name)
      #:fl (const value)) ...))
