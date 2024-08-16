#lang racket

(require math/base
         math/flonum
         racket/struct)

(provide (all-defined-out))

(struct logfl (r s e)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer (lambda (obj) 'logfl)
                                     (lambda (obj)
                                       (list (logfl-r obj) (logfl-s obj) (logfl-e obj)))))])

; NOTE: all logarithms henceforth are in base 2 unless specified otherwise
; A log-float number is made of 2 parts
; - a sign s,
; - and an exponent e
;
; Given a log-float number n_l = (s_n, e_n):
;     s_n = signof(n_f)
;     e_n = log|n_f|
; where n_f is the floating-point equivalent of n_l
(define (flonum->logfl n_f)
  (logfl n_f (>= n_f 0.0) (fllog2 (abs n_f))))

(define (log-neg A)
  (match-define (logfl a sa ea) A)
  (logfl (- a) (not sa) ea))

; Given 2 log-float numbers a_l = (s_a, e_a) and b_l = (s_b, e_b),
; let c_l = a_l ± b_l
; then s_c = s_a,
; and e_a = log|a_f ± b_f|
;         = log|a_f(1 ± b_f/a_f)|
;         = log|a_f| + log|1 ± b_f/a_f|
;         = e_a + log|1 ± 2^(e_b - e_a)|
(define (log+ A B)
  (match-define (logfl a sa ea) A)
  (match-define (logfl b sb eb) B)
  (logfl (+ a b) sa (+ ea (fllog2 (abs (+ 1 (flexp2 (- eb ea))))))))

(define (log- A B)
  (match-define (logfl a sa ea) A)
  (match-define (logfl b sb eb) B)
  (logfl (+ a b) sa (+ ea (fllog2 (abs (- 1 (flexp2 (- eb ea))))))))

; Given 2 log-float numbers a_l = (s_a, e_a) and b_l = (s_b, e_b),
; let c_l = a_l */ b_l
; then s_c = s_a ⊻ s_b,
; and e_a = e_a ± e_b
(define (log* A B)
  (match-define (logfl a sa ea) A)
  (match-define (logfl b sb eb) B)
  (logfl (* a b) (xor sa sb) (+ ea eb)))

(define (log/ A B)
  (match-define (logfl a sa ea) A)
  (match-define (logfl b sb eb) B)
  (logfl (* a b) (xor sa sb) (- ea eb)))

(define (logln A)
  (match-define (logfl a sa ea) A)
  (logfl (log a) sa (log ea)))

(define (logexp A)
  (match-define (logfl a sa ea) A)
  (logfl (exp a) #true (* a (fllog2 euler.0))))

(define (logexpt A B)
  (match-define (logfl a sa ea) A)
  (match-define (logfl b sb eb) B)
  (logfl (expt a b) #true (* b ea)))

(define (logsin A)
  (match-define (logfl a sa ea) A)
  (logfl (sin a) (>= (sin a) 0.0) (fllog2 (sin a))))

(define (logcos A)
  (match-define (logfl a sa ea) A)
  (logfl (cos a) (>= (cos a) 0.0) (fllog2 (cos a))))

(define (logtan A)
  (match-define (logfl a sa ea) A)
  (logfl (tan a) (>= (tan a) 0.0) (fllog2 (tan a))))

(define (logsqrt A)
  (match-define (logfl a sa ea) A)
  (logfl (sqrt a) #true (/ ea 2.0)))

(define (logcbrt A)
  (match-define (logfl a sa ea) A)
  (logfl (expt a (/ 1 3)) (>= a 0.0) (/ ea 3.0)))

(define (logop? symbol)
  (match symbol
    ['log+ #true]
    ['log- #true]
    ['log* #true]
    ['log/ #true]
    ['logln #true]
    ['logexp #true]
    ['logexpt #true]
    ['logsin #true]
    ['logcos #true]
    ['logtan #true]
    ['logsqrt #true]
    ['logcbrt #true]
    [_ #false]))

(define (logop symbol)
  (match symbol
    ['log+ log+]
    ['log- log-]
    ['log* log*]
    ['log/ log/]
    ['logln logln]
    ['logexp logexp]
    ['logexpt logexpt]
    ['logsin logsin]
    ['logcos logcos]
    ['logtan logtan]
    ['logsqrt logsqrt]
    ['logcbrt logcbrt]
    [_ (error 'logop "not supported")]))
