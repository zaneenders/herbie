#lang typed/racket

(require math/flonum)

; NOTE: all logarithms henceforth are in base 2 unless specified otherwise
; A log-float number is made of 2 parts
; - a sign s,
; - and an exponent e
;
; Given a log-float number n_l = (s_n, e_n):
;     s_n = signof(n_f)
;     e_n = log|n_f|
; where n_f is the floating-point equivalent of n_l
(define-type Logfl (Pairof Boolean Flonum))

(: flonum->logfl (-> Flonum Logfl))
(define (flonum->logfl n_f)
  (cons (>= n_f 0.0) (fllog2 n_f)))

(: log-s (-> Logfl Boolean))
(define (log-s a-l)
  (car a-l))

(: log-e (-> Logfl Flonum))
(define (log-e a-l)
  (cdr a-l))

; Given 2 log-float numbers a_l = (s_a, e_a) and b_l = (s_b, e_b),
; let c_l = a_l ± b_l
; then s_c = s_a,
; and e_a = log|a_f ± b_f|
;         = log|a_f(1 ± b_f/a_f)|
;         = log|a_f| + log|1 ± b_f/a_f|
;         = e_a + log|1 ± 2^(e_b - e_a)|
(: log+ (-> Logfl Logfl Logfl))
(define (log+ a b)
  (define s-a (log-s a))
  (define s-b (log-s b))
  (define e-a (log-e a))
  (define e-b (log-e b))
  (cons s-a (+ e-a (fllog2 (+ 1.0 (flexp2 (- e-b e-a)))))))

(: log- (-> Logfl Logfl Logfl))
(define (log- a b)
  (define s-a (log-s a))
  (define s-b (log-s b))
  (define e-a (log-e a))
  (define e-b (log-e b))
  (cons s-a (+ e-a (fllog2 (- 1.0 (flexp2 (- e-b e-a)))))))

; Given 2 log-float numbers a_l = (s_a, e_a) and b_l = (s_b, e_b),
; let c_l = a_l */ b_l
; then s_c = s_a ⊻ s_b,
; and e_a = e_a ± e_b
(: log- (-> Logfl Logfl Logfl))
(define (log* a b)
  (define s-a (log-s a))
  (define s-b (log-s b))
  (define e-a (log-e a))
  (define e-b (log-e b))
  (cons (xor s-a s-b) (+ e-a e-b)))

(: log- (-> Logfl Logfl Logfl))
(define (log/ a b)
  (define s-a (log-s a))
  (define s-b (log-s b))
  (define e-a (log-e a))
  (define e-b (log-e b))
  (cons (xor s-a s-b) (- e-a e-b)))
