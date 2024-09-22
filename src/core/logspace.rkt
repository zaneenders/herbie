#lang racket

(require math/base
         math/flonum
         racket/struct
         "../syntax/syntax.rkt"
         "../syntax/types.rkt")

(provide (all-defined-out))

(define MAX-EXP 1023)
(define (representable? e)
  (and (not (nan? e)) (< (abs e) (MAX-EXP . + . 1))))
(define (set-sign s num)
  (if s num (- num)))

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

(define lf flonum->logfl)

(define (lf-normalize lf)
  (match-define (logfl x s e) lf)
  (if (or (nan? x) (zero? x) (infinite? x)) (logfl (set-sign s (flexp2 e)) s e) lf))

(define (overflow? xl)
  (match-define (logfl x s e) xl)
  (and (or (infinite? x) (nan? x)) (> (abs e) MAX-EXP)))

(define (underflow? xl)
  (match-define (logfl x s e) xl)
  (and (zero? x) (> (abs e) MAX-EXP)))

(define (exact-zero? xl)
  (match-define (logfl x s e) xl)
  (and (zero? x) (infinite? e)))

(define (logneg A)
  (match-define (logfl a sa ea) A)
  (logfl (- a) (not sa) ea))

(define (log1p_2 x)
  (/ (fllog1p x) (log 2)))

(define (log=? A B)
  (match-define (logfl a sa ea) A)
  (match-define (logfl b sb eb) B)
  (and (eq? sa sb) (= ea eb)))

(define (log> A B)
  (match-define (logfl a sa ea) A)
  (match-define (logfl b sb eb) B)
  (cond
    ; Both positive
    [(and sa sb) (> ea eb)]
    ; Both Negative
    [(and (not sa) (not sb)) (> eb ea)]
    [else sa]))

(define (log>= A B)
  (or (log=? A B) (log> A B)))

(define (log< A B)
  (not (log>= A B)))

(define (log<= A B)
  (not (log> A B)))

(define (logmax A B)
  (if (log>= A B) A B))

(define (logmin A B)
  (if (log<= A B) A B))

(define (log+ A B)
  (define P (if (log>= (logabs A) (logabs B)) A B))
  (define Q (if (log< (logabs A) (logabs B)) A B))
  (match-define (logfl a sa ea) P)
  (match-define (logfl b sb eb) Q)
  (define x (- eb ea))
  (define ec
    (+ ea (if (not (xor sa sb)) (fllog2 (abs (+ 1 (flexp2 x)))) (fllog2 (abs (- 1 (flexp2 x)))))))
  (logfl (+ a b) sa ec))

(define (log- A B)
  (log+ A (logneg B)))

(define (log* A B)
  (match-define (logfl a sa ea) A)
  (match-define (logfl b sb eb) B)
  (logfl (* a b) (not (xor a b)) (+ ea eb)))

(define (log/ A B)
  (match-define (logfl a sa ea) A)
  (match-define (logfl b sb eb) B)
  (logfl (/ a b) (not (xor a b)) (- ea eb)))

(define (logln A)
  (match-define (logfl a sa ea) A)
  (logfl (log a)
    (>= ea 0.0)
    (fllog2 (abs (* ea (log 2))))))

(define (logexp A)
  (match-define (logfl a sa ea) A)
  (logfl (exp a)
    #true
    (* a (fllog2 euler.0))))

(define (logsqrt A)
  (match-define (logfl a sa ea) A)
  (logfl (sqrt a)
    #true
    (/ ea 2.0)))

(define (logcbrt A)
  (match-define (logfl a sa ea) A)
  (logfl (expt a (/ 1 3.0))
    #true
    (/ ea 3.0)))

;; Maybe do some range reduction?
(define (logcos A)
  (match-define (logfl a sa ea) A)
  (logfl (cos a)
    (>= (cos a) 0.0)
    (if (< ea -12)
        (log1p_2 (/ (* a a) 2.0))
        (fllog2 (abs (cos a))))))

(define (logsin A)
  (match-define (logfl a sa ea) A)
  (logfl (sin a)
    (>= (sin a) 0.0)
    (if (< ea -17)
        ea
        (fllog2 (abs (sin a))))))

(define (logtan A)
  (match-define (logfl a sa ea) A)
  (logfl (sin a)
    (>= (sin a) 0.0)
    (if (< ea -18)
        ea
        (fllog2 (abs (tan a))))))

(define (logabs A)
  (match-define (logfl a sa ea) A)
  (logfl (abs a) #true ea))

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
    ['logneg #true]
    ['log> #true]
    ['log>= #true]
    ['log< #true]
    ['log<= #true]
    [_ #false]))

(define (logop symbol)
  (match symbol
    ['log+ log+]
    ['log- log-]
    ['log* log*]
    ['log/ log/]
    ['logln logln]
    ['logexp logexp]
    ; ['logexpt logexpt]
    ['logsin logsin]
    ['logcos logcos]
    ['logtan logtan]
    ['logsqrt logsqrt]
    ['logcbrt logcbrt]
    ['logneg logneg]
    ['log> log>]
    ['log>= log>=]
    ['log< log<]
    ['log<= log<=]
    [_ (error 'logop symbol)]))

(define (op->logop op)
  (match op
    ['+.f64 'log+]
    ['-.f64 'log-]
    ['*.f64 'log*]
    ['/.f64 'log/]
    ['log.f64 'logln]
    ['exp.f64 'logexp]
    ['pow.f64 'logexpt]
    ['sin.f64 'logsin]
    ['cos.f64 'logcos]
    ['tan.f64 'logtan]
    ['sqrt.f64 'logsqrt]
    ['cbrt.f64 'logcbrt]
    ['neg.f64 'logneg]
    ['+.f32 'log+]
    ['-.f32 'log-]
    ['*.f32 'log*]
    ['/.f32 'log/]
    ['log.f32 'logln]
    ['exp.f32 'logexp]
    ['pow.f32 'logexpt]
    ['sin.f32 'logsin]
    ['cos.f32 'logcos]
    ['tan.f32 'logtan]
    ['sqrt.f32 'logsqrt]
    ['cbrt.f32 'logcbrt]
    ['neg.f32 'logneg]
    ['>.f64 'log>]
    ['>.f32 'log>]
    ['>=.f64 'log>=]
    ['>=.f32 'log>=]
    ['<.f64 'log<]
    ['<.f64 'log<]
    ['<=.f32 'log<=]
    ['<=.f32 'log<=]
    ['if 'if]
    [_ (error 'op->logop op)]))

(define (expr->logfl expr)
  (match expr
    [(struct literal (value prec)) (flonum->logfl (if (flonum? value) value (exact->inexact value)))]
    [(list (or 'PI.f64 'PI.f32 'E.f64 'E.f32)) (flonum->logfl ((impl-info (first expr) 'fl)))]
    [(list op args ...) (cons (op->logop op) (map expr->logfl args))]
    [sym sym]))

(define 1.l (flonum->logfl 1.0))
