
#lang racket

(require math/base
         math/flonum
         racket/struct
         "../syntax/syntax.rkt"
         "logspace.rkt"
         "dd.rkt")

(provide (all-defined-out))

(struct ddlf (r1 r2 s e1 e2)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (_) 'ddlf)
      (lambda (obj)
        (list (ddlf-r1 obj)
              (ddlf-r2 obj)
              (ddlf-s obj)
              (ddlf-e1 obj)
              (ddlf-e2 obj)))))])

(define (dl fl1 [fl2 0])
  (let*-values
      ([(e1 e2) (ddabs fl1 fl2)]
       [(e1 e2) (ddlog2 e1 e2)])
    (ddlf fl1 fl2 (dd>= fl1 fl2 0.0 0.0) e1 e2)))

(define +max.dl (ddlf +max.0 0.0 #true 1024.0 0.0))
(define +min.dl (ddlf +min.0 0.0 #true -1074.0 0.0))

(define (dl-normalize x)
  (match-define (ddlf x1 x2 s e1 e2) x)
  (define-values (l1 l2) (ddexp2 x1 x2))
  (define-values (n1 n2) (ddneg l1 l2))
  (if (or (ddnan? x1 x2) (ddzero? x1 x2) (ddinfinite? x1 x2))
      (if s
          (ddlf l1 l2 s e1 e2)
          (ddlf n1 n2 s e1 e2))
      x))

(define (dl> x y)
  (match-define (ddlf _ _ xs ex1 ex2) x)
  (match-define (ddlf _ _ ys ey1 ey2) y)
  (cond
    ; Both +ve
    [(and xs ys) (dd> ex1 ex2 ey1 ey2)]

    ; Both -ve
    [(nor xs ys) (dd> ey1 ey2 ex1 ex2)]

    [else xs]))

(define (dl< x y)
  (dl> y x))

(define (dl= x y)
  (match-define (ddlf _ _ xs ex1 ex2) x)
  (match-define (ddlf _ _ ys ey1 ey2) y)
  (and (eq? xs ys) (dd= ex1 ex2 ey1 ey2)))

(define (dl>= x y)
  (or (dl> x y) (dl= x y)))

(define (dl<= x y)
  (dl>= y x))

(define (dlmax x y)
  (if (dl>= x y) x y))

(define (dlmin x y)
  (if (dl<= x y) x y))

(define (dlabs x)
  (match-define (ddlf x1 x2 _ ex1 ex2) x)
  (let*-values
      ([(x1 x2) (ddabs x1 x2)])
    (ddlf x1 x2 #true ex1 ex2)))

(define (dloverflowed? x)
  (dl> (dlabs x) +max.dl))

(define (dlunderflowed? x)
  (dl< (dlabs x) +min.dl))

(define (dlnan? x)
  (match-define (ddlf x1 x2 _ ex1 ex2) x)
  (ddnan? ex1 ex2))

(define (dlrepresentable? x)
  (and (not (dloverflowed? x))
       (not (dlunderflowed? x))
       (not (dlnan? x))))

(define (dlzero? x)
  (match-define (ddlf x1 x2 _  e1 e2) x)
  (and (ddzero? x1 x2) (ddinfinite? e1 e2)))

(define (dlneg x)
  (match-define (ddlf x1 x2 s e1 e2) x)
  (let*-values
      ([(x1 x2) (ddneg x1 x2)])
    (ddlf x1 x2 (not s) e1 e2)))

(define (dl+ A B)
  (define P (if (dl>= (dlabs A) (dlabs B)) A B))
  (define Q (if (dl< (dlabs A) (dlabs B)) A B))
  (match-define (ddlf x1 x2 xs ex1 ex2) P)
  (match-define (ddlf y1 y2 ys ey1 ey2) Q)
  (let*-values ([(a1 a2) (dd- ey1 ey2 ex1 ex2)]
                [(b1 b2) (ddexp2 a1 a2)]
                [(c1 c2) (if (not (xor xs ys))
                             (dd+ 1.0 0.0 b1 b2)
                             (dd- 1.0 0.0 b1 b2))]
                [(d1 d2) (ddabs c1 c2)]
                [(e1 e2) (ddlog2 d1 d2)]
                [(f1 f2) (dd+ ex1 ex2 e1 e2)]
                [(z1 z2) (dd+ x1 x2 y1 y2)])
    (ddlf z1 z2 xs f1 f2)))

(define (dl- A B)
  (dl+ A (dlneg B)))

(define (dl* A B)
  (match-define (ddlf x1 x2 xs ex1 ex2) A)
  (match-define (ddlf y1 y2 ys ey1 ey2) B)
  (let*-values
      ([(z1 z2) (dd* x1 x2 y1 y2)]
       [(zs) (not (xor xs ys))]
       [(ez1 ez2) (dd+ ex1 ex2 ey1 ey2)])
    (ddlf z1 z2 zs ez1 ez2)))

(define (dl/ A B)
  (match-define (ddlf x1 x2 xs ex1 ex2) A)
  (match-define (ddlf y1 y2 ys ey1 ey2) B)
  (let*-values
      ([(z1 z2) (dd/ x1 x2 y1 y2)]
       [(zs) (not (xor xs ys))]
       [(ez1 ez2) (dd- ex1 ex2 ey1 ey2)])
    (ddlf z1 z2 zs ez1 ez2)))

(define (dllog A)
  (match-define (ddlf x1 x2 _ ex1 ex2) A)
  (let*-values ([(z1 z2) (ddlog x1 x2)]
                [(zs) (dd>= ex1 ex2 0.0 0.0)]
                [(a1 a2) (dd* ex1 ex2 log2-hi log2-lo)]
                [(a1 a2) (ddabs a1 a2)]
                [(a1 a2) (ddlog2 a1 a2)])
    (ddlf z1 z2 zs a1 a2)))

(define (dlexp A)
  (match-define (ddlf x1 x2 _ _ _) A)
  (let*-values ([(z1 z2) (ddexp x1 x2)]
                [(zs) #true]
                [(a1 a2) (ddlog2 dde1 dde2)]
                [(a1 a2) (dd* x1 x2 a1 a2)])
    (ddlf z1 z2 zs a1 a2)))

(define (dlexpt A B)
  (match-define (ddlf x1 x2 xs ex1 ex2) A)
  (match-define (ddlf y1 y2 ys ey1 ey2) B)
  (let*-values ([(z1 z2) (ddexpt x1 x2 y1 y2)]
                [(zs) (dd>= z1 z2 0.0 0.0)]
                [(int?) (ddint? y1 y2)])
    (cond
      [(dd>= x1 x2 0.0 0.0)
       (let*-values ([(a1 a2) (dd* y1 y2 ex1 ex2)])
         (ddlf z1 z2 zs a1 a2))
       (dd* y1 y2 ex1 ex2)]
      [int? (let*-values
                ([(a1 a2) (dd* y1 y2 ex1 ex2)])
              (ddlf z1 z2 zs a1 a2))]
      [else (ddlf z1 z2 zs +nan.0 0.0)])))

(define (dlsqrt A)
  (match-define (ddlf x1 x2 _ ex1 ex2) A)
  (let*-values ([(z1 z2) (ddsqrt x1 x2)]
                [(a1 a2) (dd/ ex1 ex2 2.0 0.0)])
    (ddlf z1 z2 #true a1 a2)))

(define (dlcbrt A)
  (match-define (ddlf x1 x2 xs ex1 ex2) A)
  (let*-values ([(z1 z2) (ddnroot x1 x2 3)]
                [(a1 a2) (dd/ ex1 ex2 3.0 0.0)])
    (ddlf z1 z2 xs a1 a2)))

(define (dlcos A)
  (match-define (ddlf x1 x2 _ _ _) A)
  (let*-values ([(z1 z2) (ddcos x1 x2)]
                [(zs) (dd>= z1 z2 0.0 0.0)]
                [(a1 a2) (ddabs z1 z2)]
                [(a1 a2) (ddlog2 a1 a2)])
    (ddlf z1 z2 zs a1 a2)))

(define (dlsin A)
  (match-define (ddlf x1 x2 _ _ _) A)
  (let*-values ([(z1 z2) (ddsin x1 x2)]
                [(zs) (dd>= z1 z2 0.0 0.0)]
                [(a1 a2) (ddabs z1 z2)]
                [(a1 a2) (ddlog2 a1 a2)])
    (ddlf z1 z2 zs a1 a2)))

(define (dltan A)
  (match-define (ddlf x1 x2 _ _ _) A)
  (let*-values ([(z1 z2) (ddtan x1 x2)]
                [(zs) (dd>= z1 z2 0.0 0.0)]
                [(a1 a2) (ddabs z1 z2)]
                [(a1 a2) (ddlog2 a1 a2)])
    (ddlf z1 z2 zs a1 a2)))

(define (dlop? symbol)
  (match symbol
    ['dl+ #true]
    ['dl- #true]
    ['dl* #true]
    ['dl/ #true]
    ['dllog #true]
    ['dlexp #true]
    ;['dlexpt #true]
    ['dlsin #true]
    ['dlcos #true]
    ['dltan #true]
    ['dlsqrt #true]
    ['dlcbrt #true]
    ['dlneg #true]
    ['dl> #true]
    ['dl>= #true]
    ['dl< #true]
    ['dl<= #true]
    [_ #false]))

(define (dlop symbol)
  (match symbol
    ['dl+ dl+]
    ['dl- dl-]
    ['dl* dl*]
    ['dl/ dl/]
    ['dllog dllog]
    ['dlexp dlexp]
    ;['dlexpt #true]
    ['dlsin dlsin]
    ['dlcos dlcos]
    ['dltan dltan]
    ['dlsqrt dlsqrt]
    ['dlcbrt dlcbrt]
    ['dlneg dlneg]
    ['dl> dl>]
    ['dl>= dl>=]
    ['dl< dl<]
    ['dl<= dl<=]
    [_ #false]))

(define (op->dlop op)
  (match op
    ['+.f64 'dl+]
    ['-.f64 'dl-]
    ['*.f64 'dl*]
    ['/.f64 'dl/]
    ['log.f64 'dllog]
    ['exp.f64 'dlexp]
    ; ['pow.f64 'dlexpt]
    ['sin.f64 'dlsin]
    ['cos.f64 'dlcos]
    ['tan.f64 'dltan]
    ['sqrt.f64 'dlsqrt]
    ['cbrt.f64 'dlcbrt]
    ['neg.f64 'dlneg]
    ['+.f32 'dl+]
    ['-.f32 'dl-]
    ['*.f32 'dl*]
    ['/.f32 'dl/]
    ['log.f32 'dllog]
    ['exp.f32 'dlexp]
    ['pow.f32 'dlexpt]
    ['sin.f32 'dlsin]
    ['cos.f32 'dlcos]
    ['tan.f32 'dltan]
    ['sqrt.f32 'dlsqrt]
    ['cbrt.f32 'dlcbrt]
    ['neg.f32 'dlneg]
    ['>.f64 'dl>]
    ['>.f32 'dl>]
    ['>=.f64 'dl>=]
    ['>=.f32 'dl>=]
    ['<.f64 'dl<]
    ['<.f64 'dl<]
    ['<=.f32 'dl<=]
    ['<=.f32 'dl<=]
    ['if 'if]
    [_ (error 'op->logop op)]))

(define (expr->dl expr)
  (match expr
    [(struct literal (value _))
     (dl (if (flonum? value)
             value
             (exact->inexact value)))]
    [(list (or 'PI.f64 'PI.f32))
     (dl ddpi1 ddpi2)]
    [(list (or 'E.f64 'E.f32))
     (dl dde1 dde2)]
    [(list op args ...)
     (cons (op->dlop op)
           (map expr->dl args))]
    [sym sym]))

(define 1.dl (dl 1.0))
