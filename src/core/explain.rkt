#lang racket

(require racket/set
         math/bigfloat
         racket/hash
         math/flonum
         math/base)
(require "localize.rkt"
         "points.rkt"
         "programs.rkt"
         "sampling.rkt"
         "compiler.rkt"
         "logspace.rkt"
         "../syntax/sugar.rkt"
         "../syntax/types.rkt"
         "../utils/common.rkt"
         "../utils/alternative.rkt"
         "../utils/float.rkt")

(provide explain
         actual-errors)

(define 100.l (flonum->logfl 100.0))
(define 32.l (flonum->logfl 32.0))

(define *top-3* (make-parameter #f))

(define MAX-EXP 1023)

(define (take-top-n lst)
  (if (*top-3*) (take-n 3 lst) lst))

(define (take-n n lst)
  (match lst
    ['() '()]
    [(cons x xs) (if (= n 0) '() (cons x (take-n (- n 1) xs)))]))

(define (constant? expr)
  (cond
    [(list? expr) (andmap constant? (rest expr))]
    [(symbol? expr) #f]
    [else #t]))

(define (actual-errors expr pcontext)
  (match-define (cons subexprs pt-errorss)
    (parameterize ([*pcontext* pcontext])
      (flip-lists (hash->list (first (compute-local-errors (list (all-subexpressions expr))
                                                           (*context*)))))))

  (define pt-worst-subexpr
    (append* (reap [sow]
                   (for ([pt-errors (in-list pt-errorss)]
                         [(pt _) (in-pcontext pcontext)])
                     (define sub-error (map cons subexprs pt-errors))
                     (define filtered-sub-error (filter (lambda (p) (> (cdr p) 16)) sub-error))
                     (define mapped-sub-error (map (lambda (p) (cons (car p) pt)) filtered-sub-error))
                     (unless (empty? mapped-sub-error)
                       (sow mapped-sub-error))))))

  (for/hash ([group (in-list (group-by car pt-worst-subexpr))])
    (let ([key (caar group)]) (values key (map cdr group)))))

(define (same-sign? a b)
  (or (and (bfpositive? a) (bfpositive? b)) (and (bfnegative? a) (bfnegative? b))))

(define (same-sign?* a b)
  (or (and (positive? a) (positive? b) (and (negative? a) (negative? b)))))

(define all-explanations (list 'uflow-rescue 'u/u 'u/n 'o/o 'n/o 'o*u 'u*o 'n*u 'cancellation))
(define cond-thres (bf 100))
(define maybe-cond-thres (bf 32))

(define (compile-expr expr ctx)
  (define subexprs (all-subexpressions expr #:reverse? #t))
  (define spec-list (map prog->spec subexprs))
  (define ctxs
    (for/list ([subexpr (in-list subexprs)])
      (struct-copy context ctx [repr (repr-of subexpr ctx)])))

  (define repr-hash
    (make-immutable-hash (map (lambda (e ctx) (cons e (context-repr ctx))) subexprs ctxs)))

  (define subexprs-log (compile-progs (map expr->logfl subexprs) ctx))

  (define subexprs-fn
    (parameterize ([*max-mpfr-prec* 128])
      (eval-progs-real spec-list ctxs)))
  (values subexprs repr-hash subexprs-fn subexprs-log))

(define (predict-errors ctx pctx subexprs-list repr-hash subexprs-fn subexprs-log)
  (define error-count-hash (make-hash (map (lambda (x) (cons x '())) subexprs-list)))
  (define uflow-hash (make-hash))
  (define oflow-hash (make-hash))

  (define expls->points (make-hash))
  (define maybe-expls->points (make-hash))

  (for ([(pt _) (in-pcontext pctx)])
    (define (silence expr)
      (define subexprs (all-subexpressions expr #:reverse? #t))
      (for* ([subexpr (in-list subexprs)]
             #:when (list? subexpr)
             [expl (in-list all-explanations)])
        (define key (cons subexpr expl))
        (when (hash-has-key? expls->points key)
          (hash-update! expls->points key (lambda (x) (set-remove x pt))))
        (when (hash-has-key? maybe-expls->points key)
          (hash-update! maybe-expls->points key (lambda (x) (set-remove x pt))))))

    (define (mark-erroneous! expr expl)
      (hash-update! error-count-hash expr (lambda (x) (set-add x pt)))
      (hash-update! expls->points (cons expr expl) (lambda (x) (set-add x pt)) '()))

    (define (mark-maybe! expr [expl 'sensitivity])
      (hash-update! maybe-expls->points (cons expr expl) (lambda (x) (set-add x pt)) '()))

    (define exacts (apply subexprs-fn pt))

    (define exacts-hash (make-immutable-hash (map cons subexprs-list exacts)))
    (define (exacts-ref subexpr)
      (define exacts-val (hash-ref exacts-hash subexpr))
      ((representation-repr->bf (hash-ref repr-hash subexpr)) exacts-val))

    (define logfls (apply subexprs-log (map flonum->logfl pt)))
    (define logfls-hash (make-immutable-hash (map cons subexprs-list (vector->list logfls))))
    (define (logfls-ref subexpr)
      (hash-ref logfls-hash subexpr))

    (for/list ([subexpr (in-list subexprs-list)])
      (define subexpr-val (exacts-ref subexpr))
      (define slog (logfls-ref subexpr))
      ; (match-define (logfl sfl ss se) slog)
      (define se (if (boolean? slog) '() (logfl-e slog)))

      (define (update-flow-hash flow-hash pred? . children)
        (define child-set
          (foldl (lambda (a b) (hash-union a b #:combine +))
                 (make-immutable-hash)
                 (map (lambda (a) (hash-ref flow-hash a (make-immutable-hash))) children)))
        (define parent-set (hash-ref flow-hash subexpr (make-immutable-hash)))
        (define parent+child-set (hash-union parent-set child-set #:combine (lambda (_ v) v)))
        (define new-parent-set
          (if (and (bigfloat? subexpr-val) (pred? subexpr-val))
              (hash-update parent+child-set subexpr (lambda (x) (+ x 1)) 0)
              parent+child-set))
        (hash-set! flow-hash subexpr new-parent-set))

      (match subexpr
        [(list _ x-ex y-ex z-ex)
         (update-flow-hash oflow-hash bfinfinite? x-ex y-ex z-ex)
         (update-flow-hash uflow-hash bfzero? x-ex y-ex z-ex)]
        [(list _ x-ex y-ex)
         (update-flow-hash oflow-hash bfinfinite? x-ex y-ex)
         (update-flow-hash uflow-hash bfzero? x-ex y-ex)]
        [(list _ x-ex)
         (update-flow-hash oflow-hash bfinfinite? x-ex)
         (update-flow-hash uflow-hash bfzero? x-ex)]
        [_ #f])

      (match subexpr
        [(list (or '+.f64 '+.f32) x-ex y-ex)
         #:when (or (list? x-ex) (list? y-ex))

         (define xlog (lf-normalize (logfls-ref x-ex)))
         (match-define (logfl xfl xs xe) xlog)
         (define ylog (lf-normalize (logfls-ref y-ex)))
         (match-define (logfl yfl ys ye) ylog)

         (define cond-x (abs (/ xfl (+ xfl yfl))))
         (define cond-y (abs (/ yfl (+ xfl yfl))))
         (define cond-x.l (logabs (log/ xlog (log+ xlog ylog))))
         (define cond-y.l (logabs (log/ ylog (log+ xlog ylog))))

         (define x.eps (+ 127 (bigfloat-exponent (exacts-ref x-ex))))
         (define y.eps (+ 127 (bigfloat-exponent (exacts-ref y-ex))))

         (cond
           [(> (- x.eps y.eps) 100) (silence y-ex)]
           [(> (- y.eps x.eps) 100) (silence x-ex)])

         (cond
           ; Condition number hallucination
           ; Both R(x + y) and R(x) + R(y) underflow
           ; This causes the condition number to jump up,
           ; with no real error
           ; [(underflow? slog) #f]

           ; nan rescue:
           ; R(+-inf) + R(-+inf) = nan, but should actually
           ; be inf
           [(and (overflow? xlog) (overflow? ylog) (not (same-sign?* xfl yfl)))
            (mark-erroneous! subexpr 'nan-rescue)]

           ; inf rescue:
           ; R(inf) + y = non inf value (inf rescue)
           [(and (overflow? xlog) (<= (abs se) MAX-EXP)) (mark-erroneous! subexpr 'oflow-left)]
           [(and (overflow? ylog) (<= (abs se) MAX-EXP)) (mark-erroneous! subexpr 'oflow-right)]

           ; High condition number:
           ; CN(+, x, y) = |x / x + y|
           [(or (log> cond-x.l 100.l) (log> cond-y.l 100.l)) (mark-erroneous! subexpr 'cancellation)]

           ; Maybe
           [(or (log> cond-x.l 32.l) (log> cond-y.l 32.l)) (mark-maybe! subexpr 'cancellation)]
           [else #f])]

        [(list (or '-.f64 '-.f32) x-ex y-ex)
         #:when (or (list? x-ex) (list? y-ex))
         (define xlog (lf-normalize (logfls-ref x-ex)))
         (match-define (logfl xfl xs xe) xlog)
         (define ylog (lf-normalize (logfls-ref y-ex)))
         (match-define (logfl yfl ys ye) ylog)

         (define cond-x (abs (/ xfl (- xfl yfl))))
         (define cond-y (abs (/ yfl (- xfl yfl))))
         (define cond-x.l (logabs (log/ xlog (log- xlog ylog))))
         (define cond-y.l (logabs (log/ ylog (log- xlog ylog))))

         (define x.eps (+ 127 (bigfloat-exponent (exacts-ref x-ex))))
         (define y.eps (+ 127 (bigfloat-exponent (exacts-ref y-ex))))

         (cond
           [(> (- x.eps y.eps) 100) (silence y-ex)]
           [(> (- y.eps x.eps) 100) (silence x-ex)])

         (cond
           ; Condition number hallucination:
           ; When x - y correctly underflows, CN is high
           ; even though the answer is correct
           ; [(underflow? slog) #f]

           ; nan rescue:
           ; inf - inf = nan but should actually get an inf
           [(and (overflow? xlog) (overflow? ylog) (same-sign?* xfl yfl))
            (mark-erroneous! subexpr 'nan-rescue)]

           ; inf rescue
           ; If x or y overflow and the other arg rescues
           ; it
           [(and (overflow? xlog) (<= (abs se) MAX-EXP)) (mark-erroneous! subexpr 'oflow-left)]
           [(and (overflow? ylog) (<= (abs se) MAX-EXP)) (mark-erroneous! subexpr 'oflow-right)]

           ; High condition number:
           ; CN(+, x, y) = |x / x - y|
           [(or (log> cond-x.l 100.l) (log> cond-y.l 100.l)) (mark-erroneous! subexpr 'cancellation)]

           ; Maybe
           [(or (log> cond-x.l 32.l) (log> cond-y.l 32.l)) (mark-maybe! subexpr 'cancellation)]
           [else #f])]

        [(list (or 'sin.f64 'sin.f32) x-ex)
         #:when (list? x-ex)
         (define xlog (logfls-ref x-ex))
         (match-define (logfl xfl xs xe) xlog)
         (define cot-x (abs (/ 1.0 (tan xfl))))
         (define cond-no (* (abs xfl) cot-x))

         (define cot-x.l (logabs (log/ 1.l (logtan xlog))))
         (define cond-no.l (log* (logabs xlog) cot-x.l))
         (cond
           [(overflow? xlog) (mark-erroneous! subexpr 'oflow-rescue)]

           [(and (log> cond-no.l 100.l) (log> (logabs xlog) 100.l)) (mark-erroneous! subexpr 'sensitivity)]

           [(and (log> cond-no.l 100.l) (log> cot-x.l 100.l)) (mark-erroneous! subexpr 'cancelation)]

           [(and (log> cond-no.l 100.l) (log> (logabs xlog) 100.l)) (mark-maybe! subexpr 'sensitivity)]

           [(and (log> cond-no.l 32.l) (log> cot-x.l 32.l)) (mark-maybe! subexpr 'cancellation)]

           [else #f])]

        [(list (or 'cos.f64 'cos.f32) x-ex)
         #:when (list? x-ex)
         (define xlog (logfls-ref x-ex))
         (match-define (logfl xfl xs xe) xlog)
         (define tan-x (abs (tan xfl)))
         (define cond-no (* (abs xfl) tan-x))

         (define tan-x.l (logabs (logtan xlog)))
         (define cond-no.l (log* (logabs xlog) tan-x.l))

         (cond
           ;[(and (bfinfinite? x) (not (bfnan? subexpr-val))) (mark-erroneous! subexpr 'oflow-rescue)]
           [(overflow? xlog) (mark-erroneous! subexpr 'oflow-rescue)]

           ; [(and (bf> cond-no cond-thres) (bf> (bfabs x) cond-thres))
           ;  (mark-erroneous! subexpr 'sensitivity)]
           [(and (log> cond-no.l 100.l) (log> (logabs xlog) 100.l)) (mark-erroneous! subexpr 'sensitivity)]

           ; [(and (bf> cond-no cond-thres) (bf> cot-x cond-thres))
           ;  (mark-erroneous! subexpr 'cancellation)]
           [(and (log> cond-no.l 100.l) (log> tan-x.l 100.l)) (mark-erroneous! subexpr 'cancelation)]

           ; [(and (bf> cond-no cond-thres) (bf> (bfabs x) cond-thres))
           ;  (mark-erroneous! subexpr 'sensitivity)]
           [(and (log> cond-no.l 32.l) (log> (logabs xlog) 32.l)) (mark-maybe! subexpr 'sensitivity)]

           ; [(and (bf> cond-no cond-thres) (bf> cot-x cond-thres))
           ;  (mark-erroneous! subexpr 'cancellation)]
           [(and (log> cond-no.l 32.l) (log> tan-x.l 32.l)) (mark-maybe! subexpr 'cancellation)]

           [else #f])]

        [(list (or 'tan.f64 'tan.f32) x-ex)
         #:when (list? x-ex)
         (define xlog (logfls-ref x-ex))
         (match-define (logfl xfl xs xe) xlog)
         (define tan-x (tan xfl))
         (define cot-x (/ 1.0 tan-x))
         (define cond-hlf (abs (+ tan-x cot-x)))
         (define cond-no (* (abs xfl) cond-hlf))

         (define tan-x.l (logtan xlog))
         (define cot-x.l (log/ 1.l tan-x.l))
         (define cond-hlf.l (logabs (log+ tan-x.l cot-x.l)))
         (define cond-no.l (log* (logabs xlog) cond-hlf.l))

         (cond
           [(overflow? xlog) (mark-erroneous! subexpr 'oflow-rescue)]
           [(and (log> cond-no.l 100.l) (log> (logabs xlog) 100.l)) (mark-erroneous! subexpr 'sensitivity)]
           [(and (log> cond-no.l 100.l) (log> cond-hlf.l 100.l)) (mark-erroneous! subexpr 'cancellation)]

           [(and (log> cond-no.l 32.l) (log> (logabs xlog) 32.l)) (mark-maybe! subexpr 'sensitivity)]
           [(and (log> cond-no.l 32.l) (log> cond-hlf.l 32.l)) (mark-maybe! subexpr 'cancellation)]
           [else #f])]

        [(list (or 'sqrt.f64 'sqrt.f32) x-ex)
         #:when (list? x-ex)
         (define xlog (logfls-ref x-ex))
         (match-define (logfl xfl xs xe) xlog)

         (cond
           [(and (underflow? xlog) (< (/ (abs xe) 2.0) MAX-EXP))
            (mark-erroneous! subexpr 'uflow-rescue)]
           ;; Underflow rescue:

           ;; Overflow rescue:
           [(and (overflow? xlog) (< (/ (abs xe) 2.0) MAX-EXP))
            (mark-erroneous! subexpr 'oflow-rescue)])]

        [(list (or 'cbrt.f64 'cbrt.f32) x-ex)
         #:when (list? x-ex)
         (define x (exacts-ref x-ex))
         (define xlog (logfls-ref x-ex))
         (match-define (logfl xfl xs xe) xlog)

         (cond
           ;; Underflow rescue:
           ;; [(and (bfzero? x) (not (bf= subexpr-val x))) (mark-erroneous! subexpr 'uflow-rescue)]
           [(and (underflow? xlog) (< (/ (abs xe) 2.0) MAX-EXP))
            (mark-erroneous! subexpr 'uflow-rescue)]

           ;; Overflow rescue:
           ;;[(and (bfinfinite? x) (not (bf= subexpr-val x))) (mark-erroneous! subexpr 'oflow-rescue)])]
           [(and (overflow? xlog) (< (/ (abs xe) 2.0) MAX-EXP))
            (mark-erroneous! subexpr 'oflow-rescue)])]

        [(list (or '/.f64 '/.f32) x-ex y-ex)
         #:when (or (list? x-ex) (list? y-ex))
         (define x (exacts-ref x-ex))
         (define y (exacts-ref y-ex))
         (define xlog (logfls-ref x-ex))
         (match-define (logfl xfl xs xe) xlog)
         (define ylog (logfls-ref y-ex))
         (match-define (logfl yfl ys ye) ylog)

         (cond
           ;; if the numerator underflows and the denominator:
           ;; - underflows, nan could be rescued
           [(and (underflow? xlog) (underflow? ylog)) (mark-erroneous! subexpr 'u/u)]
           ;; - is small enough, 0 underflow could be rescued
           [(and (underflow? xlog) (<= (abs (- xe ye)) MAX-EXP)) (mark-erroneous! subexpr 'u/n)]
           ;; - overflows, no rescue is possible

           ;; if the numerator overflows and the denominator:
           ;; - overflows, nan could be rescued
           [(and (overflow? xlog) (overflow? ylog)) (mark-erroneous! subexpr 'o/o)]
           ;; - is large enough, inf overflow can be rescued
           [(and (overflow? xlog) (<= (abs (- xe ye)) MAX-EXP)) (mark-erroneous! subexpr 'o/n)]
           ;; - underflow, no rescue is possible

           ;; if the numerator is normal and the denominator:
           ;; - overflows, then a rescue is possible
           ; [(and (bfinfinite? y) (not (bfzero? subexpr-val))) (mark-erroneous! subexpr 'n/o)]
           [(and (overflow? ylog) (<= (abs (- xe ye)) MAX-EXP)) (mark-erroneous! subexpr 'n/o)]
           ;; - underflows, then a rescue is possible
           ;; [(and (bfzero? y) (not (bfinfinite? subexpr-val))) (mark-erroneous! subexpr 'n/u)]
           [(and (underflow? ylog) (<= (abs (- xe ye)) MAX-EXP)) (mark-erroneous! subexpr 'n/u)]
           ;; - is normal, then no rescue is possible
           [else #f])]

        [(list (or '*.f64 '*.f32) x-ex y-ex)
         #:when (or (list? x-ex) [list? y-ex])
         (define x (exacts-ref x-ex))
         (define y (exacts-ref y-ex))
         (define xlog (logfls-ref x-ex))
         (match-define (logfl xfl xs xe) xlog)
         (define ylog (logfls-ref y-ex))
         (match-define (logfl yfl ys ye) ylog)

         (cond
           ;; if one operand underflows and the other overflows, then nan must
           ;; be rescued.
           [(and (overflow? xlog) (underflow? ylog)) (mark-erroneous! subexpr 'o*u)]
           [(and (underflow? xlog) (overflow? ylog)) (mark-erroneous! subexpr 'o*u)]

           ;; If one operand is normal and the other overflows then, inf rescue
           ;; could occur
           [(and (or (overflow? xlog) (overflow? ylog)) (<= (abs (+ xe ye)) MAX-EXP))
            (mark-erroneous! subexpr 'n*o)]
           [(and (or (underflow? xlog) (underflow? ylog)) (<= (abs (+ xe ye)) MAX-EXP))
            (mark-erroneous! subexpr 'n*u)]
           ;; If both normal then no error
           [else #f])]

        [(list (or 'log.f64 'log.f32) x-ex)
         #:when (list? x-ex)
         (define x (exacts-ref x-ex))
         (define xlog (logfls-ref x-ex))
         (match-define (logfl xfl xs xe) xlog)
         (define cond-num.l (logabs (log/ 1.l xlog)))
         (define cond-num (abs (/ 1.0 xfl)))

         (cond
           ; Condition number hallucination:
           ; Condition number is high when x = 1,
           ; but x is exactly 1, so there is no error
           ; [(and (bf= x 1.bf) (bfzero? subexpr-val)) #f]

           ; overflow rescue:
           [(overflow? xlog) (mark-erroneous! subexpr 'oflow-rescue)]

           ; underflow rescue:
           [(underflow? xlog) (mark-erroneous! subexpr 'uflow-rescue)]

           ; High Condition Number:
           ; CN(log, x) = |1 / log(x)|
           [(log> cond-num.l 100.l) (mark-erroneous! subexpr 'sensitivity)]
           [(log> cond-num.l 32.l) (mark-maybe! subexpr 'sensitivity)]

           [else #f])]

        [(list (or 'exp.f64 'exp.f32) x-ex)
         #:when (list? x-ex)
         (define x (exacts-ref x-ex))
         (define exp-x (bigfloat->flonum (bfexp x)))
         (define xlog (logfls-ref x-ex))
         (match-define (logfl xfl xs xe) xlog)

         (cond
           ; Condition Number Hallucination:
           ; When x is large enough that exp(x) overflows,
           ; condition number is also high.
           ; Condition Number Hallucination:
           ; When x is large enough (negative) that exp(x)
           ; underflows, condition number is also high
           [(and (> (abs (* xfl (fllog2 euler.0))) MAX-EXP)) #f]

           ; High Condition Number:
           ; CN(exp, x) = |x|
           [(log> (logabs xlog) 100.l) (mark-erroneous! subexpr 'sensitivity)]

           [(log> (logabs xlog) 32.l) (mark-maybe! subexpr 'sensitivity)]

           [else #f])]

        ; FIXME need to rework from scratch
        [(list (or 'pow.f64 'pow.f32) x-ex y-ex)
         #:when (or (list? x-ex) (list? y-ex))
         (define x (exacts-ref x-ex))
         (define y (exacts-ref y-ex))
         (define x^y (bigfloat->flonum (bfexpt x y)))
         (define xlog (logfls-ref x-ex))
         (match-define (logfl xfl xs xe) xlog)
         (define ylog (logfls-ref y-ex))
         (match-define (logfl yfl ys ye) ylog)
         (define cond-x (abs yfl))
         (define cond-y (abs (* yfl (log xfl))))

         (define cond-x.l (logabs ylog))
         (define cond-y.l (logabs (log* ylog (logln xlog))))

         (cond
           ;; Hallucination:
           ;; x has a large exponent and y is 1. The ylogx is large but there is
           ;; no error because the answer is exactly x
           ;; [(and (bf= y 1.bf)
           ;; (bf= x subexpr-val)) #f]

           ;; Hallucination:
           ;; y is large but x is exactly 1
           ;; [(and (= (bigfloat->flonum x) 1.0)
           ;; (= (bigfloat->flonum subexpr-val) 1.0))
           ;; #f]

           ;; Hallucination:
           ;; y is large but x is zero
           ; [(and (bfzero? x) (bfzero? subexpr-val)) #f]
           [(exact-zero? xlog) #f]

           ;; Hallucination:
           ;; if x is large enough that x^y overflows, the condition number also
           ;; is very large, but the answer correctly overflows
           [(and (> yfl 1.0) (overflow? slog)) #f]

           ;[(and (bf< y -1.bf) (zero? x^y) (bfzero? subexpr-val)) #f]
           [(and (< yfl -1.0) (overflow? slog)) #f]

           ;; if x is small enough and y is large enough that x^y underflows,
           ;; the condition number also gets very large, but the answer
           ;; correctly underflows
           ;[(and (bf> y 1.bf) (zero? x^y) (bfzero? subexpr-val)) #f]
           [(and (> yfl 1.0) (underflow? slog)) #f]

           ;[(and (bf< y -1.bf) (infinite? x^y) (bfinfinite? subexpr-val)) #f]
           [(and (< yfl -1.0) (underflow? slog)) #f]

           [(and (underflow? xlog) (<= se MAX-EXP)) (mark-erroneous! subexpr 'uflow-rescue)]

           [(and (overflow? xlog) (<= se MAX-EXP)) (mark-erroneous! subexpr 'oflow-rescue)]

           [(and (or (log> cond-x.l 100.l) (log> cond-y.l 100.l)) (not (constant? y-ex)))
            (mark-erroneous! subexpr 'sensitivity)]

           [(and (or (log> cond-x.l 32.l) (log> cond-y.l 32.l)) (not (constant? y-ex)))
            (mark-maybe! subexpr 'sensitivity)]

           [else #f])]

        ;; TODO support inv trig functions
        [_ #f])))
  (values error-count-hash expls->points maybe-expls->points oflow-hash uflow-hash))

(define (generate-timelines expr
                            ctx
                            pctx
                            error-count-hash
                            expls->points
                            maybe-expls->points
                            oflow-hash
                            uflow-hash)

  (define tcount-hash (actual-errors expr pctx))

  (define repr (repr-of expr (*context*)))
  (define (values->json vs repr)
    (map (lambda (value) (value->json value repr)) vs))

  (define fperrors
    (for/list ([subexpr (in-list (set-union (hash-keys tcount-hash) (hash-keys error-count-hash)))])
      (define pset (hash-ref error-count-hash subexpr '()))
      (define tset (hash-ref tcount-hash subexpr '()))
      (define opred (set-subtract pset tset))
      (define upred (set-subtract tset pset))
      (list (~a subexpr)
            (length tset)
            (length opred)
            (and (not (empty? opred)) (values->json (first opred) repr))
            (length upred)
            (and (not (empty? upred)) (values->json (first upred) repr)))))

  (define true-error-hash
    (for/hash ([(key _) (in-pcontext pctx)]
               [value (in-list (errors expr pctx ctx))])
      (values key value)))

  (define explanations-table
    (for/list ([(key val) (in-dict expls->points)]
               #:unless (zero? (length val)))
      (define subexpr (car key))
      (define expl (cdr key))
      (define err-count (length val))
      (define maybe-count (length (hash-ref maybe-expls->points key '())))
      (define flow-list (make-flow-table oflow-hash uflow-hash subexpr expl))

      (list (~a (car subexpr)) (~a subexpr) (~a expl) err-count maybe-count flow-list)))

  (define sorted-explanations-table (take-top-n (sort explanations-table > #:key fourth)))

  (define (expls-to-points expls->points)
    (define expls-points-list (hash->list expls->points))
    (define sorted-list (sort expls-points-list > #:key (lambda (x) (length (rest x)))))
    (define points-per-expl-test (map rest sorted-list))
    (define top-3 (take-top-n points-per-expl-test))
    (define points-err (apply set-union '() top-3))
    (for/hash ([point (in-list points-err)])
      (values point true)))

  (define predicted-total-error (expls-to-points expls->points))
  (define maybe-predicted-total-error (expls-to-points maybe-expls->points))

  (define confusion-matrix (calculate-confusion true-error-hash predicted-total-error pctx))

  (define maybe-confusion-matrix
    (calculate-confusion-maybe true-error-hash
                               predicted-total-error
                               maybe-predicted-total-error
                               pctx))
  (define has-any-error?
    (for/or ([(pt _) (in-pcontext pctx)])
      (> (hash-ref true-error-hash pt) 16)))
  (define predicted-any-error?
    (for/or ([(pt _) (in-pcontext pctx)])
      (hash-ref predicted-total-error pt false)))
  (define maybe-any-error?
    (for/or ([(pt _) (in-pcontext pctx)])
      (hash-ref maybe-predicted-total-error pt false)))

  (define total-confusion-matrix
    (list (if (and has-any-error? predicted-any-error?) 1 0)
          (if (and has-any-error? (not predicted-any-error?) maybe-any-error?) 1 0)
          (if (and has-any-error? (not predicted-any-error?) (not maybe-any-error?)) 1 0)
          (if (and (not has-any-error?) predicted-any-error?) 1 0)
          (if (and (not has-any-error?) (not predicted-any-error?) maybe-any-error?) 1 0)
          (if (and (not has-any-error?) (not predicted-any-error?) (not maybe-any-error?)) 1 0)))

  (define points->expl (make-hash))

  (for ([(_ points) (in-dict expls->points)])
    (for ([pt (in-list points)])
      (hash-update! points->expl pt (lambda (x) (+ 1 x)) 0)))

  (define freqs (make-hash))

  (for ([(pt _) (in-pcontext pctx)])
    (define freq (hash-ref points->expl pt 0))
    (hash-update! freqs freq (lambda (x) (+ 1 x)) 0))

  (values fperrors
          sorted-explanations-table
          confusion-matrix
          maybe-confusion-matrix
          total-confusion-matrix
          freqs))

(define (explain expr ctx pctx)
  (define-values (subexprs-list repr-hash subexprs-fn subexprs-log) (compile-expr expr ctx))

  (define-values (error-count-hash expls->points maybe-expls->points oflow-hash uflow-hash)
    (predict-errors ctx pctx subexprs-list repr-hash subexprs-fn subexprs-log))
  (generate-timelines expr
                      ctx
                      pctx
                      error-count-hash
                      expls->points
                      maybe-expls->points
                      oflow-hash
                      uflow-hash))

(define (flow-list flow-hash expr type)
  (for/list ([(k v) (in-dict (hash-ref flow-hash expr))])
    (list (~a k) type v)))

(define (make-flow-table oflow-hash uflow-hash expr expl)
  (match (list expl expr)
    [(list 'oflow-rescue _) (flow-list oflow-hash expr "overflow")]
    [(list 'uflow-rescue _) (flow-list uflow-hash expr "underflow")]
    [(list 'u/u (list _ num den))
     (append (flow-list uflow-hash num "underflow") (flow-list uflow-hash den "underflow"))]
    [(list 'u/n (list _ num _)) (flow-list uflow-hash num "underflow")]
    [(list 'o/o (list _ num den))
     (append (flow-list oflow-hash num "overflow") (flow-list oflow-hash den "overflow"))]
    [(list 'o/n (list _ num _)) (flow-list oflow-hash num "overflow")]
    [(list 'n/o (list _ _ den)) (flow-list oflow-hash den "overflow")]
    [(list 'n/u (list _ _ den)) (flow-list uflow-hash den "underflow")]
    [(list 'o*u (list _ left right))
     (append (flow-list oflow-hash left "overflow") (flow-list uflow-hash right "underflow"))]
    [(list 'u*o (list _ left right))
     (append (flow-list uflow-hash left "underflow") (flow-list oflow-hash right "overflow"))]
    [(list 'nan-rescue (list _ left right))
     (append (flow-list oflow-hash left "overflow") (flow-list oflow-hash right "overflow"))]
    [(list 'oflow-left (list left _)) (flow-list oflow-hash left "overflow")]
    [(list 'oflow-right (list _ right)) (flow-list oflow-hash right "overflow")]
    [_ '()]))

(define (calculate-confusion actual-error predicted-error pcontext)
  (define outcomes
    (for/list ([(pt _) (in-pcontext pcontext)])
      (define error-actual? (> (hash-ref actual-error pt) 16))
      (define error-predicted? (hash-ref predicted-error pt false))
      #;(when (and error-predicted? (not error-actual?))
          (eprintf "~a\n" pt))
      (cons error-actual? error-predicted?)))

  (define groups (group-by identity outcomes))
  (define counts
    (for/hash ([group (in-list groups)])
      (values (first group) (length group))))

  (define true-pos (hash-ref counts '(#t . #t) 0))
  (define false-pos (hash-ref counts '(#f . #t) 0))
  (define true-neg (hash-ref counts '(#f . #f) 0))
  (define false-neg (hash-ref counts '(#t . #f) 0))

  (list true-pos false-neg false-pos true-neg))

(define (calculate-confusion-maybe actual-error predicted-error maybe-error pcontext)
  (define outcomes
    (for/list ([(pt _) (in-pcontext pcontext)])
      (define error-actual? (> (hash-ref actual-error pt) 16))
      (define error-predicted? (hash-ref predicted-error pt false))
      (define maybe-error-predicted? (hash-ref maybe-error pt false))
      (cons error-actual?
            (cond
              [error-predicted? 'true]
              [maybe-error-predicted? 'maybe]
              [else 'false]))))

  (define groups (group-by identity outcomes))
  (define counts
    (for/hash ([group (in-list groups)])
      (values (first group) (length group))))

  (define true-pos (hash-ref counts '(#t . true) 0))
  (define false-pos (hash-ref counts '(#f . true) 0))
  (define false-neg (hash-ref counts '(#t . false) 0))
  (define true-neg (hash-ref counts '(#f . false) 0))
  (define true-maybe (hash-ref counts '(#t . maybe) 0))
  (define false-maybe (hash-ref counts '(#f . maybe) 0))

  (list true-pos true-maybe false-neg false-pos false-maybe true-neg))
