#lang racket

(require "../syntax/sugar.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "rules.rkt"
         "../utils/common.rkt"
         "../utils/float.rkt"
         "../syntax/platform.rkt"
         "../syntax/read.rkt"
         "../syntax/read.rkt"
         "rival.rkt"
         "points.rkt"
         "programs.rkt"
         "sampling.rkt"
         "simplify.rkt"
         "egg-herbie.rkt"
         "compiler.rkt"
         "batch.rkt")

(provide batch-localize-costs
         batch-localize-errors
         compute-local-errors
         local-error-as-tree)

(define (regroup-nested inputss outputs)
  (match* (inputss outputs)
    [((cons (cons fhead ftail) rest) (cons head tail))
     (match-define (cons fout out) (regroup-nested (cons ftail rest) tail))
     (cons (cons head fout) out)]
    [((cons '() rest) outputs) (cons '() (regroup-nested rest outputs))]
    [('() '()) '()]))

(define (fraction-with-odd-denominator? frac)
  (and (rational? frac) (let ([denom (denominator frac)]) (and (> denom 1) (odd? denom)))))

(define (pow-impl-args impl args)
  (define vars (impl-info impl 'vars))
  (match (impl-info impl 'spec)
    [(list 'pow b e)
     #:when (set-member? vars e)
     (define env (map cons vars args))
     (define b* (dict-ref env b b))
     (define e* (dict-ref env e e))
     (cons b* e*)]
    [_ #f]))

(define (default-cost-proc expr _)
  (let rec ([expr expr])
    (match expr
      [(literal _ _) 1]
      [(? symbol?) 1]
      ; approx node
      [(approx _ impl) (rec impl)]
      [(list 'if cond ift iff) (+ 1 (rec cond) (rec ift) (rec iff))]
      [(list (? impl-exists? impl) args ...)
       (match (pow-impl-args impl args)
         [(cons _ e)
          #:when (fraction-with-odd-denominator? e)
          +inf.0]
         [_ (apply + 1 (map rec args))])]
      [(list 'pow b e)
       (if (fraction-with-odd-denominator? e) +inf.0 (+ 1 (rec b) (rec e)))]
      [(list _ args ...) (apply + 1 (map rec args))])))

(define (batch-localize-costs exprs ctx)
  (define subexprss (map all-subexpressions exprs))
  (define progs (apply append subexprss))

  ; inputs to egg
  (define reprs (map (lambda (prog) (repr-of prog ctx)) progs))
  (define rules (real-rules (*simplify-rules*)))
  (define lifting-rules (platform-lifting-rules))
  (define lowering-rules (platform-lowering-rules))

  ; egg runner (2-phases for real rewrites and implementation selection)
  (define batch (progs->batch progs))
  (define runner
    (make-egg-runner batch
                     (batch-roots batch)
                     reprs
                     `((,lifting-rules . ((iteration . 1) (scheduler . simple)))
                       (,rules . ((node . ,(*node-limit*))))
                       (,lowering-rules . ((iteration . 1) (scheduler . simple))))))

  ; run egg
  (define simplified
    (map (compose debatchref last)
         (simplify-batch runner
                         (typed-egg-batch-extractor
                          (if (*egraph-platform-cost*) platform-egg-cost-proc default-egg-cost-proc)
                          batch))))

  ; run egg
  (define simplifiedss (regroup-nested subexprss simplified))

  ; build map from starting expr to simplest
  (define expr->simplest (make-hash))
  (for ([subexprs (in-list subexprss)]
        [simplifieds (in-list simplifiedss)]
        #:when #t
        [subexpr (in-list subexprs)]
        [simplified (in-list simplifieds)])
    (hash-set! expr->simplest subexpr simplified))

  ; platform-based expression cost
  (define cost-proc
    (if (*egraph-platform-cost*) (platform-cost-proc (*active-platform*)) default-cost-proc))
  (define (expr->cost expr)
    (cost-proc expr (repr-of expr ctx)))

  (define (cost-opportunity subexpr children)
    ; start and end cost of roots
    (define start-cost (expr->cost subexpr))
    (define best-cost (expr->cost (hash-ref expr->simplest subexpr)))
    ; start and end cost of children
    (define start-child-costs (map expr->cost children))
    (define best-child-costs
      (for/list ([child (in-list children)])
        (expr->cost (hash-ref expr->simplest child))))
    (unless (>= start-cost best-cost)
      (error 'cost-opportunity "Initial expression ~a is better than final expression ~a\n"
             subexpr (hash-ref expr->simplest subexpr)))
    ; compute cost opportunity
    (- (apply - start-cost start-child-costs) (apply - best-cost best-child-costs)))

  ; rank subexpressions by cost opportunity
  (define localize-costss
    (for/list ([subexprs (in-list subexprss)])
      (sort (reap [sow]
                  (for ([subexpr (in-list subexprs)])
                    (match subexpr
                      [(? literal?) (void)]
                      [(? symbol?) (void)]
                      [(approx _ impl)
                       (define cost-opp (cost-opportunity subexpr (list impl)))
                       (sow (cons cost-opp subexpr))]
                      [(list _ args ...)
                       (define cost-opp (cost-opportunity subexpr args))
                       (sow (cons cost-opp subexpr))])))
            >
            #:key car)))

  localize-costss)

(define (batch-localize-errors exprs ctx)
  (define subexprss (map all-subexpressions exprs))
  (define errss (compute-local-errors subexprss ctx))

  (define pruned-list
    (for/list ([h (in-list errss)])
      (define pruned (make-hash))
      (for ([(k v) (in-hash h)])
        (hash-set! pruned k (hash-ref v 'errs)))
      pruned))

  (for/list ([_ (in-list exprs)]
             [errs (in-list pruned-list)])
    (sort (sort (for/list ([(subexpr err) (in-hash errs)]
                           #:when (or (list? subexpr) (approx? subexpr)))
                  (cons err subexpr))
                expr<?
                #:key cdr)
          >
          #:key (compose errors-score car))))

; Compute local error or each sampled point at each node in `prog`.
(define (compute-local-errors subexprss ctx)
  (define exprs-list (append* subexprss)) ; unroll subexprss
  (define ctx-list
    (for/list ([subexpr (in-list exprs-list)])
      (struct-copy context ctx [repr (repr-of subexpr ctx)])))

  (define expr-batch (progs->batch exprs-list))
  (define nodes (batch-nodes expr-batch))
  (define roots (batch-roots expr-batch))

  ; TODO don't ignore the status code from make-real-compiler in eval-progs-real
  (define subexprs-fn (eval-progs-real (map prog->spec exprs-list) ctx-list))
  (define actual-value-fn (compile-progs exprs-list ctx))

  (define errs
    (for/vector #:length (vector-length roots)
                ([node (in-vector roots)])
      (make-vector (pcontext-length (*pcontext*)))))

  (define exacts-out
    (for/vector #:length (vector-length roots)
                ([node (in-vector roots)])
      (make-vector (pcontext-length (*pcontext*)))))

  (define actuals-out
    (for/vector #:length (vector-length roots)
                ([node (in-vector roots)])
      (make-vector (pcontext-length (*pcontext*)))))

  (for ([(pt ex) (in-pcontext (*pcontext*))]
        [pt-idx (in-naturals)])

    (define exacts (list->vector (apply subexprs-fn pt)))
    (define actuals (apply actual-value-fn pt))

    (for ([expr (in-list exprs-list)]
          [root (in-vector roots)]
          [exact (in-vector exacts)]
          [actual (in-vector actuals)]
          [expr-idx (in-naturals)])
      (define err
        (match (vector-ref nodes root)
          [(? literal?) 1]
          [(? variable?) 1]
          [(approx _ impl)
           (define repr (repr-of expr ctx))
           (ulp-difference exact (vector-ref exacts (vector-member impl roots)) repr)]
          [`(if ,c ,ift ,iff) 1]
          [(list f args ...)
           (define repr (impl-info f 'otype))
           (define argapprox
             (for/list ([idx (in-list args)])
               (vector-ref exacts (vector-member idx roots)))) ; arg's index mapping to exact
           (define approx (apply (impl-info f 'fl) argapprox))
           (ulp-difference exact approx repr)]))
      (vector-set! (vector-ref exacts-out expr-idx) pt-idx exact)
      (vector-set! (vector-ref errs expr-idx) pt-idx err)
      (vector-set! (vector-ref actuals-out expr-idx) pt-idx actual)))

  (define n 0)
  (for/list ([subexprs (in-list subexprss)])
    (for*/hash ([subexpr (in-list subexprs)])
      (begin0 (values subexpr
                      (hasheq 'errs
                              (vector->list (vector-ref errs n))
                              'exact-values
                              (vector->list (vector-ref exacts-out n))
                              'actual-values
                              (vector->list (vector-ref actuals-out n))))
        (set! n (add1 n))))))

;; Compute the local error of every subexpression of `prog`
;; and returns the error information as an S-expr in the
;; same shape as `prog`
(define (local-error-as-tree test ctx)
  (define errs (first (compute-local-errors (list (all-subexpressions (test-input test))) ctx)))

  (define local-error
    (let loop ([expr (test-input test)])
      (define expr-info (hash-ref errs expr))
      (define err-list (hash-ref expr-info 'errs))
      (match expr
        [(list op args ...) (cons err-list (map loop args))]
        [_ (list err-list)])))

  (define exact-values
    (let loop ([expr (test-input test)])
      (define expr-info (hash-ref errs expr))
      (define exacts-list (hash-ref expr-info 'exact-values))
      (match expr
        [(list op args ...) (cons exacts-list (map loop args))]
        [_ (list exacts-list)])))

  (define actual-values
    (let loop ([expr (test-input test)])
      (define expr-info (hash-ref errs expr))
      (define actual-list (hash-ref expr-info 'actual-values))
      (match expr
        [(list op args ...) (cons actual-list (map loop args))]
        [_ (list actual-list)])))

  (define tree
    (let loop ([expr (prog->fpcore (test-input test) (test-context test))]
               [err local-error]
               [exact exact-values]
               [actual actual-values])
      (match expr
        [(list op args ...)
         ;; err => (List (listof Integer) List ...)
         (hasheq 'e
                 (~a op)
                 'avg-error
                 (format-bits (errors-score (first err)))
                 'exact-value
                 (map ~s (first exact))
                 'actual-value
                 (map ~s (first actual))
                 'children
                 (map loop args (rest err) (rest exact) (rest actual)))]
        ;; err => (List (listof Integer))
        [_
         (hasheq 'e
                 (~a expr)
                 'avg-error
                 (format-bits (errors-score (first err)))
                 'exact-value
                 (map ~s (first exact))
                 'actual-value
                 (map ~s (first actual))
                 'children
                 '())])))
  tree)
