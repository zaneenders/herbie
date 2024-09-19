#lang racket

(require rackunit)

(require "../utils/common.rkt"
         "../utils/float.rkt"
         "rules.rkt"
         (submod "rules.rkt" internals)
         "../syntax/platform.rkt"
         "../syntax/load-plugin.rkt"
         "../syntax/sugar.rkt"
         "../syntax/types.rkt"
         "compiler.rkt"
         "rival.rkt"
         "sampling.rkt"
         "programs.rkt")

(load-herbie-builtins)

(define num-test-points (make-parameter 100))

(define *conditions*
  (list '[asinh-2 . (>= x 0)]
        '[pow-unpow . (>= a 0)]
        '[pow-pow . (>= a 0)]
        '[sqrt-pow1 . (>= x 0)]
        '[asin-sin-s . (<= (fabs x) (/ (PI) 2))]
        '[acos-cos-s . (and (<= 0 x) (<= x (PI)))]
        '[atan-tan-s . (<= (fabs x) (/ (PI) 2))]))

(define double-repr (get-representation 'binary64))
(define boolean-repr (get-representation 'bool))

(define (type->repr type)
  (match type
    ['real double-repr]
    ['bool boolean-repr]))

(define (env->ctx env out)
  (define vars (dict-keys env))
  (define itypes (map type->repr (dict-values env)))
  (context vars (type->repr out) itypes))

(define (rule->impl-rules rule)
  (platform-impl-rules (list rule)))

(define (check-rule-sound test-rule)
  (match-define (rule name p1 p2 env out) test-rule)
  (define ctx (env->ctx env out))

  (match-define (list pts exs)
    (parameterize ([*num-points* (num-test-points)]
                   [*max-find-range-depth* 0])
      (cdr (sample-points '(TRUE) (list p1) (list ctx)))))

  (define compiler (make-real-compiler (list p2) (list ctx)))
  (for ([pt (in-list pts)]
        [v1 (in-list exs)])
    (with-check-info* (map make-check-info (context-vars ctx) pt)
                      (λ ()
                        (define-values (status v2) (real-apply compiler pt))
                        (with-check-info (['lhs v1] ['rhs v2] ['status status])
                                         (when (and (real? v2)
                                                    (nan? v2)
                                                    (not (set-member? '(exit unsamplable) status)))
                                           (fail "Right hand side returns NaN")))))))

(define (check-rule-correct test-rule)
  (match-define (rule name p1 p2 env out) test-rule)
  (define ctx (env->ctx env out))

  (define pre (dict-ref *conditions* name '(TRUE)))
  (match-define (list pts exs1 exs2)
    (parameterize ([*num-points* (num-test-points)]
                   [*max-find-range-depth* 0])
      (cdr (sample-points pre (list p1 p2) (list ctx ctx)))))

  (for ([pt (in-list pts)]
        [v1 (in-list exs1)]
        [v2 (in-list exs2)])
    (with-check-info* (map make-check-info (context-vars ctx) pt)
                      (λ ()
                        (with-check-info (['lhs v1] ['rhs v2])
                                         (check-eq? (ulp-difference v1 v2 (context-repr ctx)) 1))))))

(define (replace expr hash)
  (define newExpr (list))
  (when (not (list? expr))
    (set! expr (list expr)))
  (for ([item expr])
    (cond
      [(list? item) (set! newExpr (cons (replace item hash) newExpr))]
      [(symbol? item)
       (if (hash-has-key? hash item)
           (set! newExpr (cons (hash-ref hash item) newExpr))
           (set! newExpr (cons item newExpr)))]))
  (reverse newExpr))
(define (permutations xs)
  (define (perms-starting x)
    (map (lambda (ps) (cons x ps)) (permutations (remove x xs))))
  (if (null? xs) '(()) (apply append (map (lambda (x) (perms-starting x)) xs))))

(define (check-rule-inverse test-rule rules-hash)
  (match-define (rule name p1 p2 env out) test-rule)
  (define inv #f)
  (define rules (hash-values rules-hash))

  (for ([rule-test rules])
    (match-define (rule name* p1* p2* env* out*) rule-test)
    ;(displayln (format "RuleTest: ~a = ~a -> ~a" name p1 p2))
    ;(displayln (format "RuleTest: ~a = ~a -> ~a" name* p1* p2*))
    (when (and (equal? p1 p2*) (equal? p1* p2))
      ;;; (displayln (format "Rule 1: ~a = ~a -> ~a" name p1 p2))
      ;;; (displayln (format "Rule 2: ~a = ~a -> ~a" name* p1* p2*))
      (hash-remove! rules-hash name*)
      (hash-remove! rules-hash name)
      (set! inv #t))

    ;Creates a hash of all the vars
    (define vars (free-variables p2*))

    ;Create a list of all permutations
    (define perm-list (permutations vars))
    (define list-hash '())
    (for ([perm perm-list])
      (define temp-hash (make-hash))
      (for ([var vars]
            [pr perm])
        (hash-set! temp-hash var pr))
      (set! list-hash (cons temp-hash list-hash)))

    (when (and (not (equal? (length vars) 1)) (not (and (equal? p1 p2*) (equal? p1* p2))))
      (for ([perm-hash list-hash])
        (set! p1* (replace p1* perm-hash))
        (set! p2* (replace p2* perm-hash))
        (when (and (equal? p1 p2*) (equal? p1* p2))
          ;;; (displayln (format "Rule 1-rep: ~a = ~a -> ~a" name p1 p2))
          ;;; (displayln (format "Rule 2-rep: ~a = ~a -> ~a" name* p1* p2*))
          (hash-remove! rules-hash name*)
          (hash-remove! rules-hash name)
          (set! inv #t))))))

(define (check-rule-fp-safe test-rule)
  (match-define (rule name p1 p2 itypes repr) test-rule)
  (define fv (dict-keys itypes))
  (define ctx (context fv repr (map (curry dict-ref itypes) fv)))
  (define (make-point _)
    (for/list ([v (in-list fv)])
      (random-generate (dict-ref itypes v))))
  (define points (build-list (num-test-points) make-point))
  (define prog (compile-progs (list p1 p2) ctx))
  (for ([pt points])
    (with-check-info (['point (map list fv pt)])
                     (match-define (vector v1 v2) (apply prog pt))
                     (check-equal? v1 v2))))

(module+ main
  (define _ (*simplify-rules*)) ; force an update
  (num-test-points (* 100 (num-test-points)))
  (command-line #:args names
                (for ([name names])
                  (eprintf "Checking ~a...\n" name)
                  (define rule (first (filter (λ (x) (equal? (~a (rule-name x)) name)) (*rules*))))
                  (check-rule-correct rule)
                  (unless (set-member? (*unsound-rules*) rule)
                    (check-rule-sound rule))
                  (when (set-member? (*fp-safe-simplify-rules*) rule)
                    (for ([rule* (rule->impl-rules rule)])
                      (check-rule-fp-safe rule*))))))

(define _ (*simplify-rules*)) ; force an update
(define rules '())
(define rule-hash (make-hash))
(for* ([(_ test-ruleset) (in-dict (*rulesets*))]
       [test-rule (first test-ruleset)])
  (set! rules (append rules (list test-rule)))
  (define name (rule-name test-rule))
  (hash-set! rule-hash name test-rule))

(for ([rule rules])
  (test-case (~a (rule-name rule))
    (check-rule-correct rule)))

;;Test for inverses
(for ([rule rules])
  (test-case (~a (rule-name rule))
    (check-rule-inverse rule rule-hash)))

(with-output-to-file "reports/output2.txt"
                     (lambda ()
                       (for ([expr (hash-values rule-hash)])
                         (match-define (rule name p1 p2 env out) expr)
                         (display (format "~a&&~a&&~a\n" name p1 p2)))))

(for ([rule rules]
      #:unless (set-member? (*unsound-rules*) rule))
  (test-case (~a (rule-name rule))
    (check-rule-sound rule)))

(for* ([rule rules]
       [rule* (rule->impl-rules rule)]
       #:when (set-member? (*fp-safe-simplify-rules*) rule))
  (test-case (~a (rule-name rule*))
    (check-rule-fp-safe rule*)))
