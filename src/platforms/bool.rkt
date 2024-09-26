#lang racket

;; Builtin boolean plugin

(require "runtime/utils.rkt")

;; Do not run this file with `raco test`
(module test racket/base
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation (bool bool boolean?)
                       identity
                       identity
                       (λ (x) (= x 0))
                       (λ (x) (if x 1 0))
                       1
                       (const #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constants bool [TRUE TRUE true] [FALSE FALSE false])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (and-fn . as)
  (andmap identity as))
(define (or-fn . as)
  (ormap identity as))

(define-operator-impl (not [x : bool]) bool #:spec (not x) #:fpcore (! (not x)) #:fl not)

(define-operator-impl (and [x : bool] [y : bool]) bool #:spec (and x y) #:fl and-fn)

(define-operator-impl (or [x : bool] [y : bool]) bool #:spec (or x y) #:fl or-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-ruleset* bool-reduce
                 (bools simplify fp-safe)
                 #:type ([a bool] [b bool])
                 [not-true (not (TRUE)) (FALSE)]
                 [not-false (not (FALSE)) (TRUE)]
                 [not-not (not (not a)) a]
                 [not-and (not (and a b)) (or (not a) (not b))]
                 [not-or (not (or a b)) (and (not a) (not b))]
                 [and-true-l (and (TRUE) a) a]
                 [and-true-r (and a (TRUE)) a]
                 [and-false-l (and (FALSE) a) (FALSE)]
                 [and-false-r (and a (FALSE)) (FALSE)]
                 [and-same (and a a) a]
                 [or-true-l (or (TRUE) a) (TRUE)]
                 [or-true-r (or a (TRUE)) (TRUE)]
                 [or-false-l (or (FALSE) a) a]
                 [or-false-r (or a (FALSE)) a]
                 [or-same (or a a) a])

(define-ruleset* bool-expand
                 (bools simplify fp-safe)
                 #:type ([a bool] [b bool])
                 ;or
                 [or-false-r-rev a (or a (FALSE))]
                 [or-false-l-rev a (or (FALSE) a)]
                 [or-same-rev a (or a a)]
                 ;not
                 [not-false-rev (TRUE) (not (FALSE))]
                 [not-true-rev (FALSE) (not (TRUE))]
                 [not-and-rev (or (not a) (not b)) (not (and a b))]
                 [not-or-rev (and (not a) (not b)) (not (or a b))]
                 [not-not-rev a (not (not a))]
                 ;and
                 [and-true-r-rev a (and a (TRUE))]
                 [and-true-l-rev a (and (TRUE) a)]
                 [and-same-rev a (and a a)])
