#lang racket

(provide types
         type-name?
         define-type
         *context*
         *active-platform*
         (struct-out representation)
         (struct-out context)
         (struct-out operator)
         (struct-out operator-impl)
         (struct-out literal)
         (struct-out approx)
         (struct-out platform))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Types": type in a real number program
;;
;; In practice, there are only two: real, boolean.
;; Types are defined by
;;  - (unique) name [symbol?]

(define types (mutable-seteq))

;; Is the argument registered as a type?
(define (type-name? x)
  (set-member? types x))

;; Declares a symbol to be a type
(define-syntax-rule (define-type name _ ...)
  (set-add! types 'name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Representations": type in a floating-point program
;;
;; Representations are defined by
;;  - (unique) name : any/c
;;  - type that the representation implements : type-name?
;;  - predicate to detect values in the representation : (-> any/c boolean?)
;;  - converter from bigfloat to representation value : (-> bigfloat? (representation-repr? repr))
;;  - converter from representation value to bigfloat : (-> (representation-repr? repr) bigfloat?)
;;  - converter from natural to representation value : (-> natural? (representation-repr? repr))
;;  - number of bits required for encoding : natural?
;;  - predicate to detect non-real values in the representation : (-> (representation-repr? repr) boolean?)

(struct representation
        (name type repr? bf->repr repr->bf ordinal->repr repr->ordinal total-bits special-value?)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc repr port mode)
     (fprintf port "#<representation:~a>" (representation-name repr)))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Context": type signature with names inputs
;;
;; Contexts are defined by
;;  - input names : (listof symbol?)
;;  - input representations : (listof representation?)
;;  - output representation : representation?

(struct context (vars repr var-reprs) #:transparent)

;; Current context
(define *context* (make-parameter #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Operator": pure, mathematical operator
;;
;; Operators are defined by
;;  - (unique) name : symbol?
;;  - input types : (listof type-name?)
;;  - output type : type-name?
;;  - deprecated flag : boolean?

(struct operator (name itype otype deprecated)
        #:methods gen:custom-write
        [(define (write-proc op port mode)
          (fprintf port "#<operator:~a>" (operator-name op)))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Operator implementations": floating-point operator
;;
;; Operator implementations _approximate_ an expression over
;; mathematical operators with a fixed interface of representations.
;;
;; Operator implementations are defined by
;;  - (unique) name : symbol?
;;  - type signature : context?
;;  - expression it approximates : expr?
;;  - FPCore representation : expr?
;;  - floating-point implementation : procedure?

(struct operator-impl (name ctx spec fpcore fl)
        #:methods gen:custom-write
        [(define (write-proc op port mode)
          (fprintf port "#<operator-impl:~a>" (operator-impl-name op)))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Literal": numerical constant with a representation
;;
;; Literals are defined by
;;  - number : (or rational? infinite? nan?)
;;  - representation name : any/c

(struct literal (value precision) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Approx": approximation of a mathematical expression by
;; some arbitrary floating-point program
;;
;; Approx nodes are defined by
;;  - mathematical expression : expr?
;;  - floating-point program : expr?

(struct approx (spec impl) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Platform": set of supports operator implementations
;; and a cost model of floating-point programs.
;;
;; Platforms are defined by
;;  - (optional) name : symbol?
;;  - supported representations : hash? [symbol? -> representation?]
;;  - supported implementation names : hash? [symbol? -> operator-impl?]
;;  - cost of literals / variables : hash? [symbol? -> number?]
;;  - cost of implementations : hash? [symbol? -> number?]

(struct platform (name reprs impls impl-costs repr-costs)
  #:methods gen:custom-write
  [(define (write-proc p port mode)
     (if (platform-name p)
         (fprintf port "#<platform:~a>" (platform-name p))
         (fprintf port "#<platform>")))])

;; Current platform
(define *active-platform* (make-parameter #f))
