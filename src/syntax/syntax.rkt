#lang racket

(require math/bigfloat)

(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "../core/rival.rkt"
         "base.rkt"
         "matcher.rkt"
         "types.rkt")

(provide (struct-out literal)
         (struct-out approx)
         variable?
         constant-operator?
         operator-exists?
         operator-deprecated?
         operator-info
         all-operators
         all-constants
         impl-exists?
         impl-info
         all-operator-impls
         *functions*
         register-function!
         get-fpcore-impl
         get-cast-impl
         generate-cast-impl
         cast-impl?)

(module+ internals
  (provide define-operator
           register-operator!
           define-operator-impl
           make-operator-impl
           register-conversion-generator!
           variable?))

(module+ test
  (require rackunit
           rival))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Real operators
;; Pure mathematical operations

;; Checks if an operator has been registered.
(define (operator-exists? op)
  (hash-has-key? operators op))

;; Checks if an operator has been registered as deprecated.
(define (operator-deprecated? op)
  (operator-deprecated (hash-ref operators op)))

;; Returns all operators.
(define (all-operators)
  (sort (hash-keys operators) symbol<?))

;; Returns all constant operators (operators with no arguments).
(define (all-constants)
  (sort (for/list ([(name rec) (in-hash operators)]
                   #:when (null? (operator-itype rec)))
          name)
        symbol<?))

;; Looks up a property `field` of an real operator `op`.
;; Panics if the operator is not found.
(define/contract (operator-info op field)
  (-> symbol? (or/c 'itype 'otype) any/c)
  (unless (hash-has-key? operators op)
    (error 'operator-info "Unknown operator ~a" op))
  (define info (hash-ref operators op))
  (case field
    [(itype) (operator-itype info)]
    [(otype) (operator-otype info)]))

;; Registers an operator with an attribute mapping.
;; Panics if an operator with name `name` has already been registered.
;; By default, the input types are specified by `itypes`, the output type
;; is specified by `otype`, and the operator is not deprecated; but
;; `attrib-dict` can override these properties.
(define (register-operator! name itypes otype attrib-dict)
  (when (hash-has-key? operators name)
    (error 'register-operator! "operator already registered: ~a" name))
  ; extract relevant fields and update tables
  (define itypes* (dict-ref attrib-dict 'itype itypes))
  (define otype* (dict-ref attrib-dict 'otype otype))
  (define deprecated? (dict-ref attrib-dict 'deprecated #f))
  (define info (operator name itypes* otype* deprecated?))
  (hash-set! operators name info))

;; Syntactic form for `register-operator!`
(define-syntax (define-operator stx)
  (define (bad! why [what #f])
    (raise-syntax-error 'define-operator why stx what))
  (syntax-case stx ()
    [(_ (id itype ...) otype [key val] ...)
     (let ([id #'id])
       (unless (identifier? id)
         (bad! "expected identifier" id))
       (with-syntax ([id id])
         #'(register-operator! 'id '(itype ...) 'otype (list (cons 'key val) ...))))]))

(define-syntax define-operators
  (syntax-rules (: ->)
    [(_ [name : itype ... -> otype] ...)
     (begin
       (define-operator (name itype ...) otype) ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rival-supported operators

; real constants (encoded as nullary operators)
(define-operators
  [PI : -> real]
  [E : -> real]
  [INFINITY : -> real]
  [NAN : -> real])

; boolean constants (encoded as nullary operators)
(define-operators
  [TRUE : -> bool]
  [FALSE : -> bool])

; boolean operators
(define-operators
  [not : bool -> bool]
  [and : bool bool -> bool]
  [or : bool bool -> bool])

; real-boolean operators
(define-operators
  [== : real real -> bool]
  [!= : real real -> bool]
  [< : real real -> bool]
  [> : real real -> bool]
  [<= : real real -> bool]
  [>= : real real -> bool])

; real operators
(define-operators
  [acos : real -> real]
  [acosh : real -> real]
  [asin : real -> real]
  [asinh : real -> real]
  [atan : real -> real]
  [atanh : real -> real]
  [cbrt : real -> real]
  [ceil : real -> real]
  [cos : real -> real]
  [cosh : real -> real]
  [erf : real -> real]
  [exp : real -> real]
  [exp2 : real -> real]
  [fabs : real -> real]
  [floor : real -> real]
  [lgamma : real -> real]
  [log : real -> real]
  [log10 : real -> real]
  [log2 : real -> real]
  [logb : real -> real]
  [neg : real -> real]
  [rint : real -> real]
  [round : real -> real]
  [sin : real -> real]
  [sinh : real -> real]
  [sqrt : real -> real]
  [tan : real -> real]
  [tanh : real -> real]
  [tgamma : real -> real]
  [trunc : real -> real]
  [+ : real real -> real]
  [- : real real -> real]
  [* : real real -> real]
  [/ : real real -> real]
  [atan2 : real real -> real]
  [copysign : real real -> real]
  [fdim : real real -> real]
  [fmax : real real -> real]
  [fmin : real real -> real]
  [fmod : real real -> real]
  [pow : real real -> real]
  [remainder : real real -> real])

(module+ test
  ; check expected number of operators
  (check-equal? (length (all-operators)) 57)

  ; check that Rival supports all non-accelerator operators
  (for ([op (in-list (all-operators))])
    (define vars (map (lambda (_) (gensym)) (operator-info op 'itype)))
    (define disc (discretization 64 #f #f)) ; fake arguments
    (rival-compile (list `(,op ,@vars)) vars (list disc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operator implementations
;; Floating-point operations that approximate mathematical operations

;; Checks if an implementation exists in the current platform.
(define (impl-exists? op)
  (define impls (platform-impls (*active-platform*)))
  (hash-has-key? impls op))

;; Looks up a property `field` of an operator implementation in the current platform.
;; Panics if the operator implementation is not found.
(define/contract (impl-info impl field)
  (-> symbol? (or/c 'vars 'itype 'otype 'spec 'fpcore 'fl) any/c)
  (define impls (platform-impls (*active-platform*)))
  (unless (hash-has-key? impls impl)
    (error 'impl-info "Unknown operator implementation ~a" impl))
  (define info (hash-ref impls impl))
  (case field
    [(vars) (context-vars (operator-impl-ctx info))]
    [(itype) (context-var-reprs (operator-impl-ctx info))]
    [(otype) (context-repr (operator-impl-ctx info))]
    [(spec) (operator-impl-spec info)]
    [(fpcore) (operator-impl-fpcore info)]
    [(fl) (operator-impl-fl info)]))

;; Returns all operator implementations in the current platform.
(define (all-operator-impls)
  (define impls (platform-impls (*active-platform*)))
  (sort (hash-keys impls) symbol<?))

;; Checks a specification.
(define (check-spec! name ctx spec)
  (define (bad! fmt . args)
    (error name "~a in `~a`" (apply format fmt args) spec))

  (define (type-error! expr actual-ty expect-ty)
    (bad! "expression `~a` has type `~a`, expected `~a`" expr actual-ty expect-ty))

  (match-define (context vars repr var-reprs) ctx)
  (define itypes (map representation-type var-reprs))
  (define otype (representation-type repr))

  (unless (= (length itypes) (length vars))
    (bad! "arity mismatch; expected ~a, got ~a" (length itypes) (length vars)))

  (define env (map cons vars itypes))
  (define actual-ty
    (let type-of ([expr spec])
      (match expr
        [(? number?) 'real]
        [(? symbol?)
         (cond
           [(assq expr env)
            =>
            cdr]
           [else (bad! "unbound variable `~a`" expr)])]
        [`(if ,cond ,ift ,iff)
         (define cond-ty (type-of cond))
         (unless (equal? cond-ty 'bool)
           (type-error! cond cond-ty 'bool))
         (define ift-ty (type-of ift))
         (define iff-ty (type-of iff))
         (unless (equal? ift-ty iff-ty)
           (type-error! iff iff-ty ift-ty))
         ift-ty]
        [`(,op ,args ...)
         (unless (operator-exists? op)
           (bad! "at `~a`, `~a` not an operator" expr op))
         (define itypes (operator-info op 'itype))
         (unless (= (length itypes) (length args))
           (bad! "arity mismatch at `~a`: expected `~a`, got `~a`"
                 expr
                 (length itypes)
                 (length args)))
         (for ([arg (in-list args)]
               [itype (in-list itypes)])
           (define arg-ty (type-of arg))
           (unless (equal? itype arg-ty)
             (type-error! arg arg-ty itype)))
         (operator-info op 'otype)]
        [_ (bad! "expected an expression, got `~a`" expr)])))

  (unless (equal? actual-ty otype)
    (type-error! spec actual-ty otype)))

;; Creates an operator implementation with a given name, type signature, and specification.
;; Optionally specify the floating-point implementation and FPCore translation.
(define/contract (make-operator-impl name ctx spec #:fl [fl-proc #f] #:fpcore [fpcore #f])
  (->* (symbol? context? any/c) (#:fl (or/c procedure? #f) #:fpcore any/c) operator-impl?)
  ; check specification
  (check-spec! name ctx spec)
  (define vars (context-vars ctx))
  ; synthesize operator (if the spec contains exactly one operator)
  (define op
    (match spec
      [(list op (or (? number?) (? symbol?)) ...) op]
      [_ #f]))
  ; check or synthesize FPCore translatin
  (define fpcore*
    (cond
      [fpcore ; provided -> TODO: check free variables, props
       (match fpcore
         [`(! ,props ... (,op ,args ...))
          (unless (even? (length props))
            (error 'register-operator-impl! "~a: umatched property in ~a" name fpcore))
          (unless (symbol? op)
            (error 'register-operator-impl! "~a: expected symbol `~a`" name op))
          (for ([arg (in-list args)])
            (unless (or (symbol? arg) (number? arg))
              (error 'register-operator-impl! "~a: expected terminal `~a`" name arg)))]
         [`(,op ,args ...)
          (unless (symbol? op)
            (error 'register-operator-impl! "~a: expected symbol `~a`" name op))
          (for ([arg (in-list args)])
            (unless (or (symbol? arg) (number? arg))
              (error 'register-operator-impl! "~a: expected terminal `~a`" name arg)))]
         [_ (error 'register-operator-impl! "Invalid fpcore for ~a: ~a" name fpcore)])
       fpcore]
      [else ; not provided => need to generate it
       (define repr (context-repr ctx))
       (if (eq? (representation-name repr) 'bool)
           `(,op ,@vars) ; special case: boolean-valued operations do not need a precision annotation
           `(! :precision ,(representation-name repr) (,op ,@vars)))]))
  ; check or synthesize floating-point operation
  (define fl-proc*
    (cond
      [fl-proc ; provided => check arity
       (unless (procedure-arity-includes? fl-proc (length vars) #t)
         (error 'register-operator-impl!
                "~a: procedure does not accept ~a arguments"
                name
                (length vars)))
       fl-proc]
      [else ; need to generate
       (define compiler (make-real-compiler (list spec) (list ctx)))
       (define fail ((representation-bf->repr (context-repr ctx)) +nan.bf))
       (procedure-rename (lambda pt
                           (define-values (_ exs) (real-apply compiler pt))
                           (if exs (first exs) fail))
                         name)]))
  ; create the implementation
  (operator-impl name ctx spec fpcore* fl-proc*))

;; Synactic form for defining a new operator implementation.
(define-syntax (define-operator-impl stx)
  (define (oops! why [sub-stx #f])
    (raise-syntax-error 'define-operator-impl why stx sub-stx))
  (syntax-case stx (:)
    [(_ (id [var : irepr] ...) orepr fields ...)
     (let ([id #'id]
           [vars (syntax->list #'(var ...))]
           [fields #'(fields ...)])
       (unless (identifier? id)
         (oops! "expected identifier" id))
       (for ([var (in-list vars)])
         (unless (identifier? var)
           (oops! "expected identifier" var)))
       (define spec #f)
       (define core #f)
       (define fl-expr #f)
       (define optional? #f)
       (let loop ([fields fields])
         (syntax-case fields ()
           [()
            (begin
              (unless spec
                (oops! "missing `#:spec` keyword"))
              (with-syntax ([id id]
                            [spec spec]
                            [core core]
                            [fl-expr fl-expr]
                            [optional? optional?])
                #'(define id
                    (let ([fl-proc fl-expr])
                      (and (implies optional? fl-proc)
                           (make-operator-impl 'id
                                               (context '(var ...) orepr (list irepr ...))
                                               'spec
                                               #:fl fl-proc
                                               #:fpcore 'core))))))]
           [(#:spec expr rest ...)
            (cond
              [spec (oops! "multiple #:spec clauses" stx)]
              [else
               (set! spec #'expr)
               (loop #'(rest ...))])]
           [(#:spec) (oops! "expected value after keyword `#:spec`" stx)]
           [(#:fpcore expr rest ...)
            (cond
              [core (oops! "multiple #:fpcore clauses" stx)]
              [else
               (set! core #'expr)
               (loop #'(rest ...))])]
           [(#:fpcore) (oops! "expected value after keyword `#:fpcore`" stx)]
           [(#:fl expr rest ...)
            (cond
              [fl-expr (oops! "multiple #:fl clauses" stx)]
              [else
               (set! fl-expr #'expr)
               (loop #'(rest ...))])]
           [(#:fl) (oops! "expected value after keyword `#:fl`" stx)]
           ; specify #:optional if the value associated with #:fl
           ; may be #f, in which case `id` is bound to #f
           [(#:optional rest ...)
            (let ([rest #'(rest ...)])
              (when optional?
                (oops! "multiple #:optional clauses" stx))
              (set! optional? #t)
              (loop rest))]
           ; bad
           [_ (oops! "bad syntax" fields)])))]
    [_ (oops! "bad syntax")]))

;; Extracts the `fpcore` field of an operator implementation
;; as a property dictionary and expression.
(define (impl->fpcore impl)
  (match (impl-info impl 'fpcore)
    [(list '! props ... body) (values (props->dict props) body)]
    [body (values '() body)]))

;; For a given FPCore operator, rounding context, and input representations,
;; finds the best operator implementation in the current platform.
;; Panics if none can be found.
(define/contract (get-fpcore-impl op prop-dict ireprs)
  (-> symbol? prop-dict/c (listof representation?) symbol?)
  (define all-impls (platform-impls (*active-platform*)))
  ; gather all implementations that have the same spec, input representations,
  ; and its FPCore translation has properties that are found in `prop-dict`
  (define impls
    (reap [sow]
          (for ([(name info) (in-hash all-impls)])
            (define ctx (operator-impl-ctx info))
            (when (equal? ireprs (context-var-reprs ctx))
              (define-values (prop-dict* expr) (impl->fpcore name))
              (define pattern (cons op (map (lambda (_) (gensym)) ireprs)))
              (when (and (andmap (lambda (prop) (member prop prop-dict)) prop-dict*)
                         (pattern-match pattern expr))
                (sow name))))))
  ; check that we have any matching impls
  (when (null? impls)
    (raise-herbie-missing-error
     "No implementation for `~a` under rounding context `~a` with types `~a`"
     op
     prop-dict
     (string-join (map (λ (r) (format "<~a>" (representation-name r))) ireprs) " ")))
  ; ; we rank implementations and select the highest scoring one
  (define scores
    (for/list ([impl (in-list impls)])
      (define-values (prop-dict* _) (impl->fpcore impl))
      (define num-matching (count (lambda (prop) (member prop prop-dict*)) prop-dict))
      (cons num-matching (- (length prop-dict) num-matching))))
  ; select the best implementation
  ; sort first by the number of matched properties,
  ; then tie break on the number of extraneous properties
  (match-define (list (cons _ best) _ ...)
    (sort (map cons scores impls)
          (lambda (x y)
            (cond
              [(> (car x) (car y)) #t]
              [(< (car x) (car y)) #f]
              [else (> (cdr x) (cdr y))]))
          #:key car))
  best)

;; Casts and precision changes

(define (cast-impl? x)
  (and (symbol? x)
       (impl-exists? x)
       (match (impl-info x 'vars)
         [(list v)
          #:when (eq? (impl-info x 'spec) v)
          #t]
         [_ #f])))

(define (get-cast-impl irepr orepr)
  (get-fpcore-impl 'cast (repr->prop orepr) (list irepr)))

; Similar to representation generators, conversion generators
; allow Herbie to query plugins for optimized implementations
; of representation conversions, rather than the default
; bigfloat implementation
(define conversion-generators '())

(define/contract (register-conversion-generator! proc)
  (-> (-> any/c any/c boolean?) void?)
  (unless (set-member? conversion-generators proc)
    (set! conversion-generators (cons proc conversion-generators))))

(define (generate-cast-impl irepr orepr)
  (match (get-cast-impl irepr orepr)
    [#f
     (for/first ([gen (in-list conversion-generators)])
       (gen (representation-name irepr) (representation-name orepr)))]
    [impl impl]))

;; Expression predicates ;;

(define (constant-operator? op)
  (and (symbol? op)
       (or (and (hash-has-key? operators op) (null? (operator-itype (hash-ref operators op)))))))

(define (variable? var)
  (and (symbol? var)
       (or (not (hash-has-key? operators var))
           (not (null? (operator-itype (hash-ref operators var)))))
       (or (not (impl-exists? var)) (not (null? (impl-info var 'vars))))))

;; name -> (vars repr body)	;; name -> (vars prec body)
(define *functions* (make-parameter (make-hasheq)))

(define (register-function! name args repr body) ;; Adds a function definition.
  (hash-set! (*functions*) name (list args repr body)))
