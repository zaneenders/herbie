#lang racket

(require racket/hash)

(require "../utils/errors.rkt"
         "../core/programs.rkt"
         "base.rkt"
         "syntax.rkt"
         "types.rkt")

(provide define-platform
         get-platform
         *active-platform*
         ;; Platform API
         ;; Operator sets
         (contract-out ;; Platforms
          [platform? (-> any/c boolean?)]
          [platform-union (-> platform? platform? ... platform?)]
          [platform-intersect (-> platform? platform? ... platform?)]
          [platform-subtract (-> platform? platform? ... platform?)]
          ; Cost model
          [platform-impl-cost (-> platform? any/c any/c)]
          [platform-repr-cost (-> platform? any/c any/c)]
          [platform-node-cost-proc (-> platform? procedure?)]
          [platform-cost-proc (-> platform? procedure?)]))

(module+ internals
  (provide define-platform
           get-platform
           register-platform!
           platform-union
           platform-intersect
           platform-subtract
           platform-filter))

;;; Platforms describe a set of representations and operator implementations
;;; Herbie should use during its improvement loop. Platforms are just
;;; a "type signature" - they provide no implementations of floating-point
;;; operations (see plugins). During runtime, platforms will verify if
;;; every listed feature is actually loaded by Herbie and will panic if
;;; implemenations are missing. Unlike plugins, only one platform may be
;;; active at any given time and platforms may be activated or deactivated.

;; Platform table, mapping name to platform
(define platforms (make-hash))

;; Looks up a platform by identifier.
;; Panics if no platform is found.
(define (get-platform name)
  (or (hash-ref platforms name #f)
      (raise-herbie-error "unknown platform `~a`, found (~a)"
                          name
                          (string-join (map ~a (hash-keys platforms)) ", "))))

;; Registers a platform under identifier `name`.
(define (register-platform! name pform)
  (when (hash-has-key? platforms name)
    (error 'register-platform! "platform already registered ~a" name))
  (hash-set! platforms name (struct-copy platform pform [name name])))

;; Constructs a platform.
;; A platform is just a set of implementations and a cost model.
(define (make-platform impl&costs repr&costs #:if-cost [if-cost #f] #:default-cost [default-cost #f])
  ; tables
  (define impls (make-hasheq)) ; name -> impl
  (define impl->cost (make-hasheq)) ; name -> cost
  (define reprs (make-hash)) ; name -> repr
  (define repr->cost (make-hasheq)) ; repr -> cost
  ; process implementations
  (for ([(impl cost) (in-dict impl&costs)])
    (define name (operator-impl-name impl))
    (define cost* (or cost default-cost))
    (when (hash-has-key? impls name)
      (error 'make-platform "duplicate implementation ~a" name))
    (hash-set! impls name impl)
    (hash-set! impl->cost name cost*)
    (define ctx (operator-impl-ctx impl))
    (for ([repr (in-list (cons (context-repr ctx) (context-var-reprs ctx)))])
      (define name (representation-name repr))
      (unless (hash-has-key? reprs name)
        (hash-set! reprs name repr))))
  ; process representations
  (for ([(repr cost) (in-dict repr&costs)])
    (when (hash-has-key? repr->cost repr)
      (error 'make-platform "duplicate representation ~a" repr))
    (hash-set! repr->cost repr cost))
  ; optionally set cost of `if`
  (when if-cost
    (hash-set! impl->cost 'if if-cost))
  ; construct the platform
  (platform #f
            (make-immutable-hash (hash->list reprs))
            (make-immutable-hasheq (hash->list impls))
            impl->cost
            repr->cost))

;; Macro version of `make-platform`
;;
;; Example usage:
;; ```
;; (define-platform default
;;   (platform
;;     #:literal [binary64 64]    ; literal representation with cost
;;     #:default-cost 1           ; default cost per impl
;;     #:if-cost 1                ; cost of an if branch (using max strategy)
;;     [fabs.f64 3]               ; implementation with explicit cost
;;     neg.f64                    ; implementation with default cost
;;     ...
;; ))
;; ```
(define-syntax (define-platform stx)
  (define (oops! why [sub-stx #f])
    (raise-syntax-error 'platform why stx sub-stx))

  (define (platform/parse-if-cost stx)
    (syntax-case stx (max sum)
      [(max x) #'(list 'max x)]
      [(sum x) #'(list 'sum x)]
      [x #'(list 'max x)]))

  (syntax-case stx ()
    [(_ id cs ...)
     (let ([if-cost #f]
           [default-cost #f]
           [optional? #f])
       (let loop ([cs #'(cs ...)]
                  [impls '()]
                  [costs '()]
                  [reprs '()]
                  [repr-costs '()])
         (syntax-case cs ()
           [()
            (let ([platform-id #'id])
              (unless (identifier? platform-id)
                (oops! "platform id is not a valid identifier" platform-id))
              (with-syntax ([platform-id platform-id]
                            [(impls ...) (reverse impls)]
                            [(costs ...) (reverse costs)]
                            [(reprs ...) reprs]
                            [(repr-costs ...) repr-costs]
                            [if-cost if-cost]
                            [default-cost default-cost]
                            [optional? optional?])
                #'
                (begin
                  (for ([name (in-list '(impls ...))]
                        [impl (in-list (list impls ...))])
                    (unless impl
                      (if optional?
                          (raise-herbie-missing-error "Missing implementation ~a required by platform"
                                                      impl)
                          (warn 'platform "platform has missing optional implementations: ~a" name))))
                  (define platform-id
                    (make-platform (list (cons impls costs) ...)
                                   (list (cons reprs repr-costs) ...)
                                   ;    #:optional? optional?
                                   #:if-cost if-cost
                                   #:default-cost default-cost)))))]
           [(#:if-cost cost rest ...)
            (cond
              [if-cost (oops! "multiple #:if-cost clauses" stx)]
              [else
               (set! if-cost (platform/parse-if-cost #'cost))
               (loop #'(rest ...) impls costs reprs repr-costs)])]
           [(#:if-cost) (oops! "expected value after keyword `#:if-cost`" stx)]
           [(#:default-cost cost rest ...)
            (cond
              [if-cost (oops! "multiple #:default-cost clauses" stx)]
              [else
               (set! default-cost #'cost)
               (loop #'(rest ...) impls costs reprs repr-costs)])]
           [(#:default-cost) (oops! "expected value after keyword `#:default-cost`" stx)]
           [(#:optional rest ...)
            (cond
              [optional? (oops! "multiple #:optional clauses" stx)]
              [else
               (set! optional? #t)
               (loop #'(rest ...) impls costs reprs repr-costs)])]
           [(#:literal [repr cost] rest ...)
            (loop #'(rest ...) impls costs (cons #'repr reprs) (cons #'cost repr-costs))]
           [(#:literals) (oops! "expected literals list after keyword `#:literals`" stx)]
           [([impl cost] rest ...)
            (loop #'(rest ...) (cons #'impl impls) (cons #'cost costs) reprs repr-costs)]
           [(impl rest ...) (loop #'(rest ...) (cons #'impl impls) (cons #f costs) reprs repr-costs)]
           [_ (oops! "bad syntax")])))]
    [_ (oops! "bad syntax")]))

;; Merger for costs.
(define (merge-cost pform-costs key #:optional? [optional? #f])
  (define costs (map (lambda (h) (hash-ref h key #f)) pform-costs))
  (match-define (list c0 cs ...) costs)
  (define cost
    (for/fold ([c0 c0]) ([c1 (in-list cs)])
      (match* (c0 c1)
        [(#f _) c1]
        [(_ #f) c0]
        [(c c) c]
        [(_ _) (error 'merge-costs "mismatch when combining cost model ~a ~a" key costs)])))
  (unless (or cost optional?)
    (error 'merge-costs "cannot find cost for implementation ~a" key))
  cost)

;; Set operations on platforms.
(define ((make-set-operation merge-impls) p1 . ps)
  ; apply set operation on impls
  (define impls (apply merge-impls (map platform-impls (cons p1 ps))))
  ; valid representations are based on impls
  (define reprs (make-hash))
  (for ([(_ info) (in-hash impls)])
    (define ctx (operator-impl-ctx info))
    (for ([repr (in-list (cons (context-repr ctx) (context-var-reprs ctx)))])
      (hash-set! reprs (representation-name repr) repr)))
  ; impl costs are based on impls
  (define pform-impl-costs (map platform-impl-costs (cons p1 ps)))
  (define impl-costs
    (for/hash ([(impl _) (in-hash impls)])
      (values impl (merge-cost pform-impl-costs impl))))
  ; special case for `if`
  (define if-cost (merge-cost pform-impl-costs 'if #:optional? #t))
  (when if-cost
    (set! impl-costs (hash-set impl-costs 'if if-cost)))
  ; repr costs are based on reprs (may be missing)
  (define pform-repr-costs (map platform-repr-costs (cons p1 ps)))
  (define repr-costs (hash))
  (for/list ([(_ repr) (in-hash reprs)])
    (define repr-cost (merge-cost pform-repr-costs repr #:optional? #t))
    (when repr-cost
      (set! repr-costs (hash-set repr-costs repr repr-cost))))
  (platform #f (make-immutable-hash (hash->list reprs)) impls impl-costs repr-costs))

;; Set union for platforms.
(define platform-union
  (make-set-operation (λ (impls . implss)
                        (apply hash-union
                               impls
                               implss
                               #:combine
                               (lambda (impl1 impl2)
                                 (unless (eq? impl1 impl2)
                                   (error 'platform-union
                                          "distinct implementations have the same name ~a ~a"
                                          impl1
                                          impl2)))))))

;; Set intersection for platforms.
(define platform-intersect
  (make-set-operation (λ (impls . implss)
                        (apply hash-intersect
                               impls
                               implss
                               #:combine
                               (lambda (impl1 impl2)
                                 (unless (eq? impl1 impl2)
                                   (error 'platform-intersect
                                          "distinct implementations have the same name ~a ~a"
                                          impl1
                                          impl2)))))))

;; Set subtract for platforms.
(define platform-subtract
  (make-set-operation (λ (impls . implss)
                        (hash-filter-keys impls
                                          (lambda (name)
                                            (not (for/or ([impls (in-list implss)])
                                                   (hash-has-key? impls name))))))))

;; Coarse-grained filters on platforms.
(define ((make-platform-filter repr-supported? op-supported?) pform)
  (define reprs* (filter repr-supported? (platform-reprs pform)))
  (define impls*
    (filter (λ (impl)
              (define spec (impl-info impl 'spec))
              (and (andmap op-supported? (ops-in-expr spec))
                   (repr-supported? (impl-info impl 'otype))
                   (andmap repr-supported? (impl-info impl 'itype))))
            (platform-impls pform)))
  (platform #f reprs* impls*))

;; Macro version of `make-platform-filter`.
(define-syntax (platform-filter stx)
  (define (oops! why [sub-stx #f])
    (raise-syntax-error 'platform why stx sub-stx))
  (syntax-case stx ()
    [(_ cs ... pform)
     (let loop ([clauses (syntax->list #'(cs ...))]
                [repr-filter #f]
                [op-filter #f])
       (syntax-case clauses ()
         [()
          (with-syntax ([repr-filter repr-filter]
                        [op-filter op-filter])
            #'((make-platform-filter (or repr-filter (const #t)) (or op-filter (const #t))) pform))]
         [(#:representations [reprs ...] rest ...)
          (begin
            (when repr-filter
              (oops! "cannot set both #:representations and #:not-representations"))
            (loop #'(rest ...)
                  #'(lambda (r)
                      (define rs (map get-representation '(reprs ...)))
                      (set-member? (list->set rs) r))
                  op-filter))]
         [(#:not-representations [reprs ...] rest ...)
          (begin
            (when repr-filter
              (oops! "cannot set both #:representations and #:not-representations"))
            (loop #'(rest ...)
                  #'(lambda (r)
                      (define rs (map get-representation '(reprs ...)))
                      (not (set-member? (list->set rs) r)))
                  op-filter))]
         [(#:operators [ops ...] rest ...)
          (begin
            (when op-filter
              (oops! "cannot set both #:operators and #:not-operators"))
            (loop #'(rest ...)
                  repr-filter
                  #'(lambda (r)
                      (define ops* '(ops ...))
                      (set-member? (list->set ops*) r))))]
         [(#:not-operators [ops ...] rest ...)
          (begin
            (when op-filter
              (oops! "cannot set both #:operators and #:not-operators"))
            (loop #'(rest ...)
                  repr-filter
                  #'(lambda (r)
                      (define ops* '(ops ...))
                      (not (set-member? (list->set '(ops ...)) r)))))]
         [_ (oops! "bad syntax")]))]
    [_ (oops! "bad syntax" stx)]))

; Implementation cost in a platform.
(define (platform-impl-cost pform impl)
  (hash-ref (platform-impl-costs pform)
            impl
            (lambda () (error 'platform-impl-cost "no cost for impl '~a" impl))))

; Representation (terminal) cost in a platform.
(define (platform-repr-cost pform repr)
  (hash-ref (platform-repr-costs pform)
            repr
            (lambda () (error 'platform-repr-cost "no cost for repr ~a" repr))))

; Cost model of a single node by a platform.
; Returns a procedure that must be called with the costs of the children.
(define (platform-node-cost-proc pform)
  (λ (expr repr)
    (match expr
      [(? literal?) (lambda () (platform-repr-cost pform repr))]
      [(? symbol?) (lambda () (platform-repr-cost pform repr))]
      [(list 'if _ _ _)
       (define if-cost (platform-impl-cost pform 'if))
       (lambda (cond-cost ift-cost iff-cost)
         (match if-cost
           [`(max ,n) (+ n cond-cost (max ift-cost iff-cost))]
           [`(sum ,n) (+ n cond-cost ift-cost iff-cost)]))]
      [(list impl args ...)
       (define impl-cost (platform-impl-cost pform impl))
       (lambda itype-costs
         (unless (= (length itype-costs) (length args))
           (error 'platform-node-cost-proc "arity mismatch, expected ~a arguments" (length args)))
         (apply + impl-cost itype-costs))])))

; Cost model parameterized by a platform.
(define (platform-cost-proc pform)
  (define bool-repr (get-representation 'bool))
  (define node-cost-proc (platform-node-cost-proc pform))
  (λ (expr repr)
    (let loop ([expr expr]
               [repr repr])
      (match expr
        [(? literal?) ((node-cost-proc expr repr))]
        [(? symbol?) ((node-cost-proc expr repr))]
        [(approx _ impl) (loop impl repr)]
        [(list 'if cond ift iff)
         (define cost-proc (node-cost-proc expr repr))
         (cost-proc (loop cond bool-repr) (loop ift repr) (loop iff repr))]
        [(list impl args ...)
         (define cost-proc (node-cost-proc expr repr))
         (define itypes (impl-info impl 'itype))
         (apply cost-proc (map loop args itypes))]))))
