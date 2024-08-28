;; Restricted interface to Racket's `ffi/unsafe` library available
;; through Herbie's plugin interface.

#lang racket

(provide ffi/proc
         ffi/datum
         _double
         _float
         _pointer
         _int
         _int32
         _int64
         _uint32
         _uint64)

;; contain ffi namespace in a submodule
(module hairy racket
  (require ffi/unsafe)

  (provide ffi/proc*
           ffi/datum*
           _double
           _float
           _pointer
           _int
           _int32
           _int64
           _uint32
           _uint64)

  (define-syntax ffi/proc*
    (syntax-rules (:)
      [(_ name libspec : itype ... otype #:fail fail-thunk)
       (get-ffi-obj 'name libspec (_fun itype ... -> otype) fail-thunk)]))

  (define-syntax ffi/datum*
    (syntax-rules (:)
      [(_ name libspec : type #:fail fail-thunk) (get-ffi-obj 'name libspec type fail-thunk)])))

(require (submod "." hairy))

(define-syntax ffi/proc
  (syntax-rules (: ->)
    [(_ name libspec : itype ... -> otype #:fail fail-thunk)
     (ffi/proc* name libspec : itype ... otype #:fail fail-thunk)]
    [(_ name libspec : itype ... -> otype) (ffi/proc* name libspec : itype ... otype #:fail #f)]))

(define-syntax ffi/datum
  (syntax-rules (:)
    [(_ name libspec : type #:fail fail-thunk) (ffi/proc* name libspec : type #:fail fail-thunk)]
    [(_ name libspec : type) (ffi/proc* name libspec : type #:fail #f)]))
