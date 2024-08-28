#lang racket

(require math/bigfloat)

(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "base.rkt")

(provide type-name?
         (struct-out representation)
         get-representation
         all-representations
         repr-exists?
         repr->symbol
         repr->prop
         (struct-out context)
         *context*
         context-extend
         context-lookup)

(module+ internals
  (provide define-type
           define-representation
           make-representation
           rename-representation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

(define-type real)
(define-type bool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Representations interface

;; Representation name sanitizer
(define (repr->symbol repr)
  (define replace-table `((" " . "_") ("(" . "") (")" . "")))
  (define repr-name (representation-name repr))
  (string->symbol (string-replace* (~a repr-name) replace-table)))

;; Converts a representation into a rounding property
(define (repr->prop repr)
  (match (representation-type repr)
    ['bool '()]
    ['real (list (cons ':precision (representation-name repr)))]))

;; Creates a new representation.
(define/contract (make-representation name
                                      type
                                      repr?
                                      bf->repr
                                      repr->bf
                                      ordinal->repr
                                      repr->ordinal
                                      total-bits
                                      special-value?)
  (-> any/c
      type-name?
      (-> any/c boolean?)
      (-> bigfloat? any/c)
      (-> any/c bigfloat?)
      (-> natural? any/c)
      (-> any/c natural?)
      natural?
      (-> any/c boolean?)
      representation?)
  (representation name
                  type
                  repr?
                  bf->repr
                  repr->bf
                  ordinal->repr
                  repr->ordinal
                  total-bits
                  special-value?))

;; Copies a representation and associates a new name.
(define/contract (rename-representation repr name)
  (-> representation? any/c representation?)
  (struct-copy representation repr [name name]))

;; Syntactic form for defining a new representation
(define-syntax (define-representation stx)
  (define (oops! why [what #f])
    (raise-syntax-error 'define-representation why stx what))
  (syntax-case stx ()
    [(_ id (name type repr?) field ...)
     (let ([id #'id]
           [fields #'(field ...)])
       (unless (identifier? id)
         (oops! "expected identifier" id))
       (syntax-case fields ()
         [(bf->repr repr->bf ordinal->repr repr->ordinal total-bits special-value?)
          (with-syntax ([id id])
            #'(define id
                (make-representation 'name
                                     'type
                                     repr?
                                     bf->repr
                                     repr->bf
                                     ordinal->repr
                                     repr->ordinal
                                     total-bits
                                     special-value?)))]
         [_ (oops! "incorrect number of fields" fields)]))]
    [_ (oops! "bad syntax")]))

;; Checks if a representation exists in the current platform.
(define (repr-exists? name)
  (define reprs (platform-reprs (*active-platform*)))
  (hash-has-key? reprs name))

;; Looks up a representation in the current platform.
(define (get-representation name)
  (define reprs (platform-reprs (*active-platform*)))
  (or (hash-ref reprs name #f)
      (raise-herbie-error "Could not find support for ~a representation: ~a"
                          name
                          (string-join (map ~s (hash-keys reprs)) ", "))))

;; Returns all representations in the current platform.
(define (all-representations)
  (define reprs (platform-reprs (*active-platform*)))
  (hash-values reprs #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context interface

;; Functionally extends a context with a variable and its representation.
(define (context-extend ctx var repr)
  (struct-copy context
               ctx
               [vars (cons var (context-vars ctx))]
               [var-reprs (cons repr (context-var-reprs ctx))]))

;; Looks up the representation of a context.
(define (context-lookup ctx var)
  (dict-ref (map cons (context-vars ctx) (context-var-reprs ctx)) var))
