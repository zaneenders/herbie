#lang racket

(require math/bigfloat)

(require (submod "../syntax/syntax.rkt" internals)
         (submod "../syntax/types.rkt" internals)
         "float32.rkt"
         "utils.rkt")

(provide binary32)

(define-representation binary32
                       (binary32 real float32?)
                       bigfloat->float32
                       bf
                       (shift 31 ordinal->float32)
                       (unshift 31 float32->ordinal)
                       32
                       (conjoin number? nan?))
