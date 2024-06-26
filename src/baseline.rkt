#lang racket

(require math/bigfloat rival)
(require (only-in math/private/bigfloat/mpfr mpfr-exp mpfr-sign))
;; Faster than bigfloat-exponent and avoids an expensive offset & contract check.
(require (only-in "syntax/syntax.rkt" operator-info)
         (only-in "common.rkt" *max-mpfr-prec*)
         "timeline.rkt")

(require "syntax/types.rkt"
         "config.rkt")

(provide make-search-func-baseline ival-eval-baseline)

(define (is-samplable-interval repr interval)
  (define <-bf (representation-bf->repr repr))
  (define (close-enough? lo hi)
    (let ([lo* (<-bf lo)] [hi* (<-bf hi)])
      (or (equal? lo* hi*) (and (number? lo*) (= lo* hi*)))))
  ((close-enough->ival close-enough?) interval))

(define (ival-eval-baseline fn ctxs pt [precision (*start-prec*)])
  (define start (current-inexact-milliseconds))
  (define <-bfs
    (for/list ([ctx (in-list ctxs)])
      (representation-bf->repr (context-repr ctx))))
  (define-values (status final-prec value)
    (let loop ([precision precision])
      (define exs
        (parameterize ([bf-precision precision]) (apply fn pt)))
      (match-define (ival err err?) (apply ival-or (map ival-error? exs)))
      (define precision* (* 2 precision))
      (cond
        [err
         (values err precision #f)]
        [(not err?)
         (values 'valid precision
                 (for/list ([ex exs] [<-bf <-bfs]) (<-bf (ival-lo ex))))]
        [(> precision* (*max-mpfr-prec*))
         (values 'exit precision #f)]
        [else
         (loop precision*)])))
  (define time (- (current-inexact-milliseconds) start))
  ;(timeline-push!/unsafe 'outcomes time final-prec (~a status) 1)
  (values status final-prec value time))

;; Returns a function that maps an ival to a list of ivals
;; The first element of that function's output tells you if the input is good
;; The other elements of that function's output tell you the output values
(define (make-search-func-baseline pre specs ctxs)
  (define fns (compile-specs-baseline (cons pre specs) (context-vars (car ctxs)) (context-repr (car ctxs))))
  ; inputs can either be intervals or representation values
  (define (compiled-spec . inputs)
    (define inputs*
      (for/list ([input (in-list inputs)]
                 [repr (context-var-reprs (car ctxs))])
        (if (ival? input) input (ival ((representation-repr->bf repr) input)))))
    (define outvec (apply fns inputs*))
    (define ival-pre (vector-ref outvec 0))
    (for/list ([y (in-vector outvec 1)] [ctx (in-list ctxs)])
      (define repr (context-repr ctx))
      (ival-then
       ; The two `invalid` ones have to go first, because later checks
       ; can error if the input is erroneous
       (ival-assert (ival-not (ival-error? y)) 'invalid)
       (ival-assert (ival-not (ival-error? ival-pre)) 'invalid)
       (ival-assert ival-pre 'precondition)
       ; 'infinte case handle in `ival-eval`
       (ival-assert
        (is-samplable-interval repr y)
        'unsamplable)
       y)))
  compiled-spec)

(define (make-progs-interpreter vars ivec rootvec repr)
  (define rootlen (vector-length rootvec))
  (define iveclen (vector-length ivec))
  (define varc (length vars))
  (define vreg-count (+ varc iveclen))
  (define vregs (make-vector vreg-count))
  (define prec-threshold (/ (*max-mpfr-prec*) 25))     ; parameter for sampling histogram table

  (define (compiled-spec . args)
    (define precision (bf-precision))
    (for ([arg (in-list args)] [n (in-naturals)])
      (vector-set! vregs n arg))
    (for ([instr (in-vector ivec)]
          [n (in-naturals varc)])
      (define timeline-stop!
        (timeline-start!/unsafe
         'mixsample-base (symbol->string (object-name (car instr))) precision))
      (parameterize ([bf-precision precision])
        (vector-set! vregs n (apply-instruction instr vregs)))
      (timeline-stop!))
    
    (for/vector #:length rootlen ([root (in-vector rootvec)])
      (vector-ref vregs root)))
  
  compiled-spec)

(define (apply-instruction instr regs)
  ;; By special-casing the 0-3 instruction case,
  ;; we avoid any allocation in the common case.
  ;; We could add more cases if we want wider instructions.
  ;; At some extreme, vector->values plus call-with-values
  ;; becomes the fastest option.
  (match instr
    [(list op) (op)]
    [(list op a)
     (op (vector-ref regs a))]
    [(list op a b)
     (op (vector-ref regs a)
         (vector-ref regs b))]
    [(list op a b c)
     (op (vector-ref regs a)
         (vector-ref regs b)
         (vector-ref regs c))]
    [(list op args ...)
     (apply op (map (curryr vector-ref regs) args))]))

(define (progs->batch exprs vars)
  (define icache (reverse vars))
  (define exprhash
    (make-hash
     (for/list ([var vars] [i (in-naturals)])
       (cons var i))))
  ; Counts
  (define size 0)
  (define exprc 0)
  (define varc (length vars))

  ; Translates programs into an instruction sequence of operations
  (define (munge prog)
    (set! size (+ 1 size))
    (define node ; This compiles to the register machine
      (match prog
        [(list op args ...)
         (cons op (map munge args))]
        [_
         prog]))
    (hash-ref! exprhash node
               (lambda ()
                 (begin0 (+ exprc varc) ; store in cache, update exprs, exprc
                   (set! exprc (+ 1 exprc))
                   (set! icache (cons node icache))))))

  (define roots (list->vector (map munge exprs)))
  (define nodes (list->vector (reverse icache)))

  (timeline-push! 'compiler (+ varc size) (+ exprc varc))
  (values nodes roots))

(define (make-compiler exprs vars repr)
  (define num-vars (length vars))
  (define-values (nodes roots)
    (progs->batch exprs vars))

  (define instructions
    (for/vector #:length (- (vector-length nodes) num-vars)
                ([node (in-vector nodes num-vars)])
      (match node
        [(? number?)
         (define x (real->ival node))
         (if (point-ival? x)
             (list (const x))
             (list (lambda () (real->ival node))))]
        [(list 'if c y f)
         (list ival-if c y f)]
        [(list op args ...)
         (cons (operator-info op 'ival) args)])))

  (make-progs-interpreter vars instructions roots repr))

(define (real->ival val)
  (define lo (parameterize ([bf-rounding-mode 'down]) (bf val)))
  (define hi (parameterize ([bf-rounding-mode 'up]) (bf val)))
  (ival lo hi))

(define (point-ival? x)
  (bf= (ival-lo x) (ival-hi x)))

(define (compile-specs-baseline specs vars repr)
  (make-compiler specs vars repr))

;; Like `compile-specs`, but for a single spec.
(define (compile-spec-baseline spec vars)
  (define core (compile-specs-baseline (list spec) vars))
  (define (compiled-spec . xs) (vector-ref (apply core xs) 0))
  compiled-spec)