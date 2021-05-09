#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require compatibility/mlist)
(require "../utils/utils.rkt")
(provide (all-defined-out))

; closure-lang-v9? -> hoisted-lang-v9?
; Hoists code to the top-level definitions.
(define (hoist-lambdas p)
  (debug-log p "hoist-lmb")

  (define-struct acc (defs codes))

  (define (primop? op)
    (ormap (λ (o) (eq? o op)) 
           `(unsafe-fx* 
             unsafe-fx+ 
             unsafe-fx- 
             eq? 
             unsafe-fx< 
             unsafe-fx<=
             unsafe-fx>
             unsafe-fx>=
             fixnum?
             boolean?
             empty?
             void?
             ascii-char?
             error?
             not
             pair?
             vector?
             procedure?
             cons
             unsafe-car
             unsafe-cdr
             unsafe-make-vector
             unsafe-vector-length
             unsafe-vector-set!
             unsafe-vector-ref
             unsafe-procedure-arity)))

  (define (process-p p)
    (match p
      [`(module ,e)
       `(module ,@(build-defs-e e empty)
          ,(process-e e))]))

  (define (build-defs-pred p defs)
    (match p
      [`(true) defs]
      [`(false) defs]
      [`(not ,pred) (build-defs-pred pred defs)]
      [`(let ([,alocs ,es] ...) ,pred)
       (build-defs-pred pred
                        (for/fold ([defs defs])
                                  ([e es])
                          (build-defs-e e defs)))]
      [`(if ,pred1 ,pred2 ,pred3)
       (build-defs-pred pred3
                        (build-defs-pred pred2
                                         (build-defs-pred pred1 defs)))]
      [_ (build-defs-e p defs)]))
 
  (define (build-defs-e e defs)
    (match e
      [`(closure-ref ,e1 ,e2)
       (build-defs-e e2
                     (build-defs-e e1 defs))]
      [`(closure-call ,e1 ,es ...)
       (let ([defs-e1 (build-defs-e e1 defs)])
         (for/fold ([defs defs-e1])
                   ([e es])
           (build-defs-e e defs)))]
      [`(call ,e1 ,es ...)
       (let ([defs-e1 (build-defs-e e1 defs)])
         (for/fold ([defs defs-e1])
                   ([e es])
           (build-defs-e e defs)))]
      [`(letrec ([,labels ,lams] ...) ,e)
       (let ([lam-defs (for/fold ([defs defs])
                                 ([lam lams])
                         (build-defs-lam lam defs))])
         (let ([ndefs (for/fold ([defs lam-defs])
                                ([label labels]
                                 [lam lams])
                        (cons `(define ,label ,lam) defs))])
           (build-defs-e e ndefs)))]
      [`(cletrec ([,labels ,closures] ...) ,e)
       (let ([clo-defs (for/fold ([defs defs])
                                 ([closure closures])
                         (build-defs-closure closure defs))])
         (build-defs-e e clo-defs))]
      [`(let ([,alocs ,es] ...) ,e)
       (build-defs-e e
                     (for/fold ([defs defs])
                               ([e es])
                       (build-defs-e e defs)))]
      [`(if ,pred ,e1 ,e2)
       (build-defs-e e2
                     (build-defs-e e1
                                   (build-defs-e pred defs)))]
      [`(begin ,effects ... ,e)
       (build-defs-e e
                     (for/fold ([defs defs])
                               ([effect effects])
                       (build-defs-effect effect defs)))]
      [`(,primop ,es ...)
       #:when (primop? primop)
       (for/fold ([defs defs])
                 ([e es])
         (build-defs-e e defs))]
      [v defs]))

  (define (build-defs-effect e defs)
    (match e
      [`(begin ,effects ... ,effect)
       (build-defs-effect effect
                          (for/fold ([defs defs])
                                    ([effect effects])
                            (build-defs-effect effect defs)))]
      [`(,primop ,es ...)
       #:when (primop? primop)
       (for/fold ([defs defs])
                 ([e es])
         (build-defs-e e defs))]))

  (define (build-defs-lam lam defs)
    (match lam
      [`(lambda (,alocs ...) ,e)
       (build-defs-e e defs)]))

  (define (build-defs-closure c defs)
    (match c
      [`(make-closure ,label ,es ...)
       (for/fold ([defs defs])
                 ([e es])
         (build-defs-e e defs))]))

  (define (process-pred p)
    (match p
      [`(true) `(true)]
      [`(false) `(false)]
      [`(not ,pred) `(not ,(process-pred pred))]
      [`(let ([,alocs ,es] ...) ,pred)
       `(let ,(for/list ([aloc alocs]
                         [e es])
                `(,aloc ,(process-e e)))
          ,(process-pred pred))]
      [`(if ,pred1 ,pred2 ,pred3)
       `(if ,(process-pred pred1)
            ,(process-pred pred2)
            ,(process-pred pred3))]
      [_ (process-e p)]))

  (define (process-e e)
    (match e
      [`(closure-ref ,e1 ,e2)
       `(closure-ref ,(process-e e1)
                     ,(process-e e2))]
      [`(closure-call ,e ,es ...)
       `(closure-call ,(process-e e)
                     ,@(for/list ([e es])
                         (process-e e)))]
      [`(call ,e ,es ...)
       `(call ,(process-e e)
              ,@(for/list ([e es])
                  (process-e e)))]
      [`(letrec ([,labels ,lams] ...) ,e)
       (process-e e)]
      [`(cletrec ([,alocs ,closures] ...) ,e)
       `(cletrec ,(for/list ([aloc alocs]
                             [closure closures])
                    `(,aloc ,(process-closure closure)))
                 ,(process-e e))]
      [`(let ([,alocs ,es] ...) ,e)
       `(let ,(for/list ([aloc alocs]
                         [e es])
                `(,aloc ,(process-e e)))
          ,(process-e e))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(process-pred pred)
            ,(process-e e1)
            ,(process-e e2))]
      [`(begin ,effects ... ,e)
       `(begin ,@(for/list ([effect effects])
                   (process-effect effect))
               ,(process-e e))]
      [`(,primop ,es ...)
       #:when (primop? primop)
       `(,primop ,@(for/list ([e es])
                     (process-e e)))]
      [v v]))

  (define (process-effect e)
    (match e
      [`(begin ,effects ... ,effect)
       `(begin ,@(for/list ([effect effects])
                   (process-effect effect))
               ,(process-effect effect))]
      [`(,primop ,es ...)
       #:when (primop? primop)
       `(,primop ,@(for/list ([e es])
                     (process-e e)))]))

  (define (process-closure c)
    (match c
      [`(make-closure ,label ,es ...)
       `(make-closure ,label ,@(for/list ([e es])
                                 (process-e e)))]))

  (process-p p))




















                
