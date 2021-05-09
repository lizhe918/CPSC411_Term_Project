#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require compatibility/mlist)
(require "../utils/utils.rkt")
(provide (all-defined-out))

; hoisted-lang-v9 -> proc-exposed-lang-v9
; Implement closures in terms of the procedure data structure
(define (implement-closures p)
  (debug-log p "impl-closure")

  (define (process-p p)
    (match p
      [`(module ,bs ... ,e)
       `(module ,@(for/list ([b bs])
                    (process-b b))
          ,(process-e e))]))

  (define (process-b b)
    (match b
      [`(define ,label (lambda (,alocs ...) ,e))
       `(define ,label (lambda ,alocs ,(process-e e)))]))

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
      [e
       (process-e e)]))

  (define (process-e e)
    (match e
      [`(closure-ref ,e1 ,e2)
       `(unsafe-procedure-ref ,(process-e e1) ,(process-e e2))]
      [`(closure-call ,e1 ,es ...)
       `(call (unsafe-procedure-label ,(process-e e1))
              ,@(for/list ([e es])
                  (process-e e)))]
      [`(call ,e1 ,es ...)
       `(call ,(process-e e1)
              ,@(for/list ([e es])
                  (process-e e)))]
      [`(cletrec ([,alocs ,closures] ...) ,e)
       `(let ,(for/list ([aloc alocs]
                         [closure closures])
                `(,aloc ,(process-closure closure)))
          ,(let ([is (for/fold ([acc empty])
                               ([aloc alocs]
                                [closure closures])
                       (build-env aloc closure acc))])
             (if (empty? is)
                 (process-e e)
                 `(begin ,@is ,(process-e e)))))]
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
       `(,primop ,@(for/list ([e es])
                     (process-e e)))]
      [v
       v]))

  (define (process-effect e)
    (match e
      [`(begin ,effects ... ,effect)
       `(begin ,@(for/list ([effect effects])
                   (process-effect effect))
               ,(process-effect effect))]
      [`(,primop ,es ...)
       `(,primop ,@(for/list ([e es])
                     (process-e e)))]))

  (define (process-closure c)
    (match c
      [`(make-closure ,label ,arity ,es ...)
       `(make-procedure ,label ,arity ,(length es))]))

  (define (build-env aloc c acc)
    (match c
      [`(make-closure ,label ,arity ,es ...)
       (append acc
               (let-values ([(count is)
                             (for/fold ([count 0]
                                        [is empty])
                                       ([e es])
                               (values (add1 count)
                                       (cons `(unsafe-procedure-set! ,aloc
                                                                     ,count
                                                                     ,(process-e e))
                                             is)))])
                 (reverse is)))]))
                            
                

  (process-p p))


