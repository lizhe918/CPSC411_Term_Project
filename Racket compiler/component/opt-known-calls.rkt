#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require compatibility/mlist)
(provide (all-defined-out))

; closure-lang-v9? -> closure-lang-v9?
; Optimizes calls to known closures.

(define (optimize-known-calls p)

  (define (process-p p binds)
    (match p
      [`(module ,e)
       `(module ,(process-e e binds))]))

  (define (process-pred p binds)
    (match p
      [`(true) `(true)]
      [`(false) `(false)]
      [`(not ,pred) `(not ,(process-pred pred binds))]
      [`(let ([,alocs ,es] ...) ,pred)
       `(let ,(for/list ([aloc alocs]
                         [e es])
                `(,aloc ,(process-e e binds)))
          ,(process-pred pred binds))]
      [`(if ,pred1 ,pred2 ,pred3)
       `(if ,(process-pred pred1 binds)
            ,(process-pred pred2 binds)
            ,(process-pred pred3 binds))]
      [e
       (process-e e binds)]))

  (define (process-e e binds)
    (match e
      [`(closure-ref ,e1 ,e2)
       `(closure-ref ,(process-e e1 binds)
                     ,(process-e e2 binds))]
      [`(closure-call ,e1 ,es ...)
       (if (false? (member e1 (dict-keys binds)))
           `(closure-call ,(process-e e1 binds)
                          ,@(for/list ([e es])
                              (process-e e binds)))
           `(call ,(dict-ref binds e1)
                  ,@(for/list ([e es])
                      (process-e e binds))))]
      [`(call ,e1 ,es)
       `(call (process-e e1 binds)
              ,@(for/list ([e es])
                  (process-e e binds)))]
      [`(letrec ([,labels ,lams] ...)
          (cletrec ([,alocs ,closures] ...)
                   ,e))
       (let ([nbinds (for/fold ([nbinds binds])
                               ([aloc alocs]
                                [closure closures])
                       (dict-set nbinds aloc (second closure)))])
         `(letrec ,(for/list ([label labels]
                              [lam lams])
                     `(,label ,(process-lam lam nbinds)))
            (cletrec ,(for/list ([aloc alocs]
                                 [closure closures])
                        `(,aloc ,(process-closure closure binds)))
                     ,(process-e e nbinds))))]
      [`(letrec ([,labels ,lams] ...) ,e)
       `(letrec ,(for/list ([label labels]
                            [lam lams])
                   `(,label ,(process-lam lam binds)))
          ,(process-e e binds))]
      [`(cletrec ([,alocs ,closures] ...) ,e)
       (let ([nbinds (for/fold ([nbinds binds])
                               ([aloc alocs]
                                [closure closures])
                       (dict-set nbinds aloc (second closure)))])
         `(cletrec ,(for/list ([aloc alocs]
                               [closure closures])
                      `(,aloc ,(process-closure closure binds)))
                   ,(process-e e nbinds)))]
      [`(let ([,alocs ,es] ...) ,e)
       `(let ,(for/list ([aloc alocs]
                         [e es])
                `(,aloc ,(process-e e binds)))
          ,(process-e e binds))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(process-pred pred binds)
            ,(process-e e1 binds)
            ,(process-e e2 binds))]
      [`(begin ,effects ... ,e)
       `(begin ,@(for/list ([effect effects])
                   (process-effect effect binds)) 
               ,(process-e e binds))]
      [`(,primop ,es ...)
       `(,primop ,@(for/list ([e es])
                    (process-e e binds)))]
      [v
       v]))

  (define (process-effect e binds)
    (match e
      [`(begin ,effects ... ,effect)
       `(begin ,@(for/list ([effect effects])
                   (process-effect effect binds))
               ,(process-effect effect binds))]
      [`(,primop ,es ...)
       `(,primop ,@(for/list ([e es])
                    (process-e e binds)))]))

  (define (process-lam lam binds)
    (match lam
      [`(lambda (,alocs ...) ,e)
       `(lambda ,alocs ,(process-e e binds))]))

  (define (process-closure c binds)
    (match c
      [`(make-closure ,label ,es ...)
       `(make-closure ,label ,@(for/list ([e es])
                                 (process-e e binds)))]))

  (process-p p empty))


;(require rackunit)
;(let ([x `(module
;              (letrec ((L.fun1.1.7
;                        (lambda (c.4 x.1 y.1 z.1)
;                          (let ((fun1.1 (closure-ref c.4 0)))
;                            (let ((tmp.5 (begin (unsafe-fx+ 1 2) fun1.1)))
;                              (closure-call tmp.5 tmp.5 y.1 z.1 x.1))))))
;                (cletrec ((fun1.1 (make-closure L.fun1.1.7 3 fun1.1))) 5)))])
;  (test-case "cannot optimize recursive calls inside"
;             (check-equal? (optimize-known-calls x)
;                           `(module
;                                (letrec ((L.fun1.1.7
;                                          (lambda (c.4 x.1 y.1 z.1)
;                                            (let ((fun1.1 (closure-ref c.4 0)))
;                                              (call L.fun1.1.7 fun1.1 y.1 z.1 x.1)))))
;                                  (cletrec ((fun1.1 (make-closure L.fun1.1.7 3 fun1.1))) 5))))))
