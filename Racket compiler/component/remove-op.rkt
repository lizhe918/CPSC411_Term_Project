#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require "../utils/utils.rkt")
(provide (all-defined-out))


(define (remove-complex-opera* p)
  (debug-log p "remove-op")
  
  (define (triv? e) (or (int64? e) (aloc? e) (label? e)))

  (define (binop? op)
    (ormap (λ (o) o op)
           `('* '+ '- 'bitwise-and 'bitwise-ior 'bitwise-xor 'arithmetic-shift-right)))

  (define (relop? op)
    (ormap (λ (o) o op) `(< <= = >= > !=)))

  (define (remove-assign a)
    (map (λ (a) `(,(first a) ,(remove-val (second a)))) a))

  (define (remove-effect e)
    (match e
      [`(mset! ,value1 ... ,value2 ,value3 ... ,value4)
       #:when (and (not (triv? value2)) (andmap triv? value1))
       (let ([tmp (fresh)]
             [new-value4 (remove-val value4)])
         `(let ((,tmp ,(remove-val value2)))
            ,(remove-effect `(mset! ,@value1 ,tmp ,@value3 ,new-value4))))]
      [`(begin ,effect ...)
       `(begin ,@(map remove-effect effect))]
      [_ e]))

  (define (remove-val v)
    (match v
      [`(let ,assign ,value)
       `(let ,(remove-assign assign) ,(remove-val value))]
      [`(begin ,effect ... ,value)
       `(begin ,@(map remove-effect effect) ,(remove-val value))]
      [`(if ,pred ,value1 ,value2)
       `(if ,(remove-pred pred) ,(remove-val value1) ,(remove-val value2))]
      [`(call ,value1 ... ,value2 ,value3 ...)
       #:when (and (not (triv? value2)) (andmap triv? value1))
       (let ([tmp (fresh)])
         `(let ((,tmp ,(remove-val value2)))
            ,(remove-val `(call ,@value1 ,tmp ,@value3))))]
      [`(,binop ,value1 ... ,value2 ,value3 ...)
       #:when (and (binop? binop) (not (triv? value2)) (andmap triv? value1))
       (let ([tmp (fresh)])
         `(let ((,tmp ,(remove-val value2)))
            ,(remove-val `(,binop ,@value1 ,tmp ,@value3))))]
      [_ v]))

  (define (remove-pred p)
    (match p
      [`(not ,pred) `(not ,(remove-pred pred))]
      [`(let ,assign ,tail)
       `(let ,(remove-assign assign) ,(remove-tail tail))]
      [`(begin ,effect ... ,pred)
       `(begin ,@(map remove-effect effect) ,(remove-pred pred))]
      [`(if ,pred ,pred1 ,pred2)
       `(if ,(remove-pred pred) ,(remove-pred pred1) ,(remove-pred pred2))]
      [`(,relop ,value1 ... ,value2 ,value3 ...)
       #:when (and (relop? relop) (not (triv? value2)) (andmap triv? value3))
       (let ([tmp (fresh)])
         `(let ((,tmp ,(remove-val value2)))
            ,(remove-val `(,relop ,@value1 ,tmp ,@value3))))]
      [_ p]))

  (define (remove-tail t)
    (match t
      [`(let ,assign ,tail) 
       `(let ,(remove-assign assign) ,(remove-tail tail))]
      [`(begin ,effect ... ,tail)
       `(begin ,@(map remove-effect effect) ,(remove-tail tail))]
      [`(if ,pred ,tail1 ,tail2)
       `(if ,(remove-pred pred) ,(remove-tail tail1) ,(remove-tail tail2))]
      [_ (remove-val t)]))

  (define (remove-fun f)
    (match f
      [`(define ,label (lambda (,aloc ...) ,tail))
       `(define ,label (lambda (,@aloc) ,(remove-tail tail)))]))

  (define (remove-p p)
    (match p
      [`(module ,fun ... ,tail)
       `(module ,@(map remove-fun fun) ,(remove-tail tail))]))
  
  (remove-p p))