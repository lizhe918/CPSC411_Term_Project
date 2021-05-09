#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require "../utils/utils.rkt")
(provide (all-defined-out))

(define (implement-safe-call p)
  (debug-log p "impl-safe-op")

  (define b-env `())
  (define arg-len-env `())

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
             
  (define (impl-assign a)
    (map (λ (a) `(,(first a) ,(impl-e (second a)))) a))

  (define (impl-v v)
    (match v
      [`(lambda (,aloc ...) ,e) 
       `(lambda (,@aloc) ,(impl-e e))]
      [_ v]))

  (define (impl-pred p)
    (match p
      [`(true) `(true)]
      [`(false) `(false)]
      [`(not ,pred) 
       `(not ,(impl-pred pred))]
      [`(let ,assign ,pred) 
       `(let ,(impl-assign assign) ,(impl-pred pred))]
      [`(if ,pred ,pred1 ,pred2)
       `(if ,(impl-pred pred) ,(impl-pred pred1) ,(impl-pred pred2))]
      [_ (impl-e p)]))

  (define (impl-effect e)
    (match e
      [`(begin ,effect ...)
       `(begin ,@(map impl-effect effect))]
      [`(,primop ,e ...)
       #:when (primop? primop)
       `(,primop ,@(map impl-e e))]))

  (define (impl-e e)
    (match e
      [`(call ,e ,es ...)
       #:when (member e b-env)
       (if (= (dict-ref arg-len-env e) (length es))
           `(unsafe-procedure-call ,(impl-e e) ,@(map impl-e es))
           `(error 42))]
      [`(call ,e ,es ...)
       (let ([new-e (impl-e e)]
             [new-es (map impl-e es)])
         (if (aloc? new-e)
             `(if (procedure? ,new-e)
                  (if (eq? (unsafe-procedure-arity ,new-e) ,(length es))
                      (unsafe-procedure-call ,new-e ,@new-es)
                      (error 42))
                  (error 43))
              (let ([tmp (fresh)])
                `(let ((,tmp ,new-e))
                   (if (procedure? ,tmp)
                       (if (eq? (unsafe-procedure-arity ,tmp) ,(length es))
                           (unsafe-procedure-call ,tmp ,@new-es)
                           (error 42))
                       (error 43))))))]
      [`(let ,assign ,e)
       `(let ,(impl-assign assign) ,(impl-e e))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(impl-pred pred) ,(impl-e e1) ,(impl-e e2))]
      [`(begin ,effect ... ,e)
       `(begin ,@(map impl-effect effect) ,(impl-e e))]
      [`(,primop ,e ...)
       #:when (primop? primop)
       `(,primop ,@(map impl-e e))]
      [_ (impl-v e)]))

  (define (impl-b b) 
    (match b
      [`(define ,b-aloc (lambda (,aloc ...) ,e))
       `(define ,b-aloc (lambda (,@aloc) ,(impl-e e)))]))

  (define (impl-b-proto b)
    (match b
      [`(define ,b-aloc (lambda (,aloc ...) ,e))
       (set! arg-len-env (dict-set arg-len-env b-aloc (length aloc)))]))

  (define (impl-p p)
    (match p
      [`(module ,b ... ,e)
       (set! b-env (map second b))
       (map impl-b-proto b)
       `(module ,@(map impl-b b) ,(impl-e e))]))

  (impl-p p))