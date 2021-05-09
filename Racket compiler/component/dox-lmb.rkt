#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require "../utils/utils.rkt")
(provide (all-defined-out))

(define (dox-lambdas p)
  (debug-log p "dox-lmb")
  
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

  (define (dox-assign a)
    (map (λ (a) `(,(first a) ,(dox-e (second a)))) a))

  (define (dox-rec-assign a)
    (map (λ (a) 
           `(,(first a) ,(match (second a)
                           [`(lambda (,aloc ...) ,e)
                            `(lambda (,@aloc) ,(dox-e e))])))
         a))

  (define (dox-v v)
    (match v
      [`(lambda (,aloc ...) ,e)
       (let ([tmp (fresh 'lam)])
         `(letrec ((,tmp (lambda (,@aloc) ,(dox-e e))))
            ,tmp))]
      [_ v]))

  (define (dox-pred p)
    (match p
      [`(true) `(true)]
      [`(false) `(false)]
      [`(not ,pred) 
       `(not ,(dox-pred pred))]
      [`(let ,assign ,pred)
       `(let ,(dox-assign assign) ,(dox-pred pred))]
      [`(if ,pred ,pred1 ,pred2)
       `(if ,(dox-pred pred) ,(dox-pred pred1) ,(dox-pred pred2))]
      [_ (dox-e p)]))

  (define (dox-effect e)
    (match e
      [`(begin ,effect ...)
       `(begin ,@(map dox-effect effect))]
      [`(,primop ,e ...)
       #:when (primop? primop)
       `(,primop ,@(map dox-e e))]))
 
  (define (dox-e e)
    (match e
      [`(,primop ,e ...)
       #:when (primop? primop)
       `(,primop ,@(map dox-e e))]
      [`(unsafe-procedure-call ,e ...)
       `(unsafe-procedure-call ,@(map dox-e e))]
      [`(letrec ,rec-assign ,e)
       `(letrec ,(dox-rec-assign rec-assign) ,(dox-e e))]
      [`(let ,assign ,e)
       `(let ,(dox-assign assign) ,(dox-e e))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(dox-pred pred) ,(dox-e e1) ,(dox-e e2))]
      [`(begin ,effect ... ,e)
       `(begin ,@(map dox-effect effect) ,(dox-e e))]
      [_ (dox-v e)]))
             
  (define (dox-p p)
    (match p
      [`(module ,e) `(module ,(dox-e e))]))
         
  (dox-p p))