#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require "../utils/utils.rkt")
(provide (all-defined-out))


(define (uncover-free p)
  (debug-log p "uncover-free")

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

  (define (uncover-rec-assign a)
    (let-values ([(f new-lmb) (map2 uncover-lmb (map second a))])
      (values (foldr set-union `() f) 
              (map (λ (x l) `(,x ,l)) (map first a) new-lmb))))

  (define (uncover-assign a)
    (let-values ([(f new-e) (map2 uncover-e (map second a))])
      (values (foldr set-union `() f)
              (map (λ (x v) `(,x ,v)) (map first a) new-e))))
 
  (define (uncover-lmb l)
    (match l
      [`(lambda (,aloc ...) ,e)
       (let-values ([(f new-e) (uncover-e e)])
         (values (remove* aloc f) 
                 `(lambda ((free ,(remove* aloc f))) (,@aloc) ,new-e)))]))
 
  (define (uncover-v v)
    (if (aloc? v) (values `(,v) v) (values `() v)))
       
  (define (uncover-pred p)
    (match p
      [`(true) (values `() `(true))]
      [`(false) (values `() `(false))]
      [`(not ,pred)
       (let-values ([(f new-p) (uncover-pred pred)])
         (values f `(not ,new-p)))]
      [`(let ,assign ,pred)
       (let-values ([(f-a new-a) (uncover-assign assign)]
                    [(f-p new-p) (uncover-pred pred)])
         (values (set-union f-a f-p) `(let ,new-a ,new-p)))]
      [`(if ,pred ,pred1 ,pred2)
       (let-values ([(f-p new-p) (uncover-pred pred)]
                    [(f-1 new-1) (uncover-pred pred1)]
                    [(f-2 new-2) (uncover-pred pred2)])
         (values (set-union f-p f-1 f-2) `(if ,new-p ,new-1 ,new-2)))]
      [_ (uncover-e p)]))

  (define (uncover-effect e)
    (match e
      [`(,primop ,e ...)
       #:when (primop? primop)
       (let-values ([(f new-e) (map2 uncover-e e)])
         (values (foldr set-union `() f) `(,primop ,@new-e)))]
      [`(begin ,effect ...)
       (let-values ([(f new-ef) (map2 uncover-effect effect)])
         (values (foldr set-union `() f) `(begin ,@new-ef)))]))

  (define (uncover-e e)
    (match e
      [`(,primop ,e ...)
      #:when (primop? primop)
      (let-values ([(f new-e) (map2 uncover-e e)])
        (values (foldr set-union `() f) `(,primop ,@new-e)))]
      [`(unsafe-procedure-call ,e ...)
       (let-values ([(f new-e) (map2 uncover-e e)])
         (values (foldr set-union `() f) `(unsafe-procedure-call ,@new-e)))]
      [`(letrec ,assign ,e)
       (let-values ([(f-a new-a) (uncover-rec-assign assign)]
                    [(f-e new-e) (uncover-e e)])
         (values (remove* (map first assign) (set-union f-a f-e))
                 `(letrec ,new-a ,new-e)))]
      [`(let ,assign ,e)
       (let-values ([(f-a new-a) (uncover-assign assign)]
                    [(f-e new-e) (uncover-e e)])
         (values (remove* (map first assign) (set-union f-a f-e))
                 `(let ,new-a ,new-e)))]
      [`(if ,pred ,e1 ,e2)
       (let-values ([(f-p new-p) (uncover-pred pred)]
                    [(f-1 new-1) (uncover-e e1)]
                    [(f-2 new-2) (uncover-e e2)])
         (values (set-union f-p f-1 f-2) `(if ,new-p ,new-1 ,new-2)))]
      [`(begin ,effect ... ,e)
       (let-values ([(f-ef new-ef) (map2 uncover-effect effect)]
                    [(f-e new-e) (uncover-e e)])
         (values (set-union (foldr set-union `() f-ef) f-e) `(begin ,@new-ef ,new-e)))]
      [_ (uncover-v e)]))

  (define (uncover-p p)
    (match p
      [`(module ,e) 
       `(module ,(let-values ([(f new-e) (uncover-e e)])
                   new-e))]))
 
  (uncover-p p))