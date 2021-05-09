#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require "../utils/utils.rkt")
(provide (all-defined-out))

(define (convert-closures p)
  (debug-log p "convert-closure")

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

  (define (convert-assign a)
    (map (λ (a) `(,(first a) ,(convert-e (second a)))) a))

  (define (convert-lmb l)
    (match l
      [`(,aloc (lambda ,info (,arg ...) ,e))
       (let ([free (list-dict-ref info 'free)]
             [new-e (convert-e e)]
             [c (fresh 'c)]
             [label (fresh-label aloc)])
         (values `(,aloc (make-closure ,label ,(length arg) ,@free))
                 `(,label 
                   (lambda (,c ,@arg)
                     (let ,(map (λ (f i) `(,f (closure-ref ,c ,i))) free (range (length free))) 
                       ,new-e)))))]))

  (define (convert-pred p)
    (match p
      [`(true) `(true)]
      [`(false) `(false)]
      [`(not ,pred)
       `(not ,(convert-pred pred))]
      [`(let ,assign ,pred)
       `(let ,(convert-assign assign) ,(convert-pred pred))]
      [`(if ,pred ,pred1 ,pred2)
       `(if ,(convert-pred pred) ,(convert-pred pred1) ,(convert-pred pred2))]
      [_ (convert-e p)]))

  (define (convert-effect e)
    (match e
      [`(,primop ,e ...)
       #:when (primop? primop)
       `(,primop ,@(map convert-e e))]
      [`(begin ,effect ...)
       `(begin ,@(map convert-effect effect))]))

  (define (convert-e e)
    (match e
      [`(letrec ,assign ,e)
       (let-values ([(new-e) (convert-e e)]
                    [(e-assign new-assign) (map2 convert-lmb assign)])
         `(letrec ,new-assign (cletrec ,e-assign ,new-e)))]
      [`(let ,assign ,e)
       `(let ,(convert-assign assign) ,(convert-e e))]
      [`(unsafe-procedure-call ,e ,es ...)
       (let ([new-e (convert-e e)]
             [new-es (map convert-e es)])
         (if (aloc? new-e)
             `(closure-call ,new-e ,new-e ,@new-es)
             (let ([tmp (fresh)])
               `(let ((,tmp ,new-e))
                  (closure-call ,tmp ,tmp ,@new-es)))))]
      [`(,primop ,e ...)
       #:when (primop? primop)
       `(,primop ,@(map convert-e e))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(convert-pred pred) ,(convert-e e1) ,(convert-e e2))]
      [`(begin ,effect ... ,e)
       `(begin ,@(map convert-effect effect) ,(convert-e e))]
      [_ e]))

  (define (convert-p p)
    (match p
      [`(module ,e) `(module ,(convert-e e))]))
  
  (convert-p p))