#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require "../utils/utils.rkt")
(provide (all-defined-out))

(define (expose-allocation-pointer p)
  (debug-log p "expose-allo")

  (define hbp (current-heap-base-pointer-register))

  (define (expose-effect e)
    (match e
      [`(begin ,effect ...)
       `(begin ,@(map expose-effect effect))]
      [`(if ,pred ,effect1 ,effect2)
       `(if ,(expose-pred pred) ,(expose-effect effect1) ,(expose-effect effect2))]
      [`(return-point ,label ,tail)
       `(return-point ,label ,(expose-tail tail))]
      [`(set! ,loc (alloc ,index))
       `(begin (set! ,loc ,hbp) (set! ,hbp (+ ,hbp ,index)))]
      [_ e]))

  (define (expose-pred p)
    (match p
      [`(begin ,effect ... ,pred)
       `(begin ,@(map expose-effect effect) ,(expose-pred pred))]
      [`(if ,pred ,pred1 ,pred2)
       `(if ,(expose-pred pred) ,(expose-pred pred1) ,(expose-pred pred2))]
      [`(not ,pred) `(not ,(expose-pred pred))]
      [_ p]))

  (define (expose-tail t)
    (match t
      [`(begin ,effect ... ,tail)
       `(begin ,@(map expose-effect effect) ,(expose-tail tail))]
      [`(if ,pred ,tail1 ,tail2)
       `(if ,(expose-pred pred) ,(expose-tail tail1) ,(expose-tail tail2))]
      [_ t]))

  (define (expose-fun f)
    (match f
      [`(define ,label ,info ,tail)
       `(define ,label ,info ,(expose-tail tail))]))

  (define (expose-p p)
    (match p
      [`(module ,info ,fun ... ,tail)
       `(module ,info ,@(map expose-fun fun) ,(expose-tail tail))]))
  
  (expose-p p))