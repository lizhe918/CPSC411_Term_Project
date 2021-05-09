#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require compatibility/mlist)
(require "../utils/utils.rkt")
(provide (all-defined-out))

; input: nested-asm-lang-v5
; output: block-pred-lang-v5
; purpose: 
(define (expose-basic-blocks p)
  (debug-log p "expose")

  (define result `())
  (define cbr (current-frame-base-pointer-register))

  (define (new-block label body)
    (let ([new-label (fresh-label label)])
      (set! result `((define ,new-label ,body) ,@result))
      new-label))
        
  (define (new-block-fun label body)
    (let ([new-label label])
      (set! result `((define ,new-label ,body) ,@result))
      new-label))
        
  (define (expose-effect e trg)
    (match e
      [`(begin) `(begin ,trg)]
      [`(begin ,effect ...)
       (foldr (lambda (e t) (expose-effect e t))
              trg
              effect)]
      [`(if ,pred ,effect1 ,effect2)
       (let* ([l-trg (new-block 'tmp trg)]
              [l2 (new-block 'tmp (expose-effect effect2 `(jump ,l-trg)))]
              [l1 (new-block 'tmp (expose-effect effect1 `(jump ,l-trg)))])
        (expose-pred pred l1 l2))]
      [`(return-point ,label ,tail)
       (let ([l-trg (new-block-fun label trg)])
         (expose-tail tail))]
      [_ 
       `(begin ,e 
               ,@(if (eq? (first trg) 'begin) (rest trg) `(,trg)))]))
  
  (define (expose-pred pr l-trg1 l-trg2)
    (match pr
      [`(begin ,effect ... ,pred)
       (expose-effect `(begin ,@effect) (expose-pred pred l-trg1 l-trg2))]
      [`(if ,pred ,pred1 ,pred2)
       (let ([l2 (new-block 'tmp (expose-pred pred2 l-trg1 l-trg2))]
             [l1 (new-block 'tmp (expose-pred pred1 l-trg1 l-trg2))])
         (expose-pred pred l1 l2))]
      [`(not ,pred) (expose-pred pred l-trg2 l-trg1)]
      [_ `(if ,pr (jump ,l-trg1) (jump ,l-trg2))]))

  (define (expose-tail t)
    (match t
      [`(begin ,effect ... ,tail) 
       (expose-effect `(begin ,@effect) (expose-tail tail))]
      [`(if ,pred ,tail1 ,tail2)
       (let ([b2 (new-block '__nested (expose-tail tail2))]
             [b1 (new-block '__nested (expose-tail tail1))])
         (expose-pred pred b1 b2))]
      [_ t]))
      
  (define (expose-fun f)
    (match f
      [`(define ,label ,tail)
       (new-block-fun label (expose-tail tail))]
      [_ f]))
  
  (begin
    (match p
      [`(module ,fun ... ,tail) 
       (for ([f fun]) (expose-fun f))
       (new-block '__main (expose-tail tail))])
    `(module ,@result)))