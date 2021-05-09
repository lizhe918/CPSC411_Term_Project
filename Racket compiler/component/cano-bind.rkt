#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(provide (all-defined-out))
(require "../utils/utils.rkt")

; Input: imp-mf-lang-v3
; Output: imp-cmf-lang-v5
; Purpose: pushing set! under begin so that the right-hand-side of each set! is 
;          simple value-producing operation.
(define (canonicalize-bind p)
  (debug-log p "cano-bind")
  
  (define crr (current-return-value-register))

  (define (bind-val v)
    (match v
      [`(begin ,effect ... ,value)
       (λ (s)
         `(begin ,@(map bind-effect effect)
                 ,((bind-val value) s)))]
      [`(if ,pred ,value1 ,value2)
       (λ (s)
         `(if ,(bind-pred pred)
              ,((bind-val value1) s)
              ,((bind-val value2) s)))]
      [`(return-point ,label ,tail)
       (λ (s)
         `(begin (return-point ,label ,(bind-tail tail))
                 (,@s ,crr)))]
      [`(mref ,loc ,opand)
       (let ([tmp (fresh)])
         (λ (s)
           (if (eq? 'mset! (first s))
               `(begin (set! ,tmp ,v) (,@s ,tmp))
               `(,@s ,v))))]
      [`(alloc ,opand)
       (let ([tmp (fresh)])
         (λ (s)
           (if (eq? 'mset! (first s))
               `(begin (set! ,tmp ,v) (,@s ,tmp))
               `(,@s ,v))))]
      [`(,binop ,opand1 ,opand2)
       (let ([tmp (fresh)])
         (λ (s)
           (if (eq? 'mset! (first s))
               `(begin (set! ,tmp ,v) (,@s ,tmp))
               `(,@s ,v))))]
      [v (λ (s) `(,@s ,v))]))

  (define (bind-pred pr)
    (match pr
      [`(not ,pred) `(not ,(bind-pred pred))]
      [`(if ,pred ,pred1, pred2)
       `(if ,(bind-pred pred) ,(bind-pred pred1) ,(bind-pred pred2))]
      [`(begin ,effect ... ,pred)
       `(begin ,@(map bind-effect effect) ,(bind-pred pred))]
      [_ pr]))

  (define (bind-effect e)
    (match e
      [`(begin ,effect ...)
       `(begin ,@(map bind-effect effect))]
      [`(if ,pred ,effect1 ,effect2)
       `(if ,(bind-pred pred) ,(bind-effect effect1) ,(bind-effect effect2))]
      [`(mset! ,loc ,opand ,value)
       ((bind-val value) `(mset! ,loc ,opand))]
      [`(set! ,aloc ,value)
       ((bind-val value) `(set! ,aloc))]))
      
  (define (bind-tail t)
    (match t
      [`(begin ,effect ... ,tail)
       `(begin ,@(map bind-effect effect) ,(bind-tail tail))]
      [`(if ,pred ,tail1 ,tail2)
       `(if ,(bind-pred pred) ,(bind-tail tail1) ,(bind-tail tail2))]
      [_ t]))

  (define (bind-fun f)
    (match f
      [`(define ,label ,info ,tail)
       `(define ,label ,info ,(bind-tail tail))]))
      
  (match p
    [`(module ,info ,fun ... ,tail) 
     `(module ,info ,@(map bind-fun fun) ,(bind-tail tail))]))
