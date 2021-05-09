#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(provide (all-defined-out))
(require "../utils/utils.rkt")

; Input: Values-lang-v5
; Output: Values-unique-lang-v5
; Purpose: Compiles Values-lang v5 to Values-unique-lang v5 by resolving all lexical 
;          identifiers to abstract locations
(define (uniquify p)
  (debug-log p "uniquify")
  
  (define fun-binds `())
  
  (define (try-replace x binds)
    (if (and (name? x) (dict-has-key? binds x)) (dict-ref binds x) x))

  (define (build-binds xs binds)
    (for/fold ([new-binds binds])
              ([x xs])
      (dict-set new-binds x (fresh x))))
      
  (define (process-v v binds)
    (match v
      [`(lambda (,xs ...) ,e)
       (let ([new-binds (build-binds xs binds)])
         `(lambda ,(map (λ (x) (dict-ref new-binds x)) xs)
                  ,(process-e e new-binds)))]
      [_ (try-replace v binds)]))

  (define (process-e e binds)
    (match e
      [`(let ([,xs ,e] ...) ,ee)
       (local [(define new-binds (build-binds xs binds))]
         `(let ,(for/list ([x xs]
                           [e e])
                  `[,(dict-ref new-binds x) ,(process-e e binds)])
            ,(process-e ee new-binds)))]
      [`(if ,e ,e1 ,e2)
       `(if ,(process-e e binds)
            ,(process-e e1 binds)
            ,(process-e e2 binds))]
      [`(call ,e  ...)
       `(call ,@(map (λ (e) (process-e e binds)) e))] 
      [_ (process-v e binds)]))

  (define (process-fun-proto fn)
    (match fn
      [`(define ,x (lambda (,xs ...) ,tail))
       (set! fun-binds (dict-set fun-binds x (fresh x)))
       `(define ,(dict-ref fun-binds x) (lambda (,@xs) ,tail))]))

  (define (process-fun f)
    (match f
      [`(define ,label (lambda (,xs ...) ,e))
       (let ([new-binds (build-binds xs fun-binds)])
         `(define ,label 
                  (lambda ,(map (lambda (x) (dict-ref new-binds x)) xs)
                  ,(process-e e new-binds))))]))

  (define (process-p p)
    (match p
      [`(module ,fun ... ,e)
       (let ([fun-proto (map process-fun-proto fun)])
         `(module ,@(map (lambda (f) (process-fun f)) fun-proto) 
                  ,(process-e e fun-binds)))]))

  (process-p p))

