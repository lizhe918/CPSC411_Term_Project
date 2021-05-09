#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(provide (all-defined-out))
(require "../utils/utils.rkt")

; input: Imp-cmf-lang-v5
; output: Asm-lang-v5
; purpose: make the coder able to write down values directly instead of using registers,
;          remove halt so that it looks nature
(define (select-instructions p)

  (define (auto-begin e)
    (if (= (length e) 1) (first e) `(begin ,@e)))

  (define (select-tail e)
    (match e
      [`(begin ,effect ... ,tail)
       `(,@(foldr append `() (map select-effect effect))
         ,@(select-tail tail))]
      [`(if ,pred ,tail1 ,tail2)
       `((if ,(auto-begin (select-pred pred))
             ,(auto-begin (select-tail tail1))
             ,(auto-begin (select-tail tail2))))]
      [`(jump ,trg ,loc ...) `(,e)]
      [_ (select-end-value e)]))

  (define (select-pred pr)
    (match pr
      [`(not ,pred) `((not ,(auto-begin (select-pred pred))))]
      [`(if ,pred ,pred1 ,pred2)
       `((if ,(auto-begin (select-pred pred))
             ,(auto-begin (select-pred pred1))
             ,(auto-begin (select-pred pred2))))]
      [`(begin ,effect ... ,pred)
       `(,@(foldr append `() (map select-effect effect)) 
         ,@(select-pred pred))]
      [`(,relop ,opand1 ,opand2)
       #:when (int64? opand1)
       (let ([tmp (fresh)])
         `((set! ,tmp ,opand1)
           (,relop ,tmp ,opand2)))]
      [_ `(,pr)]))

  (define (select-end-value v)
    (match v
      [`(,binop ,opand1 ,opand2)
       (let ([tmp (fresh)])
         `((set! ,tmp ,opand1)
           (set! ,tmp (,binop ,tmp ,opand2))
           (halt ,tmp)))]
      [`(mref ,loc ,opand)
       (let ([tmp (fresh)])
         `((set! ,tmp ,opand)
           (set! ,tmp (mref ,tmp ,opand))
           (halt ,tmp)))]
      [`(alloc ,opand)
       `(let ([tmp (fresh)])
          `((set! ,tmp (alloc ,opand))
            (halt ,tmp)))]
      [_ `((halt ,v))]))

  (define (select-effect e)
    (match e
      [`(begin ,effect ...) 
       `(,@(foldr append `() (map select-effect effect)))]
      [`(if ,pred ,effect1 ,effect2)
       `((if ,(auto-begin (select-pred pred))
             (begin ,@(select-effect effect1))
             (begin ,@(select-effect effect2))))]
      [`(set! ,loc (mref ,loc ,triv))
       `((set! ,loc (mref ,loc ,triv)))]
      [`(set! ,loc1 (mref ,loc2 ,triv))
       `((set! ,loc1 ,loc2)
         (set! ,loc1 (mref ,loc1 ,triv)))]
      [`(mset! ,loc ,opand ,triv)
       `((mset! ,loc ,opand ,triv))]
      [`(set! ,loc (,binop ,opand1 ,opand2))
       (let ([tmp (fresh)])
         `((set! ,tmp ,opand1)
           (set! ,tmp (,binop ,tmp ,opand2))
           (set! ,loc ,tmp)))]
      [`(set! ,aloc ,val) `(,e)]
      [`(return-point ,label ,tail)
       `((return-point ,label ,(auto-begin (select-tail tail))))]))
  
  (define (select-fun f)
    (match f
      [`(define ,label ,info ,tail)
       `(define ,label ,info ,(auto-begin (select-tail tail)))]))

  (match p
    [`(module ,info ,fun ... ,tail)
     `(module ,info ,@(map select-fun fun) ,(auto-begin (select-tail tail)))]))