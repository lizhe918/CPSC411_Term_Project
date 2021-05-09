#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(provide (all-defined-out))
(require "../utils/utils.rkt")


; input: asm-lang-v5
; output: asm-lang-v5/locals
; purpose: find all the abstract location names so we can replace them later
(define (uncover-locals p)
  (debug-log p "uncover")

  (define (loc? x) (if (label? x) false (aloc? x)))

  (define (uncover-tail var t)
    (foldl (lambda (x var)
             (if (list? x)
                 (uncover-tail var x)
                 (if (and (loc? x) (not (contain var x)))
                     `(,@var ,x)
                     var)))
           var
           t))

  (define (uncover-fun f)
    (match f
      [`(define ,label ,info ,tail)
       `(define ,label 
                (,@info (locals ,(uncover-tail `() tail))) 
                ,tail)]))
  
  (match p
    [`(module ,info ,fun ... ,tail)
     `(module (,@info (locals ,(uncover-tail `() tail)))
              ,@(map uncover-fun fun)
              ,tail)]))
