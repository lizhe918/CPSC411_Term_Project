#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require "../utils/utils.rkt")
(provide (all-defined-out))


; input: nested-asm-lang-v4
; output: para-asm-lang-v4
; purpose: allow us to write nested sequential statements, so that we can seperate code into
;          different blocks
(define (flatten-program p)
  (debug-log p "flatten-prog")

  (define (flatten-tail t)
    (match t
      [`(if (,relop ,loc ,opand) (jump ,trg1) (jump ,trg2))
       `((compare ,loc ,opand)
         (jump-if ,relop ,trg1)
         (jump ,trg2))]
      [`(begin ,s ... ,tail)
       (append s (flatten-tail tail))]
      [_ `(,t)]))

  (define (flatten-b b)
    (match b
      [`(define ,label ,tail)
       (let ([body (flatten-tail tail)])
         (append `((with-label ,label ,(first body)))
                 (rest body)))]))
        
  (match p
    [`(module ,b ...)
     (append `(begin)
             (foldr append `() (map flatten-b b)))]))