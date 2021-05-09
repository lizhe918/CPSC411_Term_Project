#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require "../utils/utils.rkt")
(provide (all-defined-out))

; input: 
; output: 
; purpose: 
(define (resolve-predicates p)
  (debug-log p "resolve")

  (define (reverse-relop relop)
    (match relop
      ['> '<=] ['>= '<] ['< '>=] ['<= '>] ['= '!=] ['!= =]))

  (define (resolve-tail t)
    (match t
      [`(if (not ,pred) (jump ,trg1) (jump ,trg2))
       (resolve-tail `(if ,pred (jump ,trg2) (jump ,trg1)))]
      [`(if ,pred (jump ,trg1) (jump ,trg2))
       (match pred 
         [`(true) `(jump ,trg1)]
         [`(false) `(jump ,trg2)]
         [other `(if ,other (jump ,trg1) (jump ,trg2))])]
      [`(begin ,s ... ,tail) 
       `(begin ,@s ,(resolve-tail tail))]
      [_ t]))

  (define (resolve-b b)
    (match b
      [`(define ,label ,tail)
       `(define ,label ,(resolve-tail tail))]))
    
  (match p
    [`(module ,b ...)
     (append `(module)
             (map resolve-b b))]))