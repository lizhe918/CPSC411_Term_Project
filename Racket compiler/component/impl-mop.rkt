#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require "../utils/utils.rkt")
(provide (all-defined-out))

(define (implement-mops p)
  (debug-log p "impl-mops")
  
  (define fbp (current-frame-base-pointer-register))

  (define (addr? a)
    (match a
      [`(,fbp - ,dispoffset)
       #:when (dispoffset? dispoffset)
       #t]
      [_ #f]))

  (define (impl-s s)
    (match s
      [`(with-label ,label ,s)
       `(with-label ,label ,(impl-s s))]
      [`(set! ,reg1 (mref ,reg2 ,index))
       `(set! ,reg1 (,reg2 + ,index))]
      [`(mset! ,reg ,index ,triv)
       `(set! (,reg + ,index) ,triv)]
      [_ s]))

  (define (impl-p p)
    (match p
      [`(begin ,s ...) 
       `(begin ,@(map impl-s s))]))
  
  (impl-p p))