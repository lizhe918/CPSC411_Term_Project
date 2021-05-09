#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require "../utils/utils.rkt")
(provide (all-defined-out))

(define (define->letrec p)
  (debug-log p "def->rec")

  (define (def-b b)
    (match b
      [`(define ,b-aloc ,body)
       `(,b-aloc ,body)]))

  (define (def-p p)
    (match p
      [`(module ,b ... ,e)
       `(module (letrec ,(map def-b b) ,e))]))

  (def-p p))