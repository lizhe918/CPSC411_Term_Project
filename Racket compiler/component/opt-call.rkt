#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require "../utils/utils.rkt")
(provide (all-defined-out))

(define (optimize-direct-calls p)
  (debug-log p "opt-call")

  (define (opt-e e)
    (match e
      [`(unsafe-procedure-call (lambda (,xs ...) ,e) ,args ...)
       #:when (eq? (length xs) (length args))
       `(let ,(map (λ (x a) `(,x ,(opt-e a))) xs args)
             ,(opt-e e))]
      [_ (if (list? e) (map opt-e e) e)]))
  
  (opt-e p))