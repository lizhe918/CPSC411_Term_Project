#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require compatibility/mlist)
(require "../utils/utils.rkt")
(provide (all-defined-out))

(define (optimize-predicates p)
  (debug-log p "optimize")

  (define (relop->fun r)
    (match r
      ['> >] ['< <] ['= =] ['>= >=] ['<= <=] ['!= (lambda (a b) (not (= a b)))]))

  (define (relop? r)
    (ormap (lambda (x) (eq? r x)) `(> < = >= <= !=)))

  (define (optimize-effect e)
    (match e
      [`(if ,pred ,effect1 ,effect2)
       `(if ,(optimize-pred pred) ,(optimize-effect effect1) ,(optimize-effect effect2))]
      [`(begin ,effect ...) `(begin ,@(map optimize-effect effect))]
      [_ e]))
  
  (define (optimize-pred pr)
    (match pr
      [`(not (true)) `(false)]
      [`(not (false)) `(true)]
      [`(not (not ,pred)) (optimize-pred pred)]
      [`(not ,pred) `(not ,(optimize-pred pred))]
      [`(if ,pred ,pred1 ,pred2)
       `(if ,(optimize-pred pred) ,(optimize-pred pred1) ,(optimize-pred pred2))]
      [`(begin (true)) `(true)]
      [`(begin (false)) `(false)]
      
      [`(begin ,effect ... (set! ,loc ,triv) (,relop ,loc ,triv2))
       #:when (and (relop? relop) (int64? (format-int triv)))
       `(,@(optimize-effect `(begin ,@effect)) 
         (set! ,loc ,triv)
         ,(if ((relop->fun relop) (format-int triv) (format-int triv2)) `(true) `(false)))]
         
      [`(begin ,effect ... (set! ,loc ,triv) (,relop ,loc ,triv2))
       #:when (and (relop? relop) (not (int64? (format-int triv))))
       `(,@(optimize-effect `(begin ,@effect)) 
         (set! ,loc ,triv)
         ,(if ((relop->fun relop) 0 0) `(true) `(false)))]
         
      [`(begin ,effect ... ,pred)
       `(,@(optimize-effect `(begin ,@effect)) 
         ,(optimize-pred pred))]
         
      [_ pr]))

  (define (optimize-tail t)
    (match t
      [`(if ,pred ,tail1 ,tail2)
       `(if ,(optimize-pred pred) ,(optimize-tail tail1) ,(optimize-tail tail2))]
      [`(begin ,effect ... ,tail)
       `(,@(optimize-effect `(begin ,@effect)) 
         ,(optimize-tail tail))]
      [_ t]))
    
  (match p
    [`(module ,other ... ,tail) 
     `(module ,@other ,(optimize-tail tail))]))