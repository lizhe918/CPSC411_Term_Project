#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(provide (all-defined-out))
(require "../utils/utils.rkt")

; input: 
; output: 
; purpose: 
(define (undead-analysis p)
  (debug-log p "undead")
  
  (define crr (current-return-value-register))
  (define cbr (current-frame-base-pointer-register))
  (define call-undead `())
  (define (loc? x) (if (label? x) false (or (aloc? x) (register? x) (fvar? x))))

  (define (loc-add s l)
    (if (loc? l) (set-add s l) s))
  
  (define (undead-effect req e)
    (match e
      [`(mset! ,loc ,index ,triv)
       `(,(loc-add (loc-add (loc-add req loc) index) triv) ,req)]
      [`(set! ,loc1 (mref ,loc2 ,index))
       `(,(loc-add (loc-add (set-remove req loc1) loc2) index) ,req)]
      [`(set! ,aloc (,binop ,aloc ,triv))
       (if (loc? triv)
           `(,(set-add (set-add req aloc) triv) ,req)
           `(,(set-add req aloc) ,req))]
      [`(set! ,aloc ,triv)
       (if (loc? triv)
           `(,(set-add (set-remove req aloc) triv) ,req)
           `(,(set-remove req aloc) ,req))]
      [`(begin ,effect ...)
       (foldr (lambda (e temp)
                (let ([e-temp (undead-effect (first temp) e)])
                  `(,(first e-temp) (,(second e-temp) ,@(second temp)))))
              `(,req ())
              effect)]
      [`(if ,pred ,effect1 ,effect2)
       (let* ([temp1 (undead-effect req effect1)]
              [temp2 (undead-effect req effect2)]
              [pred-req (set-union (first temp1) (first temp2))]
              [temp0 (undead-pred pred-req pred)])
         `(,(first temp0) (,(second temp0) ,(second temp1) ,(second temp2))))]
      [`(return-point ,label ,tail)
       (let* ([temp (undead-tail tail)]
              [new-call-undead (set-union call-undead (remove* `(,cbr ,crr) req))]
              [new-req (remove crr (set-union req (first temp)))])
         (set! call-undead new-call-undead)
         `(,new-req (,req ,(second temp))))]))

  (define (undead-pred req pr)
    (match pr
      [`(not ,pred) (undead-pred req pred)]
      [`(begin ,effect ... ,pred)
       (let* ([p-temp (undead-pred req pred)]
              [temp (undead-effect (first p-temp) `(begin ,@effect))])
         `(,(first temp) (,@(second temp) ,(second p-temp))))]
      [`(if ,pred ,pred1 ,pred2)
       (let* ([temp1 (undead-pred req pred1)]
              [temp2 (undead-pred req pred2)]
              [pred-req (set-union (first temp1) (first temp2))]
              [temp0 (undead-pred pred-req pred)])
         `(,(first temp0) (,(second temp0) ,(second temp1) ,(second temp2))))]
      [`(,relop ,aloc ,triv)
       (if (loc? triv)
           `(,(set-add (set-add req aloc) triv) ,req)
           `(,(set-add req aloc) ,req))]
      [_ `(,req ,req)]))

  (define (undead-tail t)
    (match t
      [`(halt ,triv) 
       `(,(if (loc? triv) `(,triv) `()) ())]
      [`(begin ,effect ... ,tail)
       (let* ([t-temp (undead-tail tail)]
              [e-temp (undead-effect (first t-temp) `(begin ,@effect))])
         `(,(first e-temp) (,@(second e-temp) ,(second t-temp))))]
      [`(jump ,trg ,loc ...)
       (if (loc? trg)
           `((,trg ,@(reverse loc)) ,(reverse loc))
           `(,(reverse loc) ,(reverse loc)))]
      [`(if ,pred ,tail1 ,tail2)
       (let* ([temp1 (undead-tail tail1)]
              [temp2 (undead-tail tail2)]
              [pred-req (set-union (first temp1) (first temp2))]
              [temp0 (undead-pred pred-req pred)])
         `(,(first temp0) (,(second temp0) ,(second temp1) ,(second temp2))))]))

  (define (undead-fun f)
    (match f
      [`(define ,label ,info ,tail)
       (set! call-undead `())
       (let ([undead (second (undead-tail tail))]
             [c-undead call-undead])
         `(define ,label
                  (,@info (undead-out ,undead) (call-undead ,c-undead))
                  ,tail))]))

  (define (undead-p p)
    (match p
      [`(module ,info ,fun ... ,tail)
       (set! call-undead `())
       (let ([undead (second (undead-tail tail))]
             [c-undead call-undead])
         `(module (,@info (call-undead ,c-undead) (undead-out ,undead))
                  ,@(map undead-fun fun)
                  ,tail))]))
  (undead-p p))
