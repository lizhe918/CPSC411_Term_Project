#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require "../utils/utils.rkt")
(provide (all-defined-out))


; input: nested-asm-lang-v8?
; output: block-pred-lang-v8?
; purpose: introduce the physical locations so that we do not need to remember the
;          stack pointer

(define-struct effret (s offset))

(define (implement-fvars p)
  (debug-log p "impl-fvars")

  (define fbp (current-frame-base-pointer-register))
  
  (define (process-p p offset)
    (match p
      [`(module ,blocks ... ,tail)
       `(module ,@(for/list ([b blocks])
                    (process-blocks b offset))
          ,(process-tail tail offset))]))

  (define (process-blocks b offset)
    (match b
      [`(define ,label ,tail)
       `(define ,label ,(process-tail tail offset))]))

  (define (process-pred p offset)
    (match p
      [`(true) p]
      [`(false) p]
      [`(not ,pred)
       `(not ,(process-pred pred offset))]
      [`(begin ,effects ... ,pred)
       (let-values ([(s offset) (for/fold ([seq empty]
                                           [offset offset])
                                          ([e effects])
                                  (process-effect e seq offset))])
         `(begin ,@s ,(process-pred pred offset)))]                               
      [`(if ,pred1 ,pred2 ,pred3)
       `(if ,(process-pred pred1 offset)
            ,(process-pred pred2 offset)
            ,(process-pred pred3 offset))]
      [`(,relop ,loc ,opand)
       `(,relop ,(process-location loc offset) ,(process-opand opand offset))]))

  (define (process-tail t offset)
    (match t
      [`(jump ,trg) `(jump ,(process-trg trg offset))]
      [`(begin ,effects ... ,tail)
       (let-values ([(seq offset) (for/fold ([seq empty]
                                             [offset offset])
                                            ([e effects])
                                    (process-effect e seq offset))])
         `(begin ,@seq ,(process-tail tail offset)))]
      [`(if ,pred ,tail1 ,tail2)
       `(if ,(process-pred pred offset)
            ,(process-tail tail1 offset)
            ,(process-tail tail2 offset))]))

  (define (process-effect e seq offset)
    (match e
      [`(set! ,loc ,triv)
       #:when (and (location? loc) (trivial? triv))
       (values (append seq (list `(set! ,(process-location loc offset) ,(process-triv triv offset))))
               offset)]
      [`(set! ,loc1 (,binop ,loc1 ,opand))
       (values (append seq (list `(set! ,(process-location loc1 offset)
                                        (,binop ,(process-location loc1 offset)
                                                ,(process-opand opand offset)))))
               (if (symbol=? loc1 fbp)
                   (compute-offset offset binop opand)
                   offset))]
      [`(set! ,loc1 (mref ,loc2 ,index))
       (values (append seq (list `(set! ,(process-location loc1 offset)
                                        (mref ,(process-location loc2 offset)
                                              ,(process-index index offset)))))
               offset)]
      [`(mset! ,loc ,index ,triv)
       (values (append seq (list `(mset! ,(process-location loc offset)
                                        ,(process-index index offset)
                                        ,(process-triv triv offset))))
               offset)]
      [`(begin ,effects ... ,effect)
       (let-values ([(s offset) (for/fold ([seq empty]
                                           [offset offset])
                                          ([e effects])
                                  (process-effect e seq offset))])
         (let-values ([(s^ o)
                       (process-effect effect empty offset)])                                 
           (values (append seq (list `(begin ,@s
                                             ,@s^)))
                   o)))]
      [`(if ,pred ,effect1 ,effect2)
       (values (append seq (list `(if ,(process-pred pred offset)
                                      ,@(let-values ([(s^ o)
                                                      (process-effect effect1  empty offset)])
                                          s^) 
                                      ,@(let-values ([(s^ o)
                                                      (process-effect effect2  empty offset)])
                                          s^))))
               offset)]
      [`(return-point ,label ,tail)
       (values (append seq (list `(return-point ,label ,(process-tail tail offset))))
               offset)]))

  (define (process-triv triv offset)
    (cond [(opand? triv) (process-opand triv offset)]
          [(label? triv) triv]))

  (define (process-location loc offset)
    (cond [(register? loc) loc]
          [(fvar? loc) `(,fbp - ,(- (* (fvar->index loc) (current-word-size-bytes)) offset))]))

  (define (process-index index offset)
    (cond [(int64? index) index]
          [(location? index) (process-location index offset)]))

  (define (process-opand opand offset)
    (cond [(int64? opand) opand]
          [(location? opand) (process-location opand offset)]))

  (define (process-trg trg offset)
    (cond [(label? trg) trg]
          [(location? trg) (process-location trg offset)]))

  (define (compute-offset offset binop opand)
    (cond [(symbol=? binop `+) (- offset opand)]
          [(symbol=? binop `-) (+ offset opand)]))

  (define (location? loc)
    (or (register? loc) (fvar? loc)))

  (define (trivial? triv)
    (or (int64? triv) (location? triv) (label? triv)))

  (define (opand? opand)
    (or (int64? opand) (location? opand)))

  (process-p p 0))