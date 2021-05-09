#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require cpsc411/graph-lib)
(require cpsc411/2c-run-time)
(require "../utils/utils.rkt")
(provide (all-defined-out))

; Input: proc-imp-mf-lang-v5
; Output: imp-mf-lang-v5
; Purpose: 
(define (impose-calling-conventions p)
  (debug-log p "impose")

  (define cpr (current-parameter-registers))
  (define cbr (current-frame-base-pointer-register))
  (define crr (current-return-value-register))
  (define cra (current-return-address-register))
  (define frame-loc cpr)
  (define new-frames `())
  (define stub `())

  (define (allo-fun-loc arg)
    (if (>= (length cpr) (length arg))
        (take cpr (length arg))
        `(,@cpr ,@(map make-fvar (range (- (length arg) (length cpr)))))))

  (define (allo-frame-loc arg)
    (if (>= (length frame-loc) (length arg))
        (let ([f-loc (take frame-loc (length arg))])
          (set! new-frames `(,(filter aloc? f-loc) ,@new-frames))
          (set! frame-loc cpr)
          f-loc)
        (begin (set! frame-loc `(,@frame-loc ,(fresh 'nfv)))
               (allo-frame-loc arg))))


  (define (impose-pred pr)
    (match pr
      [`(not ,pred) `(not ,(impose-pred pred))]
      [`(begin ,effect ... ,pred)
       `(begin ,@(map impose-effect effect) ,(impose-pred pred))]
      [`(if ,pred ,pred1 ,pred2)
       `(if ,(impose-pred pred) ,(impose-pred pred1) ,(impose-pred pred2))]
      [_ pr]))

  (define (impose-effect e)
    (match e
      [`(begin ,effect ...)
       `(begin ,@(map impose-effect effect))]
      [`(if ,pred ,effect1 ,effect2)
       `(if ,(impose-pred pred) ,(impose-effect effect1) ,(impose-effect effect2))]
      [`(mset! ,loc ,opand ,value)
       `(mset! ,loc ,opand ,(impose-value value))]
      [`(set! ,loc ,value)
       `(set! ,loc ,(impose-value value))]))

  (define (impose-tail ra allo-loc t)
    (match t
      [`(begin ,effect ... ,tail)
       `(begin ,@(map impose-effect effect) ,(impose-tail ra allo-loc tail))]
      [`(if ,pred ,tail1 ,tail2)
       `(if ,(impose-pred pred) 
            ,(impose-tail ra allo-loc tail1) 
            ,(impose-tail ra allo-loc tail2))]
      [`(call ,triv ,opand ...)
       (let ([opand-loc (allo-loc opand)])
         `(begin ,@(reverse (map (lambda (l t) `(set! ,l ,t)) opand-loc opand))
                 (set! ,cra ,ra)
                 (jump ,triv ,cbr ,cra ,@opand-loc)))]
      [_ `(begin (set! ,crr ,t) (jump ,ra ,cbr ,crr))]))

  (define (impose-value v)
    (match v
      [`(begin ,effect ... ,value)
       `(begin ,@(map impose-effect effect) ,(impose-value value))]
      [`(if ,pred ,value1 ,value2)
       `(if ,(impose-pred pred) ,(impose-value value1) ,(impose-value value2))]
      [`(call ,triv ,opand ...)
       (let ([rp (fresh-label 'rp)])
         `(return-point ,rp ,(impose-tail rp allo-frame-loc v)))]
      [_ v]))
      
  (define (impose-fun f)
    (match f
      [`(define ,label (lambda (,aloc ...) ,tail)) 
       (set! new-frames `())
       (let* ([ra (fresh 'tmp-ra)]
              [i-tail `(begin ,@(map (lambda (a l) `(set! ,a ,l)) aloc (allo-fun-loc aloc))
                              (begin (set! ,ra ,cra)
                                     ,(impose-tail ra allo-fun-loc tail)))]
              ; [i-tail `(begin (set! ,ra ,cra) 
              ;                 (begin ,@(map (lambda (a l) `(set! ,a ,l)) aloc (allo-fun-loc aloc))
              ;                        ,(impose-tail ra allo-fun-loc tail)))]
                                     )
         `(define ,label ((new-frames ,new-frames)) ,i-tail))]))
  
  (define (impose-p p)
    (match p
      [`(module ,fun ... ,tail)
       (let* ([ra (fresh 'tmp-ra)]
              [i-tail `(begin (set! ,ra ,cra) ,(impose-tail ra allo-fun-loc tail))]
              [frames new-frames]
              [i-fun (map impose-fun fun)])
         `(module ((new-frames ,frames)) ,@i-fun ,i-tail))]))
                   
  (impose-p p))
