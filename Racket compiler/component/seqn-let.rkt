#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(provide (all-defined-out))

; Input: Values-unique-lang-v4.p 
; Output: imp-mf-lang-v4.p
; Purpose: Compiles Values-unique-lang v4 to Imp-mf-lang v4 by
;          picking a particular order to implement let expressions using set!.
(define (sequentialize-let p)

  (define (process-effect e)
    (match e
      [`(mset! ,aloc ,opand ,value)
       `(mset! ,aloc ,opand ,(process-value value))]
      [`(let ([,alocs ,values] ...) ,effect)
       `(begin ,@(for/list ([aloc alocs]
                            [vi values])
                   (seq-let vi aloc))
                ,(process-effect effect))]
      [`(begin ,effect ...)
       `(begin ,@(map process-effect effect))]))

  (define (process-pred p)
    (match p
      [`(true) `(true)]
      [`(false) `(false)]
      [`(not ,pred)
       `(not ,(process-pred pred))]
      [`(let ([,alocs ,values] ...) ,pred)
       `(begin ,@(for/list ([aloc alocs]
                            [vi values])
                   (seq-let vi aloc))
               ,(process-pred pred))]
      [`(begin ,effect ... ,pred)
       `(begin ,@(map process-effect effect) ,(process-pred pred))]
      [`(if ,pred ,predt ,predf)
       `(if ,(process-pred pred)
            ,(process-pred predt)
            ,(process-pred predf))]
      [`(,relop ,triv1 ,triv2)
       `(,relop ,triv1 ,triv2)]))
     
  (define (process-tail t)
    (match t
      [`(let ([,alocs ,values] ...) ,tail)
       `(begin ,@(for/list ([aloc alocs]
                            [vi values])
                   (seq-let vi aloc))
               ,(process-tail tail))]
      [`(begin ,effect ... ,tail)
       `(begin ,@(map process-effect effect) ,(process-tail tail))]
      [`(if ,pred ,tailt ,tailf)
       `(if ,(process-pred pred)
            ,(process-tail tailt)
            ,(process-tail tailf))]
      [_ (process-value t)]))

  (define (process-value v)
    (match v
      [`(let ([,alocs ,values] ...) ,value)
       `(begin ,@(for/list ([aloc alocs]
                            [vi values])
                   (seq-let vi aloc))
               ,(process-value value))]
      [`(begin ,effect ... ,value)
       `(begin ,@(map process-effect effect) ,(process-value value))]
      [`(if ,pred ,valuet ,valuef)
       `(if ,(process-pred pred)
            ,(process-value valuet)
            ,(process-value valuef))]
      [_ v]))

  (define (seq-let v aloc)
    `(set! ,aloc ,(process-value v)))

  (define (process-fun f)
    (match f
      [`(define ,label (lambda (,aloc ...) ,tail))
       `(define ,label (lambda (,@aloc) ,(process-tail tail)))]))
       
  (define (process-p p)
    (match p
      [`(module ,fun ... ,tail)
       `(module ,@(map process-fun fun) ,(process-tail tail))]))

  (process-p p))



    ; (match v
    ;   [`(let ([,alocs ,values] ...) ,value)
    ;    `(set! ,aloc (begin ,@(for/list ([aloc alocs]
    ;                                     [vi values])
    ;                            (seq-let vi aloc))
    ;                        ,(process-value value)))]
    ;   [`(begin ,effect ... ,value)]
    ;   [`(if ,pred ,valuet ,valuef)
    ;    `(set! ,aloc (if ,(process-pred pred)
    ;                     ,(process-value valuet)
    ;                     ,(process-value valuef)))]
    ;   [`(,binop ,triv1 ,triv2)
    ;    `(set! ,aloc (,binop ,triv1 ,triv2))]
    ;   [else
    ;    `(set! ,aloc ,v)])






