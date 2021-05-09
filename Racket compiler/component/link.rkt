#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require rackunit)
(provide (all-defined-out))


; paren-x64-v4? -> paren-x64-rt-v4?
; Compiles Paren-x64 v4 to Paren-x64-rt v4 by resolving all labels to their position in the instruction sequence.

(define (link-paren-x64 p)
  (define (link-program p)
    (match p
      [`(begin ,ss ...)
       (let-values ([(env count) (for/fold ([env empty]
                                            [count 0])
                                           ([s ss])
                                   (build-env s env count))])
         `(begin ,@(for/fold ([acc empty])
                             ([s ss])
                     (link-statement s acc env))))]))

  (define (build-env s env count)
    (match s
      [`(with-label ,label ,s^)
       (let-values ([(deep-env deep-count)
                     (build-env s^ env count)])
         (values (dict-set deep-env label count)
                 (add1 count)))]
      [else (values env (add1 count))]))

  (define (link-statement s acc env)
    (match s
      [`(set! ,reg ,triv)
       #:when (and (register? reg) (trivial? triv))
       (append acc (list `(set! ,reg ,(link-triv triv env))))]

      [`(set! ,reg ,loc)
       #:when (and (register? reg) (location? loc))
       (append acc (list `(set! ,reg ,loc)))]

      [`(set! ,reg1 (,binop ,reg1 ,int32))
       #:when (and (register? reg1) (binop? binop) (int32? int32))
       (append acc (list `(set! ,reg1 (,binop ,reg1 ,int32))))]

      [`(set! ,reg1 (,binop ,reg1 ,loc))
       #:when (and (register? reg1) (binop? binop) (location? loc))
       (append acc (list `(set! ,reg1 (,binop ,reg1 ,loc))))]

      [`(set! ,addr ,int32)
       #:when (and (address? addr) (int32? int32))
       (append acc (list `(set! ,addr ,int32)))]

      [`(set! ,addr ,trg)
       #:when (and (address? addr) (trg? trg))
       (append acc (list `(set! ,addr ,(link-trg trg env))))]

      [`(with-label ,label ,s)
       #:when (label? label)
        (append acc (link-statement s empty env))]

      [`(jump ,trg)
       #:when (trg? trg)
       (append acc (list `(jump ,(link-trg trg env))))]

      [`(compare ,reg ,opand)
       #:when (and (register? reg) (opand? opand))
       (append acc (list `(compare ,reg ,opand)))]

      [`(jump-if ,relop ,label)
       (append acc (list `(jump-if ,relop ,(dict-ref env label))))]

      [else "undefined"]))

  (define (trg? trg)
    (or (register? trg) (label? trg)))

  (define (trivial? s)
    (or (trg? s) (int64? s)))
  
  (define (opand? o)
    (or (int64? o)
        (register? o)))

  (define (location? s)
    (or (register? s) (address? s)))
  
  (define (address? s)
    (and (frame-base-pointer-register? (first s))
         (equal? '- (second s))
         (dispoffset? (third s))))

  (define (binop? s)
    (or (equal? s '*) (equal? s '+)))

  (define (link-triv triv env)
    (cond [(int64? triv) triv]
          [else (link-trg triv env)]))

  (define (link-trg trg env)
    (cond [(register? trg) trg]
          [else (dict-ref env trg)]))

  (link-program p))
