#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require rackunit)
(require "link.rkt")
(provide (all-defined-out))


; paren-x64-v4? -> (integer-in/c 0 255)
; Interpret the Paren-x64 v4 program p as a value, returning the exit code for p.
(define (interp-paren-x64 p)
  (interp-loop (rest (link-paren-x64 p)) empty 0))

; (listof paren-x64-rt-v4.s) dict? natural-number/c -> (integer-in/c 0 255)
; The main loop of the interpreter for Paren-x64-rt v4.
(define (interp-loop c m pc)

  (define pc-addr? natural-number/c)

  (define (interp-s s regfile memory pcount flag)
    (match s
      [`(set! ,reg ,triv)
       #:when (and (register? reg) (trivial? triv))
       (values (dict-set regfile reg (interp-triv triv regfile pcount))
               memory
               (add1 pcount)
               flag)]

      [`(set! ,reg ,loc)
       #:when (and (register? reg) (location? loc))
       (values (dict-set regfile reg (interp-loc loc regfile memory))
               memory
               (add1 pcount)
               flag)]

      [`(set! ,reg1 (,binop ,reg1 ,int32))
       #:when (and (register? reg1) (binop? binop) (int32? int32))
       (values (dict-set regfile reg1 (interp-binop binop reg1 int32 regfile memory))
               memory
               (add1 pcount)
               flag)]
      
      [`(set! ,reg1 (,binop ,reg1 ,loc))
       #:when (and (register? reg1) (binop? binop) (location? loc))
       (values (dict-set regfile reg1 (interp-binop binop reg1 loc regfile memory))
               memory
               (add1 pcount)
               flag)]
      
      [`(set! ,addr ,int32)
       #:when (and (address? addr) (int32? int32))
       (values regfile
               (dict-set memory addr int32)
               (add1 pcount)
               flag)]

      [`(set! ,addr ,trg)
       #:when (and (address? addr) (trg? trg))
       (values regfile
               (dict-set memory addr (interp-trg trg regfile))
               (add1 pcount)
               flag)]

      [`(jump ,trg)
       #:when (trg? trg)
       (values regfile
               memory
               (interp-trg trg regfile)
               flag)]

      [`(compare ,reg ,opand)
       #:when (and (register? reg) (opand? opand))
       (values regfile
               memory
               (add1 pcount)
               (compare reg opand regfile))]

      [`(jump-if ,relop ,pcaddr)
       (values regfile
               memory
               (jump-if relop pcaddr regfile (add1 pcount) flag)
               flag)]))

  (define (interp-trg trg regfile)
    (if (register? trg)
        (dict-ref regfile trg)
        trg))

  (define (interp-triv triv regfile pcount)
    (if (trg? triv)
        (interp-trg triv regfile)
        triv))

  (define (compare reg opand regfile)
    (if (register? opand)
        (let ([r (dict-ref regfile reg)]
              [o (dict-ref regfile opand)])
          (cond [(< r o) (list 1 0 0)]
                [(= r o) (list 0 1 0)]
                [(> r o) (list 0 0 1)]))
        (let ([r (dict-ref regfile reg)])
          (cond [(< r opand) (list 1 0 0)]
                [(= r opand) (list 0 1 0)]
                [(> r opand) (list 0 0 1)]))))

  (define (interp-loc loc regfile memory)
    (if (register? loc)
        (dict-ref regfile loc)
        (dict-ref memory loc)))

  (define (interp-binop binop reg x regfile memory)
    (if (symbol=? binop `+)
        (if (int32? x)
            (+ (dict-ref regfile reg) x)
            (+ (dict-ref regfile reg) (interp-loc x regfile memory)))
        (if (int32? x)
            (* (dict-ref regfile reg) x)
            (* (dict-ref regfile reg) (interp-loc x regfile memory)))))

  (define (jump-if relop pcaddr regfile pcount flag)
    (cond [(symbol=? relop `<) (if (= 1 (first flag))
                                   pcaddr
                                   pcount)]
          [(symbol=? relop `<=) (if (or (= 1 (first flag)) (= 1 (second flag)))
                                    pcaddr
                                    pcount)]
          [(symbol=? relop `=) (if (= 1 (second flag))
                                   pcaddr
                                   pcount)]
          [(symbol=? relop `!=) (if (= 0 (second flag))
                                   pcaddr
                                   pcount)]
          [(symbol=? relop `>=) (if (or (= 1 (second flag)) (= 1 (third flag)))
                                    pcaddr
                                    pcount)]
          [(symbol=? relop `>) (if (= 1 (third flag))
                                   pcaddr
                                   pcount)]))
                                   
    
  
  (define (trg? trg)
    (or (register? trg) (pc-addr? trg)))

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

  (define (loop c regfile memory pcount flag)
    (cond [(= pcount (length c)) (dict-ref regfile `rax 0)]
          [else (let-values ([(regfile memory pcount flag)
                              (interp-s (list-ref c pcount)
                                        regfile memory pcount flag)])
                  (loop c regfile memory pcount flag))]))

  (loop c empty empty 0 '(0 0 0)))


