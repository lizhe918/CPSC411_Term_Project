#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require rackunit)
(require "../utils/utils.rkt")

(provide (all-defined-out))

; paren-x64-v7? -> (and/c string? x64-instructions?)
; Compile the Paren-x64 v7 program into a valid sequence of x64 instructions, represented as a string.

(define (generate-x64 p)

  (define (program->x64 p x64)
    (match p
      [`(begin ,ss ...)
       (for/fold ([x64 x64])
                 ([s ss])
         (statement->x64 s x64))]))

  (define (statement->x64 s x64)
    (match s
      [`(set! ,reg ,triv)
       #:when (and (register? reg) (trivial? triv))
       (string-append x64 "mov " (symbol->string reg) ", " (triv->x64 triv) "\n")]

      [`(set! ,reg ,loc)
       #:when (and (register? reg) (location? loc))
       (string-append x64 "mov " (symbol->string reg) ", " (loc->x64 loc) "\n")]

      [`(set! ,reg1 (,binop ,reg1 ,int32))
       #:when (and (register? reg1) (binop? binop) (int32? int32))
       (string-append x64 (binop->ins binop) (symbol->string reg1) ", " (number->string int32) "\n")]

      [`(set! ,reg1 (,binop ,reg1 ,loc))
       #:when (and (register? reg1) (binop? binop) (location? loc))
       (string-append x64 (binop->ins binop) (symbol->string reg1) ", " (loc->x64 loc) "\n")]

      [`(set! ,addr ,int32)
       #:when (and (address? addr) (int32? int32))
       (string-append x64 "mov " (addr->string addr) ", " (number->string int32) "\n")]

      [`(set! ,addr ,trg)
       #:when (and (address? addr) (trg? trg))
       (string-append x64 "mov " (addr->string addr) ", " (trg->x64 trg) "\n")]

      [`(with-label ,label ,s)
       #:when (label? label)
       (string-append x64 (sanitize-label label) ":\n" (statement->x64 s ""))]

      [`(jump ,trg)
       #:when (trg? trg)
       (string-append x64 "jmp " (trg->x64 trg) "\n")]

      [`(compare ,reg ,opand)
       #:when (and (register? reg) (opand? opand))
       (string-append x64 "cmp " (symbol->string reg) ", " (opand->string opand) "\n")]

      [`(jump-if ,relop ,label)
       (string-append x64 (relop->string relop) (sanitize-label label) "\n")]

      [else "undefined"]))


  (define (address? s)
    (cond [(and (frame-base-pointer-register? (first s))
                (equal? '- (second s))
                (dispoffset? (third s)))
           true]
          [(and (register? (first s))
                (equal? '+ (second s))
                (int32? (third s)))
           true]
          [(and (register? (first s))
                (equal? '+ (second s))
                (register? (third s)))
           true]
          [else false]))

  (define (addr->string addr)
    (string-append "QWORD "
                   "[" (symbol->string (first addr))
                   " " (symbol->string (second addr)) " "
                   (if (int32? (third addr))
                       (number->string (third addr))
                       (symbol->string (third addr))) "]"))

  (define (trg? trg)
    (or (register? trg) (label? trg)))

  (define (location? s)
    (or (register? s) (address? s)))

  (define (loc->x64 loc)
    (if (register? loc)
        (symbol->string loc)
        (addr->string loc)))

  (define (trivial? s)
    (or (trg? s) (int64? s)))

  (define (binop? s)
    (or (equal? s '*)
        (equal? s '+)
        (equal? s '-)
        (equal? s 'bitwise-and)
        (equal? s 'bitwise-ior)
        (equal? s 'bitwise-xor)
        (equal? s 'arithmetic-shift-right)))

  (define (triv->x64 triv)
    (if (trg? triv)
        (trg->x64 triv)
        (number->string triv)))

  (define (trg->x64 trg)
    (if (register? trg)
        (symbol->string trg)
        (sanitize-label trg)))


  (define (binop->ins b)
    (cond [(equal? b '*) "imul "]
          [(equal? b '+) "add "]
          [(equal? b '-) "sub "]
          [(equal? b 'bitwise-and) "and "]
          [(equal? b 'bitwise-ior) "or "]
          [(equal? b 'bitwise-xor) "xor "]
          [(equal? b 'arithmetic-shift-right) "sar "]))

  (define (opand? o)
    (or (int64? o)
        (register? o)))

  (define (opand->string o)
    (if (int64? o)
        (number->string o)
        (symbol->string o)))

  (define (relop->string relop)
    (cond [(symbol=? relop `<) "jl "]
          [(symbol=? relop `<=) "jle "]
          [(symbol=? relop `=) "je "]
          [(symbol=? relop `!=) "jne "]
          [(symbol=? relop `>=) "jge "]
          [(symbol=? relop `>) "jg "]))

  (program->x64 p ""))

