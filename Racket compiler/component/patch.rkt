#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require "../utils/utils.rkt")
(provide (all-defined-out))


; input: para-asm-lang-v6
; output: paren-x64-fvars-v6
; purpose: add location and triv so that we can write the code more flexiblly with
;          registers, physical locations and integer
(define (patch-instructions p)
  (debug-log p "patch")
  
  (define a-reg-1 (first (current-auxiliary-registers)))
  (define a-reg-2 (second (current-auxiliary-registers)))
  (define cbr (current-frame-base-pointer-register))

  (define (reverse-relop relop)
    (match relop
      ['> '<=] ['>= '<] ['< '>=] ['<= '>] ['= '!=] ['!= =]))

  (define (binop? op)
    (ormap (λ (o) (eq? o op))
           `(* + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right)))
        
  (define (location? loc)
    (or (register? loc) (address? loc)))

  (define (opand? opand)
    (or (int64? opand) (location? opand)))

  (define (address? s)
    (match s
      [`(,cbr - ,offset)
       #:when (dispoffset? offset)
       #t]
      [_ #f]))

  (define (addr-not-eq? a)
    (if (andmap address? a)
        (not (check-duplicates (map third a)))
        #t))

  (define (trivial? s)
    (or (opand? s) (int64? s)))
    
  (define (patch-instructions-s s)
    (match s

      [`(set! ,loc1 (mref ,loc2 ,index))
       (cond [(and (address? loc1) (address? loc2) (address? index))
              `((set! ,a-reg-1 ,loc2)
                (set! ,a-reg-2 ,index)
                (set! ,a-reg-1 (+ ,a-reg-1 ,a-reg-2))
                (set! ,a-reg-2 ,loc1)
                (set! ,a-reg-2 (mref ,a-reg-1 0))
                (set! ,loc1 ,a-reg-2))]
             [(and (address? loc1) (address? index))
              `((set! ,a-reg-1 ,loc1)
                (set! ,a-reg-2 ,index)
                (set! ,a-reg-1 (mref ,loc2 ,a-reg-2))
                (set! ,loc1 ,a-reg-1))]
             [(and (address? loc1) (address? loc2))
              `((set! ,a-reg-1 ,loc1)
                (set! ,a-reg-2 ,loc2)
                (set! ,a-reg-1 (mref ,a-reg-2 ,index))
                (set! ,loc1 ,a-reg-1))]
             [(and (address? loc2) (address? index))
              `((set! ,a-reg-1 ,loc2)
                (set! ,a-reg-2 ,index)
                (set! ,loc1 (mref ,a-reg-1 ,a-reg-2)))]
             [(address? loc1)
              `((set! ,a-reg-1 ,loc1)
                (set! ,a-reg-1 (mref ,loc2 ,index))
                (set! ,loc1 ,a-reg-1))]
             [(address? loc2)
              `((set! ,a-reg-2 ,loc2)
                (set! ,loc1 (mref ,a-reg-2 ,index)))]
             [(address? index)
              `((set! ,a-reg-2 ,index)
                (set! ,loc1 (mref ,loc2 ,a-reg-2)))]
             [#t
              `((set! ,loc1 (mref ,loc2 ,index)))])]
      [`(mset! ,loc ,index ,triv)
       (cond [(and (address? loc) (address? triv) (address? index))
              `((set! ,a-reg-1 ,loc)
                (set! ,a-reg-2 ,index)
                (set! ,a-reg-1 (+ ,a-reg-1 ,a-reg-2))
                (set! ,a-reg-2 ,triv)
                (mset! ,a-reg-1 0 ,a-reg-2))]
             [(and (address? loc) (address? triv))
              `((set! ,a-reg-1 ,triv)
                (set! ,a-reg-2 ,loc)
                (mset! ,a-reg-2 ,index ,a-reg-1))]
             [(and (address? loc) (address? index))
              `((set! ,a-reg-1 ,loc)
                (set! ,a-reg-2 ,index)
                (mset! ,a-reg-1 ,a-reg-2 ,triv))]
             [(and (address? triv) (address? index))
              `((set! ,a-reg-1 ,triv)
                (set! ,a-reg-2 ,index)
                (mset! ,loc ,a-reg-2 ,a-reg-1))]
             [(address? triv)
              `((set! ,a-reg-2 ,triv)
                (mset! ,loc ,index ,a-reg-2))]
             [(address? index)
              `((set! ,a-reg-1 ,index)
                (mset! ,loc ,a-reg-1 ,triv))]
             [(address? loc)
              `((set! ,a-reg-1 ,loc)
                (mset! ,a-reg-1 ,index ,triv))]
             [#t
              `((mset! ,loc ,index ,triv))])]
      [`(set! ,loc (,binop ,loc ,opand))
       #:when (and (location? loc) (binop? binop) (opand? opand))
       (cond [(and (register? loc) (int32? opand))
              `((set! ,loc (,binop ,loc ,opand)))]
             [(and (register? loc) (or (address? opand) (register? opand)))
              `((set! ,loc (,binop ,loc ,opand)))]
             [(and (register? loc) (opand? opand))
              `((set! ,a-reg-2 ,opand)
                (set! ,loc (,binop ,loc ,a-reg-2)))]
             [(and (address? loc) (int32? opand))
              `((set! ,a-reg-1 ,loc)
                (set! ,a-reg-1 (,binop ,a-reg-1 ,opand))
                (set! ,loc ,a-reg-1))]
             [(and (address? loc) (or (address? opand) (register? opand)))
              `((set! ,a-reg-1 ,loc)
                (set! ,a-reg-1 (,binop ,a-reg-1 ,opand))
                (set! ,loc ,a-reg-1))]
             [(and (address? loc) (opand? opand))
              `((set! ,a-reg-1 ,loc)
                (set! ,a-reg-2 ,opand)
                (set! ,a-reg-1 (,binop ,a-reg-1 ,a-reg-2))
                (set! ,loc ,a-reg-1))])]
      [`(set! ,loc ,triv)
       (cond [(int32? triv)
              `((set! ,loc ,triv))]
             [(int64? triv)
              `((set! ,a-reg-1 ,triv)
                (set! ,loc ,a-reg-1))]
             [(and (register? loc) (or (register? triv) (address? triv) (label? triv)))
              `((set! ,loc ,triv))]
             [(and (register? loc) (opand? triv))
              `((set! ,a-reg-1 ,triv)
                (set! ,loc ,a-reg-1))]
             [(and (address? loc) (or (opand? triv) (label? triv)))
              `((set! ,a-reg-1 ,triv)
                (set! ,loc ,a-reg-1))])]

      ; deal with compare
      [`(compare ,loc ,opand)
       #:when (and (location? loc) (opand? opand))
       (cond [(and (register? loc) (int64? opand))
              `((compare ,loc ,opand))]
             [(and (register? loc) (location? opand))
              (if (register? opand)
                  `((compare ,loc ,opand))
                  `((set! ,a-reg-1 ,opand)
                    (compare ,loc ,a-reg-1)))]
             [(and (address? loc) (int64? opand))
              `((set! ,a-reg-1 ,loc)
                (compare ,a-reg-1 ,opand))]
             [(and (address? loc) (location? opand))
              (if (register? opand)
                  `((set! ,a-reg-1 ,loc)
                    (compare ,a-reg-1 ,opand))
                  `((set! ,a-reg-2 ,opand)
                    (set! ,a-reg-1 ,loc)
                    (compare ,a-reg-1 ,a-reg-2)))])]
      
      ; deal with jump
      [`(jump ,trg)
       (cond [(label? trg) 
              `((jump ,trg))]
             [(register? trg) 
              `((jump ,trg))]
             [else 
              `((set! ,a-reg-1 ,trg)
                (jump ,a-reg-1))])]
      [`(jump-if ,relop ,trg)
       (cond [(label? trg) 
              `((jump-if ,relop ,trg))]
             [(register? trg) 
              (let ([tmp (fresh-label)])
                `((jump-if ,(reverse-relop relop) ,tmp)
                  (jump ,trg)
                  (with-label ,tmp (set! ,a-reg-1 ,a-reg-1))))]
             [(address? trg) 
              (let ([tmp (fresh-label)])
                `((set! ,a-reg-1 ,trg)
                  (jump-if ,(reverse-relop relop) ,tmp)
                  (jump ,a-reg-1)
                  (with-label ,tmp (set! ,a-reg-1 ,a-reg-1))))])]

      ; deal with with-label 
      [`(with-label ,label ,s)
       (let ([ps (patch-instructions-s s)])
         `((with-label ,label ,(first ps))
           ,@(rest ps)))]

      [_ (list s)]))

  (match p
    [`(begin ,s ...)
     `(begin ,@(foldr append `() (map patch-instructions-s s)))]))
