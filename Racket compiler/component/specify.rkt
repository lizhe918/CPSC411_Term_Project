#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require "../utils/utils.rkt")
(provide (all-defined-out))

(define (specify-representation p)
  (debug-log p "specify")

  (define t-ptr (current-true-ptr))
  (define f-ptr (current-false-ptr))
  (define w-size (current-word-size-bytes))
  
  (define (primop? op)
    (ormap (λ (o) (eq? o op)) 
           `(unsafe-fx* 
             unsafe-fx+ 
             unsafe-fx- 
             eq? 
             unsafe-fx< 
             unsafe-fx<=
             unsafe-fx>
             unsafe-fx>=
             fixnum?
             boolean?
             empty?
             void?
             ascii-char?
             error?
             not
             pair?
             vector?
             procedure?
             cons
             unsafe-car
             unsafe-cdr
             unsafe-make-vector
             unsafe-vector-length
             unsafe-vector-set!
             unsafe-vector-ref
             make-procedure
             unsafe-procedure-arity
             unsafe-procedure-label
             unsafe-procedure-ref
 	 	         unsafe-procedure-set!)))
          
  (define (try-shift-right e)
    (if (int61? e)
        (arithmetic-shift e (- (current-fixnum-shift)))
        `(arithmetic-shift-right ,e ,(current-fixnum-shift))))

  (define (try-set-vector-index e)
    (if (int61? e)
        (+ (* (try-shift-right e) (current-vector-base-displacement))
           (- (current-vector-base-displacement) (current-vector-tag)))
        `(+ (* ,(try-shift-right e) ,(current-vector-base-displacement))
            ,(- (current-vector-base-displacement) (current-vector-tag)))))

  (define (try-set-procedure-index e)
    (if (int61? e)
        (+ (* (try-shift-right e) (current-procedure-arity-displacement))
           (- (current-procedure-environment-displacement) (current-procedure-tag)))
        `(+ (* ,(try-shift-right e) ,(current-procedure-arity-displacement))
            ,(- (current-procedure-environment-displacement) (current-procedure-tag)))))

  (define (specify-v v)
    (match v
      ['#t t-ptr]
      ['#f f-ptr]
      ['empty (current-empty-tag)]
      [`(void) (current-void-tag)]
      [`(error ,code) (current-error-tag)
       (bitwise-ior (arithmetic-shift code (current-error-shift))
                    (current-error-tag))]
      [ascii-char 
       #:when (ascii-char-literal? ascii-char)
       (bitwise-ior (arithmetic-shift (char->integer ascii-char) (current-ascii-char-shift)) 
                    (current-ascii-char-tag))]
      [fixnum 
       #:when (int61? fixnum)
       (bitwise-ior (arithmetic-shift fixnum (current-fixnum-shift)) 
                    (current-fixnum-tag))]
      [_ v]))

  (define (specify-primop o e)
    (let ([new-e (map specify-e e)])
      (match o
        ['fixnum?
         `(if (= (bitwise-and ,@new-e ,(current-fixnum-mask)) ,(current-fixnum-tag)) ,t-ptr ,f-ptr)]
        ['boolean? 
         `(if (= (bitwise-and ,@new-e ,(current-boolean-mask)) ,(current-boolean-tag)) ,t-ptr ,f-ptr)]
        ['empty? 
         `(if (= (bitwise-and ,@new-e ,(current-empty-mask)) ,(current-empty-tag)) ,t-ptr ,f-ptr)]
        ['void?
         `(if (= (bitwise-and ,@new-e ,(current-void-mask)) ,(current-void-tag)) ,t-ptr ,f-ptr)]
        ['ascii-char?
         `(if (= (bitwise-and ,@new-e ,(current-ascii-char-mask)) ,(current-ascii-char-tag)) ,t-ptr ,f-ptr)]
        ['error?
         `(if (= (bitwise-and ,@new-e ,(current-error-mask)) ,(current-error-tag)) ,t-ptr ,f-ptr)]
        ['pair? 
         `(if (= (bitwise-and ,@new-e ,(current-pair-mask)) ,(current-pair-tag)) ,t-ptr ,f-ptr)]
        ['vector?
         `(if (= (bitwise-and ,@new-e ,(current-vector-mask)) ,(current-vector-tag)) ,t-ptr ,f-ptr)]
        ['procedure?
         `(if (= (bitwise-and ,@new-e ,(current-procedure-mask)) ,(current-procedure-tag)) ,t-ptr ,f-ptr)]
        ; ['not `(not ,@new-e)]
        ['not
         `(if (!= ,@new-e ,f-ptr) ,f-ptr ,t-ptr)]
        ['unsafe-fx* 
         (if (int61? (first new-e))
             `(* ,(try-shift-right (first new-e)) ,(second new-e))
             `(* ,(first new-e) ,(try-shift-right (second new-e))))]
        ['unsafe-fx+ `(+ ,@new-e)]
        ['unsafe-fx- `(- ,@new-e)]
        ['eq? `(if (= ,@new-e) ,t-ptr ,f-ptr)]
        ['unsafe-fx< `(if (< ,@new-e) ,t-ptr ,f-ptr)]
        ['unsafe-fx<= `(if (<= ,@new-e) ,t-ptr ,f-ptr)]
        ['unsafe-fx> `(if (> ,@new-e) ,t-ptr ,f-ptr)]
        ['unsafe-fx>= `(if (>= ,@new-e) ,t-ptr ,f-ptr)]
        ['cons
         (let ([tmp (fresh)])
           `(let ((,tmp (+ (alloc ,(current-pair-size)) ,(current-pair-tag))))
              (begin
                (mset! ,tmp ,(- (current-car-displacement) (current-pair-tag)) ,(first new-e))
                (mset! ,tmp ,(- (current-cdr-displacement) (current-pair-tag)) ,(second new-e))
                ,tmp)))]
        ['unsafe-car `(mref ,@new-e ,(- (current-car-displacement) (current-pair-tag)))]
        ['unsafe-cdr `(mref ,@new-e ,(- (current-cdr-displacement) (current-pair-tag)))]
        ['unsafe-make-vector
         (let ([tmp (fresh)])
           `(let ((,tmp (+ (alloc (* (+ 1 ,(try-shift-right (first new-e))) ,(current-vector-base-displacement))) ,(current-vector-tag))))
              (begin
                (mset! ,tmp ,(- (current-vector-length-displacement) (current-vector-tag)) ,@new-e)
                ,tmp)))]
        ['unsafe-vector-length
         `(mref ,@new-e ,(- (current-vector-length-displacement) (current-vector-tag)))]
        ['unsafe-vector-set! 
         `(mset! ,(first new-e) ,(try-set-vector-index (second new-e)) ,(third new-e))]
        ['unsafe-vector-ref 
         `(mref ,(first new-e) ,(try-set-vector-index (second new-e)))]
        ['make-procedure
         (let* ([tmp (fresh)]
                [e-size (try-shift-right (third new-e))]
                [t-size (if (int61? e-size) (+ 2 e-size) `(+ 2 ,e-size))]
                [offset (if (int61? t-size) 
                            (* t-size (current-procedure-arity-displacement))
                            `(* ,t-size ,(current-procedure-arity-displacement)))])
           `(let ((,tmp (+ (alloc ,offset) ,(current-procedure-tag))))
              (begin
                (mset! ,tmp ,(- (current-procedure-label-displacement) (current-procedure-tag)) ,(first new-e))
                (mset! ,tmp ,(- (current-procedure-arity-displacement) (current-procedure-tag)) ,(second new-e))
                ,tmp)))]
        ['unsafe-procedure-arity
         `(mref ,@new-e ,(- (current-procedure-arity-displacement) (current-procedure-tag)))]
        ['unsafe-procedure-label
         `(mref ,@new-e ,(- (current-procedure-label-displacement) (current-procedure-tag)))]
        ['unsafe-procedure-ref
         `(mref ,(first new-e) ,(try-set-procedure-index (second new-e)))]
        ['unsafe-procedure-set!
         `(mset! ,(first new-e) ,(try-set-procedure-index (second new-e)) ,(third new-e))])))

  (define (specify-assign a)
    (map (λ (a) `(,(first a) ,(specify-e (second a)))) a))

  (define (specify-pred p)
    (match p
      [`(true) `(true)]
      [`(false) `(false)]
      [`(not ,pred) `(not ,(specify-pred pred))]
      [`(let ,assign ,pred) 
       `(let ,(specify-assign assign) ,(specify-pred pred))]
      [`(if ,pred ,pred1 ,pred2)
       `(if ,(specify-pred pred) ,(specify-pred pred1) ,(specify-pred pred2))]
      [_ `(!= ,(specify-e p) ,(current-false-ptr))]))

  (define (specify-effect e)
    (match e
      [`(begin ,effect ...)
       `(begin ,@(map specify-effect effect))]
      [`(,primop ,e ...)
       #:when (primop? primop)
       (specify-primop primop e)]))

  (define (specify-e e)
    (match e
      [`(call ,e ...) 
       `(call ,@(map specify-e e))]
      [`(let ,assign ,e) 
       `(let ,(specify-assign assign) ,(specify-e e))]
      [`(begin ,effect ... ,e)
       `(begin ,@(map specify-effect effect) ,(specify-e e))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(specify-pred pred) ,(specify-e e1) ,(specify-e e2))]
      [`(,primop ,e ...)
       #:when (primop? primop)
       (specify-primop primop e)]
      [_ (specify-v e)]))

  (define (specify-b b)
    (match b
      [`(define ,label (lambda (,aloc ...) ,e))
       `(define ,label (lambda (,@aloc) ,(specify-e e)))]))
       
  (define (specify-p p)
    (match p
      [`(module ,b ... ,e)
       `(module ,@(map specify-b b) ,(specify-e e))]))
    
  (specify-p p))