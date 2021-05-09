#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require "../utils/utils.rkt")
(provide (all-defined-out))

(define (implement-safe-primops p)
  (debug-log p "impl-safe")

  (define b-op `())

  (define (primop? op)
    (ormap (λ (o) (eq? o op)) 
           `(* 
             + 
             - 
             < 
             <= 
             > 
             >=
             eq?
             cons
             vector-set!
             vector-ref
             fixnum?
             boolean?
             empty?
             void?
             ascii-char?
             error?
             not
             pair?
             procedure?
             vector?
             car
             cdr
             make-vector
             vector-length
             procedure-arity)))

  (define (unop? op)
    (ormap (λ (o) (eq? o op))
           `()))

  (define (extract-label b)
    (match b
      [`(define ,label ,body) label]))

  (define (opfun-check arg req exec err-code)
    (foldl (λ (p body) `(if ,p ,body (error ,err-code)))
           exec
           (map list req arg)))

  (define (make-opfun o arg body)
    `(define ,(fresh o) (lambda (,@arg) ,body)))

  (define (triop-body o var)
    (match o
      ['vector-set!
       (let* ([tmp1 (fresh)]
              [tmp2 (fresh)]
              [tmp3 (fresh)]
              [l-vector-set (fresh 'unsafe-vector-set!)]
              [vector-set
               `(define ,l-vector-set (lambda (,tmp1 ,tmp2 ,tmp3)
                  (if (unsafe-fx< ,tmp2 (unsafe-vector-length ,tmp1))
                      (if (unsafe-fx>= ,tmp2 0)
                          (begin (unsafe-vector-set! ,tmp1 ,tmp2 ,tmp3) (void))
                          (error 10))
                      (error 10))))])
         (set! b-op (dict-set b-op 'unsafe-vector-set! vector-set))
         (make-opfun o var (opfun-check (ex-last var) `(vector? fixnum?) `(call ,l-vector-set ,@var) 9)))]))

  (define (binop-body o var)
    (match o
      ['* 
       (make-opfun o var (opfun-check var `(fixnum? fixnum?) `(unsafe-fx* ,@var) 0))]
      ['+ 
       (make-opfun o var (opfun-check var `(fixnum? fixnum?) `(unsafe-fx+ ,@var) 1))]
      ['- 
       (make-opfun o var (opfun-check var `(fixnum? fixnum?) `(unsafe-fx- ,@var) 2))]
      ['eq? (make-opfun o var `(eq? ,@var))]
      ['< 
       (make-opfun o var (opfun-check var `(fixnum? fixnum?) `(unsafe-fx< ,@var) 3))]
      ['<= 
       (make-opfun o var (opfun-check var `(fixnum? fixnum?) `(unsafe-fx<= ,@var) 4))]
      ['> 
       (make-opfun o var (opfun-check var `(fixnum? fixnum?) `(unsafe-fx> ,@var) 5))]
      ['>= 
       (make-opfun o var (opfun-check var `(fixnum? fixnum?) `(unsafe-fx>= ,@var) 6))]
      ['cons (make-opfun o var `(cons ,@var))]
      ['vector-ref
       (let* ([tmp1 (fresh)]
              [tmp2 (fresh)]
              [l-vector-ref (fresh 'unsafe-vector-ref)]
              [vector-ref
               `(define ,l-vector-ref (lambda (,tmp1 ,tmp2)
                  (if (unsafe-fx< ,tmp2 (unsafe-vector-length ,tmp1))
                      (if (unsafe-fx>= ,tmp2 0)
                          (unsafe-vector-ref ,tmp1 ,tmp2)
                          (error 11))
                      (error 11))))])
         (set! b-op (dict-set b-op 'unsafe-vector-ref vector-ref))
         (make-opfun o var (opfun-check var `(vector? fixnum?) `(call ,l-vector-ref ,@var) 10)))]
      [_ (triop-body o `(,@var ,(fresh)))]))

  (define (unop-body o var)
    (match o
      ['fixnum? (make-opfun o var `(fixnum? ,@var))]
      ['boolean? (make-opfun o var `(boolean? ,@var))]
      ['empty? (make-opfun o var `(empty? ,@var))]
      ['void? (make-opfun o var `(void? ,@var))]
      ['ascii-char? (make-opfun o var `(ascii-char? ,@var))]
      ['error? (make-opfun o var `(error? ,@var))]
      ['pair? (make-opfun o var `(pair? ,@var))]
      ['vector? (make-opfun o var `(vector? ,@var))]
      ['procedure? (make-opfun o var `(procedure? ,@var))]
      ['not (make-opfun o var `(not ,@var))]
      ['car
       (make-opfun o var (opfun-check var `(pair?) `(unsafe-car ,@var) 11))]
      ['cdr
       (make-opfun o var (opfun-check var `(pair?) `(unsafe-cdr ,@var) 12))]
      ['make-vector
       (let* ([tmp1 (fresh)]
              [tmp2 (fresh)]
              [len (fresh 'len)]
              [i (fresh 'i)]
              [vec (fresh 'vec)]
              [l-make-init (fresh 'make-init-vector)]
              [l-init-loop (fresh 'vector-init-loop)]
              [make-init
               `(define ,l-make-init (lambda (,tmp1)
                  (let ((,tmp2 (unsafe-make-vector ,tmp1)))
                     (call ,l-init-loop ,tmp1 0 ,tmp2))))]
              [init-loop
               `(define ,l-init-loop (lambda (,len ,i ,vec)
                  (if (eq? ,len ,i)
                      ,vec
                      (begin
                        (unsafe-vector-set! ,vec ,i 0)
                        (call ,l-init-loop ,len (unsafe-fx+ ,i 1) ,vec)))))])
         (set! b-op (dict-set b-op 'vector-init-loop init-loop))
         (set! b-op (dict-set b-op 'make-init-vector make-init))
         (make-opfun o var (opfun-check var `(fixnum?) `(call ,l-make-init ,@var) 7)))]
      ['vector-length
       (make-opfun o var (opfun-check var `(vector?) `(unsafe-vector-length ,@var) 8))]
      ['procedure-arity
       (make-opfun o var (opfun-check var `(procedure?) `(unsafe-procedure-arity ,@var) 25))]
      [_ (binop-body o `(,@var ,(fresh)))]))

  (define (find-op-b o)
    (if (dict-has-key? b-op o)
        (dict-ref b-op o)
        (let ([new-op-fun (unop-body o `(,(fresh)))])
          (begin 
            (set! b-op (dict-set b-op o new-op-fun))
            (find-op-b o)))))
             
  (define (impl-assign a)
    (map (λ (a) `(,(first a) ,(impl-e (second a)))) a))

  (define (impl-v v)
    (match v
      [`(lambda (,aloc ...) ,e)
       `(lambda (,@aloc) ,(impl-e e))]
      [op
       #:when (primop? op)
       (extract-label (find-op-b op))]
      [_ v]))

  (define (impl-e e)
    (match e
      ; [`(call eq? ,e ...)
      ;  `(eq? ,@(map impl-e e))]
      ; [`(call procedure? ,e ...)
      ;  `(procedure? ,@(map impl-e e))]
      [`(call ,e ...) 
       `(call ,@(map impl-e e))]
      [`(let ,assign ,e) 
       `(let ,(impl-assign assign) ,(impl-e e))]
      [`(if ,e ,e1 ,e2)
       `(if ,(impl-e e) ,(impl-e e1) ,(impl-e e2))]
      [_ (impl-v e)]))

  (define (impl-b b)
    (match b
      [`(define ,f-aloc (lambda (,aloc ...) ,e))
       `(define ,f-aloc (lambda (,@aloc) ,(impl-e e)))]))

  (define (impl-p p)
    (match p
      [`(module ,b ... ,e)
       (let* ([new-e (impl-e e)]
              [new-b (map impl-b b)])
         `(module ,@(dict-values b-op) ,@new-b ,new-e))]))
  
  (impl-p p))