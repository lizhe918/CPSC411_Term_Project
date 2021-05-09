#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(provide (all-defined-out))
(require "../utils/utils.rkt")

;; Signature: any/c -> Exprs-lang-v7
;; Purpose: Takes an arbitrary value and either returns it,
;; if it is a valid Values-lang v3 program, or raises an error with a descriptive error message.
(define (check-exprs-lang p)

  (define primitive-funs
    `((fixnum? . 1)
      (boolean? . 1)
      (empty? . 1)
      (void? . 1)
      (ascii-char? . 1)
      (error? . 1)
      (not . 1)
      (* . 2)
      (+ . 2)
      (- . 2)
      (eq? . 2)
      (< . 2)
      (<= . 2)
      (>= . 2)
      (> . 2)
      (pair? . 1)
      (procedure? . 1)
      (vector? . 1)
      (cons . 2)
      (car . 1)
      (cdr . 1)
      (make-vector . 1)
      (vector-length . 1)
      (vector-set! . 3)
      (vector-ref . 2)
      (procedure-arity . 1)))
  
  (define-struct env (funs vars))

  (define (check-p p)
    (displayln "p")
    (displayln p)
    (match p
      [`(module ,bs ... ,e)
       (let ([funs (for/fold ([funs primitive-funs])
                             ([b bs])
                     (collect-funs b funs))])
         `(module ,@(for/list ([b bs])
                      (check-b b (make-env funs empty)))
            ,(check-e e (make-env funs empty))))]
      [else (error "Invalid program: not matched with `(module ,bs ... ,e)")]))

  (define (collect-funs b funs)
    (displayln "b")
    (displayln b)
    (match b
      [`(define ,x (lambda (,xs ...) ,e))
       (if (dict-has-key? funs x)
           (dict-set funs x (dict-ref funs x)) 
           (dict-set funs x (length xs)))]
      [else (error "Invalid function def: not matched with `(define ,x (lambda (,xs ...) ,e))")]))

  (define (check-b b env)
    (displayln "b")
    (displayln b)
    (match b
      [`(define ,x (lambda (,xs ...) ,e))
       `(define ,(check-fname x)
          (lambda ,(for/list ([x xs])
                     (check-x x))
            ,(check-e e (make-env (for/fold
                                   ([funs (env-funs env)])
                                   ([x xs])        
                                    (dict-set (dict-remove funs x) x 0))
                                  xs))))]
      [else (error "Invalid function def: not matched with `(define ,x (lambda (,xs ...) ,e))")]))

  (define (check-e e env)
    (displayln "e")
    (displayln e)
    (match e
      [`(call ,e^ ,es ...)
       (match e^
         [`(lambda (,xs ...) ,e)
          `(call (lambda ,(for/list ([x xs])
                    (check-x x))
          ,(check-e e (make-env (for/fold
                                   ([funs (env-funs env)])
                                   ([x xs])        
                                    (dict-set (dict-remove funs x) x 0))
                                  (append xs (env-vars env)))))
                 ,@(for/list ([e es])
                     (check-e e env)))]
         [e^
          (if (dict-has-key? (env-funs env) e^)
           e
           (error "Invalid function call"))])]
;      [`(let ([,xs ,es] ...) ,e)
;       `(let ,(for/list ([x xs]
;                         [e es])
;                `(,(check-x x) ,(check-e e env)))
;          ,(check-e e (make-env (for/fold ([funs (env-funs env)])
;                                          ([x xs])
;                                  (dict-set funs x 0))
;                                (append (env-vars env) xs))))]
      [`(let ([,xs ,es] ...) ,e)
       (let ([funs (for/fold ([funs (env-funs env)])
                             ([x xs]
                              [e es])
                     (if (dict-has-key? funs e)
                         (dict-set (dict-remove funs x) x (dict-ref funs e))
                         (dict-remove funs x)))])
         `(let ,(for/fold ([acc empty])
                          ([x xs]
                           [e es])
                  (append acc (list `(,(check-x x) ,(if (dict-has-key? funs e)
                                                        e
                                                        (check-e e env))))))
            ,(check-e e (make-env funs (append (env-vars env) xs)))))]
      [`(if ,e1 ,e2 ,e3)
       (display (env-funs env))
       `(if ,(check-e e1 env)
            ,(check-e e2 env)
            ,(check-e e3 env))]
      [else (check-v e env)]))
  

  (define (check-v v env)
    (displayln "v")
    (displayln v)
    (match v
      [#t
       v]
      [#f
       v]
      [`empty
       v]
      [`(void)
       v]
      [`(error ,uint8)
       (if (uint8? uint8)
           v
           (error "Invalid error: code is not uint8"))]
      [`(lambda (,xs ...) ,e)
       `(lambda ,(for/list ([x xs])
                    (check-x x))
          ,(check-e e (make-env (for/fold
                                   ([funs (env-funs env)])
                                   ([x xs])        
                                    (dict-set (dict-remove funs x) x 0))
                                  (append xs (env-vars env)))))]
      [v
       #:when (fixnum? v)
       v]
      [v
       #:when (x? v)
       (if (member v (env-vars env))
           (check-x v)
           (error "Unbound Identifier!"))]
      [v
       #:when (ascii-char-literal? v)
       v]
      [else (error "Invalid value: no match case")]))

  (define (check-x x)
    (cond [(name? x) x]
          [(prim-f? x) x]
          [else (error "Invalid x: not name? or prim-f?")]))
                

  (define (check-fname x)
    (cond [(name? x) x]
          [(prim-f? x) x]
          [else (error "Invalid function name: not name? or prim-f?")]))

  (define (check-para x names)
    (if (member x names)
        (error "Invalid parameter: duplication")
        (cond [(name? x) x]
              [(prim-f? x) x]
              [else (error "Invalid parameter: not name? or prim-f?")])))

  (define (x? x)
    (or (name? x)
        (prim-f? x)))
  
  (define (prim-f? x)
    (or (binop? x) (unop? x)))

  (define (binop? x)
    (member x (list '* '+ '- 'eq? '< '<= '> '>=)))

  (define (unop? x)
    (member x (list 'fixnum?
                    'boolean?
                    'empty?
                    'void?
                    'ascii-char?
                    'error?
                    'not)))

  (check-p p))


