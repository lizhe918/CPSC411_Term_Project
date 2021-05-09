#lang racket
(require
  cpsc411/compiler-lib
  cpsc411/ptr-run-time
  cpsc411/test-suite/utils
  cpsc411/langs/a9
  racket/match
  rackunit) 

(require "../compiler.rkt")

; input: exprs-lang v9
; output: int64
; purpose: Check if the output of exprs-lang programs matches the desired outputs. Also provide end-to-end tests which
; the result of executing compiled programs should be equal to the result of interpretation.

(define pass-list (current-pass-list))

(module+ test
    ; Some normal instructions
    (current-pass-list
    (list
    check-exprs-lang
    uniquify
    implement-safe-primops
    implement-safe-call
    define->letrec
    optimize-direct-calls
    dox-lambdas
    uncover-free
    convert-closures
    optimize-known-calls
    hoist-lambdas
    implement-closures
    specify-representation
    remove-complex-opera*
    sequentialize-let
    impose-calling-conventions
    canonicalize-bind
    select-instructions
    expose-allocation-pointer
    uncover-locals
    undead-analysis
    conflict-analysis
    assign-call-undead-variables
    allocate-frames
    assign-registers
    assign-frame-variables
    replace-locations
    optimize-predicates
    implement-fvars
    expose-basic-blocks
    resolve-predicates
    flatten-program
    patch-instructions
    implement-mops
    generate-x64
    wrap-x64-run-time
    wrap-x64-boilerplate))

    (current-run/read nasm-run/exit-code)


    (test-case
            "Call procedure?"
        (let ([x '(module (if (call procedure? (lambda () 5)) 4 6))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 4)))

    (test-case
            "Call procedure-arity"
        (let ([x '(module (call procedure-arity (lambda (x y z) 5)))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 3)))

    (test-case
            "Return lambda as value"
        (let ([x '(module (lambda () 5))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) (lambda () 5))))

    (test-case
            "call lambda as function"
        (let ([x '(module (call (lambda (x y) (call + x y)) 4 5))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 9)))

    (test-case
            "lambda as argument"
        (let ([x '(module (call (lambda (x y) (call x)) (lambda () 10) (lambda () 11)))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 10)))

    (test-case
            "bind lambda in let"
        (let ([x '(module (let ([x (lambda () 12)]) (call x)))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 12)))
    
    (test-case
            "lambda in let body"
        (let ([x '(module (let ([x 10]) (lambda () 12)))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) (lambda () 12))))

    (test-case
            "lambda as pred"
        (let ([x '(module (if (lambda () 5) 1 2))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 1)))

    (test-case
            "call lambda as pred"
        (let ([x '(module (if (call (lambda () #t)) 1 2))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 1)))

    (test-case
            "let lambda as pred"
        (let ([x '(module (if (let ([x (lambda (x y) (call <= x y))]) (call x 1 2)) 3 4))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 3)))

    (test-case
            "let lambda as pred"
        (let ([x '(module (if (let ([x 10]) (call (lambda (x y) (call <= x y)) x 2)) 3 4))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 4)))

    (test-case
            "lambda as if body"
        (let ([x '(module (if (true) (lambda () 5) (lambda () 6)))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) (lambda () 5))))

    (test-case
            "General case"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (call (lambda (x) (call procedure? x)))))
                    (if (call fun1 (lambda (x y) (call + x y)))
                        (procedure-arity fun1)
                        (error 5)))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 1)))


    (test-case
            "General case"
        (let ([x '(module
                    (define fun1
                        (lambda (x y z)
                            (let ([k (lambda () 5)] [q (lambda () 6)] [g (lambda (x y) (call * x y))]) 
                                (call g (call k) (call q)))))
                    (if (call procedure? fun1)
                        (call fun1 1 2 3)
                        (call (lambda () (error 5)))))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 30)))



    ;; ------------------------------------ old cases ------------------------------------------------
    (test-case
            "error case"
        (let ([x '(module (if (call eq? (call + 5 6) 11) 4 6))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 4)))

    (test-case
            "Call pair?"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call pair? x) 5 6)))
                    (call fun1 (call cons 1 2)))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 5)))


    (test-case
            "Call vector?"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call vector? x) 5 6)))
                    (call fun1 (call make-vector 1 2)))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 5)))

     (test-case
            "Call car"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call pair? x) (call car x) 6)))
                    (call fun1 (call cons 1 2)))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 1)))


    (test-case
            "Call cdr"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call pair? x) (call cdr x) 6)))
                    (call fun1 (call cons 1 2)))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 2)))
    ;; test
    (test-case
            "Call make-vector"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call fixnum? x) (call make-vector x) 6)))
                    (call fun1 5))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) (make-vector 5))))

    (test-case
            "Call vector-length"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call vector? x) (call vector-length x) 6)))
                    (call fun1 (call make-vector 10)))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 10)))


     (test-case
            "Call vector-set!"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call vector? x) (call vector-set! x 0 8) 6)))
                    (call fun1 (call make-vector 5)))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) (void))))


    (test-case
            "Call vector-ref"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call vector? x) (let ([y (call vector-set! x 0 8)]) (vector-ref x 0)) 6)))
                    (call fun1 (call make-vector 5)))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 8)))

    (test-case
            "ok to return prim-f"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                make-vector))
                    (call fun1 #f))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) make-vector)))

    (test-case
            "General case"
        (let ([x '(module
                    (define fun1
                        (lambda (x y z)
                            (if (call pair? x)
                                (call + (call car x) (call cdr x))
                                (if (call vector? x)
                                (call vector-length x)
                                (error 8)))))
                (call fun1 (call cons 4 5) 2 3))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 9)))

    (test-case
            "General case"
        (let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (let ([k (call vector-length (call car x))] [q (call cdr x)] [g (call make-vector (call + z y))]) 
                             (call vector-ref g (call + k q)))))
                 (call fun1 (cons (call make-vector 1) 3) 2 3))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 0)))


    ;; ------------------------------------------ old cases ------------------------------------------
    (test-case
            "Normal calling"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                x))
                    (call fun1 5))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 5)))

    (test-case
            "if statement with binop *"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (call * x 2)))
                    (call fun1 5))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 10)))


    (test-case
            "if statement with binop +"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (call + x 2)))
                    (call fun1 5))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 7)))

    (test-case
            "if statement with binop -"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (call - x 2)))
                    (call fun1 5))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 3)))

    (test-case
            "if statement with binop eq?"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call eq? x 5) x (call + x 2))))
                    (call fun1 5))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 5)))

    (test-case
            "if statement with binop <"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call < x 5) x (call + x 2))))
                    (call fun1 5))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 7)))

    (test-case
            "if statement with binop <="
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call <= x 5) x (call + x 2))))
                    (call fun1 5))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 5)))

    (test-case
            "if statement with binop >"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call > x 5) x (call + x 2))))
                    (call fun1 5))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 7)))

    (test-case
            "if statement with binop >="
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call >= x 5) x (call + x 2))))
                    (call fun1 5))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 5)))

    (test-case
            "if statement with unop fixnum?"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call fixnum? x) x (call + x 2))))
                    (call fun1 5))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 5)))

    (test-case
            "if statement with unop boolean?"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call boolean? x) x (call + x 2))))
                    (call fun1 #t))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) #t)))

    (test-case
            "if statement with unop empty?"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call empty? x) x (call + x 2))))
                    (call fun1 empty))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) empty)))

    (test-case
            "if statement with unop void?"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call void? x) x (call + x 2))))
                    (call fun1 (void)))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) (void))))

    (test-case
            "if statement with unop ascii-char?"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call ascii-char? x) x (call + x 2))))
                    (call fun1 #\c))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) #\c)))

    (test-case
            "if statement with unop error?"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call error? x) x (call + x 2))))
                    (call fun1 (error 5)))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) '(error 5))))

    (test-case
            "if statement with unop not -- treat anything not #f as #t"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call not x) x (call + x 2))))
                    (call fun1 5))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 7)))

    (test-case
            "if statement with unop not -- treat anything not #f as #t"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                (if (call not x) x (call + x 2))))
                    (call fun1 #f))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) #f)))

    ;; ----------------------------------------------------------------------

    (test-case
            "ok to return prim-f binop"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                eq?))
                    (call fun1 #f))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) eq?)))

    (test-case
            "ok to return prim-f unop"
        (let ([x '(module
                        (define fun1
                            (lambda (x)
                                fixnum?))
                    (call fun1 #f))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) fixnum?)))

    ;; ---------------------------------------------------------------------

    (test-case
            "General cases 1.1"
        (let ([x '(module
                        (define handler
                            (lambda (x)
                                (error 2)))
                        (define fun1
                            (lambda (x)
                                (if (call fixnum? x) (let ([y 2]) (call + x y)) (call handler x))))
                    (call fun1 #f))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) '(error 2))))

    (test-case
            "General cases 1.2"
        (let ([x '(module
                        (define handler
                            (lambda (x)
                                (error 2)))
                        (define fun1
                            (lambda (x)
                                (if (call fixnum? x) (let ([y 2]) (call * x y)) (call handler x))))
                    (call fun1 4))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 8)))

    (test-case
            "General cases 2.1"
        (let ([x '(module
                        (define handler
                            (lambda ()
                                (error 2)))
                        (define fun1
                            (lambda (x)
                                (if (call fixnum? x) 
                                    (let ([y 7]) (call - y x)) 
                                    (if (call ascii-char? x)
                                        x
                                        (if (call void? x)
                                            (call handler)
                                            (void))))))
                    (call fun1 4))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 3)))

    (test-case
            "General cases 2.2"
        (let ([x '(module
                        (define handler
                            (lambda ()
                                (error 2)))
                        (define fun1
                            (lambda (x)
                                (if (call fixnum? x) 
                                    (let ([y 7]) (call - y x)) 
                                    (if (call ascii-char? x)
                                        x
                                        (if (call void? x)
                                            (call handler)
                                            (void))))))
                    (call fun1 #\a))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) #\a)))

    (test-case
            "General cases 2.3"
        (let ([x '(module
                        (define handler
                            (lambda ()
                                (error 2)))
                        (define fun1
                            (lambda (x)
                                (if (call fixnum? x) 
                                    (let ([y 7]) (call - y x)) 
                                    (if (call ascii-char? x)
                                        x
                                        (if (call void? x)
                                            (call handler)
                                            (void))))))
                    (call fun1 #f))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) (void))))

    (test-case
            "General cases 2.4"
        (let ([x '(module
                        (define handler
                            (lambda ()
                                (error 2)))
                        (define fun1
                            (lambda (x)
                                (if (call fixnum? x) 
                                    (let ([y 7]) (call - y x)) 
                                    (if (call ascii-char? x)
                                        x
                                        (if (call void? x)
                                            (call handler)
                                            (void))))))
                    (call fun1 (void)))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) '(error 2))))


    (test-case
            "General cases 3"
        (let ([x '(module
                    (define fib_loop
                        (lambda (n acc1 acc2)
                        (if (call <= n 0)
                            acc1
                            (if (call eq? n 1)
                                acc2
                                (let ([new-n (call + n -2)])
                                    (let ([new-acc2 (call + acc1 acc2)])
                                    (call fib_loop new-n acc2 new-acc2)))))))
                    (call fib_loop 9 2 3))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 21)))

    (test-case
            "General cases 4"
        (let ([x '(module
                    (define handler
                            (lambda (x y)
                                (call >= x y)))
                        (define fun1
                            (lambda (x y)
                                (let ([y x] [z (call handler x y)]) (if z (let ([x x] [y y]) (call * x y)) (call - x 3)))))
                    (call fun1 3 2))])
                (check-confluent?/upto (execute x) (interp-exprs-lang-v9 x) 9)))
)