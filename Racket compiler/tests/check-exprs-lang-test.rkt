#lang racket
(require
  cpsc411/compiler-lib
  cpsc411/ptr-run-time
  cpsc411/test-suite/utils
  racket/match
  rackunit)

(require "../component/check-val.rkt")

; input: exprs-lang-v9 
; output: exprs-lang-v9 
; purpose: Check if the output of check-exprs-lang matches the output of interrogator

(test-case
    "OK return procedure"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                     (lambda () x))) 
            (call add1 5 6))])
        (check-equal? (check-exprs-lang x)
                        `(module (define add1 (lambda (x y) (lambda () x))) (call add1 5 6)))))


(test-case
    "OK use procedure as arguments"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                     (call x))) 
            (call add1 (lambda () 5) 6))])
        (check-equal? (check-exprs-lang x)
                        `(module (define add1 (lambda (x y) (call x))) (call add1 (lambda () 5) 6)))))


(test-case
    "OK use procedure as pred"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                     x)) 
            (if (lambda () 5) 1 2))])
        (check-equal? (check-exprs-lang x)
                        `(module (define add1 (lambda (x y) x)) (if (lambda () 5) 1 2)))))


(test-case
    "OK use procedure in if"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                     (if #t (lambda () 5) 7))) 
            (call add1 1 2))])
        (check-equal? (check-exprs-lang x)
                        '(module 
                            (define add1 
                                (lambda (x y) 
                                    (if #t (lambda () 5) 7))) 
                            (call add1 1 2)))))

(test-case
    "OK use procedure in let bind"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                     (let ([z (lambda () 5)]) (call x)))) 
            (call add1 2 6))])
        (check-equal? (check-exprs-lang x)
                        `(module 
                            (define add1 
                                (lambda (x y) 
                                    (let ([z (lambda () 5)]) (call x)))) 
                            (call add1 2 6)))))

(test-case
    "OK use procedure in let body"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                     (let ([z 6]) (lambda () z)))) 
            (call add1 2 6))])
        (check-equal? (check-exprs-lang x)
                        `(module
                        (define add1 (lambda (x y) (let ((z 6)) (lambda () z))))
                        (call add1 2 6)))))


(test-case
    "OK use procedure in lambda itself"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                     (lambda () (lambda () x)))) 
            (call add1 2 7))])
        (check-equal? (check-exprs-lang x)
                        `(module (define add1 (lambda (x y) (lambda () (lambda () x)))) (call add1 2 7)))))

(test-case
    "OK call with arbitary numbers of variables"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                     (lambda () (lambda () x)))) 
            (call add1 2 7 8 8))])
        (check-equal? (check-exprs-lang x)
                        `(module
                        (define add1 (lambda (x y) (lambda () (lambda () x))))
                        (call add1 2 7 8 8)))))

(test-case
    "OK call with arbitary numbers of variables primops"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                     (call cons x y 5 6 7 8 9))) 
            (call add1 2 7 8 8))])
        (check-equal? (check-exprs-lang x)
                        `(module
                            (define add1 (lambda (x y) (call cons x y 5 6 7 8 9)))
                            (call add1 2 7 8 8)))))


(test-case
    "error case1"
(let ([x '(module (let ((x 5)) (call (lambda (y) x) 2)))])
        (check-equal? (check-exprs-lang x)
                        '(module (let ((x 5)) (call (lambda (y) x) 2))))))

(test-case
    "error case2"
(let ([x '(module (let ((x 5) (y 6) (z 7)) (call (lambda (a) (call + (call + x y) z)) 2)))])
        (check-equal? (check-exprs-lang x)
                        '(module (let ((x 5) (y 6) (z 7)) (call (lambda (a) (call + (call + x y) z)) 2))))))


(test-case
    "error case3"
(let ([x '(module (call (lambda (lambda) (call lambda lambda)) (lambda (lambda) lambda)))])
        (check-equal? (check-exprs-lang x)
                        '(module (call (lambda (lambda) (call lambda lambda)) (lambda (lambda) lambda))))))
;; -------------------------------------- old cases --------------------------------------------------

(test-case
    "call a function that return any value other than procedure"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    make-vector)) 
            (call add1 5 6))])
        (check-exn exn:fail? (lambda () (check-exprs-lang x)))))

(test-case
    "Cannot use proc in position other than operator position"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    (if vector-length x y))) 
            (call add1 5 6))])
        (check-exn exn:fail? (lambda () (check-exprs-lang x)))))


;; valid cases
(test-case
    "OK with ill-typed function"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                     (call pair? #\b ))) 
            (call add1 5 6))])
        (check-equal? (check-exprs-lang x)
                        `(module 
                        (define add1 
                            (lambda (x y) 
                                (call pair? #\b ))) 
                        (call add1 5 6)))))


(test-case
    "OK with ill-typed function"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                     (call vector? 5))) 
            (call add1 5 6))])
        (check-equal? (check-exprs-lang x)
                        '(module 
                        (define add1 
                            (lambda (x y) 
                                (call vector? 5))) 
                        (call add1 5 6)))))


(test-case
    "OK with ill-typed function"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                     (call cons #\b (void)))) 
            (call add1 5 6))])
        (check-equal? (check-exprs-lang x)
                        `(module 
                        (define add1 
                            (lambda (x y) 
                                (call cons #\b (void)))) 
                        (call add1 5 6)))))

(test-case
    "OK with ill-typed function"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                     (call car (void)))) 
            (call add1 5 6))])
        (check-equal? (check-exprs-lang x)
                        `(module 
                        (define add1 
                            (lambda (x y) 
                                (call car (void)))) 
                        (call add1 5 6)))))

(test-case
    "OK with ill-typed function"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                     (call cdr (void)))) 
            (call add1 5 6))])
        (check-equal? (check-exprs-lang x)
                        `(module 
                        (define add1 
                            (lambda (x y) 
                                (call cdr (void)))) 
                        (call add1 5 6)))))


(test-case
    "OK with ill-typed function"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                     (call make-vector (error 8)))) 
            (call add1 5 6))])
        (check-equal? (check-exprs-lang x)
                        `(module
                        (define add1 (lambda (x y) (call make-vector (error 8))))
                        (call add1 5 6)))))

(test-case
    "OK with ill-typed function"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                     (call vector-length #\b))) 
            (call add1 5 6))])
        (check-equal? (check-exprs-lang x)
                        `(module
                        (define add1 (lambda (x y) (call vector-length #\b)))
                        (call add1 5 6)))))


(test-case
    "OK with ill-typed function"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                     (call vector-set! #\b (void) empty))) 
            (call add1 5 6))])
        (check-equal? (check-exprs-lang x)
                        `(module
                        (define add1 (lambda (x y) (call vector-set! #\b (void) empty)))
                        (call add1 5 6)))))

(test-case
    "OK with ill-typed function"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                     (call vector-ref #\b (void)))) 
            (call add1 5 6))])
        (check-equal? (check-exprs-lang x)
                        `(module 
                        (define add1 
                            (lambda (x y) 
                                (call vector-ref #\b (void)))) 
                        (call add1 5 6)))))


(test-case
    "General case"
(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (call eq? (call make-vector (call vector-length (call make-vector z))) (call cons (let ([x (call make-vector 2)]) (let ([x (call vector-length x)]) x)) z))))
            (call fun1 1 2 3))])
        (check-equal? (check-exprs-lang x)
                        '(module
                        (define fun1
                            (lambda (x y z)
                                (call eq? (call make-vector (call vector-length (call make-vector z))) (call cons (let ([x (call make-vector 2)]) (let ([x (call vector-length x)]) x)) z))))
                        (call fun1 1 2 3)))))


(test-case
    "General case"
(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (if (if (call vector-ref x y) (call cons y z) (call cdr x)) (if #t (call vector-ref x y) (call vector-set! x y 5)) (call error? z))))
            (call fun1 1 2 3))])
        (check-equal? (check-exprs-lang x)
                        '(module
                        (define fun1
                            (lambda (x y z)
                            (if (if (call vector-ref x y) (call cons y z) (call cdr x))
                                (if #t (call vector-ref x y) (call vector-set! x y 5))
                                (call error? z))))
                        (call fun1 1 2 3)))))

(test-case
    "Test case"
(let ([x `(module (define id1 (lambda (x) x)) (define id2 (lambda (x) x)) (let ((y (if #t id1 id2))) (call y 5)))])
        (check-equal? (check-exprs-lang x) `(module (define id1 (lambda (x) x)) (define id2 (lambda (x) x)) (let ((y (if #t id1 id2))) (call y 5))))))

(test-case
    "Test case2"
(let ([x `(module (if (call eq? (call + 5 6) 11) 4 6))])
        (check-equal? (check-exprs-lang x) `(module (if (call eq? (call + 5 6) 11) 4 6)))))

(test-case
    "Test case3"
(let ([x `(module empty)])
        (check-equal? (check-exprs-lang x) `(module empty))))

;;---------------------------------------- old cases -------------------------------------------------
(test-case
    "call a function that is not exist"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    x)) 
            (call add2 5 6))])
        (check-exn exn:fail? (lambda () (check-exprs-lang x)))))

(test-case
    "when value is invalid fixnum"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    (- (min-int 61) 1))) 
            (call add1 5 6))])
        (check-exn exn:fail? (lambda () (check-exprs-lang x)))))


(test-case
    "when value is invalid error code"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    (error 256))) 
            (call add1 5 6))])
        (check-exn exn:fail? (lambda () (check-exprs-lang x)))))


(test-case
    "when value is invalid ascii"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    #\λ)) 
            (call add1 5 6))])
        (check-exn exn:fail? (lambda () (check-exprs-lang x)))))



(test-case
    "call a function that return any value other than procedure"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    fixnum?)) 
            (call add1 5 6))])
        (check-exn exn:fail? (lambda () (check-exprs-lang x)))))

(test-case
    "call a function that return any value other than procedure"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    >=)) 
            (call add1 5 6))])
        (check-exn exn:fail? (lambda () (check-exprs-lang x)))))


(test-case
    "wrong expression of e"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    x)) 
            (call))])
        (check-exn exn:fail? (lambda () (check-exprs-lang x)))))


(test-case
    "wrong expression of e"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    (if #t x))) 
            (call add1 5 6))])
        (check-exn exn:fail? (lambda () (check-exprs-lang x)))))


(test-case
    "wrong expression of e"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    (let ([x 5])))) 
            (call add1 5 6))])
        (check-exn exn:fail? (lambda () (check-exprs-lang x)))))

(test-case
    "Bad calling"
(let ([x `(module (call 5 5))])
            (check-exn exn:fail? (lambda () (check-exprs-lang x)))))

(test-case
    "Cannot use proc in position other than operator position"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    (if + x y))) 
            (call add1 5 6))])
        (check-exn exn:fail? (lambda () (check-exprs-lang x)))))





;; valid cases
(test-case
    "OK with ill-typed function"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    (if 5 x y))) 
            (call add1 5 6))])
        (check-equal? (check-exprs-lang x)
                        `(module (define add1 (lambda (x y) (if 5 x y))) (call add1 5 6)))))

(test-case
    "OK with ill-typed function"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    (if (void) x y))) 
            (call add1 5 6))])
        (check-equal? (check-exprs-lang x)
                        `(module (define add1 (lambda (x y) (if (void) x y))) (call add1 5 6)))))

(test-case
    "OK with ill-typed function"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    (call + (void) empty))) 
            (call add1 5 6))])
        (check-equal? (check-exprs-lang x)
                        `(module (define add1 (lambda (x y) (call + (void) empty))) (call add1 5 6)))))

(test-case
    "OK with ill-type function"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    (if #t (call - #\b (error 5)) (call >= (void) y)))) 
            (call add1 5 6))])
        (check-equal? (check-exprs-lang x)
                        `(module
                        (define add1
                            (lambda (x y) (if #t (call - #\b (error 5)) (call >= (void) y))))
                        (call add1 5 6)))))

(test-case
    "OK with shadowed prim-f"
(let ([x '(module
            (define eq?
                (lambda (x y)
                    x))
            (define add1 
                (lambda (x y) 
                    (call eq? (void) empty))) 
            (call add1 5 6))])
        (check-equal? (check-exprs-lang x)
                        `(module
                        (define eq? (lambda (x y) x))
                        (define add1 (lambda (x y) (call eq? (void) empty)))
                        (call add1 5 6)))))

(test-case
    "OK with shadowed prim-f"
(let ([x '(module
            (define eq?
                (lambda (x y)
                    x))
            (define add1 
                (lambda (x y) 
                    (let ([eq? x] [y 5]) (call > eq? y)))) 
            (call add1 5 6))])
        (check-equal? (check-exprs-lang x)
                        `(module
                        (define eq? (lambda (x y) x))
                        (define add1 (lambda (x y) (let ((eq? x) (y 5)) (call > eq? y))))
                        (call add1 5 6)))))

(test-case
    "OK with local binding"
(let ([x '(module
            (define eq?
                (lambda (x y)
                    x))
            (define add1 
                (lambda (x y) 
                    (let ([q eq?]) (call q x y)))) 
            (call add1 5 6))])
        (check-equal? (check-exprs-lang x)
                        `(module
                        (define eq? (lambda (x y) x))
                        (define add1 (lambda (x y) (let ((q eq?)) (call q x y))))
                        (call add1 5 6)))))

(test-case
    "OK redefine proc"
(let ([x '(module
            (define add1
                (lambda (x y)
                    x))
            (define add1 
                (lambda (x y) 
                    (let ([q eq?]) (call q x y)))) 
            (call add1 5 6))])
        (check-equal? (check-exprs-lang x)
                        `(module
                            (define add1 (lambda (x y) x))
                            (define add1 (lambda (x y) (let ((q eq?)) (call q x y))))
                            (call add1 5 6)))))


(test-case
    "OK to call proc defined after"
(let ([x '(module
            (define add2
                (lambda (x y)
                    (call add1 1 2)))
            (define add1 
                (lambda (x y) 
                    (let ([q eq?]) (call q x y)))) 
            (call add1 5 6))])
        (check-equal? (check-exprs-lang x)
                        `(module
                        (define add2 (lambda (x y) (call add1 1 2)))
                        (define add1 (lambda (x y) (let ((q eq?)) (call q x y))))
                        (call add1 5 6)))))


(test-case
    "General case"
(let ([x '(module
        (define odd?
          (lambda (x)
            (if (call eq? x 0)
                0
                (let ([y (call + x -1)])
                  (call even? y)))))
        (define even?
          (lambda (x)
            (if (call eq? x 0)
                1
                (let ([y (call + x -1)])
                  (call odd? y)))))
      (call even? 5))])
        (check-equal? (check-exprs-lang x)
                        `(module
                        (define odd?
                            (lambda (x)
                            (if (call eq? x 0) 0 (let ((y (call + x -1))) (call even? y)))))
                        (define even?
                            (lambda (x) (if (call eq? x 0) 1 (let ((y (call + x -1))) (call odd? y)))))
                        (call even? 5)))))

(test-case
    "General case"
(let ([x '(module
      (define fib_loop
        (lambda (n acc1 acc2)
          (if (call eq? n 0)
              acc1
              (if (call eq? n 1)
                  acc2
                  (let ([new-n (call + n -1)])
                    (let ([new-acc2 (call + acc1 acc2)])
                      (call fib_loop new-n acc2 new-acc2)))))))
      (call fib_loop 5 0 1))])
        (check-equal? (check-exprs-lang x)
                        `(module
                            (define fib_loop
                                (lambda (n acc1 acc2)
                                (if (call eq? n 0)
                                    acc1
                                    (if (call eq? n 1)
                                    acc2
                                    (let ((new-n (call + n -1)))
                                        (let ((new-acc2 (call + acc1 acc2)))
                                        (call fib_loop new-n acc2 new-acc2)))))))
                            (call fib_loop 5 0 1)))))

