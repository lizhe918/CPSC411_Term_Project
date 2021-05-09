#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/ptr-run-time
 racket/match
 rackunit)

(require "../component/impl-safe.rkt")


; input: exprs-unique-lang-v9
; output: exprs-unsafe-data-lang-v9
; purpose: Check if the outputs of implement-safe-primops matches the desired outputs of the interrogator.


(let ([x '(module (call eq? (call make-vector 1) (call make-vector 1)))])
  (test-case "new new error case"
             (check-match (implement-safe-primops x)
                           `(module
                                (define ,eq?.57 (lambda (,tmp.53 ,tmp.54) (eq? ,tmp.53 ,tmp.54)))
                                (define ,vector-init-loop.6
                                (lambda (,len.7 ,i.9 ,vec.8)
                                    (if (eq? ,len.7 i.9)
                                    ,vec.8
                                    (begin
                                        (unsafe-vector-set! ,vec.8 ,i.9 0)
                                        (call ,vector-init-loop.6 ,len.7 (unsafe-fx+ ,i.9 1) ,vec.8)))))
                                (define ,make-init-vector.1
                                (lambda (,tmp.4)
                                    (let ((,tmp.5 (unsafe-make-vector ,tmp.4)))
                                    (call ,vector-init-loop.6 ,tmp.4 0 ,tmp.5))))
                                (define ,make-vector.56
                                (lambda (,tmp.32)
                                    (if (fixnum? ,tmp.32) (call ,make-init-vector.1 ,tmp.32) (error 8))))
                                (call ,eq?.57 (call ,make-vector.56 1) (call ,make-vector.56 1))))))


(let ([x '(module (call procedure? empty))])
  (test-case "new error case"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,procedure?.56 (lambda (,tmp.50) (procedure? ,tmp.50)))
                            (call ,procedure?.56 empty)))))

(let ([x '(module (call procedure? not))])
  (test-case "error case"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,procedure?.57 (lambda (,tmp.50) (procedure? ,tmp.50)))
                            (define ,not.56 (lambda (,tmp.49) (not ,tmp.49)))
                            (call ,procedure?.57 ,not.56)))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        procedure?))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement value as prim-f procedure?"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,procedure?.56 (lambda (,tmp.50) (procedure? ,tmp.50)))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,procedure?.56))
                            (if (call fun1.1 1 2 3) #t #f)))))



(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        procedure-arity))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement value as prim-f procedure?"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,procedure-arity.56
                                (lambda (,tmp.55)
                                (if (procedure? ,tmp.55) (unsafe-procedure-arity ,tmp.55) (error 25))))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,procedure-arity.56))
                            (if (call fun1.1 1 2 3) #t #f)))))


;; ??? 
(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        (lambda (x.2) (call procedure? x.2))))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement supports lambda"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,procedure?.56 (lambda (,tmp.50) (procedure? ,tmp.50)))
                            (define fun1.1
                                (lambda (x.1 y.1 z.1) (lambda (x.2) (call ,procedure?.56 x.2))))
                            (if (call fun1.1 1 2 3) #t #f)))))



(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        (call procedure-arity (lambda (x.2 y.2) (call + x.2 y.2)))))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement supports lambda 2"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,procedure-arity.57
                                (lambda (,tmp.55)
                                (if (procedure? ,tmp.55) (unsafe-procedure-arity ,tmp.55) (error 25))))
                            (define ,|+.56|
                                (lambda (,tmp.20 ,tmp.21)
                                (if (fixnum? ,tmp.21)
                                    (if (fixnum? ,tmp.20) (unsafe-fx+ ,tmp.20 ,tmp.21) (error 1))
                                    (error 1))))
                            (define fun1.1
                                (lambda (x.1 y.1 z.1)
                                (call ,procedure-arity.57 (lambda (x.2 y.2) (call ,|+.56| x.2 y.2)))))
                            (if (call fun1.1 1 2 3) #t #f)))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (call procedure? x.1)
                            (call procedure-arity x.1)
                            (if (call pair? x.1)
                                (lambda (x.1) (call + (call car x.1) (call cdr x.1)))
                                (error 10)))))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "General cases 1 (fragile test)"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,procedure-arity.56
                                (lambda (,tmp.55)
                                (if (procedure? ,tmp.55) (unsafe-procedure-arity ,tmp.55) (error 25))))
                            (define ,|+.59|
                                (lambda (,tmp.20 ,tmp.21)
                                (if (fixnum? ,tmp.21)
                                    (if (fixnum? ,tmp.20) (unsafe-fx+ ,tmp.20 ,tmp.21) (error 1))
                                    (error 1))))
                            (define ,car.57
                                (lambda (,tmp.39) (if (pair? ,tmp.39) (unsafe-car ,tmp.39) (error 11))))
                            (define ,cdr.58
                                (lambda (,tmp.40) (if (pair? ,tmp.40) (unsafe-cdr ,tmp.40) (error 12))))
                            (define fun1.1
                                (lambda (x.1 y.1 z.1)
                                (if (procedure? x.1)
                                    (call ,procedure-arity.56 x.1)
                                    (if (pair? x.1)
                                    (lambda (x.1) (call ,|+.59| (call ,car.57 x.1) (call ,cdr.58 x.1)))
                                    (error 10)))))
                            (if (call fun1.1 1 2 3) #t #f)))))



(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        (let ([x.2 (lambda (x.4) x.4)]
                              [x.3 (call cons 1 2)])
                              (if (call procedure? x.2)
                                    (call + (call x.2 (call cdr x.3)) (call car x.3))
                                    (call make-vector (call - (call procedure-arity y.1) (call cdr x.3)))))))
            (if (call fun1.1 1 (lambda (q.1 m.1 z.1) q.1) 3) #t #f))])
  (test-case "General cases 2 (fragile test)"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,vector-init-loop.6
                                (lambda (,len.7 ,i.9 ,vec.8)
                                (if (eq? ,len.7 ,i.9)
                                    ,vec.8
                                    (begin
                                    (unsafe-vector-set! ,vec.8 ,i.9 0)
                                    (call ,vector-init-loop.6 ,len.7 (unsafe-fx+ ,i.9 1) ,vec.8)))))
                            (define ,make-init-vector.1
                                (lambda (,tmp.4)
                                (let ((,tmp.5 (unsafe-make-vector ,tmp.4)))
                                    (call ,vector-init-loop.6 ,tmp.4 0 ,tmp.5))))
                            (define ,make-vector.61
                                (lambda (,tmp.32)
                                (if (fixnum? ,tmp.32) (call ,make-init-vector.1 ,tmp.32) (error 7))))
                            (define ,|-.60|
                                (lambda (,tmp.22 ,tmp.23)
                                (if (fixnum? ,tmp.23)
                                    (if (fixnum? ,tmp.22) (unsafe-fx- ,tmp.22 ,tmp.23) (error 2))
                                    (error 2))))
                            (define ,procedure-arity.59
                                (lambda (,tmp.55)
                                (if (procedure? ,tmp.55) (unsafe-procedure-arity ,tmp.55) (error 25))))
                            (define ,|+.58|
                                (lambda (,tmp.20 ,tmp.21)
                                (if (fixnum? ,tmp.21)
                                    (if (fixnum? ,tmp.20) (unsafe-fx+ ,tmp.20 ,tmp.21) (error 1))
                                    (error 1))))
                            (define ,car.57
                                (lambda (,tmp.39) (if (pair? ,tmp.39) (unsafe-car ,tmp.39) (error 11))))
                            (define ,cdr.56
                                (lambda (,tmp.40) (if (pair? ,tmp.40) (unsafe-cdr ,tmp.40) (error 12))))
                            (define fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((x.2 (lambda (x.4) x.4)) (x.3 (cons 1 2)))
                                    (if (procedure? x.2)
                                    (call ,|+.58| (call x.2 (call ,cdr.56 x.3)) (call ,car.57 x.3))
                                    (call
                                    ,make-vector.61
                                    (call ,|-.60| (call ,procedure-arity.59 y.1) (call ,cdr.56 x.3)))))))
                            (if (call fun1.1 1 (lambda (q.1 m.1 z.1) q.1) 3) #t #f)))))


;; --------------------------------------- old tests -------------------------------------------------
(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        pair?))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement value as prim-f pair?"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,pair?.5 (lambda (,tmp.43) (pair? ,tmp.43)))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,pair?.5))
                            (if (call fun1.1 1 2 3) #t #f)))))



(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        vector?))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement value as prim-f vector?"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,vector?.5 (lambda (,tmp.44) (vector? ,tmp.44)))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,vector?.5))
                            (if (call fun1.1 1 2 3) #t #f)))))



(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        cons))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement value as prim-f cons"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,cons.5 (lambda (,tmp.46 ,tmp.47) (cons ,tmp.46 ,tmp.47)))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,cons.5))
                            (if (call fun1.1 1 2 3) #t #f)))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        car))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement value as prim-f car"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,car.5
                                (lambda (,tmp.35) (if (pair? ,tmp.35) (unsafe-car ,tmp.35) (error 11))))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,car.5))
                            (if (call fun1.1 1 2 3) #t #f)))))

(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        cdr))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement value as prim-f cdr"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,cdr.5
                                (lambda (,tmp.36) (if (pair? ,tmp.36) (unsafe-cdr ,tmp.36) (error 12))))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,cdr.5))
                            (if (call fun1.1 1 2 3) #t #f)))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        make-vector))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement value as prim-f make-vector"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,vector-init-loop.4
                                (lambda (,len.3 ,i.5 ,vec.4)
                                (if (eq? ,len.3 ,i.5)
                                    ,vec.4
                                    (begin
                                    (unsafe-vector-set! ,vec.4 ,i.5 0)
                                    (call ,vector-init-loop.4 ,len.3 (unsafe-fx+ ,i.5 1) ,vec.4)))))
                            (define ,make-init-vector.1
                                (lambda (,tmp.1)
                                (let ((,tmp.2 (unsafe-make-vector ,tmp.1)))
                                    (call ,vector-init-loop.4 ,tmp.1 0 ,tmp.2))))
                            (define ,make-vector.5
                                (lambda (,tmp.28)
                                (if (fixnum? ,tmp.28) (call ,make-init-vector.1 ,tmp.28) (error 7))))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,make-vector.5))
                            (if (call fun1.1 1 2 3) #t #f)))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        vector-length))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement value as prim-f vector-length"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,vector-length.5
                                (lambda (,tmp.29)
                                (if (vector? ,tmp.29) (unsafe-vector-length ,tmp.29) (error 8))))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,vector-length.5))
                            (if (call fun1.1 1 2 3) #t #f)))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        vector-set!))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement value as prim-f vector-set!"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,unsafe-vector-set!.2
                                (lambda (,tmp.6 ,tmp.7 ,tmp.8)
                                (if (unsafe-fx< ,tmp.7 (unsafe-vector-length ,tmp.6))
                                    (if (unsafe-fx>= ,tmp.7 0)
                                    (begin (unsafe-vector-set! ,tmp.6 ,tmp.7 ,tmp.8) (void))
                                    (error 10))
                                    (error 10))))
                            (define ,vector-set!.5
                                (lambda (,tmp.30 ,tmp.31 ,tmp.32)
                                (if (fixnum? ,tmp.31)
                                    (if (vector? ,tmp.30)
                                    (call ,unsafe-vector-set!.2 ,tmp.30 ,tmp.31 ,tmp.32)
                                    (error 9))
                                    (error 9))))
                            (define ,fun1.1 (lambda (x.1 y.1 z.1) ,vector-set!.5))
                            (if (call fun1.1 1 2 3) #t #f)))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        vector-ref))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement value as prim-f vector-ref"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,unsafe-vector-ref.3
                                (lambda (,tmp.11 ,tmp.12)
                                (if (unsafe-fx< ,tmp.12 (unsafe-vector-length ,tmp.11))
                                    (if (unsafe-fx>= ,tmp.12 0)
                                    (unsafe-vector-ref ,tmp.11 ,tmp.12)
                                    (error 11))
                                    (error 11))))
                            (define ,vector-ref.5
                                (lambda (,tmp.33 ,tmp.34)
                                (if (fixnum? ,tmp.34)
                                    (if (vector? ,tmp.33)
                                    (call ,unsafe-vector-ref.3 ,tmp.33 ,tmp.34)
                                    (error 10))
                                    (error 10))))
                            (define ,fun1.1 (lambda (x.1 y.1 z.1) ,vector-ref.5))
                            (if (call fun1.1 1 2 3) #t #f)))))

(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (call vector? x.1)
                            (call make-vector (call * 2 (call vector-length x.1)))
                            (if (call pair? x.1)
                                (call + (call car x.1) (call cdr x.1))
                                (error 10)))))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement new prim-f as general case 1 (fragile test)"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,|+.10|
                                (lambda (,tmp.16 ,tmp.17)
                                (if (fixnum? ,tmp.17)
                                    (if (fixnum? ,tmp.16) (unsafe-fx+ ,tmp.16 ,tmp.17) (error 1))
                                    (error 1))))
                            (define ,cdr.9
                                (lambda (,tmp.36) (if (pair? ,tmp.36) (unsafe-cdr ,tmp.36) (error 12))))
                            (define ,car.8
                                (lambda (,tmp.35) (if (pair? ,tmp.35) (unsafe-car ,tmp.35) (error 11))))
                            (define ,vector-init-loop.4
                                (lambda (,len.3 ,i.5 ,vec.4)
                                (if (eq? ,len.3 ,i.5)
                                    ,vec.4
                                    (begin
                                    (unsafe-vector-set! ,vec.4 ,i.5 0)
                                    (call ,vector-init-loop.4 ,len.3 (unsafe-fx+ ,i.5 1) ,vec.4)))))
                            (define ,make-init-vector.1
                                (lambda (,tmp.1)
                                (let ((,tmp.2 (unsafe-make-vector ,tmp.1)))
                                    (call ,vector-init-loop.4 ,tmp.1 0 ,tmp.2))))
                            (define ,make-vector.7
                                (lambda (,tmp.28)
                                (if (fixnum? ,tmp.28) (call ,make-init-vector.1 ,tmp.28) (error 7))))
                            (define ,*.6
                                (lambda (,tmp.14 ,tmp.15)
                                (if (fixnum? ,tmp.15)
                                    (if (fixnum? ,tmp.14) (unsafe-fx* ,tmp.14 ,tmp.15) (error 0))
                                    (error 0))))
                            (define ,vector-length.5
                                (lambda (,tmp.29)
                                (if (vector? ,tmp.29) (unsafe-vector-length ,tmp.29) (error 8))))
                            (define ,fun1.1
                                (lambda (x.1 y.1 z.1)
                                (if (vector? x.1)
                                    (call ,make-vector.7 (call *.6 2 (call ,vector-length.5 x.1)))
                                    (if (pair? x.1)
                                    (call ,|+.10| (call ,car.8 x.1) (call ,cdr.9 x.1))
                                    (error 10)))))
                            (if (call fun1.1 1 2 3) #t #f)))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        (let ([x.2 (call make-vector y.1)]
                              [x.3 (call cons 1 2)])
                              (if (call vector? x.2)
                                    (call + (call vector-ref y.1) (call car x.3))
                                    (call make-vector (call - (call vector-length y.1) (call cdr x.3)))))))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement new prim-f as general case 2 (fragile test)"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,|-.11|
                                (lambda (,tmp.18 ,tmp.19)
                                (if (fixnum? ,tmp.19)
                                    (if (fixnum? ,tmp.18) (unsafe-fx- ,tmp.18 ,tmp.19) (error 2))
                                    (error 2))))
                            (define ,cdr.10
                                (lambda (,tmp.36) (if (pair? ,tmp.36) (unsafe-cdr ,tmp.36) (error 12))))
                            (define ,vector-length.9
                                (lambda (,tmp.29)
                                (if (vector? ,tmp.29) (unsafe-vector-length ,tmp.29) (error 8))))
                            (define ,|+.8|
                                (lambda (,tmp.16 ,tmp.17)
                                (if (fixnum? ,tmp.17)
                                    (if (fixnum? ,tmp.16) (unsafe-fx+ ,tmp.16 ,tmp.17) (error 1))
                                    (error 1))))
                            (define ,car.7
                                (lambda (,tmp.35) (if (pair? ,tmp.35) (unsafe-car ,tmp.35) (error 11))))
                            (define ,unsafe-vector-ref.3
                                (lambda (,tmp.11 ,tmp.12)
                                (if (unsafe-fx< ,tmp.12 (unsafe-vector-length ,tmp.11))
                                    (if (unsafe-fx>= ,tmp.12 0)
                                    (unsafe-vector-ref ,tmp.11 ,tmp.12)
                                    (error 11))
                                    (error 11))))
                            (define ,vector-ref.6
                                (lambda (,tmp.33 ,tmp.34)
                                (if (fixnum? ,tmp.34)
                                    (if (vector? ,tmp.33)
                                    (call ,unsafe-vector-ref.3 ,tmp.33 ,tmp.34)
                                    (error 10))
                                    (error 10))))
                            (define ,vector-init-loop.4
                                (lambda (,len.3 ,i.5 ,vec.4)
                                (if (eq? ,len.3 ,i.5)
                                    ,vec.4
                                    (begin
                                    (unsafe-vector-set! ,vec.4 ,i.5 0)
                                    (call ,vector-init-loop.4 ,len.3 (unsafe-fx+ ,i.5 1) ,vec.4)))))
                            (define ,make-init-vector.1
                                (lambda (,tmp.1)
                                (let ((,tmp.2 (unsafe-make-vector ,tmp.1)))
                                    (call ,vector-init-loop.4 ,tmp.1 0 ,tmp.2))))
                            (define ,make-vector.5
                                (lambda (,tmp.28)
                                (if (fixnum? ,tmp.28) (call ,make-init-vector.1 ,tmp.28) (error 7))))
                            (define ,fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((x.2 (call ,make-vector.5 y.1)) (x.3 (cons 1 2)))
                                    (if (vector? x.2)
                                    (call ,|+.8| (call ,vector-ref.6 y.1) (call ,car.7 x.3))
                                    (call
                                    ,make-vector.5
                                    (call ,|-.11| (call ,vector-length.9 y.1) (call ,cdr.10 x.3)))))))
                            (if (call fun1.1 1 2 3) #t #f)))))



(let ([x '(module (if (call eq? (call + 5 6) 11) 4 6))])
  (test-case "error case"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,|+.5|
                            (lambda (,tmp.16 ,tmp.17)
                                (if (fixnum? ,tmp.17)
                                (if (fixnum? ,tmp.16) (unsafe-fx+ ,tmp.16 ,tmp.17) (error 1))
                                (error 1))))
                            (if (eq? (call ,|+.5| 5 6) 11) 4 6)))))
;; --------------------------------------------- old cases ---------------------------------------------
(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        +))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement value as prim-f +"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,|+.1|
                                (lambda (,tmp.3 ,tmp.4)
                                (if (fixnum? ,tmp.4)
                                    (if (fixnum? ,tmp.3) (unsafe-fx+ ,tmp.3 ,tmp.4) (error 1))
                                    (error 1))))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,|+.1|))
                            (if (call fun1.1 1 2 3) #t #f)))))

(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        -))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement value as prim-f -"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,|-.1|
                                (lambda (,tmp.5 ,tmp.6)
                                (if (fixnum? ,tmp.6)
                                    (if (fixnum? ,tmp.5) (unsafe-fx- ,tmp.5 ,tmp.6) (error 2))
                                    (error 2))))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,|-.1|))
                            (if (call fun1.1 1 2 3) #t #f)))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        *))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement value as prim-f *"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,*.1
                                (lambda (,tmp.3 ,tmp.4)
                                (if (fixnum? ,tmp.4)
                                    (if (fixnum? ,tmp.3) (unsafe-fx* ,tmp.3 ,tmp.4) (error 0))
                                    (error 0))))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,*.1))
                            (if (call fun1.1 1 2 3) #t #f)))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        eq?))
            eq?)])
  (test-case "implement value as prim-f eq?"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,eq?.1 (lambda (,tmp.15 ,tmp.16) (eq? ,tmp.15 ,tmp.16)))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,eq?.1))
                            ,eq?.1))))

(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        >))
            eq?)])
  (test-case "implement value as prim-f >"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,eq?.2 (lambda (,tmp.15 ,tmp.16) (eq? ,tmp.15 ,tmp.16)))
                            (define ,>.1
                                (lambda (,tmp.11 ,tmp.12)
                                (if (fixnum? ,tmp.12)
                                    (if (fixnum? ,tmp.11) (unsafe-fx> ,tmp.11 ,tmp.12) (error 5))
                                    (error 5))))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,>.1))
                            ,eq?.2))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        <))
            eq?)])
  (test-case "implement value as prim-f <"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,eq?.2 (lambda (,tmp.15 ,tmp.16) (eq? ,tmp.15 ,tmp.16)))
                            (define ,<.1
                                (lambda (,tmp.7 ,tmp.8)
                                (if (fixnum? ,tmp.8)
                                    (if (fixnum? ,tmp.7) (unsafe-fx< ,tmp.7 ,tmp.8) (error 3))
                                    (error 3))))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,<.1))
                            ,eq?.2))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        <=))
            (if eq? 1 2))])
  (test-case "implement value as prim-f <="
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,eq?.2 (lambda (,tmp.15 ,tmp.16) (eq? ,tmp.15 ,tmp.16)))
                            (define ,<=.1
                                (lambda (,tmp.9 ,tmp.10)
                                (if (fixnum? ,tmp.10)
                                    (if (fixnum? ,tmp.9) (unsafe-fx<= ,tmp.9 ,tmp.10) (error 4))
                                    (error 4))))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,<=.1))
                            (if ,eq?.2 1 2)))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        >=))
            (if eq? 1 2))])
  (test-case "implement value as prim-f >="
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,eq?.2 (lambda (,tmp.15 ,tmp.16) (eq? ,tmp.15 ,tmp.16)))
                            (define ,>=.1
                                (lambda (,tmp.9 ,tmp.10)
                                (if (fixnum? ,tmp.10)
                                    (if (fixnum? ,tmp.9) (unsafe-fx>= ,tmp.9 ,tmp.10) (error 6))
                                    (error 6))))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,>=.1))
                            (if ,eq?.2 1 2)))))



(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        fixnum?))
            fixnum?)])
  (test-case "implement value as prim-f as unop fixnum"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,fixnum?.1 (lambda (,tmp.17) (fixnum? ,tmp.17)))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,fixnum?.1))
                            ,fixnum?.1))))

(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        boolean?))
            boolean?)])
  (test-case "implement value as prim-f as unop boolean"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,boolean?.1 (lambda (,tmp.17) (boolean? ,tmp.17)))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,boolean?.1))
                            ,boolean?.1))))

(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        empty?))
            empty?)])
  (test-case "implement value as prim-f as unop empty"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,empty?.1 (lambda (,tmp.17) (empty? ,tmp.17)))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,empty?.1))
                            ,empty?.1))))

(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        void?))
            void?)])
  (test-case "implement value as prim-f as unop void"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,void?.1 (lambda (,tmp.17) (void? ,tmp.17)))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,void?.1))
                            ,void?.1))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        ascii-char?))
            ascii-char?)])
  (test-case "implement value as prim-f as unop ascii"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,ascii-char?.1 (lambda (,tmp.17) (ascii-char? ,tmp.17)))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,ascii-char?.1))
                            ,ascii-char?.1))))



(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        error?))
            error?)])
  (test-case "implement value as prim-f as unop error"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,error?.1 (lambda (,tmp.17) (error? ,tmp.17)))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,error?.1))
                            ,error?.1))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        not))
            not)])
  (test-case "implement value as prim-f as unop not"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,not.1 (lambda (,tmp.17) (not ,tmp.17)))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,not.1))
                            ,not.1))))

(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        x.1))
            (if (call fun1.1 1 2 3) empty 1))])
  (test-case "implement when pred is call (no change - ill-typed)"
             (check-equal? (implement-safe-primops x)
                           `(module
                            (define fun1.1 (lambda (x.1 y.1 z.1) x.1))
                            (if (call fun1.1 1 2 3) empty 1)))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        x.1))
            (if (let ([x.2 (call fun1.1 1 2 3)]) x.2) 1 2))])
  (test-case "implement when pred is let e (no change - ill-typed)"
             (check-equal? (implement-safe-primops x)
                           `(module
                            (define fun1.1 (lambda (x.1 y.1 z.1) x.1))
                            (if (let ((x.2 (call fun1.1 1 2 3))) x.2) 1 2)))))



(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        fixnum?))
            (if (call fun1.1 #t) 1 2))])
  (test-case "General case with unop"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,fixnum?.1 (lambda (,tmp.17) (fixnum? ,tmp.17)))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,fixnum?.1))
                            (if (call fun1.1 #t) 1 2)))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        >=))
            (if (call fun1.1 1 2) 1 2))])
  (test-case "General case with binop"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,>=.1
                                (lambda (,tmp.13 ,tmp.14)
                                (if (fixnum? ,tmp.14)
                                    (if (fixnum? ,tmp.13) (unsafe-fx>= ,tmp.13 ,tmp.14) (error 6))
                                    (error 6))))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,>=.1))
                            (if (call fun1.1 1 2) 1 2)))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        empty?))
            (if (let ([x.2 2] [x.3 empty]) (call fun1.1 1 2 3)) 1 2))])
  (test-case "General case with let"
             (check-match (implement-safe-primops x)
                           `(module
                            (define ,empty?.1 (lambda (,tmp.19) (empty? ,tmp.19)))
                            (define fun1.1 (lambda (x.1 y.1 z.1) ,empty?.1))
                            (if (let ((x.2 2) (x.3 empty)) (call fun1.1 1 2 3)) 1 2)))))