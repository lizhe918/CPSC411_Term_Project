#lang racket
(require
  cpsc411/compiler-lib
  cpsc411/ptr-run-time
  cpsc411/test-suite/utils
  racket/match
  rackunit)

(require "../component/uniquify.rkt")


; input: exprs-lang v9
; output: exprs-unique-lang-v9
; purpose: Check if the outputs of uniquify matches the desired outputs of the interrogator.



(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (call procedure? x)))
            (call fun1 1 2 3))])
  (test-case "Simple case allows procedure?"
             (check-match (uniquify x)
                           `(module
                            (define ,fun1.4 (lambda (,x.7 ,y.6 ,z.5) (call procedure? ,x.7)))
                            (call ,fun1.4 1 2 3)))))

(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (call procedure-arity x)))
            (call fun1 1 2 3))])
  (test-case "Simple case allows procedure-arity"
             (check-match (uniquify x)
                           `(module
                            (define ,fun1.4 (lambda (,x.7 ,y.6 ,z.5) (call procedure-arity ,x.7)))
                            (call ,fun1.4 1 2 3)))))

(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (lambda (x y z) (call + x y))))
            (call fun1 1 2 3))])
  (test-case "Simple case allows return lambda"
             (check-match (uniquify x)
                           `(module
                            (define ,fun1.4
                              (lambda (,x.7 ,y.6 ,z.5) (lambda (,x.10 ,y.9 ,z.8) (call + ,x.10 ,y.9))))
                            (call ,fun1.4 1 2 3)))))

(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (lambda (x y z) (lambda (x y) (lambda (y) y)))))
            (call fun1 1 2 3))])
  (test-case "Simple case allows nested lambda "
             (check-match (uniquify x)
                           `(module
                            (define ,fun1.4
                              (lambda (,x.7 ,y.6 ,z.5)
                                (lambda (,x.10 ,y.9 ,z.8) (lambda (,x.12 ,y.11) (lambda (,y.13) ,y.13)))))
                            (call ,fun1.4 1 2 3)))))


(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (lambda (x y z) (lambda (x y) (lambda (y) (call void? y))))))
            (call fun1 1 2 3))])
  (test-case "Simple case allows lambda nest call"
             (check-match (uniquify x)
                           `(module
                            (define ,fun1.4
                              (lambda (,x.7 ,y.6 ,z.5)
                                (lambda (,x.10 ,y.9 ,z.8)
                                  (lambda (,x.12 ,y.11) (lambda (,y.13) (call void? ,y.13))))))
                            (call ,fun1.4 1 2 3)))))


(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (lambda (x y z) (lambda (x y) (lambda (y) (if (call void? y) y (error 5)))))))
            (call fun1 1 2 3))])
  (test-case "Simple case allows lambda nest if"
             (check-match (uniquify x)
                           `(module
                            (define ,fun1.4
                              (lambda (,x.7 ,y.6 ,z.5)
                                (lambda (,x.10 ,y.9 ,z.8)
                                  (lambda (,x.12 ,y.11)
                                    (lambda (,y.13) (if (call void? ,y.13) ,y.13 (error 5)))))))
                            (call ,fun1.4 1 2 3)))))




(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (lambda (x y z) (lambda (x y) (lambda (y) (let ([x y] [z (call + y y)]) (call + x z)))))))
            (call fun1 1 2 3))])
  (test-case "Simple case allows lambda nest let"
             (check-match (uniquify x)
                           `(module
                            (define ,fun1.4
                              (lambda (,x.7 ,y.6 ,z.5)
                                (lambda (,x.10 ,y.9 ,z.8)
                                  (lambda (,x.12 ,y.11)
                                    (lambda (,y.13)
                                      (let ((,x.15 ,y.13) (,z.14 (call + ,y.13 ,y.13)))
                                        (call + ,x.15 ,z.14)))))))
                            (call ,fun1.4 1 2 3)))))


(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (lambda (x y z) (call (lambda (x y) (call + x y)) (lambda (y z) (call + y z)) y))))
            (call fun1 1 2 3))])
  (test-case "Simple case allows lambda in call"
             (check-match (uniquify x)
                           `(module
                            (define ,fun1.4
                              (lambda (,x.7 ,y.6 ,z.5)
                                (lambda (,x.10 ,y.9 ,z.8)
                                  (call
                                  (lambda (,x.12 ,y.11) (call + ,x.12 ,y.11))
                                  (lambda (,y.14 ,z.13) (call + ,y.14 ,z.13))
                                  ,y.9))))
                            (call ,fun1.4 1 2 3)))))



(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (if (call procedure? x)
                            (lambda (y z) (call x y z))
                            (call procedure-arity (lambda (x y) x)))))
            (call fun1 1 2 3))])
  (test-case "Simple case allows lambda in if"
             (check-match (uniquify x)
                           `(module
                            (define ,fun1.4
                              (lambda (,x.7 ,y.6 ,z.5)
                                (if (call procedure? ,x.7)
                                  (lambda (,y.9 ,z.8) (call ,x.7 ,y.9 ,z.8))
                                  (call procedure-arity (lambda (,x.11 ,y.10) ,x.11)))))
                            (call ,fun1.4 1 2 3)))))

(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (if (call (lambda (x y) (call < x y)) y z)
                            (lambda (y z) (call x y z))
                            (lambda () empty))))
            (call fun1 1 2 3))])
  (test-case "Simple case allows lambda in if 2"
             (check-match (uniquify x)
                           `(module
                            (define ,fun1.4
                              (lambda (,x.7 ,y.6 ,z.5)
                                (if (call (lambda (,x.9 ,y.8) (call < ,x.9 ,y.8)) ,y.6 ,z.5)
                                  (lambda (,y.11 ,z.10) (call ,x.7 ,y.11 ,z.10))
                                  (lambda () empty))))
                            (call ,fun1.4 1 2 3)))))



(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (let ([x (call (lambda (x y) (call + x y)) x y)] 
                              [y (lambda (x y) (call * x y))])
                              (call y x (call (lambda (x z) (call - x z)) x z)))))
            (call fun1 1 2 3))])
  (test-case "Simple case allows lambda in let"
             (check-match (uniquify x)
                           `(module
                            (define ,fun1.4
                              (lambda (,x.7 ,y.6 ,z.5)
                                (let ((,x.9 (call (lambda (,x.11 ,y.10) (call + ,x.11 ,y.10)) ,x.7 ,y.6))
                                      (,y.8 (lambda (,x.13 ,y.12) (call * ,x.13 ,y.12))))
                                  (call
                                  ,y.8
                                  ,x.9
                                  (call (lambda (,x.15 ,z.14) (call - ,x.15 ,z.14)) ,x.9 ,z.5)))))
                            (call ,fun1.4 1 2 3)))))


(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (let ([+ procedure-arity])
                              (call + x))))
            (call fun1 1 2 3))])
  (test-case "Simple case shadow"
             (check-match (uniquify x)
                           `(module
                            (define ,fun1.4
                              (lambda (,x.7 ,y.6 ,z.5) (let ((,|+.8| procedure-arity)) (call ,|+.8| ,x.7))))
                            (call ,fun1.4 1 2 3)))))




(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (let ([cons (lambda (x y z) (call procedure-arity x))])
                              (call cons x y z))))
            (call fun1 1 2 3))])
  (test-case "Simple case shadow"
             (check-match (uniquify x)
                           `(module
                            (define ,fun1.4
                              (lambda (,x.7 ,y.6 ,z.5)
                                (let ((,cons.8 (lambda (,x.11 ,y.10 ,z.9) (call procedure-arity ,x.11))))
                                  (call ,cons.8 ,x.7 ,y.6 ,z.5))))
                            (call ,fun1.4 1 2 3)))))


;; ---------------------------------------- old tests ------------------------------------------------
(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (call cons x y)))
            (call fun1 1 2 3))])
  (test-case "Simple case with call"
             (check-match (uniquify x)
                           `(module
                            (define ,fun.4 (lambda (,x.3 ,y.2 ,z.1) (call cons ,x.3 ,y.2)))
                            (call ,fun.4 1 2 3)))))

(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (if (call pair? x)
                            (call + (call car x) (call cdr x))
                            (if (call vector? x)
                              (call vector-length x)
                              (error 8)))))
            (call fun1 1 2 3))])
  (test-case "Simple case with if"
             (check-match (uniquify x)
                           `(module
                            (define ,fun.4
                              (lambda (,x.3 ,y.2 ,z.1)
                                (if (call pair? ,x.3)
                                  (call + (call car ,x.3) (call cdr ,x.3))
                                  (if (call vector? ,x.3) (call vector-length ,x.3) (error 8)))))
                            (call ,fun.4 1 2 3)))))


(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (let ([x (call make-vector x)]
                              [y (call vector-ref y z)])
                              (call vector-set! x y z))))
            (call fun1 1 2 3))])
  (test-case "Simple case with let"
             (check-match (uniquify x)
                           `(module
                            (define ,fun.4
                              (lambda (,x.3 ,y.2 ,z.1)
                                (let ((,x.5 (call make-vector ,x.3)) (,y.4 (call vector-ref ,y.2 ,z.1)))
                                  (call vector-set! ,x.5 ,y.4 ,z.1))))
                            (call ,fun.4 1 2 3)))))



(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (let ([vector-ref make-vector])
                              (call vector-ref x))))
            (call fun1 1 2 3))])
  (test-case "Simple case shadow"
             (check-match (uniquify x)
                           `(module
                            (define ,fun.4
                              (lambda (,x.3 ,y.2 ,z.1)
                                (let ((,vector-ref.4 make-vector)) (call ,vector-ref.4 ,x.3))))
                            (call ,fun.4 1 2 3)))))




(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (let ([cons vector-set!])
                              (call cons x y z))))
            (call fun1 1 2 3))])
  (test-case "Simple case shadow"
             (check-match (uniquify x)
                           `(module
                            (define ,fun.4
                              (lambda (,x.3 ,y.2 ,z.1)
                                (let ((,cons.4 vector-set!)) (call ,cons.4 ,x.3 ,y.2 ,z.1))))
                            (call ,fun.4 1 2 3)))))



(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (if (call (let ([vector? pair?]) vector?) x) (call cdr (call fun1 y (call car z))) (call vector-set! (call make-vector z) (call vector-length x) z))))
            (call fun1 1 2 3))])
  (test-case "if with let"
             (check-match (uniquify x)
                           `(module
                            (define ,fun.4
                              (lambda (,x.3 ,y.2 ,z.1)
                                (if (call (let ((,vector?.4 pair?)) ,vector?.4) ,x.3)
                                  (call cdr (call ,fun.4 ,y.2 (call car ,z.1)))
                                  (call
                                  vector-set!
                                  (call make-vector ,z.1)
                                  (call vector-length ,x.3)
                                  ,z.1))))
                            (call ,fun.4 1 2 3)))))

;; error
(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (call eq? (call make-vector (call vector-length (call make-vector z))) (call cons (let ([x (call make-vector 2)]) (let ([x (call vector-length x)]) x)) z))))
            (call fun1 1 2 3))])
  (test-case "nested call"
             (check-match (uniquify x)
                           `(module
                            (define ,fun.4
                              (lambda (,x.3 ,y.2 ,z.1)
                                (call
                                eq?
                                (call make-vector (call vector-length (call make-vector ,z.1)))
                                (call
                                  cons
                                  (let ((,x.4 (call make-vector 2)))
                                    (let ((,x.5 (call vector-length ,x.4))) ,x.5))
                                  ,z.1))))
                            (call ,fun.4 1 2 3)))))
;; ok
(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (let ([x (call vector-set! y x z)] [y (call vector-length y)]) (let ([x (call vector? y)] [y 5]) (if x (call make-vector z) (call make-vector y))))))
            (call fun1 1 2 3))])
  (test-case "nested let"
             (check-match (uniquify x)
                           `(module
                            (define ,fun.4
                              (lambda (,x.3 ,y.2 ,z.1)
                                (let ((,x.5 (call vector-set! ,y.2 ,x.3 ,z.1))
                                      (,y.4 (call vector-length ,y.2)))
                                  (let ((,x.7 (call vector? ,y.4)) (,y.6 5))
                                    (if ,x.7 (call make-vector ,z.1) (call make-vector ,y.6))))))
                            (call ,fun.4 1 2 3)))))
;; ok
(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (if (if (call vector-ref x y) (call cons y z) (call cdr x)) (if #t (call vector-ref x y) (call vector-set! x y 5)) (call error? z))))
            (call fun1 1 2 3))])
  (test-case "nested if"
             (check-match (uniquify x)
                           `(module
                            (define ,fun.4
                              (lambda (,x.3 ,y.2 ,z.1)
                                (if (if (call vector-ref ,x.3 ,y.2) (call cons ,y.2 ,z.1) (call cdr ,x.3))
                                  (if #t (call vector-ref ,x.3 ,y.2) (call vector-set! ,x.3 ,y.2 5))
                                  (call error? ,z.1))))
                            (call ,fun.4 1 2 3)))))

;; -------------------------------------------------- old cases --------------------------------
(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        x))
            (call fun1 1 2 3))])
  (test-case "Simple case with value"
             (check-match (uniquify x)
                           `(module (define ,fun.1 (lambda (,x.3 ,y.2 ,z.1) ,x.3)) (call ,fun.1 1 2 3)))))

(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (call fun2 x y z)))
                (define fun2
                    (lambda (x y z)
                        y))
            (call fun1 1 2 3))])
  (test-case "Simple case with call"
             (check-match (uniquify x)
                           `(module
                            (define ,fun.1 (lambda (,x.3 ,y.2 ,z.1) (call ,fun.2 ,x.3 ,y.2 ,z.1)))
                            (define ,fun.2 (lambda (,x.6 ,y.5 ,z.4) ,y.5))
                            (call ,fun.1 1 2 3)))))

(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (if x (call fun2 2 y z) (call fun2 y y z))))
                (define fun2
                    (lambda (x y z)
                        y))
            (call fun1 #t 2 3))])
  (test-case "Simple case with if"
             (check-match (uniquify x)
                           `(module
                            (define ,fun.1
                              (lambda (,x.3 ,y.2 ,z.1)
                                (if ,x.3 (call ,fun.2 2 ,y.2 ,z.1) (call ,fun.2 ,y.2 ,y.2 ,z.1))))
                            (define ,fun.2 (lambda (,x.6 ,y.5 ,z.4) ,y.5))
                            (call ,fun.1 #t 2 3)))))


(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (let ([x 1] [y 2] [z 3]) x)))
            (call fun1 1 2 3))])
  (test-case "Simple case with let"
             (check-match (uniquify x)
                           `(module
                            (define ,fun.1 (lambda (,x.3 ,y.2 ,z.1) (let ((,x.6 1) (,y.5 2) (,z.4 3)) ,x.6)))
                            (call ,fun.1 1 2 3)))))

(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (let ([fixnum? void?] [y 2] [z 3]) fixnum?)))
            (call fun1 1 2 3))])
  (test-case "Shadow unop"
             (check-match (uniquify x)
                           `(module
                            (define ,fun.1
                              (lambda (,x.3 ,y.2 ,z.1) (let ((,fixnum?.6 void?) (,y.5 2) (,z.4 3)) ,fixnum?.6)))
                            (call ,fun.1 1 2 3)))))

(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (let ([<= >=] [y 2] [z 3]) <=)))
            (call fun1 1 2 3))])
  (test-case "Shadow binop"
             (check-match (uniquify x)
                           `(module
                            (define ,fun.1
                              (lambda (,x.3 ,y.2 ,z.1) (let ((,<=.6 >=) (,y.5 2) (,z.4 3)) ,<=.6)))
                            (call ,fun.1 1 2 3)))))


(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (if (call < x y) (call > x z) (call eq? y z))))
            (call fun1 1 2 3))])
  (test-case "if with call"
             (check-match (uniquify x)
                           `(module
                            (define ,fun.1
                              (lambda (,x.3 ,y.2 ,z.1)
                                (if (call < ,x.3 ,y.2) (call > ,x.3 ,z.1) (call eq? ,y.2 ,z.1))))
                            (call ,fun.1 1 2 3)))))

(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (if (call (let ([>= <=]) >=) x y) (call > (call fun1 y x z) z) (call eq? (let ([y 5]) y) z))))
            (call fun1 1 2 3))])
  (test-case "if with let"
             (check-match (uniquify x)
                           `(module
                            (define ,fun.1
                              (lambda (,x.3 ,y.2 ,z.1)
                                (if (call (let ((,>=.4 <=)) ,>=.4) ,x.3 ,y.2)
                                  (call > (call ,fun.1 ,y.2 ,x.3 ,z.1) ,z.1)
                                  (call eq? (let ((,y.5 5)) ,y.5) ,z.1))))
                            (call ,fun.1 1 2 3)))))


(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (call eq? (call fun1 x y (call fun1 1 2 z)) (call <= x z))))
            (call fun1 1 2 3))])
  (test-case "nested call"
             (check-match (uniquify x)
                           `(module
                            (define ,fun.1
                              (lambda (,x.3 ,y.2 ,z.1)
                                (call
                                eq?
                                (call ,fun.1 ,x.3 ,y.2 (call ,fun.1 1 2 ,z.1))
                                (call <= ,x.3 ,z.1))))
                            (call ,fun.1 1 2 3)))))

(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (let ([x (call fun1 y x z)] [y 5]) (let ([x 10] [y 5]) (call void? x)))))
            (call fun1 1 2 3))])
  (test-case "nested let"
             (check-match (uniquify x)
                           `(module
                            (define ,fun.1
                              (lambda (,x.3 ,y.2 ,z.1)
                                (let ((,x.5 (call ,fun.1 ,y.2 ,x.3 ,z.1)) (,y.4 5))
                                  (let ((,x.7 10) (,y.6 5)) (call void? ,x.7)))))
                            (call ,fun.1 1 2 3)))))

(let ([x '(module
                (define fun1
                    (lambda (x y z)
                        (if (if (call eq? x y) (call <= y z) (call > x z)) (if #t 5 empty) (call error? z))))
            (call fun1 1 2 3))])
  (test-case "nested if"
             (check-match (uniquify x)
                           `(module
                            (define ,fun.1
                              (lambda (,x.3 ,y.2 ,z.1)
                                (if (if (call eq? ,x.3 ,y.2) (call <= ,y.2 ,z.1) (call > ,x.3 ,z.1))
                                  (if #t 5 empty)
                                  (call error? ,z.1))))
                            (call ,fun.1 1 2 3)))))


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
  (test-case "General case in textbook"
             (check-match (uniquify x)
                           `(module
                            (define ,odd?.1
                              (lambda (,x.1)
                                (if (call eq? ,x.1 0)
                                  0
                                  (let ((,y.2 (call + ,x.1 -1))) (call ,even?.2 ,y.2)))))
                            (define ,even?.2
                              (lambda (,x.3)
                                (if (call eq? ,x.3 0)
                                  1
                                  (let ((,y.4 (call + ,x.3 -1))) (call ,odd?.1 ,y.4)))))
                            (call ,even?.2 5)))))