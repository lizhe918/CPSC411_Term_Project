#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/ptr-run-time
 racket/match
 rackunit)

(require "../component/impl-safe-call.rkt")

; input: exprs-unsafe-data-lang-v9?
; output: exprs-unsafe-lang-v9?
; purpose: Check if the outputs of implement-safe-call matches the desired outputs of the interrogator.

;; "If you keep track of the arity of procedures, you can optimize this transformation in some cases."


(let ([x '(module
                (define eq?.57 (lambda (tmp.53 tmp.54) (eq? tmp.53 tmp.54)))
                (define vector-init-loop.6
                (lambda (len.7 i.9 vec.8)
                    (if (eq? len.7 i.9)
                    vec.8
                    (begin
                        (unsafe-vector-set! vec.8 i.9 0)
                        (call vector-init-loop.6 len.7 (unsafe-fx+ i.9 1) vec.8)))))
                (define make-init-vector.1
                (lambda (tmp.4)
                    (let ((tmp.5 (unsafe-make-vector tmp.4)))
                    (call vector-init-loop.6 tmp.4 0 tmp.5))))
                (define make-vector.56
                (lambda (tmp.32)
                    (if (fixnum? tmp.32) (call make-init-vector.1 tmp.32) (error 8))))
                (call eq?.57 (call make-vector.56 1) (call make-vector.56 1)))])
  (test-case "new error case"
             (check-equal? (implement-safe-call x)
                           '(module
                            (define eq?.57 (lambda (tmp.53 tmp.54) (eq? tmp.53 tmp.54)))
                            (define vector-init-loop.6
                            (lambda (len.7 i.9 vec.8)
                                (if (eq? len.7 i.9)
                                vec.8
                                (begin
                                    (unsafe-vector-set! vec.8 i.9 0)
                                    (unsafe-procedure-call
                                    vector-init-loop.6
                                    len.7
                                    (unsafe-fx+ i.9 1)
                                    vec.8)))))
                            (define make-init-vector.1
                            (lambda (tmp.4)
                                (let ((tmp.5 (unsafe-make-vector tmp.4)))
                                (unsafe-procedure-call vector-init-loop.6 tmp.4 0 tmp.5))))
                            (define make-vector.56
                            (lambda (tmp.32)
                                (if (fixnum? tmp.32)
                                (unsafe-procedure-call make-init-vector.1 tmp.32)
                                (error 8))))
                            (unsafe-procedure-call
                            eq?.57
                            (unsafe-procedure-call make-vector.56 1)
                            (unsafe-procedure-call make-vector.56 1))))))

(let ([x '(module
                (define |+.63|
                (lambda (tmp.26 tmp.27)
                    (if (fixnum? tmp.27)
                    (if (fixnum? tmp.26) (unsafe-fx+ tmp.26 tmp.27) (error 2))
                    (error 2))))
                (define eq?.62 (lambda (tmp.59 tmp.60) (eq? tmp.59 tmp.60)))
                (define odd?.4
                (lambda (x.6)
                    (if (call eq?.62 x.6 0)
                    #f
                    (let ((y.7 (call |+.63| x.6 -1))) (call even?.5 y.7)))))
                (define even?.5
                (lambda (x.8)
                    (if (call eq?.62 x.8 0)
                    #t
                    (let ((y.9 (call |+.63| x.8 -1))) (call odd?.4 y.9)))))
                (call even?.5 5))])
  (test-case "error case"
             (check-equal? (implement-safe-call x)
                           '(module
                                (define |+.63|
                                (lambda (tmp.26 tmp.27)
                                    (if (fixnum? tmp.27)
                                    (if (fixnum? tmp.26) (unsafe-fx+ tmp.26 tmp.27) (error 2))
                                    (error 2))))
                                (define eq?.62 (lambda (tmp.59 tmp.60) (eq? tmp.59 tmp.60)))
                                (define odd?.4
                                (lambda (x.6)
                                    (if (unsafe-procedure-call eq?.62 x.6 0)
                                    #f
                                    (let ((y.7 (unsafe-procedure-call |+.63| x.6 -1)))
                                        (unsafe-procedure-call even?.5 y.7)))))
                                (define even?.5
                                (lambda (x.8)
                                    (if (unsafe-procedure-call eq?.62 x.8 0)
                                    #t
                                    (let ((y.9 (unsafe-procedure-call |+.63| x.8 -1)))
                                        (unsafe-procedure-call odd?.4 y.9)))))
                                (unsafe-procedure-call even?.5 5)))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        (call fun1.1 y.1 z.1 x.1)))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement unsafe call 1"
             (check-equal? (implement-safe-call x)
                           `(module
                            (define fun1.1
                                (lambda (x.1 y.1 z.1) (unsafe-procedure-call fun1.1 y.1 z.1 x.1)))
                            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))))



(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        (call (lambda (x.2 y.2) (unsafe-fx+ x.2 y.2)) x.1 y.1)))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement unsafe call 2"
             (check-match (implement-safe-call x)
                           `(module
                            (define fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((,tmp.4 (lambda (x.2 y.2) (unsafe-fx+ x.2 y.2))))
                                    (if (procedure? ,tmp.4)
                                    (if (eq? (unsafe-procedure-arity ,tmp.4) 2)
                                        (unsafe-procedure-call ,tmp.4 x.1 y.1)
                                        (error 42))
                                    (error 43)))))
                            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))))

(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        (eq? (call (lambda (x.2 y.2) (unsafe-fx+ x.2 y.2)) x.1 y.1)
                              z.1)))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement unsafe call in (primop e ...)"
             (check-match (implement-safe-call x)
                           `(module
                            (define fun1.1
                                (lambda (x.1 y.1 z.1)
                                (eq?
                                (let ((,tmp.4 (lambda (x.2 y.2) (unsafe-fx+ x.2 y.2))))
                                    (if (procedure? ,tmp.4)
                                    (if (eq? (unsafe-procedure-arity ,tmp.4) 2)
                                        (unsafe-procedure-call ,tmp.4 x.1 y.1)
                                        (error 42))
                                    (error 43)))
                                z.1)))
                            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))))

(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        (call (lambda (x.2 y.2) (unsafe-fx+ x.2 y.2)) (call (lambda (x.3 y.3) (unsafe-fx- x.3 y.3)) x.1 y.1) y.1)))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement unsafe call in (call e ...)"
             (check-match (implement-safe-call x)
                           `(module
                            (define fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((,tmp.4 (lambda (x.2 y.2) (unsafe-fx+ x.2 y.2))))
                                    (if (procedure? ,tmp.4)
                                    (if (eq? (unsafe-procedure-arity ,tmp.4) 2)
                                        (unsafe-procedure-call
                                        ,tmp.4
                                        (let ((,tmp.5 (lambda (x.3 y.3) (unsafe-fx- x.3 y.3))))
                                        (if (procedure? ,tmp.5)
                                            (if (eq? (unsafe-procedure-arity ,tmp.5) 2)
                                            (unsafe-procedure-call ,tmp.5 x.1 y.1)
                                            (error 42))
                                            (error 43)))
                                        y.1)
                                        (error 42))
                                    (error 43)))))
                            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))))

(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        (let ([q.1 (call (lambda (x.2 y.2) (unsafe-fx+ x.2 y.2)) x.1 y.1)]
                              [m.1 (call (lambda (y.3 z.2) (unsafe-fx- y.3 z.2)) y.1 z.1)])
                            (call (lambda (x.3 y.4) x.3)))))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement unsafe call in (let ([aloc e] ...))"
             (check-match (implement-safe-call x)
                           `(module
                            (define fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((q.1
                                        (let ((,tmp.4 (lambda (x.2 y.2) (unsafe-fx+ x.2 y.2))))
                                        (if (procedure? ,tmp.4)
                                            (if (eq? (unsafe-procedure-arity ,tmp.4) 2)
                                            (unsafe-procedure-call ,tmp.4 x.1 y.1)
                                            (error 42))
                                            (error 43))))
                                        (m.1
                                        (let ((,tmp.5 (lambda (y.3 z.2) (unsafe-fx- y.3 z.2))))
                                        (if (procedure? ,tmp.5)
                                            (if (eq? (unsafe-procedure-arity ,tmp.5) 2)
                                            (unsafe-procedure-call ,tmp.5 y.1 z.1)
                                            (error 42))
                                            (error 43)))))
                                    (let ((,tmp.6 (lambda (x.3 y.4) x.3)))
                                    (if (procedure? ,tmp.6)
                                        (if (eq? (unsafe-procedure-arity ,tmp.6) 0)
                                        (unsafe-procedure-call ,tmp.6)
                                        (error 42))
                                        (error 43))))))
                            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (not (call (lambda (x.2 y.2) (unsafe-fx<= x.2 y.2)) x.1 y.1))
                            empty
                            (void))))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement unsafe call in pred"
             (check-match (implement-safe-call x)
                           `(module
                            (define fun1.1
                                (lambda (x.1 y.1 z.1)
                                (if (not
                                    (let ((,tmp.4 (lambda (x.2 y.2) (unsafe-fx<= x.2 y.2))))
                                        (if (procedure? ,tmp.4)
                                        (if (eq? (unsafe-procedure-arity ,tmp.4) 2)
                                            (unsafe-procedure-call ,tmp.4 x.1 y.1)
                                            (error 42))
                                        (error 43))))
                                    empty
                                    (void))))
                            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (procedure? x.1)
                            (call (lambda (x.2 y.2) (unsafe-fx+ x.2 y.2)) x.1 z.1)
                            empty)))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement unsafe call in if"
             (check-match (implement-safe-call x)
                           `(module
                            (define fun1.1
                                (lambda (x.1 y.1 z.1)
                                (if (procedure? x.1)
                                    (let ((,tmp.4 (lambda (x.2 y.2) (unsafe-fx+ x.2 y.2))))
                                    (if (procedure? ,tmp.4)
                                        (if (eq? (unsafe-procedure-arity ,tmp.4) 2)
                                        (unsafe-procedure-call ,tmp.4 x.1 z.1)
                                        (error 42))
                                        (error 43)))
                                    empty)))
                            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        (begin (call (lambda (x.2 y.2) (unsafe-fx+ x.2 y.2)) x.1 z.1))))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement unsafe call in begin as e"
             (check-match (implement-safe-call x)
                           `(module
                            (define fun1.1
                                (lambda (x.1 y.1 z.1)
                                (begin
                                    (let ((,tmp.4 (lambda (x.2 y.2) (unsafe-fx+ x.2 y.2))))
                                    (if (procedure? ,tmp.4)
                                        (if (eq? (unsafe-procedure-arity ,tmp.4) 2)
                                        (unsafe-procedure-call ,tmp.4 x.1 z.1)
                                        (error 42))
                                        (error 43))))))
                            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))))



(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        (begin (unsafe-procedure-arity (call (lambda (x.2) x.2) x.1)) empty)))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement unsafe call in begin primop as e"
             (check-match (implement-safe-call x)
                           `(module
                            (define fun1.1
                                (lambda (x.1 y.1 z.1)
                                (begin
                                    (unsafe-procedure-arity
                                    (let ((,tmp.4 (lambda (x.2) x.2)))
                                    (if (procedure? ,tmp.4)
                                        (if (eq? (unsafe-procedure-arity ,tmp.4) 1)
                                        (unsafe-procedure-call ,tmp.4 x.1)
                                        (error 42))
                                        (error 43))))
                                    empty)))
                            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))))


(let ([x '(module
                (define fun1.1
                    (lambda (x.1 y.1 z.1)
                        (lambda (x.2 y.2 z.2) (lambda (x.3 y.3) (lambda (y.4) (call (lambda (x.5 y.5) x.5) y.4))))))
            (if (call fun1.1 1 2 3) #t #f))])
  (test-case "implement unsafe call in lambda"
             (check-match (implement-safe-call x)
                           `(module
                            (define fun1.1
                                (lambda (x.1 y.1 z.1)
                                (lambda (x.2 y.2 z.2)
                                    (lambda (x.3 y.3)
                                    (lambda (y.4)
                                        (let ((,tmp.4 (lambda (x.5 y.5) x.5)))
                                        (if (procedure? ,tmp.4)
                                            (if (eq? (unsafe-procedure-arity ,tmp.4) 1)
                                            (unsafe-procedure-call ,tmp.4 y.4)
                                            (error 42))
                                            (error 43))))))))
                            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))))


(let ([x '(module
            (define procedure-arity.57
            (lambda (tmp.56)
                (if (procedure? tmp.56) (unsafe-procedure-arity tmp.56) (error 26))))
            (call procedure-arity.57 (lambda (x.4) x.4) 2 3 4))])
  (test-case "error case"
             (check-match (implement-safe-call x)
                           `(module
                            (define ,procedure-arity.57
                            (lambda (,tmp.56)
                                (if (procedure? ,tmp.56) (unsafe-procedure-arity ,tmp.56) (error 26))))
                            (error 42)))))