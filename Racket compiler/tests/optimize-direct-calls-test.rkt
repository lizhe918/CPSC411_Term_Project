#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/ptr-run-time
 racket/match
 rackunit)

(require "../component/opt-call.rkt")

; input: just-exprs-lang-v9
; output: just-exprs-lang-v9
; purpose: Check if the outputs of optimize-direct-calls matches the desired outputs of the interrogator.



(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call fun1.1 y.1 z.1 x.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "cannot optimize general lambda"
             (check-equal? (optimize-direct-calls x)
                           `(module
                            (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call fun1.1 y.1 z.1 x.1))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))

(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call (lambda (x.2) x.2) 1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "optimize direct call"
             (check-equal? (optimize-direct-calls x)
                           `(module
                                (letrec ((fun1.1 (lambda (x.1 y.1 z.1) (let ((x.2 1)) x.2))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))

(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call (lambda (x.2 y.2 z.2) (unsafe-fx+ x.2 y.2)) 1 2 3))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "optimize direct call with several arguments"
             (check-equal? (optimize-direct-calls x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1)
                                        (let ((x.2 1) (y.2 2) (z.2 3)) (unsafe-fx+ x.2 y.2)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))

(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call (lambda (x.2 y.2 z.2) (unsafe-fx+ x.2 y.2)) x.1 y.1 z.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "optimize direct call with several arguments with v"
             (check-equal? (optimize-direct-calls x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1)
                                        (let ((x.2 x.1) (y.2 y.1) (z.2 z.1)) (unsafe-fx+ x.2 y.2)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))

(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call (lambda (x.2 y.2 z.2) (unsafe-fx+ x.2 y.2)) (unsafe-fx+ x.1 y.1) y.1 z.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "optimize direct call with several arguments with primop"
             (check-equal? (optimize-direct-calls x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1)
                                        (let ((x.2 (unsafe-fx+ x.1 y.1)) (y.2 y.1) (z.2 z.1))
                                                (unsafe-fx+ x.2 y.2)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call (lambda (x.2 y.2 z.2) (unsafe-fx+ x.2 y.2)) (unsafe-procedure-call fun1.1 1 2 3) y.1 z.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "optimize direct call with several arguments with call"
             (check-equal? (optimize-direct-calls x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1)
                                        (let ((x.2 (unsafe-procedure-call fun1.1 1 2 3))
                                                (y.2 y.1)
                                                (z.2 z.1))
                                                (unsafe-fx+ x.2 y.2)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call (lambda (x.2 y.2 z.2) (unsafe-fx+ x.2 y.2)) (letrec ([x.3 (lambda () x.3)]) x.3) y.1 z.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "optimize direct call with several arguments with letrec"
             (check-equal? (optimize-direct-calls x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1)
                                        (let ((x.2 (letrec ((x.3 (lambda () x.3))) x.3))
                                                (y.2 y.1)
                                                (z.2 z.1))
                                                (unsafe-fx+ x.2 y.2)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))



(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call (lambda (x.2 y.2 z.2) (unsafe-fx+ x.2 y.2)) (let ([x.3 5]) x.3) y.1 z.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "optimize direct call with several arguments with let"
             (check-equal? (optimize-direct-calls x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1)
                                        (let ((x.2 (let ((x.3 5)) x.3)) (y.2 y.1) (z.2 z.1))
                                                (unsafe-fx+ x.2 y.2)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call (lambda (x.2 y.2 z.2) (unsafe-fx+ x.2 y.2)) (if (true) x.1 y.1) y.1 z.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "optimize direct call with several arguments with if"
             (check-equal? (optimize-direct-calls x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1)
                                        (let ((x.2 (if (true) x.1 y.1)) (y.2 y.1) (z.2 z.1))
                                                (unsafe-fx+ x.2 y.2)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call (lambda (x.2 y.2 z.2) (unsafe-fx+ x.2 y.2)) (begin x.1) y.1 z.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "optimize direct call with several arguments with begin"
             (check-equal? (optimize-direct-calls x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1)
                                        (let ((x.2 (begin x.1)) (y.2 y.1) (z.2 z.1))
                                                (unsafe-fx+ x.2 y.2)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))



;; nested case
;; I think interrogator is wrong in this case
(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call (lambda (x.2 y.2 z.2) (unsafe-fx+ x.2 y.2)) (lambda () (unsafe-procedure-call (lambda (x.3 y.3 z.3) x.3) 1 2 3)) y.1 z.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "optimize nested call"
             (check-equal? (optimize-direct-calls x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1)
                                        (let ((x.2 (lambda () (let ((x.3 1) (y.3 2) (z.3 3)) x.3)))
                                                (y.2 y.1)
                                                (z.2 z.1))
                                                (unsafe-fx+ x.2 y.2)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call (lambda (x.2 y.2 z.2) (unsafe-fx+ x.2 y.2)) (unsafe-procedure-call (lambda (x.3 y.3 z.3) x.3) 1 2 3) y.1 z.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "optimize nested call"
             (check-equal? (optimize-direct-calls x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1)
                                        (let ((x.2 (let ((x.3 1) (y.3 2) (z.3 3)) x.3))
                                                (y.2 y.1)
                                                (z.2 z.1))
                                                (unsafe-fx+ x.2 y.2)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))

(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call (lambda (x.2 y.2 z.2) (unsafe-fx+ x.2 y.2)) (unsafe-fx+ (unsafe-procedure-call (lambda (x.3 y.3 z.3) x.3) 1 2 3) 5) y.1 z.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "optimize nested call in primop"
             (check-equal? (optimize-direct-calls x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1)
                                        (let ((x.2 (unsafe-fx+ (let ((x.3 1) (y.3 2) (z.3 3)) x.3) 5))
                                                (y.2 y.1)
                                                (z.2 z.1))
                                                (unsafe-fx+ x.2 y.2)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call (lambda (x.2 y.2 z.2) (unsafe-fx+ x.2 y.2)) (letrec ([x.3 (lambda () (unsafe-procedure-call (lambda (x.3 y.3 z.3) x.3) 1 2 3))]) (unsafe-procedure-call (lambda (x.3 y.3 z.3) x.3) 1 2 3)) y.1 z.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "optimize nested call in letrec"
             (check-equal? (optimize-direct-calls x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1)
                                        (let ((x.2
                                                (letrec ((x.3
                                                        (lambda ()
                                                                (let ((x.3 1) (y.3 2) (z.3 3)) x.3))))
                                                (let ((x.3 1) (y.3 2) (z.3 3)) x.3)))
                                                (y.2 y.1)
                                                (z.2 z.1))
                                                (unsafe-fx+ x.2 y.2)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))

;; ok
(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call (lambda (x.2 y.2 z.2) (unsafe-fx+ x.2 y.2)) (let ([x.3 (unsafe-procedure-call (lambda (x.3 y.3 z.3) x.3) 1 2 3)]) 5) y.1 z.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "optimize nested call in let bind"
             (check-equal? (optimize-direct-calls x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1)
                                        (let ((x.2 (let ((x.3 (let ((x.3 1) (y.3 2) (z.3 3)) x.3))) 5))
                                                (y.2 y.1)
                                                (z.2 z.1))
                                                (unsafe-fx+ x.2 y.2)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call (lambda (x.2 y.2 z.2) (unsafe-fx+ x.2 y.2)) (let ([x.3 5]) (unsafe-procedure-call (lambda (x.3 y.3 z.3) x.3) 1 2 3)) y.1 z.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "optimize nested call in let body"
             (check-equal? (optimize-direct-calls x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1)
                                        (let ((x.2 (let ((x.3 5)) (let ((x.3 1) (y.3 2) (z.3 3)) x.3)))
                                                (y.2 y.1)
                                                (z.2 z.1))
                                                (unsafe-fx+ x.2 y.2)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))

(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call (lambda (x.2 y.2 z.2) (unsafe-fx+ x.2 y.2)) (if (true) 5 (unsafe-procedure-call (lambda (x.3 y.3 z.3) x.3) 1 2 3)) y.1 z.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "optimize nested call in if e"
             (check-equal? (optimize-direct-calls x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1)
                                        (let ((x.2 (if (true) 5 (let ((x.3 1) (y.3 2) (z.3 3)) x.3)))
                                                (y.2 y.1)
                                                (z.2 z.1))
                                                (unsafe-fx+ x.2 y.2)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call (lambda (x.2 y.2 z.2) (unsafe-fx+ x.2 y.2)) (if (let ([x.3 (unsafe-procedure-call (lambda (x.3 y.3 z.3) x.3) 1 2 3)]) (true)) 1 2) y.1 z.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "optimize nested call in pred"
             (check-equal? (optimize-direct-calls x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1)
                                        (let ((x.2
                                                (if (let ((x.3 (let ((x.3 1) (y.3 2) (z.3 3)) x.3)))
                                                        (true))
                                                1
                                                2))
                                                (y.2 y.1)
                                                (z.2 z.1))
                                                (unsafe-fx+ x.2 y.2)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))

(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call (lambda (x.2 y.2 z.2) (unsafe-fx+ x.2 y.2)) (begin (unsafe-procedure-call (lambda (x.3 y.3 z.3) x.3) 1 2 3)) y.1 z.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "optimize nested call in begin e"
             (check-equal? (optimize-direct-calls x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1)
                                        (let ((x.2 (begin (let ((x.3 1) (y.3 2) (z.3 3)) x.3)))
                                                (y.2 y.1)
                                                (z.2 z.1))
                                                (unsafe-fx+ x.2 y.2)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))

(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call (lambda (x.2 y.2 z.2) (unsafe-fx+ x.2 y.2)) (begin (unsafe-vector-set! x.1 (unsafe-procedure-call (lambda (x.3 y.3 z.3) x.3) 1 2 3) 10) 5) y.1 z.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "optimize nested call in begin effect"
             (check-equal? (optimize-direct-calls x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1)
                                        (let ((x.2
                                                (begin
                                                (unsafe-vector-set!
                                                        x.1
                                                        (let ((x.3 1) (y.3 2) (z.3 3)) x.3)
                                                        10)
                                                5))
                                                (y.2 y.1)
                                                (z.2 z.1))
                                                (unsafe-fx+ x.2 y.2)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))

(check-equal?    (optimize-direct-calls `(module
                            (lambda (x.1 x.2 x.3)
                              (unsafe-procedure-call (lambda (x.4)
                                                     x.4) 1))))
                       `(module (lambda (x.1 x.2 x.3) 
                                  (let ((x.4 1))
                                    x.4)
                                  )))