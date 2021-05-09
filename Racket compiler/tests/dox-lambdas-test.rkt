#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/ptr-run-time
 racket/match
 rackunit)

(require "../component/dox-lmb.rkt")

; input: just-exprs-lang-v9
; output: lam-opticon-lang-v9
; purpose: Check if the outputs of dox-lambdas matches the desired outputs of the interrogator.


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call fun1.1 y.1 z.1 x.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "No lambda to dox"
             (check-equal? (dox-lambdas x)
                           `(module
                            (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call fun1.1 y.1 z.1 x.1))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call fun1.1 y.1 z.1 x.1))))
            (lambda (x.2 y.2) (unsafe-fx+ x.2 y.2))))])
  (test-case "Dox lambda in v"
             (check-match (dox-lambdas x)
                           `(module
                            (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call fun1.1 y.1 z.1 x.1))))
                                (letrec ((,lam.4 (lambda (x.2 y.2) (unsafe-fx+ x.2 y.2)))) ,lam.4))))))

(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) x.1)))
            (fixnum? (lambda (x.2 y.2) x.2))))])
  (test-case "Dox lambda in primop"
             (check-match (dox-lambdas x)
                           `(module
                            (letrec ((fun1.1 (lambda (x.1 y.1 z.1) x.1)))
                                (fixnum? (letrec ((,lam.4 (lambda (x.2 y.2) x.2))) ,lam.4)))))))

(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) x.1)))
            (unsafe-procedure-call (lambda (x.2 y.2) x.2) 1 2)))])
  (test-case "Dox lambda in call 1"
             (check-match (dox-lambdas x)
                           `(module
                            (letrec ((fun1.1 (lambda (x.1 y.1 z.1) x.1)))
                                (unsafe-procedure-call
                                (letrec ((,lam.4 (lambda (x.2 y.2) x.2))) ,lam.4)
                                1
                                2))))))


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) x.1)))
            (unsafe-procedure-call fun1.1 (unsafe-procedure-call (lambda (x.2 y.2) x.2) 1 2) 2 3)))])
  (test-case "Dox lambda in call 2"
             (check-match (dox-lambdas x)
                           `(module
                            (letrec ((fun1.1 (lambda (x.1 y.1 z.1) x.1)))
                                (unsafe-procedure-call
                                fun1.1
                                (unsafe-procedure-call
                                (letrec ((,lam.4 (lambda (x.2 y.2) x.2))) ,lam.4)
                                1
                                2)
                                2
                                3))))))


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) x.1)))
            (letrec ([x.2 (lambda (x.3) (unsafe-procedure-call (lambda (x.4) x.4) 3))]) 5)))])
  (test-case "Dox lambda in letrec 1"
             (check-match (dox-lambdas x)
                           `(module
                            (letrec ((fun1.1 (lambda (x.1 y.1 z.1) x.1)))
                                (letrec ((x.2
                                        (lambda (x.3)
                                            (unsafe-procedure-call
                                            (letrec ((,lam.4 (lambda (x.4) x.4))) ,lam.4)
                                            3))))
                                5))))))


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) x.1)))
            (letrec ([x.2 (lambda (x.3) (unsafe-procedure-call (lambda (x.4) x.4) 3))]) 
                    (lambda (x.1 y.1 z.1) (unsafe-procedure-call fun1.1 y.1 z.1 x.1)))))])
  (test-case "Dox lambda in letrec 1"
             (check-match (dox-lambdas x)
                           `(module
                            (letrec ((fun1.1 (lambda (x.1 y.1 z.1) x.1)))
                                (letrec ((x.2
                                        (lambda (x.3)
                                            (unsafe-procedure-call
                                            (letrec ((,lam.4 (lambda (x.4) x.4))) ,lam.4)
                                            3))))
                                (letrec ((,lam.5
                                            (lambda (x.1 y.1 z.1)
                                            (unsafe-procedure-call fun1.1 y.1 z.1 x.1))))
                                    ,lam.5)))))))


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) x.1)))
            (let ([x.2 (lambda (x.3 y.3) (unsafe-fx+ x.3 y.3))])
                 (unsafe-procedure-call x.2 2 3))))])
  (test-case "Dox lambda in let"
             (check-match (dox-lambdas x)
                           `(module
                            (letrec ((fun1.1 (lambda (x.1 y.1 z.1) x.1)))
                                (let ((x.2
                                    (letrec ((,lam.4 (lambda (x.3 y.3) (unsafe-fx+ x.3 y.3)))) ,lam.4)))
                                (unsafe-procedure-call x.2 2 3)))))))


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) x.1)))
            (if (true)
                (lambda (x.2 y.2) x.2)
                (unsafe-procedure-call (lambda (x.2 y.2) x.2) 2 3))))])
  (test-case "Dox lambda in if e"
             (check-match (dox-lambdas x)
                           `(module
                            (letrec ((fun1.1 (lambda (x.1 y.1 z.1) x.1)))
                                (if (true)
                                (letrec ((,lam.4 (lambda (x.2 y.2) x.2))) ,lam.4)
                                (unsafe-procedure-call
                                (letrec ((,lam.5 (lambda (x.2 y.2) x.2))) ,lam.5)
                                2
                                3)))))))

(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) x.1)))
            (if (not (let ([x.2 (lambda (x.2 y.2) x.2)]) (true)))
                (void)
                (void))))])
  (test-case "Dox lambda in if pred"
             (check-match (dox-lambdas x)
                           `(module
                            (letrec ((fun1.1 (lambda (x.1 y.1 z.1) x.1)))
                                (if (not
                                    (let ((x.2 (letrec ((,lam.4 (lambda (x.2 y.2) x.2))) ,lam.4))) (true)))
                                (void)
                                (void)))))))

(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) x.1)))
            (begin (unsafe-vector-set! (unsafe-make-vector 10) 0 (unsafe-procedure-call (lambda (x.2 y.2) (unsafe-fx+ x.2 y.2)) 2 3)) 10)))])
  (test-case "Dox lambda in effect"
             (check-match (dox-lambdas x)
                           `(module
                            (letrec ((fun1.1 (lambda (x.1 y.1 z.1) x.1)))
                                (begin
                                (unsafe-vector-set!
                                (unsafe-make-vector 10)
                                0
                                (unsafe-procedure-call
                                    (letrec ((,lam.4 (lambda (x.2 y.2) (unsafe-fx+ x.2 y.2)))) ,lam.4)
                                    2
                                    3))
                                10))))))



(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) x.1)))
            (begin (unsafe-vector-set! (unsafe-make-vector 10) 0 10) (lambda (x.2 y.2) x.2))))])
  (test-case "Dox lambda in begin"
             (check-match (dox-lambdas x)
                           `(module
                            (letrec ((fun1.1 (lambda (x.1 y.1 z.1) x.1)))
                                (begin
                                (unsafe-vector-set! (unsafe-make-vector 10) 0 10)
                                (letrec ((,lam.4 (lambda (x.2 y.2) x.2))) ,lam.4)))))))

(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) x.1)))
            (let ([x.2 10]) (let ([x.3 (lambda (x.2 x.3) x.2)])
                (if (let ([x.4 5]) (unsafe-fx<= (unsafe-procedure-call x.3 x.4 5) 8))
                    x.2
                    (unsafe-procedure-call (lambda (x.5) x.5) x.2))))))])
  (test-case "nested case 1"
             (check-match (dox-lambdas x)
                           `(module
                            (letrec ((fun1.1 (lambda (x.1 y.1 z.1) x.1)))
                                (let ((x.2 10))
                                (let ((x.3 (letrec ((,lam.4 (lambda (x.2 x.3) x.2))) ,lam.4)))
                                    (if (let ((x.4 5)) (unsafe-fx<= (unsafe-procedure-call x.3 x.4 5) 8))
                                    x.2
                                    (unsafe-procedure-call
                                    (letrec ((,lam.5 (lambda (x.5) x.5))) ,lam.5)
                                    x.2)))))))))

                                    
;; broke its own contract
;(let ([x `(module
;            (letrec ((fun1.1
;                        (lambda (x.1 y.1 z.1) x.1)))
;            (unsafe-fx+ (if (let ([x.4 5]) (unsafe-fx<= (unsafe-procedure-call x.3 x.4 5) 8))
;                            (unsafe-procedure-call (lambda (x.5) x.5) 10)
;                            (if (true)
;                                (let ([q.1 5] [q.2 6]) (unsafe-procedure-call (lambda (q.3 q.4) (unsafe-fx+ q.3 q.4)) q.1 q.2))
;                                (lambda (q.5) q.5)))
;                        (unsafe-procedure-call fun1.1 (let ([x.8 (lambda (x.9) x.9)]) (begin (unsafe-vector-set! 10) (unsafe-procedure-call x.8 5)))))))])
;  (test-case "nested case 2"
;             (check-match (dox-lambdas x)
;                           `(module
;                            (letrec ((fun1.1 (lambda (x.1 y.1 z.1) x.1)))
;                                (let ((x.2 10))
;                                (let ((x.3 (letrec ((,lam.4 (lambda (x.2 x.3) x.2))) ,lam.4)))
;                                    (if (let ((x.4 5)) (unsafe-fx<= (unsafe-procedure-call x.3 x.4 5) 8))
;                                    x.2
;                                    (unsafe-procedure-call
;                                    (letrec ((,lam.5 (lambda (x.5) x.5))) ,lam.5)
;                                    x.2)))))))))