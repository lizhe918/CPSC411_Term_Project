#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/ptr-run-time
 racket/match
 rackunit)

(require "../component/uncover-free.rkt")

; input: lam-opticon-lang-v9
; output: lam-free-lang-v9
; purpose: Check if the outputs of uncover-free matches the desired outputs of the interrogator.


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call fun1.1 y.1 z.1 x.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "uncover-free in lambda"
             (check-equal? (uncover-free x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda ((free (fun1.1)))
                                        (x.1 y.1 z.1)
                                        (unsafe-procedure-call fun1.1 y.1 z.1 x.1))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))))) 



(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call fun1.1 y.1 z.1 q.1))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "uncover-free in lambda in arguments"
             (check-match (uncover-free x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda ((free ,(list-no-order 'q.1 'fun1.1)))
                                        (x.1 y.1 z.1)
                                        (unsafe-procedure-call fun1.1 y.1 z.1 q.1))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))     


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (let ([q.1 2] [m.1 3] [g.1 fun.1]) (unsafe-procedure-call g.1 y.1 z.1 q.1)))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "uncover-free in let in bind"
             (check-equal? (uncover-free x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda ((free (fun.1)))
                                        (x.1 y.1 z.1)
                                        (let ((q.1 2) (m.1 3) (g.1 fun.1))
                                                (unsafe-procedure-call g.1 y.1 z.1 q.1)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))

(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (let ([q.1 2] [m.1 3]) (unsafe-procedure-call fun.1 y.1 z.1 q.1)))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "uncover-free in let in body"
             (check-equal? (uncover-free x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda ((free (fun.1)))
                                        (x.1 y.1 z.1)
                                        (let ((q.1 2) (m.1 3))
                                                (unsafe-procedure-call fun.1 y.1 z.1 q.1)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (let ([q.1 2] [m.1 3]) (let ([g.1 (letrec ([x.1 (lambda () (unsafe-procedure-call x.1))])
                                        x.1)]) g.1)))))
            5))])
  (test-case "uncover-free in nested let in body"
             (check-equal? (uncover-free x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda ((free ()))
                                        (x.1 y.1 z.1)
                                        (let ((q.1 2) (m.1 3))
                                                (let ((g.1
                                                (letrec ((x.1
                                                                (lambda ((free (x.1)))
                                                                ()
                                                                (unsafe-procedure-call x.1))))
                                                        x.1)))
                                                g.1)))))
                                5)))))




(let ([x `(module
            (letrec ([x.1 (lambda () (unsafe-procedure-call x.1))]) x.1))])
  (test-case "uncover-free in letrec in bind"
             (check-equal? (uncover-free x)
                           `(module
                                (letrec ((x.1 (lambda ((free (x.1))) () (unsafe-procedure-call x.1)))) x.1)))))



(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) (letrec ([x.1 (lambda () (unsafe-procedure-call x.1))]) (unsafe-procedure-call fun2.2 x.1)))))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f)))])
  (test-case "uncover-free in letrec in body"
             (check-equal? (uncover-free x)
                           `(module
                                (letrec ((fun1.1
                                        (lambda ((free (fun2.2)))
                                        (x.1 y.1 z.1)
                                        (letrec ((x.1
                                                        (lambda ((free (x.1)))
                                                        ()
                                                        (unsafe-procedure-call x.1))))
                                                (unsafe-procedure-call fun2.2 x.1)))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))



(let ([x `(module
                (letrec ([f.1 (lambda ()
                                (letrec ([x.1 (lambda () (unsafe-procedure-call x.1))])
                                        x.1))])
                f.1))])
  (test-case "uncover-free in letrec in body"
             (check-equal? (uncover-free x)
                           `(module
                                (letrec ((f.1
                                        (lambda ((free ()))
                                        ()
                                        (letrec ((x.1
                                                        (lambda ((free (x.1)))
                                                        ()
                                                        (unsafe-procedure-call x.1))))
                                                x.1))))
                                f.1)))))





;; uncover-free in different locations
(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) x.1)))
            (fixnum? (letrec ([x.2 (lambda (x.3 y.3 z.3) (unsafe-procedure-call fun1.1 y.3 z.3 x.3))]) x.2))))])
  (test-case "uncover-free lambda in primops"
             (check-equal? (uncover-free x)
                           `(module
                                (letrec ((fun1.1 (lambda ((free ())) (x.1 y.1 z.1) x.1)))
                                (fixnum?
                                (letrec ((x.2
                                        (lambda ((free (fun1.1)))
                                                (x.3 y.3 z.3)
                                                (unsafe-procedure-call fun1.1 y.3 z.3 x.3))))
                                x.2)))))))


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) x.1)))
            (unsafe-procedure-call (letrec ([x.2 (lambda (x.3 y.3 z.3) (unsafe-procedure-call fun1.1 y.3 z.3 x.3))]) x.2) 1 2 3)))])
  (test-case "uncover-free lambda in call"
             (check-equal? (uncover-free x)
                           `(module
                                (letrec ((fun1.1 (lambda ((free ())) (x.1 y.1 z.1) x.1)))
                                (unsafe-procedure-call
                                (letrec ((x.2
                                        (lambda ((free (fun1.1)))
                                                (x.3 y.3 z.3)
                                                (unsafe-procedure-call fun1.1 y.3 z.3 x.3))))
                                x.2)
                                1
                                2
                                3))))))


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) x.1)))
            (if (not (let ([x.2 (letrec ([x.2 (lambda (x.3 y.3 z.3) (unsafe-procedure-call fun1.1 y.3 z.3 x.3))]) x.2)]) (true)))
                 1
                 2)))])
  (test-case "uncover-free lambda in pred"
             (check-equal? (uncover-free x)
                           `(module
                                (letrec ((fun1.1 (lambda ((free ())) (x.1 y.1 z.1) x.1)))
                                (if (not
                                        (let ((x.2
                                                (letrec ((x.2
                                                        (lambda ((free (fun1.1)))
                                                        (x.3 y.3 z.3)
                                                        (unsafe-procedure-call fun1.1 y.3 z.3 x.3))))
                                                x.2)))
                                        (true)))
                                1
                                2))))))


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) x.1)))
            (if (true)
                (letrec ([x.2 (lambda (x.3 y.3 z.3) (unsafe-procedure-call fun1.1 y.3 z.3 x.3))]) x.2)
                10)))])
  (test-case "uncover-free lambda in if e"
             (check-equal? (uncover-free x)
                           `(module
                                (letrec ((fun1.1 (lambda ((free ())) (x.1 y.1 z.1) x.1)))
                                (if (true)
                                (letrec ((x.2
                                                (lambda ((free (fun1.1)))
                                                (x.3 y.3 z.3)
                                                (unsafe-procedure-call fun1.1 y.3 z.3 x.3))))
                                        x.2)
                                10))))))


(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) x.1)))
            (begin (letrec ([x.2 (lambda (x.3 y.3 z.3) (unsafe-procedure-call fun1.1 y.3 z.3 x.3))]) x.2))))])
  (test-case "uncover-free lambda in begin e"
             (check-equal? (uncover-free x)
                           `(module
                                (letrec ((fun1.1 (lambda ((free ())) (x.1 y.1 z.1) x.1)))
                                (begin
                                (letrec ((x.2
                                                (lambda ((free (fun1.1)))
                                                (x.3 y.3 z.3)
                                                (unsafe-procedure-call fun1.1 y.3 z.3 x.3))))
                                        x.2)))))))

(let ([x `(module
            (letrec ((fun1.1
                        (lambda (x.1 y.1 z.1) x.1)))
            (begin (unsafe-fx+ (letrec ([x.2 (lambda (x.3 y.3 z.3) (unsafe-procedure-call fun1.1 y.3 z.3 x.3))]) x.2) 5) 5)))])
  (test-case "uncover-free lambda in begin effect"
             (check-equal? (uncover-free x)
                           `(module
                                (letrec ((fun1.1 (lambda ((free ())) (x.1 y.1 z.1) x.1)))
                                (begin
                                (unsafe-fx+
                                (letrec ((x.2
                                                (lambda ((free (fun1.1)))
                                                (x.3 y.3 z.3)
                                                (unsafe-procedure-call fun1.1 y.3 z.3 x.3))))
                                        x.2)
                                5)
                                5))))))

(let ([x  '(module
     (letrec ((vector-init-loop.7
               (lambda (len.8 i.10 vec.9)
                 (if (eq? len.8 i.10)
                   vec.9
                   (begin
                     (unsafe-vector-set! vec.9 i.10 0)
                     (unsafe-procedure-call
                      vector-init-loop.7
                      len.8
                      (unsafe-fx+ i.10 1)
                      vec.9)))))
              (make-init-vector.1
               (lambda (tmp.5)
                 (let ((tmp.6 (unsafe-make-vector tmp.5)))
                   (unsafe-procedure-call vector-init-loop.7 tmp.5 0 tmp.6))))
              (make-vector.57
               (lambda (tmp.33)
                 (if (fixnum? tmp.33)
                   (unsafe-procedure-call make-init-vector.1 tmp.33)
                   (error 8)))))
       (let ((x.1.4 (unsafe-procedure-call make-vector.57 0))) x.1.4)))])
  (test-case "error case"
             (check-equal? (uncover-free x)
                           '(module
                        (letrec ((vector-init-loop.7
                                (lambda ((free (vector-init-loop.7)))
                                        (len.8 i.10 vec.9)
                                        (if (eq? len.8 i.10)
                                        vec.9
                                        (begin
                                        (unsafe-vector-set! vec.9 i.10 0)
                                        (unsafe-procedure-call
                                        vector-init-loop.7
                                        len.8
                                        (unsafe-fx+ i.10 1)
                                        vec.9)))))
                                (make-init-vector.1
                                (lambda ((free (vector-init-loop.7)))
                                        (tmp.5)
                                        (let ((tmp.6 (unsafe-make-vector tmp.5)))
                                        (unsafe-procedure-call vector-init-loop.7 tmp.5 0 tmp.6))))
                                (make-vector.57
                                (lambda ((free (make-init-vector.1)))
                                        (tmp.33)
                                        (if (fixnum? tmp.33)
                                        (unsafe-procedure-call make-init-vector.1 tmp.33)
                                        (error 8)))))
                        (let ((x.1.4 (unsafe-procedure-call make-vector.57 0))) x.1.4))))))