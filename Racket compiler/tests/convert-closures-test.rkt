#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/ptr-run-time
 racket/match
 rackunit)

(require "../component/convert-closures.rkt")

; input: lam-opticon-lang-v9
; output: lam-free-lang-v9
; purpose: Check if the outputs of convert-closures matches the desired outputs of the interrogator.

(let ([x `(module 5)])
  (test-case "no convert"
             (check-equal? (convert-closures x)
                           `(module 5))))

(let ([x `(module
                (unsafe-procedure-call x.1 1 2 3))])
  (test-case "convert simple call when operator is aloc"
             (check-equal? (convert-closures x)
                           `(module (closure-call x.1 x.1 1 2 3)))))


(let ([x `(module
                (unsafe-procedure-call (begin (unsafe-fx+ 1 2) x.1) 1 2 3))])
  (test-case "convert simple call when operator is not aloc"
             (check-match (convert-closures x)
                           `(module
                                (let ((,tmp.4 (begin (unsafe-fx+ 1 2) x.1)))
                                (closure-call ,tmp.4 ,tmp.4 1 2 3))))))


(let ([x `(module
            (letrec ((fun1.1
                    (lambda ((free ()))
                    (x.1 y.1 z.1)
                    x.1)))
            5))])
  (test-case "convert simple letrec no free variable"
             (check-match (convert-closures x)
                           `(module
                                (letrec ((,L.fun1.1.7 (lambda (,c.4 x.1 y.1 z.1) (let () x.1))))
                                (cletrec ((fun1.1 (make-closure ,L.fun1.1.7 3))) 5))))))


(let ([x `(module
            (letrec ((fun1.1
                    (lambda ((free (q.1 m.1)))
                    (x.1 y.1 z.1)
                    (unsafe-fx+ q.1 m.1))))
            5))])
  (test-case "convert letrec with multiple free variables"
             (check-match (convert-closures x)
                           `(module
                                (letrec ((,L.fun1.1.7
                                        (lambda (,c.4 x.1 y.1 z.1)
                                        (let ((q.1 (closure-ref ,c.4 0)) (m.1 (closure-ref ,c.4 1)))
                                                (unsafe-fx+ q.1 m.1)))))
                                (cletrec ((fun1.1 (make-closure ,L.fun1.1.7 3 q.1 m.1))) 5))))))



(let ([x `(module
            (letrec ((fun1.1
                    (lambda ((free (fun1.1)))
                    (x.1 y.1 z.1)
                    (unsafe-procedure-call fun1.1 y.1 z.1 x.1))))
            5))])
  (test-case "convert letrec and call"
             (check-match (convert-closures x)
                           `(module
                                (letrec ((,L.fun1.1.7
                                        (lambda (,c.4 x.1 y.1 z.1)
                                        (let ((fun1.1 (closure-ref ,c.4 0)))
                                                (closure-call fun1.1 fun1.1 y.1 z.1 x.1)))))
                                (cletrec ((fun1.1 (make-closure ,L.fun1.1.7 3 fun1.1))) 5))))))


(let ([x `(module
            (letrec ([fun1.1
                    (lambda ((free (fun1.2)))
                    (x.1 y.1 z.1)
                    (unsafe-procedure-call fun1.2 y.1 z.1 x.1))]
                    [fun1.2
                    (lambda ((free ()))
                    (x.1 y.1)
                    x.1)])
            5))])
  (test-case "convert multiple letrecs"
             (check-match (convert-closures x)
                           `(module
                                (letrec ((,L.fun1.1.7
                                        (lambda (,c.4 x.1 y.1 z.1)
                                        (let ((fun1.2 (closure-ref ,c.4 0)))
                                                (closure-call fun1.2 fun1.2 y.1 z.1 x.1))))
                                        (,L.fun1.2.8 (lambda (,c.5 x.1 y.1) (let () x.1))))
                                (cletrec
                                ((fun1.1 (make-closure ,L.fun1.1.7 3 fun1.2))
                                (fun1.2 (make-closure ,L.fun1.2.8 2)))
                                5))))))


(let ([x `(module
                (letrec ((f.1
                        (lambda ((free ()))
                        ()
                        (letrec ((x.1
                                        (lambda ((free (x.1)))
                                        ()
                                        (unsafe-procedure-call x.1))))
                                x.1))))
                f.1))])
  (test-case "convert nested letrec in bind"
             (check-match (convert-closures x)
                           `(module
                                (letrec ((,L.f.1.7
                                        (lambda (,c.4)
                                        (let ()
                                                (letrec ((,L.x.1.8
                                                        (lambda (,c.5)
                                                        (let ((x.1 (closure-ref ,c.5 0)))
                                                        (closure-call x.1 x.1)))))
                                                (cletrec ((x.1 (make-closure ,L.x.1.8 0 x.1))) x.1))))))
                                (cletrec ((f.1 (make-closure ,L.f.1.7 0))) f.1))))))  


(let ([x `(module
                (letrec ((f.1
                        (lambda ((free ()))
                        ()
                        #t)))
                (letrec ((x.1
                                        (lambda ((free (x.1)))
                                        ()
                                        (unsafe-procedure-call x.1))))
                                x.1)))])
  (test-case "convert nested letrec in body"
             (check-match (convert-closures x)
                           `(module
                                (letrec ((,L.f.1.7 (lambda (,c.4) (let () #t))))
                                (cletrec
                                ((f.1 (make-closure ,L.f.1.7 0)))
                                (letrec ((,L.x.1.8
                                        (lambda (,c.5)
                                                (let ((x.1 (closure-ref ,c.5 0))) (closure-call x.1 x.1)))))
                                (cletrec ((x.1 (make-closure ,L.x.1.8 0 x.1))) x.1))))))))  



;; test cases for letrec under different positions
(let ([x `(module
                (fixnum? (letrec ([x.1 (lambda ((free (q.1))) () (unsafe-procedure-call q.1))]) 5)))])
  (test-case "convert letrec in primop"
             (check-match (convert-closures x)
                           `(module
                                (fixnum?
                                (letrec ((,L.x.1.7
                                        (lambda (,c.4)
                                        (let ((q.1 (closure-ref ,c.4 0))) (closure-call q.1 q.1)))))
                                (cletrec ((x.1 (make-closure ,L.x.1.7 0 q.1))) 5)))))))  

;; ? error
;; interrogator broke its own contract
(let ([x `(module
                (unsafe-fx+ x.1 (letrec ([x.1 (lambda ((free (q.1))) () (unsafe-procedure-call q.1))]) x.1)))])
  (test-case "convert letrec in primop"
             (check-match (convert-closures x)
                           `(module
                                (unsafe-fx+
                                x.1
                                (letrec ((,L.x.1.7
                                        (lambda (,c.4)
                                        (let ((q.1 (closure-ref ,c.4 0))) (closure-call q.1 q.1)))))
                                (cletrec ((x.1 (make-closure ,L.x.1.7 0 q.1))) x.1))))))) 


(let ([x `(module
                (let ([x.3 (letrec ([x.1 (lambda ((free (q.1))) (x.2) (unsafe-procedure-call q.1 x.2))]) x.1)]) (unsafe-procedure-call x.1 2)))])
  (test-case "convert letrec in let bind"
             (check-match (convert-closures x)
                           `(module
                                (let ((x.3
                                        (letrec ((,L.x.1.7
                                                (lambda (,c.4 x.2)
                                                (let ((q.1 (closure-ref ,c.4 0)))
                                                (closure-call q.1 q.1 x.2)))))
                                        (cletrec ((x.1 (make-closure ,L.x.1.7 1 q.1))) x.1))))
                                (closure-call x.1 x.1 2))))))  


(let ([x `(module
                (let ([x.3 5]) (letrec ([x.1 (lambda ((free (q.1))) (x.2) (unsafe-procedure-call q.1 x.2))]) x.1)))])
  (test-case "convert letrec in let body"
             (check-match (convert-closures x)
                           `(module
                                (let ((x.3 5))
                                (letrec ((,L.x.1.7
                                        (lambda (,c.4 x.2)
                                                (let ((q.1 (closure-ref ,c.4 0))) (closure-call q.1 q.1 x.2)))))
                                (cletrec ((x.1 (make-closure ,L.x.1.7 1 q.1))) x.1)))))))  



(let ([x `(module
                (if (let ([x.3 (letrec ([x.1 (lambda ((free (q.1))) (x.2) (unsafe-procedure-call q.1 x.2))]) x.1)]) (true))
                        5
                        6))])
  (test-case "convert letrec in pred"
             (check-match (convert-closures x)
                           `(module
                                (if (let ((x.3
                                        (letrec ((,L.x.1.7
                                                (lambda (,c.4 x.2)
                                                        (let ((q.1 (closure-ref ,c.4 0)))
                                                        (closure-call q.1 q.1 x.2)))))
                                        (cletrec ((x.1 (make-closure ,L.x.1.7 1 q.1))) x.1))))
                                        (true))
                                5
                                6)))))


(let ([x `(module
                (if (true)
                        (letrec ([x.1 (lambda ((free (q.1))) (x.2) (unsafe-procedure-call q.1 x.2))]) x.1)
                        6))])
  (test-case "convert letrec in if e"
             (check-match (convert-closures x)
                           `(module
                                (if (true)
                                (letrec ((,L.x.1.7
                                        (lambda (,c.4 x.2)
                                                (let ((q.1 (closure-ref ,c.4 0))) (closure-call q.1 q.1 x.2)))))
                                (cletrec ((x.1 (make-closure ,L.x.1.7 1 q.1))) x.1))
                                6)))))  
  


(let ([x `(module
                (begin (unsafe-fx+ (letrec ([x.1 (lambda ((free (q.1))) (x.2) (unsafe-procedure-call q.1 x.2))]) x.1) 5) 5))])
  (test-case "convert letrec in if effect"
             (check-match (convert-closures x)
                           `(module
                                (begin
                                (unsafe-fx+
                                (letrec ((,L.x.1.7
                                        (lambda (,c.4 x.2)
                                                (let ((q.1 (closure-ref ,c.4 0)))
                                                (closure-call q.1 q.1 x.2)))))
                                (cletrec ((x.1 (make-closure ,L.x.1.7 1 q.1))) x.1))
                                5)
                                5)))))


(let ([x `(module
                (begin (unsafe-fx+ 1 5) (letrec ([x.1 (lambda ((free (q.1))) (x.2) (unsafe-procedure-call q.1 x.2))]) x.1)))])
  (test-case "convert letrec in if begin e"
             (check-match (convert-closures x)
                           `(module
                                (begin
                                (unsafe-fx+ 1 5)
                                (letrec ((,L.x.1.7
                                        (lambda (,c.4 x.2)
                                                (let ((q.1 (closure-ref ,c.4 0))) (closure-call q.1 q.1 x.2)))))
                                (cletrec ((x.1 (make-closure ,L.x.1.7 1 q.1))) x.1)))))))



;; test cases from piazza
(let ([x `(module
                (letrec ([foo.4 (lambda ((free ()))
                                        (a.7 b.6 c.5)
                                        0)])
                        (error 42)))])
  (test-case "piazza"
             (check-match (convert-closures x)
                           `(module
                                (letrec ([,L.foo.4.1 (lambda
                                                        (,c.2 a.7 b.6 c.5)
                                                        (let ()
                                                        0))])
                                (cletrec ([foo.4 (make-closure ,L.foo.4.1 3)])
                                                (error 42)))))))


(let ([x `(module
      (letrec ([<.60
                (lambda ((free ()))
                  (tmp.28 tmp.29)
                  (if (fixnum? tmp.29)
                      (if (fixnum? tmp.28)
                          (unsafe-fx< tmp.28 tmp.29)
                          (error 3))
                      (error 3)))]
               [swap.4
                (lambda ((free (b.9 swap.4 <.60)))
                  (x.6 y.5)
                  (if (unsafe-procedure-call <.60 y.5 x.6)
                      x.6
                      (let ([z.7 (unsafe-procedure-call swap.4 y.5 x.6)]
                            [a.8 #t])
                        b.9)))])
        (unsafe-procedure-call swap.4 1 2)))])
  (test-case "piazza"
             (check-match (convert-closures x)
                           `(module
                                (letrec ([,L.<.60.1
                                        (lambda (,c.3 ,tmp.28 ,tmp.29)
                                                (let ()
                                                (if (fixnum? ,tmp.29)
                                                (if (fixnum? ,tmp.28)
                                                        (unsafe-fx< ,tmp.28 ,tmp.29)
                                                        (error 3))
                                                (error 3))))]
                                        [,L.swap.4.2
                                        (lambda (,c.4 ,x.6 ,y.5)
                                                (let ([b.9    (closure-ref ,c.4 0)]
                                                [swap.4 (closure-ref ,c.4 1)]
                                                [<.60   (closure-ref ,c.4 2)])
                                                (if (closure-call <.60 <.60 y.5 x.6)
                                                x.6
                                                (let ([z.7 (closure-call swap.4 swap.4 y.5 x.6)]
                                                        [a.8 #t])
                                                        b.9))))])
                                (cletrec ([<.60   (make-closure ,L.<.60.1 2)]
                                                [swap.4 (make-closure ,L.swap.4.2 2 b.9 swap.4 <.60)])
                                                (closure-call swap.4 swap.4 1 2)))))))


(test-case "piazza"
              (check-match
               (convert-closures
                `(module (let ([y.0 1])
                           (unsafe-fx>= (letrec ([x.1 (lambda ((free (y.0))) (y.1) (unsafe-fx+ y.0 y.1))])
                                          (unsafe-procedure-call x.1 5))
                                        2))))
               `(module
                    (let ((y.0 1))
                      (unsafe-fx>=
                       (letrec ((,L.x.1.7 (lambda (,c.8 y.1)
                                            (let ((y.0 (closure-ref ,c.8 0)))
                                              (unsafe-fx+ y.0 y.1)))))
                         (cletrec
                          ((x.1 (make-closure ,L.x.1.7 1 y.0)))
                          (closure-call x.1 x.1 5)))
                       2)))))

(test-case "piazza"
              (check-match
               (convert-closures
                `(module (let ([a.1 0]
                               [b.1 100])
                           (unsafe-procedure-call (letrec ([x.1 (lambda ((free (a.1 b.1))) (c.1)
                                                                  (unsafe-fx* (unsafe-fx+ a.1 b.1) c.1))])
                                                    x.1)
                                                  -50))))
               `(module
                    (let ((a.1 0)
                          (b.1 100))
                      (let ((,tmp.18
                             (letrec ((,L.x.1.15
                                       (lambda (,c.19 c.1)
                                         (let ((a.1 (closure-ref ,c.19 0))
                                               (b.1 (closure-ref ,c.19 1)))
                                           (unsafe-fx* (unsafe-fx+ a.1 b.1) c.1)))))
                               (cletrec ((x.1 (make-closure ,L.x.1.15 1 a.1 b.1))) x.1))))
                        (closure-call ,tmp.18 ,tmp.18 -50))))))