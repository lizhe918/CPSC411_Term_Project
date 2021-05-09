#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/ptr-run-time
 racket/match
 rackunit)

(require "../component/hoist-lambdas.rkt")

; input: closure-lang-v9
; output: hoisted-lang-v9
; purpose: Check if the outputs of hoist-lambdas matches the desired outputs of the interrogator.


(let ([x '(module
     (letrec ((L.+.71.7
               (lambda (c.74 tmp.32 tmp.33)
                 (let ()
                   (if (fixnum? tmp.33)
                     (if (fixnum? tmp.32) (unsafe-fx+ tmp.32 tmp.33) (error 2))
                     (error 2)))))
              (L.cons.70.8
               (lambda (c.75 tmp.63 tmp.64) (let () (cons tmp.63 tmp.64))))
              (L.-.69.9
               (lambda (c.76 tmp.34 tmp.35)
                 (let ()
                   (if (fixnum? tmp.35)
                     (if (fixnum? tmp.34) (unsafe-fx- tmp.34 tmp.35) (error 3))
                     (error 3)))))
              (L.eq?.68.10
               (lambda (c.77 tmp.65 tmp.66) (let () (eq? tmp.65 tmp.66))))
              (L.zero?.4.11
               (lambda (c.78 n.8)
                 (let ((eq?.68 (closure-ref c.78 0)))
                   (call L.eq?.68.10 eq?.68 n.8 0))))
              (L.sub1.5.12
               (lambda (c.79 n.9)
                 (let ((|-.69| (closure-ref c.79 0)))
                   (call L.-.69.9 |-.69| n.9 1))))
              (L.curry.6.13
               (lambda (c.80 f.11 x.10)
                 (let ()
                   (letrec ((L.lam.72.15
                             (lambda (c.81 y.12)
                               (let ((x.10 (closure-ref c.81 0))
                                     (f.11 (closure-ref c.81 1)))
                                 (if (procedure? f.11)
                                   (if (eq? (unsafe-procedure-arity f.11) 2)
                                     (closure-call f.11 f.11 x.10 y.12)
                                     (error 42))
                                   (error 43))))))
                     (cletrec
                      ((lam.72 (make-closure L.lam.72.15 1 x.10 f.11)))
                      lam.72)))))
              (L.build-list.7.14
               (lambda (c.82 f.14 n.13)
                 (let ((sub1.5 (closure-ref c.82 0))
                       (build-list.7 (closure-ref c.82 1))
                       (cons.70 (closure-ref c.82 2))
                       (zero?.4 (closure-ref c.82 3)))
                   (if (call L.zero?.4.11 zero?.4 n.13)
                     empty
                     (call
                      L.cons.70.8
                      cons.70
                      (if (procedure? f.14)
                        (if (eq? (unsafe-procedure-arity f.14) 1)
                          (closure-call f.14 f.14 n.13)
                          (error 42))
                        (error 43))
                      (call
                       L.build-list.7.14
                       build-list.7
                       f.14
                       (call L.sub1.5.12 sub1.5 n.13))))))))
       (cletrec
        ((|+.71| (make-closure L.+.71.7 2))
         (cons.70 (make-closure L.cons.70.8 2))
         (|-.69| (make-closure L.-.69.9 2))
         (eq?.68 (make-closure L.eq?.68.10 2))
         (zero?.4 (make-closure L.zero?.4.11 1 eq?.68))
         (sub1.5 (make-closure L.sub1.5.12 1 |-.69|))
         (curry.6 (make-closure L.curry.6.13 2))
         (build-list.7
          (make-closure
           L.build-list.7.14
           2
           sub1.5
           build-list.7
           cons.70
           zero?.4)))
        (call
         L.cons.70.8
         cons.70
         (call
          L.build-list.7.14
          build-list.7
          (letrec ((L.lam.73.16 (lambda (c.83 n.15) (let () 1))))
            (cletrec ((lam.73 (make-closure L.lam.73.16 1))) lam.73))
          5)
         (call
          L.build-list.7.14
          build-list.7
          (call L.curry.6.13 curry.6 |+.71| 1)
          5)))))])
  (test-case "new error case"
             (check-equal? (hoist-lambdas x)
                           `(module
                                (define L.lam.73.16 (lambda (c.83 n.15) (let () 1)))
                                (define L.build-list.7.14
                                (lambda (c.82 f.14 n.13)
                                    (let ((sub1.5 (closure-ref c.82 0))
                                        (build-list.7 (closure-ref c.82 1))
                                        (cons.70 (closure-ref c.82 2))
                                        (zero?.4 (closure-ref c.82 3)))
                                    (if (call L.zero?.4.11 zero?.4 n.13)
                                        empty
                                        (call
                                        L.cons.70.8
                                        cons.70
                                        (if (procedure? f.14)
                                        (if (eq? (unsafe-procedure-arity f.14) 1)
                                            (closure-call f.14 f.14 n.13)
                                            (error 42))
                                        (error 43))
                                        (call
                                        L.build-list.7.14
                                        build-list.7
                                        f.14
                                        (call L.sub1.5.12 sub1.5 n.13)))))))
                                (define L.curry.6.13
                                (lambda (c.80 f.11 x.10)
                                    (let ()
                                    (cletrec ((lam.72 (make-closure L.lam.72.15 1 x.10 f.11))) lam.72))))
                                (define L.lam.72.15
                                (lambda (c.81 y.12)
                                    (let ((x.10 (closure-ref c.81 0)) (f.11 (closure-ref c.81 1)))
                                    (if (procedure? f.11)
                                        (if (eq? (unsafe-procedure-arity f.11) 2)
                                        (closure-call f.11 f.11 x.10 y.12)
                                        (error 42))
                                        (error 43)))))
                                (define L.sub1.5.12
                                (lambda (c.79 n.9)
                                    (let ((|-.69| (closure-ref c.79 0))) (call L.-.69.9 |-.69| n.9 1))))
                                (define L.zero?.4.11
                                (lambda (c.78 n.8)
                                    (let ((eq?.68 (closure-ref c.78 0))) (call L.eq?.68.10 eq?.68 n.8 0))))
                                (define L.eq?.68.10
                                (lambda (c.77 tmp.65 tmp.66) (let () (eq? tmp.65 tmp.66))))
                                (define L.-.69.9
                                (lambda (c.76 tmp.34 tmp.35)
                                    (let ()
                                    (if (fixnum? tmp.35)
                                        (if (fixnum? tmp.34) (unsafe-fx- tmp.34 tmp.35) (error 3))
                                        (error 3)))))
                                (define L.cons.70.8
                                (lambda (c.75 tmp.63 tmp.64) (let () (cons tmp.63 tmp.64))))
                                (define L.+.71.7
                                (lambda (c.74 tmp.32 tmp.33)
                                    (let ()
                                    (if (fixnum? tmp.33)
                                        (if (fixnum? tmp.32) (unsafe-fx+ tmp.32 tmp.33) (error 2))
                                        (error 2)))))
                                (cletrec
                                ((|+.71| (make-closure L.+.71.7 2))
                                (cons.70 (make-closure L.cons.70.8 2))
                                (|-.69| (make-closure L.-.69.9 2))
                                (eq?.68 (make-closure L.eq?.68.10 2))
                                (zero?.4 (make-closure L.zero?.4.11 1 eq?.68))
                                (sub1.5 (make-closure L.sub1.5.12 1 |-.69|))
                                (curry.6 (make-closure L.curry.6.13 2))
                                (build-list.7
                                (make-closure L.build-list.7.14 2 sub1.5 build-list.7 cons.70 zero?.4)))
                                (call
                                L.cons.70.8
                                cons.70
                                (call
                                L.build-list.7.14
                                build-list.7
                                (cletrec ((lam.73 (make-closure L.lam.73.16 1))) lam.73)
                                5)
                                (call
                                L.build-list.7.14
                                build-list.7
                                (call L.curry.6.13 curry.6 |+.71| 1)
                                5))))))) 


(let ([x `(module
            (letrec ((L.cons.3.1 (lambda (c.7 tmp.1 tmp.2) (let () (cons tmp.1 tmp.2))))
                    (L.*.6.2
                        (lambda (c.8 tmp.4 tmp.5)
                        (let ()
                            (if (fixnum? tmp.5)
                            (if (fixnum? tmp.4) (unsafe-fx* tmp.4 tmp.5) (error 0))
                            (error 0))))))
                (cletrec
                ((cons.3 (make-closure L.cons.3.1 2)) (*.6 (make-closure L.*.6.2 2)))
                (call
                L.cons.3.1
                cons.3
                (if (eq? 7 8) (call L.*.6.2 *.6 7 8) (call L.*.6.2 *.6 8 7))
                empty))))])
  (test-case "error case"
             (check-equal? (hoist-lambdas x)
                           `(module
                            (define L.*.6.2
                                (lambda (c.8 tmp.4 tmp.5)
                                (let ()
                                    (if (fixnum? tmp.5)
                                    (if (fixnum? tmp.4) (unsafe-fx* tmp.4 tmp.5) (error 0))
                                    (error 0)))))
                            (define L.cons.3.1 (lambda (c.7 tmp.1 tmp.2) (let () (cons tmp.1 tmp.2))))
                            (cletrec
                            ((cons.3 (make-closure L.cons.3.1 2)) (*.6 (make-closure L.*.6.2 2)))
                            (call
                                L.cons.3.1
                                cons.3
                                (if (eq? 7 8) (call L.*.6.2 *.6 7 8) (call L.*.6.2 *.6 8 7))
                                empty)))))) 


(let ([x `(module 5)])
  (test-case "No letrec is used"
             (check-equal? (hoist-lambdas x)
                           `(module 5)))) 


(let ([x `(module (closure-call x.1 x.1 1 2 3))])
  (test-case "No letrec is used"
             (check-equal? (hoist-lambdas x)
                           `(module (closure-call x.1 x.1 1 2 3))))) 

(let ([x `(module
            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (call L.fun1.1.7 fun1.1 1 2 3))))])
  (test-case "Hoist letrec"
             (check-equal? (hoist-lambdas x)
                           `(module
                            (define L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1)))
                            (cletrec
                            ((fun1.1 (make-closure L.fun1.1.7 3)))
                            (call L.fun1.1.7 fun1.1 1 2 3)))))) 


(let ([x `(module
            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                (call L.fun1.1.7 fun1.1 1 2 3)))])
  (test-case "Hoist letrec but no cletrec (???)"
             (check-equal? (hoist-lambdas x)
                           `(module
                                (define L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1)))
                                (call L.fun1.1.7 fun1.1 1 2 3))))) 


(let ([x `(module
            (letrec ([L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))] [L.fun1.1.8 (lambda (c.5) (let () (call L.fun1.1.7 fun1.1 1 2 3)))])
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (call L.fun1.1.7 fun1.1 1 2 3))))])
  (test-case "Hoist letrecs with multiple labels"
             (check-equal? (hoist-lambdas x)
                           `(module
                            (define L.fun1.1.8 (lambda (c.5) (let () (call L.fun1.1.7 fun1.1 1 2 3))))
                            (define L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1)))
                            (cletrec
                            ((fun1.1 (make-closure L.fun1.1.7 3)))
                            (call L.fun1.1.7 fun1.1 1 2 3)))))) 



;; hoist in different positions
(let ([x `(module
            (letrec ([L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))] [L.fun1.1.8 (lambda (c.5) (let () (call L.fun1.1.7 fun1.1 1 2 3)))])
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (fixnum? 
                    (letrec ((L.fun1.1.9 (lambda (c.6 x.1 y.1 z.1) (let () x.1))))
                    (cletrec
                    ((fun1.1 (make-closure L.fun1.1.7 3)))
                    (call L.fun1.1.7 fun1.1 1 2 3)))))))])
  (test-case "Hoist letrecs in primops"
             (check-equal? (hoist-lambdas x)
                           `(module
                                (define L.fun1.1.9 (lambda (c.6 x.1 y.1 z.1) (let () x.1)))
                                (define L.fun1.1.8 (lambda (c.5) (let () (call L.fun1.1.7 fun1.1 1 2 3))))
                                (define L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1)))
                                (cletrec
                                ((fun1.1 (make-closure L.fun1.1.7 3)))
                                (fixnum?
                                    (cletrec
                                    ((fun1.1 (make-closure L.fun1.1.7 3)))
                                    (call L.fun1.1.7 fun1.1 1 2 3)))))))) 


(let ([x `(module
            (letrec ([L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))])
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (closure-ref (letrec ((L.fun1.1.9 (lambda (c.6 x.1 y.1 z.1) (let () x.1))))
                    (cletrec
                    ((fun1.1 (make-closure L.fun1.1.7 3)))
                    (call L.fun1.1.7 fun1.1 1 2 3))) 10))))])
  (test-case "Hoist letrecs in closure-ref"
             (check-equal? (hoist-lambdas x)
                           `(module
                                (define L.fun1.1.9 (lambda (c.6 x.1 y.1 z.1) (let () x.1)))
                                (define L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1)))
                                (cletrec
                                ((fun1.1 (make-closure L.fun1.1.7 3)))
                                (closure-ref
                                    (cletrec
                                    ((fun1.1 (make-closure L.fun1.1.7 3)))
                                    (call L.fun1.1.7 fun1.1 1 2 3))
                                    10)))))) 

(let ([x `(module
            (letrec ([L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))] [L.fun1.1.8 (lambda (c.5) (let () (call L.fun1.1.7 fun1.1 1 2 3)))])
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (closure-call fun1.1 fun1.1 (letrec ((L.fun1.1.9 (lambda (c.6 x.1 y.1 z.1) (let () x.1))))
                    (cletrec
                    ((fun1.1 (make-closure L.fun1.1.7 3)))
                    (call L.fun1.1.7 fun1.1 1 2 3))) 
                    2 3))))])
  (test-case "Hoist letrecs in closure-call"
             (check-equal? (hoist-lambdas x)
                           `(module
                                (define L.fun1.1.9 (lambda (c.6 x.1 y.1 z.1) (let () x.1)))
                                (define L.fun1.1.8 (lambda (c.5) (let () (call L.fun1.1.7 fun1.1 1 2 3))))
                                (define L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1)))
                                (cletrec
                                ((fun1.1 (make-closure L.fun1.1.7 3)))
                                (closure-call
                                    fun1.1
                                    fun1.1
                                    (cletrec
                                    ((fun1.1 (make-closure L.fun1.1.7 3)))
                                    (call L.fun1.1.7 fun1.1 1 2 3))
                                    2
                                    3)))))) 


(let ([x `(module
            (letrec ([L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))])
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (call L.fun1.1.7 fun1.1 (letrec ((L.fun1.1.9 (lambda (c.6 x.1 y.1 z.1) (let () x.1))))
                    (cletrec
                    ((fun1.2 (make-closure L.fun1.1.9 3)))
                    (call L.fun1.1.7 fun1.1 1 2 3)))
                    2 3))))])
  (test-case "Hoist letrecs in call"
             (check-equal? (hoist-lambdas x)
                           `(module
                                (define L.fun1.1.9 (lambda (c.6 x.1 y.1 z.1) (let () x.1)))
                                (define L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1)))
                                (cletrec
                                ((fun1.1 (make-closure L.fun1.1.7 3)))
                                (call
                                    L.fun1.1.7
                                    fun1.1
                                    (cletrec
                                    ((fun1.2 (make-closure L.fun1.1.9 3)))
                                    (call L.fun1.1.7 fun1.1 1 2 3))
                                    2
                                    3)))))) 

(let ([x `(module
            (letrec ([L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))])
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (fixnum? 
                    (letrec ((L.fun1.1.9 (lambda (c.6 x.1 y.1 z.1) (let () x.1))))
                    (cletrec
                    ((fun1.2 (make-closure L.fun1.1.9 (letrec ((L.fun1.1.10 (lambda (c.7 x.1 y.1 z.1) (let () x.1))))
                                                        (cletrec
                                                        ((fun1.3 (make-closure L.fun1.1.10 3)))
                                                        (call L.fun1.1.7 fun1.1 1 2 3))))))
                    (call L.fun1.1.7 fun1.1 1 2 3)))))))])
  (test-case "Hoist letrecs in make-closure"
             (check-equal? (hoist-lambdas x)
                           `(module
                                (define L.fun1.1.10 (lambda (c.7 x.1 y.1 z.1) (let () x.1)))
                                (define L.fun1.1.9 (lambda (c.6 x.1 y.1 z.1) (let () x.1)))
                                (define L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1)))
                                (cletrec
                                ((fun1.1 (make-closure L.fun1.1.7 3)))
                                (fixnum?
                                    (cletrec
                                    ((fun1.2
                                    (make-closure
                                        L.fun1.1.9
                                        (cletrec
                                        ((fun1.3 (make-closure L.fun1.1.10 3)))
                                        (call L.fun1.1.7 fun1.1 1 2 3)))))
                                    (call L.fun1.1.7 fun1.1 1 2 3))))))))

(let ([x `(module
            (letrec ([L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))])
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (letrec ((L.fun1.1.10 (lambda (c.7 x.1 y.1 z.1) (let () x.1))))
                                                        (cletrec
                                                        ((fun1.3 (make-closure L.fun1.1.10 3)))
                                                        (call L.fun1.1.7 fun1.1 1 2 3))))))])
  (test-case "Hoist letrecs in cletrec"
             (check-equal? (hoist-lambdas x)
                           `(module
                                (define L.fun1.1.10 (lambda (c.7 x.1 y.1 z.1) (let () x.1)))
                                (define L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1)))
                                (cletrec
                                ((fun1.1 (make-closure L.fun1.1.7 3)))
                                (cletrec
                                    ((fun1.3 (make-closure L.fun1.1.10 3)))
                                    (call L.fun1.1.7 fun1.1 1 2 3)))))))

(let ([x `(module
            (letrec ([L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))])
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (let ([x.3 (letrec ((L.fun1.1.10 (lambda (c.7 x.1 y.1 z.1) (let () x.1))))
                                                        (cletrec
                                                        ((fun1.3 (make-closure L.fun1.1.10 3)))
                                                        (call L.fun1.1.7 fun1.1 1 2 3)))])
                    10))))])
  (test-case "Hoist letrecs in let bind"
             (check-equal? (hoist-lambdas x)
                           `(module
                                (define L.fun1.1.10 (lambda (c.7 x.1 y.1 z.1) (let () x.1)))
                                (define L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1)))
                                (cletrec
                                ((fun1.1 (make-closure L.fun1.1.7 3)))
                                (let ((x.3
                                        (cletrec
                                        ((fun1.3 (make-closure L.fun1.1.10 3)))
                                        (call L.fun1.1.7 fun1.1 1 2 3))))
                                    10))))))



(let ([x `(module
            (letrec ([L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))])
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (let ([x.3 10]) (letrec ((L.fun1.1.10 (lambda (c.7 x.1 y.1 z.1) (let () x.1))))
                                                        (cletrec
                                                        ((fun1.3 (make-closure L.fun1.1.10 3)))
                                                        (call L.fun1.1.7 fun1.1 1 2 3)))))))])
  (test-case "Hoist letrecs in let body"
             (check-equal? (hoist-lambdas x)
                           `(module
                            (define L.fun1.1.10 (lambda (c.7 x.1 y.1 z.1) (let () x.1)))
                            (define L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1)))
                            (cletrec
                            ((fun1.1 (make-closure L.fun1.1.7 3)))
                            (let ((x.3 10))
                                (cletrec
                                ((fun1.3 (make-closure L.fun1.1.10 3)))
                                (call L.fun1.1.7 fun1.1 1 2 3))))))))


(let ([x `(module
            (letrec ([L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))] )
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (if (not (let ([x.3 (letrec ((L.fun1.1.10 (lambda (c.7 x.1 y.1 z.1) (let () x.1))))
                                                        (cletrec
                                                        ((fun1.3 (make-closure L.fun1.1.10 3)))
                                                        (call L.fun1.1.7 fun1.1 1 2 3)))]) (true)))
                    1 2))))])
  (test-case "Hoist letrecs in pred"
             (check-equal? (hoist-lambdas x)
                           `(module
                                (define L.fun1.1.10 (lambda (c.7 x.1 y.1 z.1) (let () x.1)))
                                (define L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1)))
                                (cletrec
                                ((fun1.1 (make-closure L.fun1.1.7 3)))
                                (if (not
                                        (let ((x.3
                                            (cletrec
                                                ((fun1.3 (make-closure L.fun1.1.10 3)))
                                                (call L.fun1.1.7 fun1.1 1 2 3))))
                                        (true)))
                                    1
                                    2))))))



(let ([x `(module
            (letrec ([L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))])
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (if (true) 
                    (letrec ((L.fun1.1.10 (lambda (c.7 x.1 y.1 z.1) (let () x.1))))
                                                        (cletrec
                                                        ((fun1.3 (make-closure L.fun1.1.10 3)))
                                                        (call L.fun1.1.7 fun1.1 1 2 3)))
                    5))))])
  (test-case "Hoist letrecs in if e"
             (check-equal? (hoist-lambdas x)
                           `(module
                                (define L.fun1.1.10 (lambda (c.7 x.1 y.1 z.1) (let () x.1)))
                                (define L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1)))
                                (cletrec
                                ((fun1.1 (make-closure L.fun1.1.7 3)))
                                (if (true)
                                    (cletrec
                                    ((fun1.3 (make-closure L.fun1.1.10 3)))
                                    (call L.fun1.1.7 fun1.1 1 2 3))
                                    5))))))



(let ([x `(module
            (letrec ([L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))])
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (begin (letrec ((L.fun1.1.10 (lambda (c.7 x.1 y.1 z.1) (let () x.1))))
                                                        (cletrec
                                                        ((fun1.3 (make-closure L.fun1.1.10 3)))
                                                        (call L.fun1.1.7 fun1.1 1 2 3)))))))])
  (test-case "Hoist letrecs in begin e"
             (check-equal? (hoist-lambdas x)
                           `(module
                                (define L.fun1.1.10 (lambda (c.7 x.1 y.1 z.1) (let () x.1)))
                                (define L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1)))
                                (cletrec
                                ((fun1.1 (make-closure L.fun1.1.7 3)))
                                (begin
                                    (cletrec
                                    ((fun1.3 (make-closure L.fun1.1.10 3)))
                                    (call L.fun1.1.7 fun1.1 1 2 3))))))))


(let ([x `(module
            (letrec ([L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))])
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (begin (unsafe-fx+ (letrec ((L.fun1.1.10 (lambda (c.7 x.1 y.1 z.1) (let () x.1))))
                                                        (cletrec
                                                        ((fun1.3 (make-closure L.fun1.1.10 3)))
                                                        (call L.fun1.1.7 fun1.1 1 2 3))) 5) 5))))])
  (test-case "Hoist letrecs in begin effect"
             (check-equal? (hoist-lambdas x)
                           `(module
                                (define L.fun1.1.10 (lambda (c.7 x.1 y.1 z.1) (let () x.1)))
                                (define L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1)))
                                (cletrec
                                ((fun1.1 (make-closure L.fun1.1.7 3)))
                                (begin
                                    (unsafe-fx+
                                    (cletrec
                                    ((fun1.3 (make-closure L.fun1.1.10 3)))
                                    (call L.fun1.1.7 fun1.1 1 2 3))
                                    5)
                                    5))))))



(let ([x `(module
            (letrec ([L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))])
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (begin (begin (unsafe-fx+ (letrec ((L.fun1.1.10 (lambda (c.7 x.1 y.1 z.1) (let () x.1))))
                                                        (cletrec
                                                        ((fun1.3 (make-closure L.fun1.1.10 3)))
                                                        (call L.fun1.1.7 fun1.1 1 2 3))) 5)) 5))))])
  (test-case "Hoist letrecs in nested begin"
             (check-equal? (hoist-lambdas x)
                           `(module
                                (define L.fun1.1.10 (lambda (c.7 x.1 y.1 z.1) (let () x.1)))
                                (define L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1)))
                                (cletrec
                                ((fun1.1 (make-closure L.fun1.1.7 3)))
                                (begin
                                    (begin
                                    (unsafe-fx+
                                        (cletrec
                                        ((fun1.3 (make-closure L.fun1.1.10 3)))
                                        (call L.fun1.1.7 fun1.1 1 2 3))
                                        5))
                                    5))))))