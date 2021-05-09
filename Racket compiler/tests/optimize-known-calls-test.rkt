#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/ptr-run-time
 racket/match
 rackunit)

 (require "../component/opt-known-calls.rkt")

; input: closure-lang-v9
; output: closure-lang-v9
; purpose: Check if the outputs of optimize-known-calls matches the desired outputs of the interrogator.

(let ([x `(module 5)])
  (test-case "special case"
             (check-equal? (optimize-known-calls x)
                           `(module 5)))) 

(let ([x `(module (closure-call x.1 x.1 1 2 3))])
  (test-case "cannot optimize closure-call (no label avaliable)"
             (check-equal? (optimize-known-calls x)
                           `(module (closure-call x.1 x.1 1 2 3))))) 


(let ([x `(module
            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (closure-call fun1.1 fun1.1 1 2 3))))])
  (test-case "optimize closure-call under make-closure"
             (check-equal? (optimize-known-calls x)
                           `(module
                            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                                (cletrec
                                ((fun1.1 (make-closure L.fun1.1.7 3)))
                                (call L.fun1.1.7 fun1.1 1 2 3))))))) 


(let ([x `(module
          (letrec ((L.fun1.1.7
                    (lambda (c.4 x.1 y.1 z.1)
                      (let ((fun1.1 (closure-ref c.4 0)))
                        (let ((tmp.5 (begin (unsafe-fx+ 1 2) fun1.1)))
                          (closure-call tmp.5 tmp.5 y.1 z.1 x.1))))))
            (cletrec ((fun1.1 (make-closure L.fun1.1.7 3 fun1.1))) 5)))])
  (test-case "cannot optimize recursive calls inside"
             (check-equal? (optimize-known-calls x)
                           `(module
                            (letrec ((L.fun1.1.7
                                      (lambda (c.4 x.1 y.1 z.1)
                                        (let ((fun1.1 (closure-ref c.4 0)))
                                          (let ((tmp.5 (begin (unsafe-fx+ 1 2) fun1.1)))
                                            (closure-call tmp.5 tmp.5 y.1 z.1 x.1))))))
                              (cletrec ((fun1.1 (make-closure L.fun1.1.7 3 fun1.1))) 5)))))) 




(let ([x `(module
          (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
            (cletrec
            ((fun1.1 (make-closure L.fun1.1.7 3)))
            (let ((tmp.5 (begin (unsafe-fx+ 1 2) x.1)))
              (closure-call tmp.5 tmp.5 1 2 3)))))])
  (test-case "cannot optimize closure-call without aloc (same to case let)"
             (check-equal? (optimize-known-calls x)
                           `(module
                            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                              (cletrec
                              ((fun1.1 (make-closure L.fun1.1.7 3)))
                              (let ((tmp.5 (begin (unsafe-fx+ 1 2) x.1)))
                                (closure-call tmp.5 tmp.5 1 2 3)))))))) 


;; cases that optimize in different positions 
(let ([x `(module
            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (let ([x.3 (closure-call fun1.1 fun1.1 1 2 3)]) x.3))))])
  (test-case "optimize closure-call in let bind"
             (check-equal? (optimize-known-calls x)
                           `(module
                            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                              (cletrec
                              ((fun1.1 (make-closure L.fun1.1.7 3)))
                              (let ((x.3 (call L.fun1.1.7 fun1.1 1 2 3))) x.3))))))) 

(let ([x `(module
            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (let ([x.3 5]) (closure-call fun1.1 fun1.1 x.3 2 3)))))])
  (test-case "optimize closure-call in let body"
             (check-equal? (optimize-known-calls x)
                           `(module
                            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                              (cletrec
                              ((fun1.1 (make-closure L.fun1.1.7 3)))
                              (let ((x.3 5)) (call L.fun1.1.7 fun1.1 x.3 2 3)))))))) 

(let ([x `(module
            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (unsafe-fx+ (closure-call fun1.1 fun1.1 x.3 2 3) 5))))])
  (test-case "optimize closure-call in primops"
             (check-equal? (optimize-known-calls x)
                           `(module
                            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                              (cletrec
                              ((fun1.1 (make-closure L.fun1.1.7 3)))
                              (unsafe-fx+ (call L.fun1.1.7 fun1.1 x.3 2 3) 5))))))) 

(let ([x `(module
            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (closure-call fun1.1 fun1.1 1 2 (closure-call fun1.1 fun1.1 1 2 3)))))])
  (test-case "optimize closure-call in closure-call"
             (check-equal? (optimize-known-calls x)
                           `(module
                            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                              (cletrec
                              ((fun1.1 (make-closure L.fun1.1.7 3)))
                              (call L.fun1.1.7 fun1.1 1 2 (call L.fun1.1.7 fun1.1 1 2 3)))))))) 

(let ([x `(module
            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (closure-ref c.4 (closure-call fun1.1 fun1.1 1 2 3)))))])
  (test-case "optimize closure-call in closure-ref (impossible, but still test)"
             (check-equal? (optimize-known-calls x)
                           `(module
                            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                              (cletrec
                              ((fun1.1 (make-closure L.fun1.1.7 3)))
                              (closure-ref c.4 (call L.fun1.1.7 fun1.1 1 2 3))))))))

(let ([x `(module
            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (call L.fun1.1.7 fun1.1 (closure-call fun1.1 fun1.1 1 2 3) 2 3))))])
  (test-case "optimize closure-call in call (impossible, but still test)"
             (check-equal? (optimize-known-calls x)
                           `(module
                            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                              (cletrec
                              ((fun1.1 (make-closure L.fun1.1.7 3)))
                              (call L.fun1.1.7 fun1.1 (call L.fun1.1.7 fun1.1 1 2 3) 2 3)))))))


(let ([x `(module
            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (letrec ([L.fun1.1.8 (lambda (c.5) (let () (closure-call fun1.1 fun1.1 1 2 3)))]) (cletrec ((fun1.2 (make-closure L.fun1.1.8 0))) 6)))))])
  (test-case "optimize closure-call in letrec 1"
             (check-equal? (optimize-known-calls x)
                           `(module
                            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                              (cletrec
                              ((fun1.1 (make-closure L.fun1.1.7 3)))
                              (letrec ((L.fun1.1.8
                                        (lambda (c.5) (let () (call L.fun1.1.7 fun1.1 1 2 3)))))
                                (cletrec ((fun1.2 (make-closure L.fun1.1.8 0))) 6))))))))


(let ([x `(module
            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (letrec ([L.fun1.1.8 (lambda (c.5) (let () 0))]) (cletrec ((fun1.2 (make-closure L.fun1.1.8 0))) (closure-call fun1.1 fun1.1 1 2 3))))))])
  (test-case "optimize closure-call in letrec 2"
             (check-equal? (optimize-known-calls x)
                           `(module
                            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                              (cletrec
                              ((fun1.1 (make-closure L.fun1.1.7 3)))
                              (letrec ((L.fun1.1.8 (lambda (c.5) (let () 0))))
                                (cletrec
                                  ((fun1.2 (make-closure L.fun1.1.8 0)))
                                  (call L.fun1.1.7 fun1.1 1 2 3)))))))))



(let ([x `(module
            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (letrec ([L.fun1.1.8 (lambda (c.5) (let () 0))]) (cletrec ((fun1.2 (make-closure L.fun1.1.8 (closure-call fun1.1 fun1.1 1 2 3)))) 0)))))])
  (test-case "optimize closure-call in letrec 3 (impossible)"
             (check-equal? (optimize-known-calls x)
                           `(module
                            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                              (cletrec
                              ((fun1.1 (make-closure L.fun1.1.7 3)))
                              (letrec ((L.fun1.1.8 (lambda (c.5) (let () 0))))
                                (cletrec
                                  ((fun1.2 (make-closure L.fun1.1.8 (call L.fun1.1.7 fun1.1 1 2 3))))
                                  0))))))))

(let ([x `(module
            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (if (not (let ([x.3 (closure-call fun1.1 fun1.1 1 2 3)]) (true)))
                    5
                    6))))])
  (test-case "optimize closure-call in pred "
             (check-equal? (optimize-known-calls x)
                           `(module
                            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                              (cletrec
                              ((fun1.1 (make-closure L.fun1.1.7 3)))
                              (if (not (let ((x.3 (call L.fun1.1.7 fun1.1 1 2 3))) (true))) 5 6)))))))

(let ([x `(module
            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (if (true)
                    (unsafe-fx+ (closure-call fun1.1 fun1.1 1 2 3) 5)
                    6))))])
  (test-case "optimize closure-call in if e"
             (check-equal? (optimize-known-calls x)
                           `(module
                            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                              (cletrec
                              ((fun1.1 (make-closure L.fun1.1.7 3)))
                              (if (true) (unsafe-fx+ (call L.fun1.1.7 fun1.1 1 2 3) 5) 6)))))))

(let ([x `(module
            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (begin (closure-call fun1.1 fun1.1 1 2 3)))))])
  (test-case "optimize closure-call in begin e"
             (check-equal? (optimize-known-calls x)
                           `(module
                            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                              (cletrec
                              ((fun1.1 (make-closure L.fun1.1.7 3)))
                              (begin (call L.fun1.1.7 fun1.1 1 2 3))))))))

(let ([x `(module
            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (begin (unsafe-fx+ (unsafe-fx+ (closure-call fun1.1 fun1.1 1 2 3) 10) 5) 10))))])
  (test-case "optimize closure-call in begin effect"
             (check-equal? (optimize-known-calls x)
                           `(module
                            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                              (cletrec
                              ((fun1.1 (make-closure L.fun1.1.7 3)))
                              (begin
                                (unsafe-fx+ (unsafe-fx+ (call L.fun1.1.7 fun1.1 1 2 3) 10) 5)
                                10)))))))

(let ([x `(module
            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                (cletrec
                ((fun1.1 (make-closure L.fun1.1.7 3)))
                (begin (begin (unsafe-fx+ (closure-call fun1.1 fun1.1 1 2 3) 10)) 10))))])
  (test-case "optimize closure-call in nested begin effect"
             (check-equal? (optimize-known-calls x)
                           `(module
                            (letrec ((L.fun1.1.7 (lambda (c.4 x.1 y.1 z.1) (let () x.1))))
                              (cletrec
                              ((fun1.1 (make-closure L.fun1.1.7 3)))
                              (begin (begin (unsafe-fx+ (call L.fun1.1.7 fun1.1 1 2 3) 10)) 10)))))))