#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/ptr-run-time
 racket/match
 rackunit)

(require "../component/impl-closure.rkt")

; input: hoisted-lang-v9
; output: proc-exposed-lang-v9
; purpose: Check if the outputs of implement-closures matches the desired outputs of the interrogator.


(let ([x `(module 5)])
  (test-case "No closure is used"
             (check-equal? (implement-closures x)
                           `(module 5)))) 

(let ([x `(module (cletrec ([x.1 (make-closure L.fun1.1 3)]) 5))])
  (test-case "Transform cletrec"
             (check-equal? (implement-closures x)
                           `(module (let ((x.1 (make-procedure L.fun1.1 3 0))) 5))))) 


(let ([x `(module (cletrec ([x.1 (make-closure L.fun1.1 3 c.1 c.2 c.3)]) 5))])
  (test-case "Transform cletrec with arguments"
             (check-equal? (implement-closures x)
                           `(module
                            (let ((x.1 (make-procedure L.fun1.1 3 3)))
                                (begin
                                (unsafe-procedure-set! x.1 0 c.1)
                                (unsafe-procedure-set! x.1 1 c.2)
                                (unsafe-procedure-set! x.1 2 c.3)
                                5)))))) 


(let ([x `(module (closure-ref c.4 0))])
  (test-case "Transform closure-ref"
             (check-equal? (implement-closures x)
                           `(module (unsafe-procedure-ref c.4 0))))) 



(let ([x `(module (closure-call L.fun1.1 fun1.1 1 2 3))])
  (test-case "Transform closure-call"
             (check-equal? (implement-closures x)
                           `(module (call (unsafe-procedure-label L.fun1.1) fun1.1 1 2 3)))))


;; some nested cases
(let ([x `(module (closure-call L.fun1.1 fun1.1 (closure-ref c.4 0) 2 3))])
  (test-case "Transform nested closure-call and closure-ref"
             (check-equal? (implement-closures x)
                           `(module
                                (call
                                (unsafe-procedure-label L.fun1.1)
                                fun1.1
                                (unsafe-procedure-ref c.4 0)
                                2
                                3)))))

(let ([x `(module (closure-call L.fun1.1 fun1.1 (cletrec ([x.1 (make-closure L.fun1.2 3 c.1 c.2 c.3)]) 5) 2 3))])
  (test-case "Transform nested closure-call and cletrec"
             (check-equal? (implement-closures x)
                           `(module
                                (call
                                (unsafe-procedure-label L.fun1.1)
                                fun1.1
                                (let ((x.1 (make-procedure L.fun1.2 3 3)))
                                    (begin
                                    (unsafe-procedure-set! x.1 0 c.1)
                                    (unsafe-procedure-set! x.1 1 c.2)
                                    (unsafe-procedure-set! x.1 2 c.3)
                                    5))
                                2
                                3)))))

(let ([x `(module (closure-call (closure-call L.fun1.3 fun1.3) fun1.1 1 2 3))])
  (test-case "Transform nested closure-call with label as another call"
             (check-equal? (implement-closures x)
                           `(module
                                (call
                                (unsafe-procedure-label (call (unsafe-procedure-label L.fun1.3) fun1.3))
                                fun1.1
                                1
                                2
                                3)))))


(let ([x `(module (closure-ref (closure-call L.fun1.3 fun1.3) 3))])
  (test-case "Transform nested closure-ref with label"
             (check-equal? (implement-closures x)
                           `(module (unsafe-procedure-ref (call (unsafe-procedure-label L.fun1.3) fun1.3) 3)))))



(let ([x `(module (cletrec ([x.1 (make-closure L.fun1.1 3 c.1 c.2 c.3)]) (cletrec ([x.2 (make-closure L.fun1.2 0)]) 5)))])
  (test-case "Transform nested cletrec"
             (check-equal? (implement-closures x)
                           `(module
                            (let ((x.1 (make-procedure L.fun1.1 3 3)))
                                (begin
                                (unsafe-procedure-set! x.1 0 c.1)
                                (unsafe-procedure-set! x.1 1 c.2)
                                (unsafe-procedure-set! x.1 2 c.3)
                                (let ((x.2 (make-procedure L.fun1.2 0 0))) 5)))))))


;; implement in different positions 
;; error
(let ([x `(module (unsafe-fx+ (closure-call L.fun1.1 fun1.1 1 2 3) 5))])
  (test-case "Transform cletrec in primop"
             (check-equal? (implement-closures x)
                           `(module
                            (unsafe-fx+ (call (unsafe-procedure-label L.fun1.1) fun1.1 1 2 3) 5)))))


(let ([x `(module (closure-ref (cletrec ([x.1 (make-closure L.fun1.1 3 c.1 c.2 c.3)]) 5) 3))])
  (test-case "Transform in closure-ref"
             (check-equal? (implement-closures x)
                           `(module
                            (unsafe-procedure-ref
                            (let ((x.1 (make-procedure L.fun1.1 3 3)))
                              (begin
                                (unsafe-procedure-set! x.1 0 c.1)
                                (unsafe-procedure-set! x.1 1 c.2)
                                (unsafe-procedure-set! x.1 2 c.3)
                                5))
                            3)))))

(let ([x `(module (call (closure-call L.fun1.1 fun1.1 1 2 3) (cletrec ([x.1 (make-closure L.fun1.1 3 c.1 c.2 c.3)]) 5)))])
  (test-case "Transform in call"
             (check-equal? (implement-closures x)
                           `(module
                            (call
                            (call (unsafe-procedure-label L.fun1.1) fun1.1 1 2 3)
                            (let ((x.1 (make-procedure L.fun1.1 3 3)))
                              (begin
                                (unsafe-procedure-set! x.1 0 c.1)
                                (unsafe-procedure-set! x.1 1 c.2)
                                (unsafe-procedure-set! x.1 2 c.3)
                                5)))))))


(let ([x `(module (cletrec ([x.1 (make-closure L.fun1.1 3 1 2 3)]) (closure-call L.fun1.1 fun1.1 1 2 3)))])
  (test-case "Transform in cletrec"
             (check-equal? (implement-closures x)
                           `(module
                            (let ((x.1 (make-procedure L.fun1.1 3 3)))
                              (begin
                                (unsafe-procedure-set! x.1 0 1)
                                (unsafe-procedure-set! x.1 1 2)
                                (unsafe-procedure-set! x.1 2 3)
                                (call (unsafe-procedure-label L.fun1.1) fun1.1 1 2 3)))))))


(let ([x `(module (let ([x.3 (cletrec ([x.1 (make-closure L.fun1.1 3 c.1 c.2 c.3)]) 5)]) 5))])
  (test-case "Transform in let bind"
             (check-equal? (implement-closures x)
                           `(module
                              (let ((x.3
                                    (let ((x.1 (make-procedure L.fun1.1 3 3)))
                                      (begin
                                        (unsafe-procedure-set! x.1 0 c.1)
                                        (unsafe-procedure-set! x.1 1 c.2)
                                        (unsafe-procedure-set! x.1 2 c.3)
                                        5))))
                                5)))))


(let ([x `(module (let ([x.3 (closure-ref c.4 0)]) (closure-call L.fun1.1 fun1.1 1 2 3)))])
  (test-case "Transform in let body"
             (check-equal? (implement-closures x)
                           `(module
                            (let ((x.3 (unsafe-procedure-ref c.4 0)))
                              (call (unsafe-procedure-label L.fun1.1) fun1.1 1 2 3))))))


(let ([x `(module (if (let ([x.4 (cletrec ([x.1 (make-closure L.fun1.1 3 c.1 c.2 c.3)]) 5)]) (true)) 5 6))])
  (test-case "Transform in pred"
             (check-equal? (implement-closures x)
                           `(module
                            (if (let ((x.4
                                      (let ((x.1 (make-procedure L.fun1.1 3 3)))
                                        (begin
                                          (unsafe-procedure-set! x.1 0 c.1)
                                          (unsafe-procedure-set! x.1 1 c.2)
                                          (unsafe-procedure-set! x.1 2 c.3)
                                          5))))
                                  (true))
                              5
                              6)))))



(let ([x `(module (if (true)
                      (closure-call L.fun1.1 fun1.1 1 2 3)
                      (closure-ref c.4 15)))])
  (test-case "Transform in if e"
             (check-equal? (implement-closures x)
                           `(module
                            (if (true)
                              (call (unsafe-procedure-label L.fun1.1) fun1.1 1 2 3)
                              (unsafe-procedure-ref c.4 15))))))



(let ([x `(module (begin (cletrec ([x.1 (make-closure L.fun1.1 3 c.1 c.2 c.3)]) 5)))])
  (test-case "Transform in begin e"
             (check-equal? (implement-closures x)
                           `(module
                              (begin
                                (let ((x.1 (make-procedure L.fun1.1 3 3)))
                                  (begin
                                    (unsafe-procedure-set! x.1 0 c.1)
                                    (unsafe-procedure-set! x.1 1 c.2)
                                    (unsafe-procedure-set! x.1 2 c.3)
                                    5)))))))


(let ([x `(module (begin (unsafe-fx+ (cletrec ([x.1 (make-closure L.fun1.1 3 c.1 c.2 c.3)]) 5) 5) 5))])
  (test-case "Transform in begin effect"
             (check-equal? (implement-closures x)
                           `(module
                            (begin
                              (unsafe-fx+
                              (let ((x.1 (make-procedure L.fun1.1 3 3)))
                                (begin
                                  (unsafe-procedure-set! x.1 0 c.1)
                                  (unsafe-procedure-set! x.1 1 c.2)
                                  (unsafe-procedure-set! x.1 2 c.3)
                                  5))
                              5)
                              5)))))


(let ([x `(module (begin (begin (begin (unsafe-fx+ (closure-call L.fun1.1 fun1.1 1 2 3) (closure-ref c.4 15)))) 5))])
  (test-case "Transform in nested effect"
             (check-equal? (implement-closures x)
                           `(module
                            (begin
                              (begin
                                (begin
                                  (unsafe-fx+
                                  (call (unsafe-procedure-label L.fun1.1) fun1.1 1 2 3)
                                  (unsafe-procedure-ref c.4 15))))
                              5)))))