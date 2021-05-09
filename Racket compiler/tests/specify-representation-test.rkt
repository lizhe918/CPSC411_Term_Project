#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/ptr-run-time
 racket/match
 rackunit)

(require "../component/specify.rkt")

; input: proc-exposed-lang-v9
; output: exprs-bits-lang-v8/contexts
; purpose: Check if the outputs of specify-representation matches the desired outputs of the interrogator.

(let ([x  '(module
            (define L.not.56.8 (lambda (c.59 tmp.49) (let () (not tmp.49))))
            (define L.procedure?.57.7
            (lambda (c.58 tmp.50) (let () (procedure? tmp.50))))
            (let ((procedure?.57 (make-procedure L.procedure?.57.7 1 0))
                (not.56 (make-procedure L.not.56.8 1 0)))
            (call L.procedure?.57.7 procedure?.57 not.56)))])
  (test-case "error case"
             (check-match (specify-representation x)
                           `(module
                            (define L.not.56.8 (lambda (c.59 tmp.49) (let () (if (!= tmp.49 6) 6 14))))
                            (define L.procedure?.57.7
                            (lambda (c.58 tmp.50) (let () (if (= (bitwise-and tmp.50 7) 2) 14 6))))
                            (let ((procedure?.57
                                (let ((,tmp.60 (+ (alloc 16) 2)))
                                    (begin
                                    (mset! ,tmp.60 -2 L.procedure?.57.7)
                                    (mset! ,tmp.60 6 8)
                                    ,tmp.60)))
                                (not.56
                                (let ((,tmp.61 (+ (alloc 16) 2)))
                                    (begin (mset! ,tmp.61 -2 L.not.56.8) (mset! ,tmp.61 6 8) ,tmp.61))))
                            (call L.procedure?.57.7 procedure?.57 not.56))))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (make-procedure L.fun1.1 3 3)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify make-procedure"
             (check-match (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((,tmp.4 (+ (alloc 40) 2)))
                                    (begin (mset! ,tmp.4 -2 L.fun1.1) (mset! ,tmp.4 6 24) ,tmp.4))))
                            (call L.fun1.1 8 16 24)))))
                            
(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-procedure-arity L.fun1.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify unsafe-procedure-arity"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (mref L.fun1.1 6)))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-procedure-label L.fun1.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify unsafe-procedure-label"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (mref L.fun1.1 -2)))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-procedure-ref c.4 0)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify unsafe-procedure-ref"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (mref c.4 14)))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-procedure-set! L.fun1.1 0 10)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify unsafe-procedure-set!"
             (check-equal? (specify-representation x)
                           '(module 
                           (define L.fun1.1 
                           (lambda (x.1 y.1 z.1) (mset! L.fun1.1 14 80))) 
                           (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-fx+ (unsafe-procedure-ref c.4 0) (unsafe-procedure-arity L.fun1.1))))
            (call L.fun1.1 1 2 3))])
  (test-case "specify new primops in primops"
             (check-equal? (specify-representation x)
                           '(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (+ (mref c.4 14) (mref L.fun1.1 6))))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (call (make-procedure L.fun1.1 3 3) (unsafe-procedure-ref c.4 0) (unsafe-procedure-arity L.fun1.1) 3)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify new primops in call"
             (check-match (specify-representation x)
                          `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (call
                                (let ((,tmp.4 (+ (alloc 40) 2)))
                                    (begin (mset! ,tmp.4 -2 L.fun1.1) (mset! ,tmp.4 6 24) ,tmp.4))
                                (mref c.4 14)
                                (mref L.fun1.1 6)
                                24)))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (let ([x.3 (unsafe-procedure-label L.fun1.1)]) (call x.3 1 2 3))))
            (call L.fun1.1 1 2 3))])
  (test-case "specify new primops in let bind"
             (check-equal? (specify-representation x)
                           '(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1) (let ((x.3 (mref L.fun1.1 -2))) (call x.3 8 16 24))))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (let ([x.3 10]) (unsafe-procedure-ref c.4 0))))
            (call L.fun1.1 1 2 3))])
  (test-case "specify new primops in let body"
             (check-equal? (specify-representation x)
                           '(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (let ((x.3 80)) (mref c.4 14))))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (not (let ([x.3 (make-procedure L.fun1.1 3 3)]) (false)))
                            5
                            6)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify new primops in pred"
             (check-match (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (if (not
                                    (let ((x.3
                                            (let ((,tmp.4 (+ (alloc 40) 2)))
                                                (begin
                                                (mset! ,tmp.4 -2 L.fun1.1)
                                                (mset! ,tmp.4 6 24)
                                                ,tmp.4))))
                                        (false)))
                                    40
                                    48)))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (true)
                            (make-procedure L.fun1.1 3 3)
                            (unsafe-procedure-arity L.fun1.1))))
            (call L.fun1.1 1 2 3))])
  (test-case "specify new primops in if e"
             (check-match (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (if (true)
                                    (let ((,tmp.4 (+ (alloc 40) 2)))
                                    (begin (mset! ,tmp.4 -2 L.fun1.1) (mset! ,tmp.4 6 24) ,tmp.4))
                                    (mref L.fun1.1 6))))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (begin (unsafe-procedure-arity L.fun1.1))))
            (call L.fun1.1 1 2 3))])
  (test-case "specify new primops in begin e"
             (check-equal? (specify-representation x)
                           '(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (begin (mref L.fun1.1 6))))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (begin (unsafe-procedure-set! L.fun1.1 0 10) 5)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify new primops in begin effect"
             (check-equal? (specify-representation x)
                           '(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (begin (mset! L.fun1.1 14 80) 40)))
                            (call L.fun1.1 8 16 24)))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (begin (begin (begin (unsafe-procedure-set! L.fun1.1 0 10))) 5)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify new primops in nested begin"
             (check-equal? (specify-representation x)
                           '(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1) (begin (begin (begin (mset! L.fun1.1 14 80))) 40)))
                            (call L.fun1.1 8 16 24)))))

;; --- some nested cases
(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (make-procedure L.fun1.1 (unsafe-procedure-arity L.fun1.1) 3)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify nested 1"
             (check-match (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((,tmp.4 (+ (alloc 40) 2)))
                                    (begin
                                    (mset! ,tmp.4 -2 L.fun1.1)
                                    (mset! ,tmp.4 6 (mref L.fun1.1 6))
                                    ,tmp.4))))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-procedure-ref (make-procedure L.fun1.1 3 3) 3)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify nested 2"
             (check-match (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (mref
                                (let ((,tmp.4 (+ (alloc 40) 2)))
                                    (begin (mset! ,tmp.4 -2 L.fun1.1) (mset! ,tmp.4 6 24) ,tmp.4))
                                38)))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-procedure-label (unsafe-car x.1))))
            (call L.fun1.1 1 2 3))])
  (test-case "specify nested 3"
             (check-equal? (specify-representation x)
                           '(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (mref (mref x.1 -1) -2)))
                            (call L.fun1.1 8 16 24)))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-procedure-arity (unsafe-vector-ref x.1 0))))
            (call L.fun1.1 1 2 3))])
  (test-case "specify nested 4"
             (check-equal? (specify-representation x)
                           '(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (mref (mref x.1 5) 6)))
                            (call L.fun1.1 8 16 24)))))


;; --------------------------------------------- old cases -------------------------------------------
(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (pair? 5)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify pair?"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (if (= (bitwise-and 40 7) 1) 14 6)))
                            (call L.fun1.1 8 16 24)))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (vector? x.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify vector?"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (if (= (bitwise-and x.1 7) 3) 14 6)))
                            (call L.fun1.1 8 16 24)))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (cons 5 6)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify cons"
             (check-match (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((,tmp.1 (+ (alloc 16) 1)))
                                    (begin (mset! ,tmp.1 -1 40) (mset! ,tmp.1 7 48) ,tmp.1))))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-car x.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify car"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (mref x.1 -1)))
                            (call L.fun1.1 8 16 24)))))



(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-cdr x.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify cdr"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (mref x.1 7)))
                            (call L.fun1.1 8 16 24)))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-make-vector z.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify make-vector"
             (check-match (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((,tmp.1 (+ (alloc (* (+ 1 (arithmetic-shift-right z.1 3)) 8)) 3)))
                                    (begin (mset! ,tmp.1 -3 z.1) ,tmp.1))))
                            (call L.fun1.1 8 16 24)))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-vector-length x.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify vector-length"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (mref x.1 -3)))
                            (call L.fun1.1 8 16 24)))))

;; only allow to be used in effect 
(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (begin (unsafe-vector-set! x.1 5 y.1) 5)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify vector-set!"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (begin (mset! x.1 45 y.1) 40)))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-vector-ref x.1 5)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify vector-ref"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (mref x.1 45)))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (begin (unsafe-vector-set! x.1 5 y.1) (unsafe-vector-set! x.1 4 z.1) (unsafe-fx+ (unsafe-vector-ref x.1 5) (unsafe-vector-ref x.1 4)))))
            (call L.fun1.1 1 2 3))])
  (test-case "specify begin"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (begin
                                    (mset! x.1 45 y.1)
                                    (mset! x.1 37 z.1)
                                    (+ (mref x.1 45) (mref x.1 37)))))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (begin (begin (begin (unsafe-vector-set! x.1 (unsafe-fx+ 1 2) (unsafe-fx- 5 6)))) (eq? (unsafe-vector-ref x.1 3) -1))))
            (call L.fun1.1 1 2 3))])
  (test-case "specify nested begin"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (begin
                                    (begin
                                    (begin
                                        (mset!
                                        x.1
                                        (+ (* (arithmetic-shift-right (+ 8 16) 3) 8) 5)
                                        (- 40 48))))
                                    (if (= (mref x.1 29) -8) 14 6))))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (pair? y.1) (unsafe-fx+ (unsafe-car y.1) (unsafe-cdr y.1))
                            (if (vector? y.1) (unsafe-vector-length y.1)
                                (error 8)))))
            (call L.fun1.1 1 2 3))])
  (test-case "specify new primops with if"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (if (!= (if (= (bitwise-and y.1 7) 1) 14 6) 6)
                                    (+ (mref y.1 -1) (mref y.1 7))
                                    (if (!= (if (= (bitwise-and y.1 7) 3) 14 6) 6) (mref y.1 -3) 2110))))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (call L.fun1.1 (unsafe-make-vector (unsafe-fx- (unsafe-vector-length x.1) 1))
                                       (unsafe-fx* (unsafe-vector-ref y.1 0) (unsafe-vector-ref z.1 0))
                                       (begin (unsafe-vector-set! y.1 (unsafe-vector-length y.1) 0) 
                                              (unsafe-vector-set! z.1 (unsafe-vector-length z.1) 0) 
                                              (eq? y.1 z.1)))))
            (call L.fun1.1 1 2 3))])
  (test-case "specify new primops with call"
             (check-match (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (call
                                L.fun1.1
                                (let ((,tmp.1
                                        (+
                                        (alloc
                                            (* (+ 1 (arithmetic-shift-right (- (mref x.1 -3) 8) 3)) 8))
                                        3)))
                                    (begin (mset! ,tmp.1 -3 (- (mref x.1 -3) 8)) ,tmp.1))
                                (* (mref y.1 5) (arithmetic-shift-right (mref z.1 5) 3))
                                (begin
                                    (mset! y.1 (+ (* (arithmetic-shift-right (mref y.1 -3) 3) 8) 5) 0)
                                    (mset! z.1 (+ (* (arithmetic-shift-right (mref z.1 -3) 3) 8) 5) 0)
                                    (if (= y.1 z.1) 14 6)))))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (let ([k.1 (unsafe-car x.1)] [q.1 (unsafe-cdr x.1)] [g.1 (unsafe-make-vector (unsafe-fx+ z.1 y.1))]) 
                             (unsafe-vector-ref g.1 (unsafe-fx+ k.1 q.1)))))
            (call L.fun1.1 1 2 3))])
  (test-case "specify new primops with let"
             (check-match (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((k.1 (mref x.1 -1))
                                        (q.1 (mref x.1 7))
                                        (g.1
                                        (let ((,tmp.1
                                                (+
                                                (alloc (* (+ 1 (arithmetic-shift-right (+ z.1 y.1) 3)) 8))
                                                3)))
                                        (begin (mset! ,tmp.1 -3 (+ z.1 y.1)) ,tmp.1))))
                                    (mref g.1 (+ (* (arithmetic-shift-right (+ k.1 q.1) 3) 8) 5)))))
                            (call L.fun1.1 8 16 24)))))



(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (let ([q.1 (unsafe-vector-ref x.1 5)] 
                                  [b.1 (begin (unsafe-vector-set! (unsafe-make-vector z.1) (unsafe-vector-length z.1) z.1) (unsafe-vector-ref y.1 5))])
                            (vector? y.1))
                            (cons y.1 z.1)
                            (error 6))))
            (call L.fun1.1 1 2 3))])
  (test-case "specify new primops with let in pred"
             (check-match (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (if (let ((q.1 (mref x.1 45))
                                            (b.1
                                            (begin
                                            (mset!
                                                (let ((,tmp.1
                                                    (+
                                                        (alloc (* (+ 1 (arithmetic-shift-right z.1 3)) 8))
                                                        3)))
                                                (begin (mset! ,tmp.1 -3 z.1) ,tmp.1))
                                                (+ (* (arithmetic-shift-right (mref z.1 -3) 3) 8) 5)
                                                z.1)
                                            (mref y.1 45))))
                                        (!= (if (= (bitwise-and y.1 7) 3) 14 6) 6))
                                    (let ((,tmp.2 (+ (alloc 16) 1)))
                                    (begin (mset! ,tmp.2 -1 y.1) (mset! ,tmp.2 7 z.1) ,tmp.2))
                                    1598)))
                            (call L.fun1.1 8 16 24)))))
;; -------------------------------------------- old cases ---------------------------------
(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        #t))
            (call L.fun1.1 1 2 3))])
  (test-case "specify true"
             (check-equal? (specify-representation x)
                           `(module (define L.fun1.1 (lambda (x.1 y.1 z.1) 14)) (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        #f))
            (call L.fun1.1 1 2 3))])
  (test-case "specify false"
             (check-equal? (specify-representation x)
                           `(module (define L.fun1.1 (lambda (x.1 y.1 z.1) 6)) (call L.fun1.1 8 16 24)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        empty))
            (call L.fun1.1 1 2 3))])
  (test-case "specify empty"
             (check-equal? (specify-representation x)
                           `(module (define L.fun1.1 (lambda (x.1 y.1 z.1) 22)) (call L.fun1.1 8 16 24)))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (void)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify void"
             (check-equal? (specify-representation x)
                           `(module (define L.fun1.1 (lambda (x.1 y.1 z.1) 30)) (call L.fun1.1 8 16 24)))))

(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        5))
            10)])
  (test-case "specify fixnum"
             (check-equal? (specify-representation x)
                           `(module (define L.fun1.1 (lambda (x.1 y.1 z.1) 40)) 80))))

(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        #\b))
            10)])
  (test-case "specify ascii"
             (check-equal? (specify-representation x)
                           `(module (define L.fun1.1 (lambda (x.1 y.1 z.1) 25134)) 80))))


(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-fx+ 1 2)))
            (unsafe-fx- 2 3))])
  (test-case "specify binop + -"
             (check-equal? (specify-representation x)
                           `(module (define L.fun1.1 (lambda (x.1 y.1 z.1) (+ 8 16))) (- 16 24)))))


(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-fx+ x.1 y.1)))
            (unsafe-fx- 2 3))])
  (test-case "specify binop + -"
             (check-equal? (specify-representation x)
                           `(module (define L.fun1.1 (lambda (x.1 y.1 z.1) (+ x.1 y.1))) (- 16 24)))))
                           


(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-fx* 1 2)))
            (unsafe-fx* 2 3))])
  (test-case "specify binop *"
             (check-equal? (specify-representation x)
                           `(module (define L.fun1.1 (lambda (x.1 y.1 z.1) (* 1 16))) (* 2 24)))))


(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-fx* x.1 y.1)))
            (unsafe-fx* 2 3))])
  (test-case "specify binop *"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1) (* x.1 (arithmetic-shift-right y.1 3))))
                            (* 2 24)))))
;; bug bounty
(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-fx* x.1 2)))
            (unsafe-fx* 2 3))])
  (test-case "specify binop *"
             (check-equal? (specify-representation x)
                           `(module (define L.fun1.1 (lambda (x.1 y.1 z.1) (* x.1 2))) (* 2 24)))))

(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-fx* 2 x.1)))
            (unsafe-fx* 2 3))])
  (test-case "specify binop *"
             (check-equal? (specify-representation x)
                           `(module (define L.fun1.1 (lambda (x.1 y.1 z.1) (* 2 x.1))) (* 2 24)))))


(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-fx>= x.1 y.1)))
            (unsafe-fx<= 2 3))])
  (test-case "specify binop <= >="
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (if (>= x.1 y.1) 14 6)))
                            (if (<= 16 24) 14 6)))))

(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (unsafe-fx> z.1 5)))
            (unsafe-fx< 2 3))])
  (test-case "specify binop < >"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (if (> z.1 40) 14 6)))
                            (if (< 16 24) 14 6)))))


(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (eq? z.1 y.1)))
            (eq? 2 3))])
  (test-case "specify binop eq?"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (if (= z.1 y.1) 14 6)))
                            (if (= 16 24) 14 6)))))
                            

(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if #t z.1 y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if value"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (if (!= 14 6) z.1 y.1)))
                            (call L.fun1.1 8 16 24)))))

(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (error 5) z.1 y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if value"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (if (!= 1342 6) z.1 y.1)))
                            (call L.fun1.1 8 16 24)))))

(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if empty z.1 y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if value"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (if (!= 22 6) z.1 y.1)))
                            (call L.fun1.1 8 16 24)))))

(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if x.1 z.1 y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if value"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (if (!= x.1 6) z.1 y.1)))
                            (call L.fun1.1 8 16 24)))))

(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if #\a z.1 y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if value"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (if (!= 24878 6) z.1 y.1)))
                            (call L.fun1.1 8 16 24)))))

(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if L.fun1.1 z.1 y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if value"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (if (!= L.fun1.1 6) z.1 y.1)))
                            (call L.fun1.1 8 16 24)))))

(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (eq? x.1 y.1) z.1 y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if binop"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1) (if (!= (if (= x.1 y.1) 14 6) 6) z.1 y.1)))
                            (call L.fun1.1 8 16 24)))))

(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (unsafe-fx>= x.1 y.1) z.1 y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if binop"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1) (if (!= (if (>= x.1 y.1) 14 6) 6) z.1 y.1)))
                            (call L.fun1.1 8 16 24)))))

(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (unsafe-fx>= (unsafe-fx+ x.1 z.1) y.1) z.1 y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if nested binop"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1) (if (!= (if (>= (+ x.1 z.1) y.1) 14 6) 6) z.1 y.1)))
                            (call L.fun1.1 8 16 24)))))

(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (unsafe-fx>= (unsafe-fx+ x.1 z.1) (unsafe-fx- y.1 z.1)) z.1 y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if nested binop"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (if (!= (if (>= (+ x.1 z.1) (- y.1 z.1)) 14 6) 6) z.1 y.1)))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (unsafe-fx>= (unsafe-fx+ x.1 (unsafe-fx+ z.1 y.1)) (unsafe-fx- (unsafe-fx* x.1 y.1) z.1)) z.1 y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if nested binop"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (if (!=
                                    (if (>=
                                            (+ x.1 (+ z.1 y.1))
                                            (- (* x.1 (arithmetic-shift-right y.1 3)) z.1))
                                        14
                                        6)
                                    6)
                                    z.1
                                    y.1)))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (empty? x.1) z.1 y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if unop"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (if (!= (if (= (bitwise-and x.1 255) 22) 14 6) 6) z.1 y.1)))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (fixnum? x.1) z.1 y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if unop"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (if (!= (if (= (bitwise-and x.1 7) 0) 14 6) 6) z.1 y.1)))
                            (call L.fun1.1 8 16 24)))))

(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (not (fixnum? x.1)) z.1 y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if nested unop"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (if (not (!= (if (= (bitwise-and x.1 7) 0) 14 6) 6)) z.1 y.1)))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (error? (empty? (fixnum? x.1))) z.1 y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if nested unop"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (if (!=
                                    (if (=
                                            (bitwise-and
                                            (if (=
                                                (bitwise-and (if (= (bitwise-and x.1 7) 0) 14 6) 255)
                                                22)
                                            14
                                            6)
                                            255)
                                            62)
                                        14
                                        6)
                                    6)
                                    z.1
                                    y.1)))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (empty? (fixnum? (unsafe-fx< (unsafe-fx* x.1 y.1) z.1))) z.1 y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if nested unop and binop"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (if (!=
                                    (if (=
                                            (bitwise-and
                                            (if (=
                                                (bitwise-and
                                                (if (< (* x.1 (arithmetic-shift-right y.1 3)) z.1) 14 6)
                                                7)
                                                0)
                                            14
                                            6)
                                            255)
                                            22)
                                        14
                                        6)
                                    6)
                                    z.1
                                    y.1)))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (true) z.1 (error 5))))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if true"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (if (true) z.1 1342)))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (let ([x.2 empty] [x.3 (void)]) (eq? x.2 x.3)) z.1 (error 5))))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if nested pred"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (if (let ((x.2 22) (x.3 30)) (!= (if (= x.2 x.3) 14 6) 6)) z.1 1342)))
                            (call L.fun1.1 8 16 24)))))

(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (if (true) (unsafe-fx< x.1 y.1) (empty? z.1)) z.1 (error 5))))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if nested pred"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (if (if (true)
                                        (!= (if (< x.1 y.1) 14 6) 6)
                                        (!= (if (= (bitwise-and z.1 255) 22) 14 6) 6))
                                    z.1
                                    1342)))
                            (call L.fun1.1 8 16 24)))))


;; Newly added cases
(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (if (true) (true) (false))
                            x.1
                            y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if pred"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1) (if (if (true) (true) (false)) x.1 y.1)))
                            (call L.fun1.1 8 16 24)))))


(let ([x '(module
            (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (if (true) #t (false))
                            x.1
                            y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "specify if pred"
             (check-equal? (specify-representation x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1) (if (if (true) (!= 14 6) (false)) x.1 y.1)))
                            (call L.fun1.1 8 16 24)))))