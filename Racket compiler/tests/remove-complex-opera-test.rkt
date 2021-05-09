#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/ptr-run-time
 racket/match
 rackunit)

(require "../component/remove-op.rkt")

; input: exprs-bigs-lang-v8/contexts
; output: values-bits-lang-v8
; purpose: Check if the outputs of remove-complex-opera* matches the desired outputs of the interrogator.


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (mref x.1 8)))
            (call L.fun1.1 1 2 3))])
  (test-case "Support mref in value"
             (check-equal? (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (mref x.1 8)))
                            (call L.fun1.1 1 2 3)))))



(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (alloc x.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "Support alloc in value"
             (check-equal? (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (alloc x.1)))
                            (call L.fun1.1 1 2 3)))))
;; begin cases

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (begin
                            (mset! x.1 8 8)
                            y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "Support begin in tail"
             (check-equal? (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) (begin (mset! x.1 8 8) y.1)))
                            (call L.fun1.1 1 2 3)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (let ([x.2 (begin (mset! x.1 8 8) y.1)]) z.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "Support begin in value"
             (check-equal? (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1) (let ((x.2 (begin (mset! x.1 8 8) y.1))) z.1)))
                            (call L.fun1.1 1 2 3)))))
;; error
(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (begin (mset! y.1 8 8) (true))
                            x.1
                            y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "Support begin in pred"
             (check-equal? (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1) (if (begin (mset! y.1 8 8) (true)) x.1 y.1)))
                            (call L.fun1.1 1 2 3)))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (begin
                            (begin (mset! x.1 8 32) (mset! x.2 32 8))
                            y.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "Support begin in effect"
             (check-equal? (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (begin (begin (mset! x.1 8 32) (mset! x.2 32 8)) y.1)))
                            (call L.fun1.1 1 2 3)))))

;; ---------------
(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (mref (bitwise-and x.1 y.1) (arithmetic-shift-right x.1 8))))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in mref binop"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((,tmp.1 (bitwise-and x.1 y.1)))
                                    (let ((,tmp.2 (arithmetic-shift-right x.1 8))) (mref ,tmp.1 ,tmp.2)))))
                            (call L.fun1.1 1 2 3)))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (mref (mref x.1 y.1) (mref x.1 8))))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in mref mref"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((,tmp.1 (mref x.1 y.1)))
                                    (let ((,tmp.2 (mref x.1 8))) (mref ,tmp.1 ,tmp.2)))))
                            (call L.fun1.1 1 2 3)))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (mref (alloc x.1) (alloc 8))))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in mref aloc"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((,tmp.1 (alloc x.1)))
                                    (let ((,tmp.2 (alloc 8))) (mref ,tmp.1 ,tmp.2)))))
                            (call L.fun1.1 1 2 3)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (mref (call L.end.1 x.2) (call L.addup.1 8))))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in mref call"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((,tmp.1 (call L.end.1 x.2)))
                                    (let ((,tmp.2 (call L.addup.1 8))) (mref ,tmp.1 ,tmp.2)))))
                            (call L.fun1.1 1 2 3)))))



(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (mref (let ([x.1 8]) x.1) (let ([x.2 8]) 8))))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in mref let"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((,tmp.1 (let ((x.1 8)) x.1)))
                                    (let ((,tmp.2 (let ((x.2 8)) 8))) (mref ,tmp.1 ,tmp.2)))))
                            (call L.fun1.1 1 2 3)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (mref (if (true) x.1 2) 8)))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in mref if"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1) (let ((,tmp.1 (if (true) x.1 2))) (mref ,tmp.1 8))))
                            (call L.fun1.1 1 2 3)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (mref (begin (mset! x.1 (+ y.2 8) (+ x.1 8)) (mref x.1 8)) z.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in mref begin"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((,tmp.1
                                        (begin
                                        (let ((,tmp.2 (+ y.2 8))) (mset! x.1 ,tmp.2 (+ x.1 8)))
                                        (mref x.1 8))))
                                    (mref ,tmp.1 z.1))))
                            (call L.fun1.1 1 2 3)))))
;; ---------
(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (alloc (bitwise-and x.1 y.1))))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in alloc binop"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1) (let ((,tmp.1 (bitwise-and x.1 y.1))) (alloc ,tmp.1))))
                            (call L.fun1.1 1 2 3)))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (alloc (mref x.1 y.1))))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in alloc mref"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1) (let ((,tmp.1 (mref x.1 y.1))) (alloc ,tmp.1))))
                            (call L.fun1.1 1 2 3)))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (alloc (alloc x.1))))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in alloc alloc"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1) (let ((,tmp.1 (alloc x.1))) (alloc ,tmp.1))))
                            (call L.fun1.1 1 2 3)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (alloc (call L.end.1 x.2))))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in alloc call"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1) (let ((,tmp.1 (call L.end.1 x.2))) (alloc ,tmp.1))))
                            (call L.fun1.1 1 2 3)))))



(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (alloc (let ([x.1 8]) x.1))))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in alloc let"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1) (let ((,tmp.1 (let ((x.1 8)) x.1))) (alloc ,tmp.1))))
                            (call L.fun1.1 1 2 3)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (alloc (if (true) x.1 2))))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in alloc if"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1) (let ((,tmp.1 (if (true) x.1 2))) (alloc ,tmp.1))))
                            (call L.fun1.1 1 2 3)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (alloc (begin (mset! x.1 (+ y.2 8) (+ x.1 8)) (mref x.1 8)))))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in alloc begin"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((,tmp.1
                                        (begin
                                        (let ((,tmp.2 (+ y.2 8))) (mset! x.1 ,tmp.2 (+ x.1 8)))
                                        (mref x.1 8))))
                                    (alloc ,tmp.1))))
                            (call L.fun1.1 1 2 3)))))

;; ---------- begin as value
(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (let ([x.2 (begin (mset! x.1 8 16) (+ 1 2))]) z.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in begin binop"
             (check-equal? (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1) (let ((x.2 (begin (mset! x.1 8 16) (+ 1 2)))) z.1)))
                            (call L.fun1.1 1 2 3)))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (let ([x.2 (begin (mset! x.1 8 16) (mref y.1 8))]) z.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in begin mref"
             (check-equal? (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((x.2 (begin (mset! x.1 8 16) (mref y.1 8)))) z.1)))
                            (call L.fun1.1 1 2 3)))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (let ([x.2 (begin (mset! x.1 8 16) (alloc 8))]) z.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in begin alloc"
             (check-equal? (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((x.2 (begin (mset! x.1 8 16) (alloc 8)))) z.1)))
                            (call L.fun1.1 1 2 3)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (let ([x.2 (alloc (begin (mset! x.1 8 16) (call L.end.1 8)))]) z.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in begin call"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((x.2
                                        (let ((,tmp.1 (begin (mset! x.1 8 16) (call L.end.1 8))))
                                        (alloc ,tmp.1))))
                                    z.1)))
                            (call L.fun1.1 1 2 3)))))



(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (let ([x.2 (alloc (begin (mset! x.1 8 16) (let ([z.2 8]) z.2)))]) z.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in begin let"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((x.2
                                        (let ((,tmp.1 (begin (mset! x.1 8 16) (let ((z.2 8)) z.2))))
                                        (alloc ,tmp.1))))
                                    z.1)))
                            (call L.fun1.1 1 2 3)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (let ([x.2 (begin (mset! x.1 8 16) (if (true) (+ 1 2) z.1))]) z.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in begin if"
             (check-equal? (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((x.2 (begin (mset! x.1 8 16) (if (true) (+ 1 2) z.1)))) z.1)))
                            (call L.fun1.1 1 2 3)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (let ([x.2 (begin (mset! x.1 8 16) (begin (mset! x.1 8 16) z.2))]) z.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera in begin begin"
             (check-equal? (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((x.2 (begin (mset! x.1 8 16) (begin (mset! x.1 8 16) z.2)))) z.1)))
                            (call L.fun1.1 1 2 3)))))
;; ---------- nested in mset!
(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (begin
                            (mset! (+ 1 2) (alloc 8) (mref v.1 8))
                            z.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera nested in mset!"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (begin
                                    (let ((,tmp.1 (+ 1 2)))
                                    (let ((,tmp.2 (alloc 8))) (mset! ,tmp.1 ,tmp.2 (mref v.1 8))))
                                    z.1)))
                            (call L.fun1.1 1 2 3)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (begin
                            (mset! (call (alloc 3) (mref v.1 2)) (if (true) (mref q.1 32) (alloc 8)) (+ (mref v.1 8) (mref q.1 32)))
                            z.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera nested in mset!"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (begin
                                    (let ((,tmp.1
                                        (let ((,tmp.2 (alloc 3)))
                                            (let ((,tmp.3 (mref v.1 2))) (call ,tmp.2 ,tmp.3)))))
                                    (let ((,tmp.4 (if (true) (mref q.1 32) (alloc 8))))
                                        (mset!
                                        ,tmp.1
                                        ,tmp.4
                                        (let ((,tmp.5 (mref v.1 8)))
                                        (let ((,tmp.6 (mref q.1 32))) (+ ,tmp.5 ,tmp.6))))))
                                    z.1)))
                            (call L.fun1.1 1 2 3)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (begin
                            (mset! (let ([x.2 (alloc 8)]) (mref q.1 8)) (let ([x.3 (mref q.1 8)]) (alloc 16)) (bitwise-xor (alloc 8) (alloc 32)))
                            z.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "Remove opera nested in mset!"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (begin
                                    (let ((,tmp.1 (let ((x.2 (alloc 8))) (mref q.1 8))))
                                    (let ((,tmp.2 (let ((x.3 (mref q.1 8))) (alloc 16))))
                                        (mset!
                                        ,tmp.1
                                        ,tmp.2
                                        (let ((,tmp.3 (alloc 8)))
                                        (let ((,tmp.4 (alloc 32))) (bitwise-xor ,tmp.3 ,tmp.4))))))
                                    z.1)))
                            (call L.fun1.1 1 2 3)))))
;; ------------------------------------ old cases --------------------------------------
(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        x.1))
            (call L.fun1.1 1 2 3))])
  (test-case "Nothing to remove"
             (check-equal? (remove-complex-opera* x)
                           `(module (define L.fun1.1 (lambda (x.1 y.1 z.1) x.1)) (call L.fun1.1 1 2 3)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (bitwise-and (bitwise-ior x.1 y.1) (bitwise-xor y.1 z.1))))
            (call L.fun1.1 1 2 3))])
  (test-case "binop with binop"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((,tmp.1 (bitwise-ior x.1 y.1)))
                                    (let ((,tmp.2 (bitwise-xor y.1 z.1))) (bitwise-and ,tmp.1 ,tmp.2)))))
                            (call L.fun1.1 1 2 3)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (bitwise-and (call L.fun1.1 1 2 3) (call L.fun1.1 3 4 5))))
            (call L.fun1.1 1 2 3))])
  (test-case "binop with call"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((,tmp.1 (call L.fun1.1 1 2 3)))
                                    (let ((,tmp.2 (call L.fun1.1 3 4 5))) (bitwise-and ,tmp.1 ,tmp.2)))))
                            (call L.fun1.1 1 2 3)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (bitwise-and (let ([x.2 2]) x.2) x.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "binop with let"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((,tmp.1 (let ((x.2 2)) x.2))) (bitwise-and ,tmp.1 x.1))))
                            (call L.fun1.1 1 2 3)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (bitwise-and (if (< x.1 x.2) x.1 x.2) x.1)))
            (call L.fun1.1 1 2 3))])
  (test-case "binop with if"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((,tmp.1 (if (< x.1 x.2) x.1 x.2))) (bitwise-and ,tmp.1 x.1))))
                            (call L.fun1.1 1 2 3)))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        x.1))
            (call L.fun1.1 (bitwise-xor 1 2) (arithmetic-shift-right 7 2) 3))])
  (test-case "call with binop"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) x.1))
                            (let ((,tmp.1 (bitwise-xor 1 2)))
                                (let ((,tmp.2 (arithmetic-shift-right 7 2)))
                                (call L.fun1.1 ,tmp.1 ,tmp.2 3)))))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (bitwise-and (if (< x.1 x.2) x.1 x.2) x.1)))
                (define L.fun1.2
                    (lambda ()
                        L.fun1.1))
            (call (call L.fun1.2) 1 2 3))])
  (test-case "call with call in procedure name"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((,tmp.1 (if (< x.1 x.2) x.1 x.2))) (bitwise-and ,tmp.1 x.1))))
                            (define L.fun1.2 (lambda () L.fun1.1))
                            (let ((,tmp.2 (call L.fun1.2))) (call ,tmp.2 1 2 3))))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        x.1))
            (call L.fun1.1 (call L.fun1.1 1 2 3) 2 3))])
  (test-case "call with call in argument"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) x.1))
                            (let ((,tmp.1 (call L.fun1.1 1 2 3))) (call L.fun1.1 ,tmp.1 2 3))))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        x.1))
            (call L.fun1.1 (call L.fun1.1 1 2 3) 2 3))])
  (test-case "call with let in argument"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) x.1))
                            (let ((,tmp.1 (call L.fun1.1 1 2 3))) (call L.fun1.1 ,tmp.1 2 3))))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        x.1))
            (call (let ([x.2 L.fun1.1]) x.2) 1 2 3))])
  (test-case "call with let in procedure name"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) x.1))
                            (let ((,tmp.1 (let ((x.2 L.fun1.1)) x.2))) (call ,tmp.1 1 2 3))))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        x.1))
            (call (if (< 2 3) L.fun1.1 L.fun1.2) 1 2 3))])
  (test-case "call with if in proc name"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) x.1))
                            (let ((,tmp.1 (if (< 2 3) L.fun1.1 L.fun1.2))) (call ,tmp.1 1 2 3))))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        x.1))
            (call L.fun1.1 (if (true) 1 2) 2 3))])
  (test-case "call with if in argument"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) x.1))
                            (let ((,tmp.1 (if (true) 1 2))) (call L.fun1.1 ,tmp.1 2 3))))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        x.1))
            (call L.fun1.1 (if (< (call L.fun1.1 1 2 3) (call L.fun1.1 3 2 1)) 1 2) 2 3))])
  (test-case "remove relop call"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) x.1))
                            (let ((,tmp.1
                                    (if (let ((,tmp.2 (call L.fun1.1 3 2 1)))
                                        (let ((,tmp.3 (call L.fun1.1 1 2 3))) (< ,tmp.3 ,tmp.2)))
                                    1
                                    2)))
                                (call L.fun1.1 ,tmp.1 2 3))))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        x.1))
            (call L.fun1.1 (if (< (bitwise-and 1 2) (bitwise-ior 3 4)) 1 2) 2 3))])
  (test-case "remove relop binop"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) x.1))
                            (let ((,tmp.1
                                    (if (let ((,tmp.2 (bitwise-ior 3 4)))
                                        (let ((,tmp.3 (bitwise-and 1 2))) (< ,tmp.3 ,tmp.2)))
                                    1
                                    2)))
                                (call L.fun1.1 ,tmp.1 2 3))))))

(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        x.1))
            (call L.fun1.1 (if (< (let ([x.2 3]) x.2) (let ([x.3 (call L.fun1.1 3 2 4)]) x.3)) 1 2) 2 3))])
  (test-case "remove relop let"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) x.1))
                            (let ((,tmp.1
                                    (if (let ((,tmp.2 (let ((x.3 (call L.fun1.1 3 2 4))) x.3)))
                                        (let ((,tmp.3 (let ((x.2 3)) x.2))) (< ,tmp.3 ,tmp.2)))
                                    1
                                    2)))
                                (call L.fun1.1 ,tmp.1 2 3))))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        x.1))
            (call L.fun1.1 (if (< (if (true) 1 2) (if (true) 3 4)) 1 2) 2 3))])
  (test-case "remove relop if"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1 (lambda (x.1 y.1 z.1) x.1))
                            (let ((,tmp.1
                                    (if (let ((,tmp.2 (if (true) 3 4)))
                                        (let ((,tmp.3 (if (true) 1 2))) (< ,tmp.3 ,tmp.2)))
                                    1
                                    2)))
                                (call L.fun1.1 ,tmp.1 2 3))))))



(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (let ([x.2 (call L.fun1.1 (call L.fun1.1 1 2 3) 2 3)]) 
                            (let ([x.3 (call (call L.fun1.2) (call L.fun1.1 1 2 3) 2 (bitwise-and (call L.fun1.1 1 2 3) (call L.fun1.1 3 2 1)))])
                                (arithmetic-shift-right (arithmetic-shift-right x.3 2) (bitwise-ior x.2 x.3))))))
                (define L.fun1.2
                    (lambda ()
                        L.fun1.1))
            (call L.fun1.1 (if (< (if (true) 1 2) (if (true) 3 4)) 1 2) 2 3))])
  (test-case "complex case 1"
             (check-match (remove-complex-opera* x)
                           `(module
                            (define L.fun1.1
                                (lambda (x.1 y.1 z.1)
                                (let ((x.2
                                        (let ((,tmp.1 (call L.fun1.1 1 2 3))) (call L.fun1.1 ,tmp.1 2 3))))
                                    (let ((x.3
                                        (let ((,tmp.2 (call L.fun1.2)))
                                            (let ((,tmp.3 (call L.fun1.1 1 2 3)))
                                            (let ((,tmp.4
                                                    (let ((,tmp.5 (call L.fun1.1 1 2 3)))
                                                        (let ((,tmp.6 (call L.fun1.1 3 2 1)))
                                                        (bitwise-and ,tmp.5 ,tmp.6)))))
                                                (call ,tmp.2 ,tmp.3 2 ,tmp.4))))))
                                    (let ((,tmp.7 (arithmetic-shift-right x.3 2)))
                                        (let ((,tmp.8 (bitwise-ior x.2 x.3)))
                                        (arithmetic-shift-right ,tmp.7 ,tmp.8)))))))
                            (define L.fun1.2 (lambda () L.fun1.1))
                            (let ((,tmp.9
                                    (if (let ((,tmp.10 (if (true) 3 4)))
                                        (let ((,tmp.11 (if (true) 1 2))) (< ,tmp.11 ,tmp.10)))
                                    1
                                    2)))
                                (call L.fun1.1 ,tmp.9 2 3))))))


(let ([x '(module
                (define L.fun1.1
                    (lambda (x.1 y.1 z.1)
                        (if (< (let ([x.2 (bitwise-and (call L.fun1.1 1 2 3) (bitwise-and (call L.fun1.1 3 3 3) 3))]
                                     [x.3 (bitwise-ior (arithmetic-shift-right 2 (bitwise-xor x.1 y.1)) z.1)])
                                    (bitwise-ior x.2 x.3))
                               (if (if (true) (< (call L.fun1.1 1 2 3) (bitwise-and 1 2)) (> (arithmetic-shift-right 1 (bitwise-and 1 2)) 3))
                                    (bitwise-and (call L.fun1.1 3 2 1) (call L.fun1.2))
                                    (bitwise-ior (bitwise-and 1 2) (+ 3 4))))
                            (bitwise-and 1 2)
                            (bitwise-ior 3 4))))
                (define L.fun1.2
                    (lambda ()
                        L.fun1.1))
            (call L.fun1.1 (if (< (if (true) 1 2) (if (true) 3 4)) 1 2) 2 3))])
  (test-case "complex case 2"
             (check-match (remove-complex-opera* x)
                           `(module
                                (define L.fun1.1
                                    (lambda (x.1 y.1 z.1)
                                    (if (let ((,tmp.1
                                                (if (if (true)
                                                    (let ((,tmp.2 (bitwise-and 1 2)))
                                                        (let ((,tmp.3 (call L.fun1.1 1 2 3))) (< ,tmp.3 ,tmp.2)))
                                                    (let ((,tmp.4
                                                            (let ((,tmp.5 (bitwise-and 1 2)))
                                                                (arithmetic-shift-right 1 ,tmp.5))))
                                                        (> ,tmp.4 3)))
                                                (let ((,tmp.6 (call L.fun1.1 3 2 1)))
                                                    (let ((,tmp.7 (call L.fun1.2))) (bitwise-and ,tmp.6 ,tmp.7)))
                                                (let ((,tmp.8 (bitwise-and 1 2)))
                                                    (let ((,tmp.9 (+ 3 4))) (bitwise-ior ,tmp.8 ,tmp.9))))))
                                            (let ((,tmp.10
                                                (let ((x.2
                                                        (let ((,tmp.11 (call L.fun1.1 1 2 3)))
                                                            (let ((,tmp.12
                                                                (let ((,tmp.13 (call L.fun1.1 3 3 3)))
                                                                    (bitwise-and ,tmp.13 3))))
                                                            (bitwise-and ,tmp.11 ,tmp.12))))
                                                        (x.3
                                                        (let ((,tmp.14
                                                                (let ((,tmp.15 (bitwise-xor x.1 y.1)))
                                                                (arithmetic-shift-right 2 ,tmp.15))))
                                                            (bitwise-ior ,tmp.14 z.1))))
                                                    (bitwise-ior x.2 x.3))))
                                            (< ,tmp.10 ,tmp.1)))
                                        (bitwise-and 1 2)
                                        (bitwise-ior 3 4))))
                                (define L.fun1.2 (lambda () L.fun1.1))
                                (let ((,tmp.16
                                        (if (let ((,tmp.17 (if (true) 3 4)))
                                            (let ((,tmp.18 (if (true) 1 2))) (< ,tmp.18 ,tmp.17)))
                                        1
                                        2)))
                                    (call L.fun1.1 ,tmp.16 2 3))))))

;; todo add general case
;'(module
;                (define fun1
;                    (lambda (x y)
;                        (if (< x y)
;                            (call fun1 y x)
;                            x)))
;            (let ([z fun1] [u 1] [q 2]) (call z u q)))