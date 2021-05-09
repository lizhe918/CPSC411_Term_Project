#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/2c-run-time
 cpsc411/test-suite/utils
 racket/match
 rackunit)


(require "../component/check-val.rkt")

; input: values-lang-v6
; output: values-lang-v6 / exception
; purpose: Check if the program is a valid values-lang-v6 language. 


(test-case
    "relop with unbound name"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    (if (< x p) (+ 1 1) x))) 
            (call add1 5 6))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

(test-case
    "relop with bounded name, but wrong type"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    (let ([q add1]) (if (< q 1) x y)))) 
            (call add1 5 6))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

(test-case
    "binop with unbound name"
(let ([x '(module 
            (define add1 (lambda (x y) (+ q 1))) 
            (call add1 5 6))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))


(test-case
    "binop with bounded name, but wrong type"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    (let ([q add1]) (+ q 1)))) 
            (call add1 5 6))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))


(test-case
    "call with wrong number of arguments in value"
(let ([x '(module 
            (define add1 (lambda (x y) (+ x 1))) 
            (let ([x (call add1 5)]) x))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

(test-case
    "call with wrong number of arguments in value "
(let ([x '(module 
            (define add1 (lambda (x y) (let ([x (call add1 5)]) x))) 
            (let ([x (call add1 5 6)]) x))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))


(test-case
    "call with wrong number of arguments in value (if pred value value)"
(let ([x '(module 
            (define add1 (lambda (x y) (+ x 1))) 
            (if (true) (call add1 5) (call add1 5 6)))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

(test-case
    "call with right number of arguments in value, but wrong type"
(let ([x '(module 
            (define add1 (lambda (x y) (+ x 1))) 
            (let ([x (call add1 5 add1)]) x))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

(test-case
    "call with right number of arguments in value, but unbound"
(let ([x '(module 
            (define add1 (lambda (x y) (+ x 1))) 
            (let ([x (call add1 1 q)]) x))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

(test-case
    "call with undefined proc name in value"
(let ([x '(module 
            (define add1 (lambda (x y) (+ x 1))) 
            (let ([x (call add2 5 6)]) x))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))
        
(test-case
    "call with wrong type proc (not name?) in value"
(let ([x '(module 
            (define add1 (lambda (x y) (+ x 1))) 
            (let ([x (call 1 5 6)]) x))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; pass
(test-case
    "calls are allowed in value"
(let ([x '(module 
            (define add1 (lambda (x y) (+ x 1))) 
            (let ([x (call add1 5 6)]) x))])
        (check-equal? (check-values-lang x) '(module 
            (define add1 (lambda (x y) (+ x 1))) 
            (let ([x (call add1 5 6)]) x)))))


(test-case
    "calls with name"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    (+ x 1))) 
            (call add1 5 6))])
        (check-equal? (check-values-lang x) '(module (define add1 (lambda (x y) (+ x 1))) (call add1 5 6)))))


(test-case
    "calls in let in pred"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    (if (let ([x (call add1 1 2)]) (< x 1))
                        x
                        y))) 
            (call add1 2 3))])
        (check-equal? (check-values-lang x) '(module
                    (define add1 (lambda (x y) (if (let ((x (call add1 1 2))) (< x 1)) x y)))
                    (call add1 2 3)))))

(test-case
    "calls in nested if"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    (if (if (true) 
                            (let ([x (call add1 x y)]) (< x 0))
                            (let ([y (call add1 y x)]) (!= x y)))
                        x
                        y))) 
            (let ([x (call add1 5 6)]) x))])
        (check-equal? (check-values-lang x)
        '(module
            (define add1
                (lambda (x y)
                (if (if (true)
                        (let ((x (call add1 x y))) (< x 0))
                        (let ((y (call add1 y x))) (!= x y)))
                    x
                    y)))
            (let ((x (call add1 5 6))) x)))))


(test-case
    "calls in nested let"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    (+ x 1))) 
            (let ([x (call add1 1 2)]) (let ([x x] [y 2]) (call add1 x y))))])
        (check-equal? (check-values-lang x) '(module
                    (define add1 (lambda (x y) (+ x 1)))
                    (let ((x (call add1 1 2))) (let ((x x) (y 2)) (call add1 x y)))))))

(test-case
    "calls in nested let in pred"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    (if (let ([x (call add1 x y)]) (let ([x x]) (< x 2)))
                        x
                        y))) 
            (let ([x (call add1 5 6)]) x))])
        (check-equal? (check-values-lang x) '(module
                        (define add1
                            (lambda (x y) (if (let ((x (call add1 x y))) (let ((x x)) (< x 2))) x y)))
                        (let ((x (call add1 5 6))) x)))))


(test-case
    "calls proc bounded to another name in values"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    (let ([q 2] [z add1]) (let ([x y] [y (call z x y)]) (+ x y))))) 
            (let ([x (call add1 5 6)]) x))])
        (check-equal? (check-values-lang x) '(module
                        (define add1
                            (lambda (x y)
                            (let ((q 2) (z add1)) (let ((x y) (y (call z x y))) (+ x y)))))
                        (let ((x (call add1 5 6))) x)))))

(test-case
    "calls proc in values inside a proc"
(let ([x '(module 
            (define add1 
                (lambda (x y) 
                    (let ([x add1] [y x]) (call x y 2)))) 
            (let ([x (call add1 5 6)]) x))])
        (check-equal? (check-values-lang x) '(module
                        (define add1 (lambda (x y) (let ((x add1) (y x)) (call x y 2))))
                        (let ((x (call add1 5 6))) x)))))

;; ok
(test-case
    "General case"
(let ([x '(module
                (define addup1
                    (lambda (x y z)
                        (let ([x (call addup2 x y z)] [y (call addup2 x y z)] [z z]) (+ z y))))
                (define addup2
                    (lambda (x y z)
                        (let ([addup1 x] [x addup1] [z (call addup1 x y z)]) (call x addup1 y z))))
                (define addup3
                    (lambda (x y z)
                        (let ([x addup1] [y addup2]) (let ([x x] [q y]) (call q 1 2 3)))))
            (call addup3 5 6 7))])
        (check-equal? (check-values-lang x)
            '(module
                (define addup1
                    (lambda (x y z)
                        (let ([x (call addup2 x y z)] [y (call addup2 x y z)] [z z]) (+ z y))))
                (define addup2
                    (lambda (x y z)
                        (let ([addup1 x] [x addup1] [z (call addup1 x y z)]) (call x addup1 y z))))
                (define addup3
                    (lambda (x y z)
                        (let ([x addup1] [y addup2]) (let ([x x] [q y]) (call q 1 2 3)))))
            (call addup3 5 6 7)))))
;; ---------------------- old cases -------------------------

(test-case
    "interrogator does not allow duplicated parameters"
(let ([x '(module (define add1 (lambda (x x) (+ x 1))) (call add1 5 6))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; procedure name appears in wrong position
(test-case
    "procedure name appears in wrong position"
     (let ([x '(module
                  (define add1
                    (lambda (x)
                        (+ x 1)))
                (let ([x add1]) x))])
       (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; procedure name appears in wrong position
(test-case
    "procedure name appears in wrong position"
     (let ([x '(module
                  (define add1
                    (lambda (x)
                        (+ x 1)))
                add1)])
       (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; procedure name appears in wrong position
(test-case
    "procedure name appears in wrong position"
     (let ([x '(module
                  (define add1
                    (lambda (x)
                        (+ x 1)))
                (+ 1 add1))])
       (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; procedure name appears in wrong position
(test-case
    "procedure name appears in wrong position"
     (let ([x '(module
                  (define add1
                    (lambda (x)
                        (+ x 1)))
                (if (true) add1 x))])
       (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; procedure name appears in wrong position
(test-case
    "procedure name appears in wrong position"
     (let ([x '(module
                  (define add1
                    (lambda (x)
                        (+ x 1)))
                (let ([x 5]) add1))])
       (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; Type error
;; argument is not in the range of int64
(test-case
    "argument is not in the range of int64"
     (let ([x '(module
                  (define add1
                    (lambda (x)
                        (+ x 1)))
                (call add1 ,(+ (max-int 64) 1)))])
       (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; argument is not valid integer
(test-case
    "argument is not in the range of int64"
     (let ([x '(module
                  (define add1
                    (lambda (x)
                        (+ x 1)))
                (call add1 "failed test"))])
       (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; call an undefined procedure
(test-case
    "call an undefined procedure"
    (let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (call add2 5))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; call a procedure but with more arguments
(test-case
    "call a procedure but with more arguments"
    (let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (call add1 5 6))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; call a procedure but with less arguments
(test-case
    "call a procedure but with less arguments"
    (let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (call add1))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; relop with non-int triv
(test-case
    "relop with non-int triv"
(let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (if (< 1 (let ([x 5]) x)) 1 2))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; relop with non-int triv
(test-case
    "relop with non-int triv"
(let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (if (< 1 (if (true) 1 2)) 1 2))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; relop with non-int triv
(test-case
    "relop with non-int triv"
(let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (if (< 1 (+ 1 (let ([x 5]) x))) 1 2))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; relop with non-int triv
(test-case
    "relop with non-int triv"
(let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (if (< 1 (+ 1 (if (true) 1 2))) 1 2))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

(test-case
    "relop with non-int triv"
(let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (if (< 1 (call add1 1)) 1 2))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))  

;; binop with non-int triv
(test-case
    "binop with non-int triv"
(let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (+ 1 (let ([x 5]) x)))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; binop with non-int triv
(test-case
    "binop with non-int triv"
(let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (+ 1 (if (true) 1 2)))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; binop with nested non-int triv
(test-case
    "binop with non-int triv"
(let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (+ 1 (+ 1 (let ([x 5]) 5))))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; binop with nested non-int triv
(test-case
    "binop with non-int triv"
(let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (+ 1 (+ 1 (if (true) 1 2))))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; binop with nested non-int triv
(test-case
    "binop with non-int triv"
(let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (+ 1 (call add1 5)))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; nested binop operation is not allowed
(test-case
    "nested binop operation is not allowed"
(let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (+ (+ (+ 1 2) (+ 3 4)) (* (+ 1 (+ 1 2)) (+ 1 2))))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))
;; if statement with wrong predicate
(test-case
    "if statement with wrong predicate"
(let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (if (5) 1 2))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; if statement with wrong predicate
(test-case
    "if statement with wrong predicate"
(let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (if (not (5)) 1 2))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; if statement with wrong predicate
(test-case
    "if statement with wrong predicate"
(let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (if ((let ([x 5]) 5)) 1 2))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))

;; if statement with wrong predicate
(test-case
    "if statement with wrong predicate"
(let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (if ((if (true) 5 6)) 1 2))])
        (check-exn exn:fail? (lambda () (check-values-lang x)))))


;; --------------------moderate correct cases---------------------------
;; without defined procedure -- ok
(test-case
    "without defined procedure"
(let ([x '(module
            (let ([x 5]) x))])
        (check-equal? (check-values-lang x) 
            '(module
            (let ([x 5]) x)))))
;; call a procedure
(test-case
    "relop with nested (binop triv triv)"
(let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (call add1 5))])
        (check-equal? (check-values-lang x) 
            '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (call add1 5)))))

;; proc name is shadowed in a let expression --ok
(test-case
    "proc name is shadowed in a let expression --ok"
(let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (let ([add1 5]) add1))])
        (check-equal? (check-values-lang x) 
            '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (let ([add1 5]) add1)))))

;; proc is bound to a lexical indentifier in a let expression -- ok
(test-case
     "proc is bound to a lexical indentifier in a let expression -- ok"
(let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (let ([x add1]) (call add1 5)))])
        (check-equal? (check-values-lang x) '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (let ([x add1]) (call add1 5))))))

;; allow duplicated procedures -- ok
(test-case
    "allow duplicated procedures"
(let ([x '(module
                (define add1
                    (lambda (x)
                        (+ x 1)))
                (define add1
                    (lambda (x)
                        (+ x 1)))
            (call add1 5))])
        (check-equal? (check-values-lang x) 
                '(module
                        (define add1 (lambda (x) (+ x 1)))
                        (define add1 (lambda (x) (+ x 1)))
                        (call add1 5)))))