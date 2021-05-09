#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/2c-run-time
 racket/match
 rackunit)

(require "../component/impl-fvar.rkt")

; input: nested-asm-lang-fvar-v8
; output: nested-asm-lang-v8
; purpose: Check if the output of implement-fvars matches the output of interrogator 


(test-case
    "error case"
(let ([x '(module
                (define L.not.56.8
                (begin
                    (set! r15 r15)
                    (set! r14 rdi)
                    (set! r14 rsi)
                    (if (!= r14 6)
                    (begin (set! rax 6) (jump r15))
                    (begin (set! rax 14) (jump r15)))))
                (define L.procedure?.57.7
                (begin
                    (set! r15 r15)
                    (set! r14 rdi)
                    (set! r14 rsi)
                    (if (begin
                        (begin (set! r14 r14) (set! r14 (bitwise-and r14 7)))
                        (= r14 2))
                    (begin (set! rax 14) (jump r15))
                    (begin (set! rax 6) (jump r15)))))
                (begin
                (set! r15 r15)
                (begin (set! r14 r12) (set! r12 (+ r12 16)))
                (set! r14 r14)
                (set! r14 (+ r14 2))
                (mset! r14 -2 L.procedure?.57.7)
                (mset! r14 6 8)
                (set! r14 r14)
                (begin (set! r13 r12) (set! r12 (+ r12 16)))
                (set! r13 r13)
                (set! r13 (+ r13 2))
                (mset! r13 -2 L.not.56.8)
                (mset! r13 6 8)
                (set! r13 r13)
                (set! rsi r13)
                (set! rdi r14)
                (set! r15 r15)
                (jump L.procedure?.57.7)))])
        (check-equal? (implement-fvars x)
                    '(module
                    (define L.not.56.8
                    (begin
                        (set! r15 r15)
                        (set! r14 rdi)
                        (set! r14 rsi)
                        (if (!= r14 6)
                        (begin (set! rax 6) (jump r15))
                        (begin (set! rax 14) (jump r15)))))
                    (define L.procedure?.57.7
                    (begin
                        (set! r15 r15)
                        (set! r14 rdi)
                        (set! r14 rsi)
                        (if (begin
                            (begin (set! r14 r14) (set! r14 (bitwise-and r14 7)))
                            (= r14 2))
                        (begin (set! rax 14) (jump r15))
                        (begin (set! rax 6) (jump r15)))))
                    (begin
                    (set! r15 r15)
                    (begin (set! r14 r12) (set! r12 (+ r12 16)))
                    (set! r14 r14)
                    (set! r14 (+ r14 2))
                    (mset! r14 -2 L.procedure?.57.7)
                    (mset! r14 6 8)
                    (set! r14 r14)
                    (begin (set! r13 r12) (set! r12 (+ r12 16)))
                    (set! r13 r13)
                    (set! r13 (+ r13 2))
                    (mset! r13 -2 L.not.56.8)
                    (mset! r13 6 8)
                    (set! r13 r13)
                    (set! rsi r13)
                    (set! rdi r14)
                    (set! r15 r15)
                    (jump L.procedure?.57.7))))))

(test-case
    "implement fvars in mset! in loc"
(let ([x '(module 
            (define L.addup.1 
                (begin
                    (mset! fv0 rax 5)
                    (jump L.end.1))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1 (begin (mset! (,rbp - 0) rax 5) (jump L.end.1)))
                    (jump L.addup.1)))))


(test-case
    "implement fvars in mset! in index"
(let ([x '(module 
            (define L.addup.1 
                (begin
                    (mset! rax fv3 5)
                    (jump L.end.1))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1 (begin (mset! rax (,rbp - 24) 5) (jump L.end.1)))
                    (jump L.addup.1)))))


(test-case
    "implement fvars in mset! in triv"
(let ([x '(module 
            (define L.addup.1 
                (begin
                    (mset! rax rbx fv4)
                    (jump L.end.1))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1 (begin (mset! rax rbx (,rbp - 32)) (jump L.end.1)))
                    (jump L.addup.1)))))


(test-case
    "implement fvars in mset! with different rbp pos"
(let ([x '(module 
            (define L.addup.1 
                (begin
                    (set! rbp (- rbp 24))
                    (mset! fv0 fv2 fv4)
                    (jump L.end.1))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1
                        (begin
                        (set! ,rbp (- ,rbp 24))
                        (mset! (,rbp - -24) (,rbp - -8) (,rbp - 8))
                        (jump L.end.1)))
                    (jump L.addup.1)))))



(test-case
    "implement fvars in mref in loc1"
(let ([x '(module 
            (define L.addup.1 
                (begin
                    (set! fv0 (mref rax 5))
                    (jump L.end.1))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1 (begin (set! (,rbp - 0) (mref rax 5)) (jump L.end.1)))
                    (jump L.addup.1)))))

(test-case
    "implement fvars in mref in loc2"
(let ([x '(module 
            (define L.addup.1 
                (begin
                    (set! rax (mref fv2 5))
                    (jump L.end.1))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1 (begin (set! rax (mref (,rbp - 16) 5)) (jump L.end.1)))
                    (jump L.addup.1)))))

(test-case
    "implement fvars in mref in index"
(let ([x '(module 
            (define L.addup.1 
                (begin
                    (set! rax (mref rbx fv3))
                    (jump L.end.1))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1 (begin (set! rax (mref rbx (,rbp - 24))) (jump L.end.1)))
                    (jump L.addup.1)))))


(test-case
    "implement fvars in mref with different rbp pos"
(let ([x '(module 
            (define L.addup.1 
                (begin
                    (set! rbp (- rbp 8))
                    (set! fv0 (mref fv5 fv3))
                    (jump L.end.1))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1
                        (begin
                        (set! ,rbp (- ,rbp 8))
                        (set! (,rbp - -8) (mref (,rbp - 32) (,rbp - 16)))
                        (jump L.end.1)))
                    (jump L.addup.1)))))

(test-case
    "fbp is not modified by mops 1"
(let ([x '(module 
            (define L.addup.1 
                (begin
                    (set! rbx 8)
                    (set! fv3 8)
                    (set! rbp (mref rbx fv3))
                    (set! fv3 3)
                    (jump L.end.1))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1
                        (begin
                        (set! rbx 8)
                        (set! (,rbp - 24) 8)
                        (set! ,rbp (mref rbx (,rbp - 24)))
                        (set! (,rbp - 24) 3)
                        (jump L.end.1)))
                    (jump L.addup.1)))))


(test-case
    "fbp is not modified by mops 2"
(let ([x '(module 
            (define L.addup.1 
                (begin
                    (set! rbx 8)
                    (set! fv3 8)
                    (mset! rbp  rbx fv3)
                    (set! fv3 3)
                    (jump L.end.1))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1
                        (begin
                        (set! rbx 8)
                        (set! (,rbp - 24) 8)
                        (mset! ,rbp rbx (,rbp - 24))
                        (set! (,rbp - 24) 3)
                        (jump L.end.1)))
                    (jump L.addup.1)))))

(test-case
    "General case"
(let ([x '(module 
            (define L.addup.1 
                (if (begin (set! rbp (- rbp 16)) (set! fv2 (mref fv0 0)) (< fv2 0))
                    (begin
                        (return-point L.rp.8 (begin (set! rbp (+ rbp 8)) (mset! fv2 rax 8) (jump L.addup.1)))
                        (begin
                            (set! rbp (mref rax 8))
                            (if (true)
                                (mset! fv0 8 8)
                                (set! fv2 (mref fv3 8))))
                        (jump L.end.1))
                    (begin
                        (jump L.end.2))))
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1
                        (if (begin
                            (set! ,rbp (- ,rbp 16))
                            (set! (,rbp - 0) (mref (,rbp - -16) 0))
                            (< (,rbp - 0) 0))
                        (begin
                            (return-point
                            L.rp.8
                            (begin
                            (set! ,rbp (+ ,rbp 8))
                            (mset! (,rbp - 24) rax 8)
                            (jump L.addup.1)))
                            (begin
                            (set! ,rbp (mref rax 8))
                            (if (true)
                                (mset! (,rbp - 0) 8 8)
                                (set! (,rbp - 16) (mref (,rbp - 24) 8))))
                            (jump L.end.1))
                        (begin (jump L.end.2))))
                    (jump L.addup.1)))))

;; Unavailable with following operations on rbp
;;                        (set! rbp (+ rbp fv2))
;;                        (mset! fv0 8 8)
;;                        (set! rbp (bitwise-and rbp 8))
;;                        (mset! fv0 8 8)
;;                        (set! rbp (+ rbp rax))
;;                        (mset! fv0 8 8)
;; ------------------------------------------ old cases ---------------------------------------------
(test-case
    "implement fvars in jump inside result-point begin -- bitwise-ior"
(let ([x '(module 
            (define L.addup.1 
                (begin (set! fv0 (bitwise-ior fv0 5)) (return-point L.addup.1 (begin (set! rax fv0) (jump fv2))) (jump fv0))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1
                        (begin
                        (set! (,rbp - 0) (bitwise-ior (,rbp - 0) 5))
                        (return-point L.addup.1 (begin (set! rax (,rbp - 0)) (jump (,rbp - 16))))
                        (jump (,rbp - 0))))
                    (jump L.addup.1))
                    (frame-base-pointer-register? rbp))))


(test-case
    "implement fvars in jump inside result-point if -- bitwise-xor"
(let ([x '(module 
            (define L.addup.1 
                (begin (set! fv0 (bitwise-xor fv0 5)) (return-point L.addup.1 (if (begin (set! fv2 5) (!= fv2 5)) (jump fv1) (jump fv2))) (jump fv0))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1
                        (begin
                        (set! (,rbp - 0) (bitwise-xor (,rbp - 0) 5))
                        (return-point
                        L.addup.1
                        (if (begin (set! (,rbp - 16) 5) (!= (,rbp - 16) 5))
                            (jump (,rbp - 8))
                            (jump (,rbp - 16))))
                        (jump (,rbp - 0))))
                    (jump L.addup.1))
                    (frame-base-pointer-register? rbp))))

(test-case
    "implement fvars in jump inside result-point if -- bitwise-and"
(let ([x '(module 
            (define L.addup.1 
                (begin (set! fv0 (bitwise-and fv0 5)) (return-point L.addup.1 (if (begin (set! fv2 5) (!= fv2 5)) (jump fv1) (jump fv2))) (jump fv0))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1
                        (begin
                        (set! (,rbp - 0) (bitwise-and (,rbp - 0) 5))
                        (return-point
                        L.addup.1
                        (if (begin (set! (,rbp - 16) 5) (!= (,rbp - 16) 5))
                            (jump (,rbp - 8))
                            (jump (,rbp - 16))))
                        (jump (,rbp - 0))))
                    (jump L.addup.1))
                    (frame-base-pointer-register? rbp))))

(test-case
    "implement fvars in jump inside result-point if -- arithmetic-shift-right"
(let ([x '(module 
            (define L.addup.1 
                (begin (set! fv0 (arithmetic-shift-right fv0 5)) (return-point L.addup.1 (if (begin (set! fv2 5) (!= fv2 5)) (jump fv1) (jump fv2))) (jump fv0))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1
                        (begin
                        (set! (,rbp - 0) (arithmetic-shift-right (,rbp - 0) 5))
                        (return-point
                        L.addup.1
                        (if (begin (set! (,rbp - 16) 5) (!= (,rbp - 16) 5))
                            (jump (,rbp - 8))
                            (jump (,rbp - 16))))
                        (jump (,rbp - 0))))
                    (jump L.addup.1))
                    (frame-base-pointer-register? rbp))))


;; -----------------------------------------------------------------
(test-case
    "implement fvars in jump"
(let ([x '(module 
            (define L.addup.1 
                (jump fv0)) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module (define L.addup.1 (jump (,rbp - 0))) (jump L.addup.1))
                    (frame-base-pointer-register? rbp))))



(test-case
    "implement fvars in jump inside begin"
(let ([x '(module 
            (define L.addup.1 
                (begin (jump fv2))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module (define L.addup.1 (begin (jump (,rbp - 16)))) (jump L.addup.1))
                    (frame-base-pointer-register? rbp))))

(test-case
    "implement fvars in jump inside if"
(let ([x '(module 
            (define L.addup.1 
                (if (true) (jump fv2) (jump fv3))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1 (if (true) (jump (,rbp - 16)) (jump (,rbp - 24))))
                    (jump L.addup.1))
                    (frame-base-pointer-register? rbp))))

(test-case
    "implement fvars in jump inside result-point jump"
(let ([x '(module 
            (define L.addup.1 
                (begin (set! fv0 5) (return-point L.addup.1 (jump fv1)) (jump fv0))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1
                        (begin
                        (set! (,rbp - 0) 5)
                        (return-point L.addup.1 (jump (,rbp - 8)))
                        (jump (,rbp - 0))))
                    (jump L.addup.1))
                    (frame-base-pointer-register? rbp))))

                    


(test-case
    "implement fvars based on current fbp"
(let ([x '(module 
            (define L.addup.1 
                (begin
                (set! rbp (+ rbp 24))
                (set! rbp (- rbp 24))
                (jump fv3))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1
                        (begin (set! ,rbp (+ ,rbp 24)) (set! ,rbp (- ,rbp 24)) (jump (,rbp - 24))))
                    (jump L.addup.1))
                    (frame-base-pointer-register? rbp))))

(test-case
    "implement fvars based on current fbp"
(let ([x '(module 
            (define L.addup.1 
                (begin
                (set! fv3 5)
                (set! rbp (- rbp 24))
                (return-point L.rp.8
                    (jump fv3))
                (jump fv3))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1
                        (begin
                        (set! (,rbp - 24) 5)
                        (set! ,rbp (- ,rbp 24))
                        (return-point L.rp.8 (jump (,rbp - 0)))
                        (jump (,rbp - 0))))
                    (jump L.addup.1))
                    (frame-base-pointer-register? rbp))))

(test-case
    "implement fvars based on current fbp"
(let ([x '(module 
            (define L.addup.1 
                (begin
                (set! rbp (- rbp 24))
                (set! fv3 5)
                (set! rbp (+ rbp 8))
                (jump fv0))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1
                        (begin
                        (set! ,rbp (- ,rbp 24))
                        (set! (,rbp - 0) 5)
                        (set! ,rbp (+ ,rbp 8))
                        (jump (,rbp - -16))))
                    (jump L.addup.1))
                    (frame-base-pointer-register? rbp))))

(test-case
    "implement fvars based on current fbp"
(let ([x '(module 
            (define L.addup.1 
                (begin
                (set! rbp (- rbp 24))
                (return-point L.rp.8
                    (begin
                    (set! rbp (+ rbp 16))
                    (set! rdi fv3)
                    (set! rbp (+ rbp 8))
                    (jump fv3)))
                (jump fv3))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1
                        (begin
                        (set! ,rbp (- ,rbp 24))
                        (return-point
                        L.rp.8
                        (begin
                            (set! ,rbp (+ ,rbp 16))
                            (set! rdi (,rbp - 16))
                            (set! ,rbp (+ ,rbp 8))
                            (jump (,rbp - 24))))
                        (jump (,rbp - 0))))
                    (jump L.addup.1))
                    (frame-base-pointer-register? rbp))))

(test-case
    "implement fvars based on current fbp"
(let ([x '(module 
            (define L.addup.1 
                (begin
                (set! rbp (- rbp 24))
                (return-point L.rp.8
                   (if (not (true))
                        (begin (set! rbp (+ rbp 8)) (jump fv0))
                        (begin (set! rbp (+ rbp 16)) (jump fv0))))
                (jump fv3))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1
                        (begin
                        (set! ,rbp (- ,rbp 24))
                        (return-point
                        L.rp.8
                        (if (not (true))
                            (begin (set! ,rbp (+ ,rbp 8)) (jump (,rbp - -16)))
                            (begin (set! ,rbp (+ ,rbp 16)) (jump (,rbp - -8)))))
                        (jump (,rbp - 0))))
                    (jump L.addup.1))
                    (frame-base-pointer-register? rbp))))


;; small nested cases
(test-case
    "implement fvars based on current fbp"
(let ([x '(module 
            (define L.addup.1 
                (begin
                (set! rbp (- rbp 24))
                (return-point L.rp.8
                   (if (begin (set! rbp (+ rbp 8)) (set! fv3 5) (set! rbp (- rbp 8)) (set! fv2 4) (= fv2 4))
                        (begin (set! fv0 5) (begin (set! rbp (+ rbp 8))) (jump fv0))
                        (begin (set! rbp (+ rbp 16)) (jump fv0))))
                (jump fv3))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1
                        (begin
                        (set! ,rbp (- ,rbp 24))
                        (return-point
                        L.rp.8
                        (if (begin
                                (set! ,rbp (+ ,rbp 8))
                                (set! (,rbp - 8) 5)
                                (set! ,rbp (- ,rbp 8))
                                (set! (,rbp - -8) 4)
                                (= (,rbp - -8) 4))
                            (begin
                            (set! (,rbp - -24) 5)
                            (begin (set! ,rbp (+ ,rbp 8)))
                            (jump (,rbp - -16)))
                            (begin (set! ,rbp (+ ,rbp 16)) (jump (,rbp - -8)))))
                        (jump (,rbp - 0))))
                    (jump L.addup.1))
                    (frame-base-pointer-register? rbp))))

(test-case
    "implement fvars based on current fbp"
(let ([x '(module 
            (define L.addup.1 
                (begin
                (set! rbp (- rbp 24))
                (return-point L.rp.8
                   (begin (set! rbp (- rbp 8)) (if (< fv0 5)
                                                    (begin (set! rbp (+ rbp 8)) (jump fv0))
                                                    (begin (set! rbp (- rbp 8)) (set! rbp (- rbp 16)) (jump fv1)))))
                (jump fv3))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1
                        (begin
                        (set! ,rbp (- ,rbp 24))
                        (return-point
                        L.rp.8
                        (begin
                            (set! ,rbp (- ,rbp 8))
                            (if (< (,rbp - -32) 5)
                            (begin (set! ,rbp (+ ,rbp 8)) (jump (,rbp - -24)))
                            (begin
                                (set! ,rbp (- ,rbp 8))
                                (set! ,rbp (- ,rbp 16))
                                (jump (,rbp - -48))))))
                        (jump (,rbp - 0))))
                    (jump L.addup.1))
                    (frame-base-pointer-register? rbp))))



;; complex general cases

(test-case
    "implement fvars based on current fbp"
(let ([x '(module 
            (define L.addup.1 
                (begin
                (set! rbp (- rbp 24))
                (return-point L.rp.8
                   (if (not (begin (set! rbp (+ rbp 8)) (set! fv3 5) (set! rbp (- rbp 8)) (set! fv2 4) (= fv2 4)))
                        (if (< fv0 2) 
                            (begin (set! rbp (- rbp 0)) (jump fv0)) 
                            (begin (set! rbp (+ rbp 8)) (return-point L.addup.1 (jump fv1)) (jump fv2)))
                        (if (begin (set! rbp (- rbp 16)) (< fv0 fv2))
                            (jump fv0)
                            (begin (set! rbp (+ rbp 16)) (return-point L.addup.1 (jump fv2)) (jump fv2)))))
                (jump fv3))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1
                        (begin
                        (set! ,rbp (- ,rbp 24))
                        (return-point
                        L.rp.8
                        (if (not
                                (begin
                                (set! ,rbp (+ ,rbp 8))
                                (set! (,rbp - 8) 5)
                                (set! ,rbp (- ,rbp 8))
                                (set! (,rbp - -8) 4)
                                (= (,rbp - -8) 4)))
                            (if (< (,rbp - -24) 2)
                            (begin (set! ,rbp (- ,rbp 0)) (jump (,rbp - -24)))
                            (begin
                                (set! ,rbp (+ ,rbp 8))
                                (return-point L.addup.1 (jump (,rbp - -8)))
                                (jump (,rbp - 0))))
                            (if (begin (set! ,rbp (- ,rbp 16)) (< (,rbp - -40) (,rbp - -24)))
                            (jump (,rbp - -24))
                            (begin
                                (set! ,rbp (+ ,rbp 16))
                                (return-point L.addup.1 (jump (,rbp - 8)))
                                (jump (,rbp - 8))))))
                        (jump (,rbp - 0))))
                    (jump L.addup.1))
                    (frame-base-pointer-register? rbp))))


(test-case
    "implement fvars based on current fbp"
(let ([x '(module 
            (define L.addup.1 
                (begin
                (set! rbp (- rbp 24))
                (return-point L.rp.8
                   (begin (set! rbp (- rbp 8)) (if (< fv0 5)
                                                    (begin (set! rbp (+ rbp 8)) (jump fv0))
                                                    (begin (set! rbp (- rbp 8)) (set! rbp (- rbp 16)) (jump fv1)))))
                (jump fv3))) 
            (jump L.addup.1))])
        (check-match (implement-fvars x)
                    `(module
                    (define L.addup.1
                        (begin
                        (set! ,rbp (- ,rbp 24))
                        (return-point
                        L.rp.8
                        (begin
                            (set! ,rbp (- ,rbp 8))
                            (if (< (,rbp - -32) 5)
                            (begin (set! ,rbp (+ ,rbp 8)) (jump (,rbp - -24)))
                            (begin
                                (set! ,rbp (- ,rbp 8))
                                (set! ,rbp (- ,rbp 16))
                                (jump (,rbp - -48))))))
                        (jump (,rbp - 0))))
                    (jump L.addup.1))
                    (frame-base-pointer-register? rbp))))

(test-case
    "implement fvars in jump"
(let ([x '(module (define L.id1.37 (begin (set! r14 rdi) (set! r15 r15) (set! rax r14) (jump r15))) (define L.id2.38 (begin (set! r14 rdi) (set! r15 r15) (set! rax r14) (jump r15))) (begin (set! r15 r15) (if (true) (begin (set! r14 L.id1.37)) (begin (set! r14 L.id2.38))) (set! rdi 5) (set! r15 r15) (jump r14)))])
        (check-equal? (implement-fvars x)       
                    `(module
                    (define L.id1.37
                        (begin (set! r14 rdi) (set! r15 r15) (set! rax r14) (jump r15)))
                    (define L.id2.38
                        (begin (set! r14 rdi) (set! r15 r15) (set! rax r14) (jump r15)))
                    (begin
                        (set! r15 r15)
                        (if (true) (begin (set! r14 L.id1.37)) (begin (set! r14 L.id2.38)))
                        (set! rdi 5)
                        (set! r15 r15)
                        (jump r14))))))
