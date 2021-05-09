#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/2c-run-time
 racket/match
 rackunit)

(require "../component/expose.rkt")


; input: nested-asm-lang-v6
; output: block-pred-lang-v6
; purpose: Check if the output of expose-basic-blocks matches the output of interrogator 

;; reuse previous test cases, change binop only
(let ([x `(module
              (define L.addup.1 
                (begin (set! (rbp - 0) 1)
                      (set! (rbp - 8) 2)
                      (set! (rbp - 0) (bitwise-and (rbp - 0) (rbp - 8)))
                      (jump L.addup.2)))
               (jump L.addup.1))])
     (test-case "Simple case jump inside the proc declaration -- and"
     (check-match (expose-basic-blocks x)
                  `(module
                  (define ,L.__main.1 (jump ,L.addup.1))
                  (define ,L.addup.1
                    (begin
                      (set! (,rbp - 0) 1)
                      (set! (,rbp - 8) 2)
                      (set! (,rbp - 0) (bitwise-and (,rbp - 0) (,rbp - 8)))
                      (jump L.addup.2))))
                      (and (frame-base-pointer-register? rbp) (label? L.__main.1) (label? L.addup.1)))))

(let ([x `(module
              (define L.addup.1 
                (begin (set! (rbp - 0) 1)
                      (set! (rbp - 8) 2)
                      (set! (rbp - 0) (bitwise-ior (rbp - 0) (rbp - 8)))
                      (if (< r15 (rbp - 0)) (jump (rbp - 0)) (jump rax))))
               (jump L.addup.1))])
     (test-case "Simple case jump in if inside the proc declaration -- ior"
     (check-match (expose-basic-blocks x)
                  `(module
                    (define ,L.__main.1 (jump L.addup.1))
                    (define ,L.addup.1
                      (begin
                        (set! (,rbp - 0) 1)
                        (set! (,rbp - 8) 2)
                        (set! (,rbp - 0) (bitwise-ior (,rbp - 0) (,rbp - 8)))
                        (if (< r15 (,rbp - 0)) (jump ,L.__nested.2) (jump ,L.__nested.3))))
                    (define ,L.__nested.2 (jump (,rbp - 0)))
                    (define ,L.__nested.3 (jump rax)))
                    (and (frame-base-pointer-register? rbp)
                    (label? L.__main.1) (label? L.addup.1)
                    (label? L.__nested.2) (label? L.__nested.3)))))

(let ([x `(module
              (define L.addup.1
                (begin (return-point L.addup.1 
                        (begin (set! (rbp - 8) L.addup.1) (jump (rbp - 8))))
                      (begin (set! (rbp - 0) 2)
                        (begin (set! (rbp - 0) (bitwise-xor (rbp - 0) 5))))
                      (jump rax)))
                (jump L.addup.1))])
     (test-case "Simple case with nested begins - nested begin in effect -- xor"
     (check-match (expose-basic-blocks x)
                 `(module
                  (define ,L.__main.1 (jump ,L.addup.1))
                  (define ,L.addup.1 (begin (set! (,rbp - 8) ,L.addup.1) (jump (,rbp - 8))))
                  (define ,L.addup.1
                    (begin (set! (,rbp - 0) 2) (set! (,rbp - 0) (bitwise-xor (,rbp - 0) 5)) (jump rax))))
                  (and (frame-base-pointer-register? rbp)
                  (label? L.__main.1) (label? L.addup.1)))))

(let ([x `(module
              (define L.addup.1
                (begin (set! (rbp - 0) 1)
                      (begin (set! (rbp - 8) 2)
                      (set! (rbp - 0) (arithmetic-shift-right (rbp - 0) 12))
                      (return-point L.addup.1 
                        (begin (set! (rbp - 8) L.addup.1) (jump (rbp - 8)))))
                      (jump L.addup.1)))
               (jump L.addup.1))])
     (test-case "Simple case with nested begins - effect in begin -- sar"
     (check-match (expose-basic-blocks x)
                 `(module
                  (define ,L.__main.1 (jump L.addup.1))
                  (define ,L.addup.1
                    (begin
                      (set! (,rbp - 0) 1)
                      (set! (,rbp - 8) 2)
                      (set! (,rbp - 0) (arithmetic-shift-right (,rbp - 0) 12))
                      (set! (,rbp - 8) ,L.addup.1)
                      (jump (,rbp - 8))))
                  (define ,L.addup.1 (jump ,L.addup.1)))
                  (and (frame-base-pointer-register? rbp)
                  (label? L.__main.1) (label? L.addup.1)))))

;; -------------------------------------------------------------------------------


(let ([x `(module
              (define L.addup.1 
                (begin (set! (rbp - 16) 1)
                      (return-point L.addup.2 (jump (rbp - 8)))
                      (jump L.addup.1)))
               (jump L.addup.1))])
     (test-case "Simple case with return-point"
     (check-match (expose-basic-blocks x)
                  `(module
                    (define ,L.__main.1 (jump ,L.addup.1))
                    (define ,L.addup.1 (begin (set! (,rbp - 16) 1) (jump (,rbp - 8))))
                    (define ,L.addup.2 (jump ,L.addup.1)))
                    (and (frame-base-pointer-register? rbp)
                    (label? L.__main.1) (label? L.addup.1)
                    (label? L.addup.2)))))

(let ([x `(module
              (define L.addup.1 
                (if (begin (set! (rbp - 8) 7) (set! rax (rbp - 8)) (= rax (rbp - 8))) (jump rax) (jump L.addup.1)))
               (jump L.addup.1))])
     (test-case "Simple case with if begin"
     (check-match (expose-basic-blocks x)
                  `(module
                    (define ,L.__main.1 (jump ,L.addup.1))
                    (define ,L.addup.1
                      (begin
                        (set! (,rbp - 8) 7)
                        (set! rax (,rbp - 8))
                        (if (= rax (,rbp - 8)) (jump ,L.__nested.2) (jump ,L.__nested.3))))
                    (define ,L.__nested.2 (jump rax))
                    (define ,L.__nested.3 (jump ,L.addup.1)))
                    (and (frame-base-pointer-register? rbp)
                    (label? L.__main.1) (label? L.addup.1)
                    (label? L.__nested.2) (label? L.__nested.3)))))


(let ([x `(module
              (define L.addup.1 
                (begin (begin (begin (begin (jump rax))))))
               (begin (set! (rbp - 8) L.addup.1) (jump (rbp - 8))))])
     (test-case "Simple case with nested begins - no effect, begin in the tail"
     (check-match (expose-basic-blocks x)
                 `(module
                  (define ,L.__main.1 (begin (set! (,rbp - 8) ,L.addup.1) (jump (,rbp - 8))))
                  (define ,L.addup.1 (begin (begin (begin (begin (jump rax)))))))
                  (and (frame-base-pointer-register? rbp)
                    (label? L.__main.1) (label? L.addup.1)))))




(let ([x `(module
              (define L.addup.1
                (begin (set! (rbp - 0) 1)
                      (begin (set! (rbp - 8) 2)
                        (if (true) (set! (rbp - 8) 10) (return-point L.addup.2 (jump L.addup.2))))
                      (jump (rbp - 16))))
               (jump L.addup.1))])
     (test-case "Simple case with begin nesting if in effect position"
     (check-match (expose-basic-blocks x)
                 `(module
                  (define ,L.__main.1 (jump ,L.addup.1))
                  (define ,L.addup.1
                    (begin
                      (set! (,rbp - 0) 1)
                      (set! (,rbp - 8) 2)
                      (if (true) (jump ,L.tmp.2) (jump ,L.tmp.3))))
                  (define ,L.tmp.2 (begin (set! (,rbp - 8) 10) (jump ,L.tmp.4)))
                  (define ,L.tmp.3 (jump ,L.addup.2))
                  (define ,L.addup.2 (jump ,L.tmp.4))
                  (define ,L.tmp.4 (jump (,rbp - 16))))
                  (and (frame-base-pointer-register? rbp)
                  (label? L.__main.1) (label? L.addup.1) (label? L.addup.2)
                  (label? L.tmp.2) (label? L.tmp.3) (label? L.tmp.4)))))

(let ([x `(module
              (define L.addup.1
                (begin 
                (begin 
                  (begin (if (< (rbp - 8) 5) 
                             (jump (rbp - 0)) 
                             (begin (return-point L.addup.1 (jump L.addup.1)) (jump rcx)))))))
              (define L.addup.2
                (begin (set! (rbp - 0) 1)
                      (begin (set! (rbp - 8) 2)
                        (if (true) (set! rax 10) (set! (rbp - 8) (- (rbp - 8) 10))))
                      (jump L.addup.1)))   
               (if (true) (jump L.addup.1) (jump L.addup.2)))])
     (test-case "Simple case with begin nesting if in tail position"
     (check-match (expose-basic-blocks x)
                 `(module
                  (define ,L.__main.3 (if (true) (jump ,L.__nested.1) (jump ,L.__nested.2)))
                  (define ,L.__nested.1 (jump ,L.addup.1))
                  (define ,L.__nested.2 (jump ,L.addup.2))
                  (define ,L.addup.2
                    (begin
                      (set! (,rbp - 0) 1)
                      (set! (,rbp - 8) 2)
                      (if (true) (jump ,L.tmp.6) (jump ,L.tmp.7))))
                  (define ,L.tmp.6 (begin (set! rax 10) (jump ,L.tmp.8)))
                  (define ,L.tmp.7 (begin (set! (,rbp - 8) (- (,rbp - 8) 10)) (jump ,L.tmp.8)))
                  (define ,L.tmp.8 (jump ,L.addup.1))
                  (define ,L.addup.1
                  (begin 
                    (begin
                      (begin
                    (if (< (,rbp - 8) 5) (jump ,L.__nested.4) (jump ,L.__nested.5))))))
                  (define ,L.__nested.4 (jump (,rbp - 0)))
                  (define ,L.__nested.5 (jump ,L.addup.1))
                  (define ,L.addup.1 (jump rcx)))
                  (frame-base-pointer-register? rbp))))


(let ([x `(module
              (define L.addup.1
                (begin (set! (rbp - 0) 1)
                      (begin
                          (if (true) 
                              (begin (return-point L.addup.1 (if (< (rbp - 8) 5) 
                                                            (jump (rbp - 0)) 
                                                            (begin (return-point L.addup.1 (jump L.addup.1)) (jump rcx)))) (jump L.addup.2))
                              (begin (set! rcx rax) (if (false) (jump (rbp - 8)) (jump (rbp - 16))))) 
                              )))
               (jump L.addup.1))])
     (test-case "Simple case with begin nesting nested if in tail position"
     (check-match (expose-basic-blocks x)
                 `(module
                  (define ,L.__main.1 (jump ,L.addup.1))
                  (define ,L.addup.1
                    (begin
                      (set! (,rbp - 0) 1)
                      (if (true) (jump ,L.__nested.2) (jump ,L.__nested.3))))
                  (define ,L.__nested.2
                    (if (< (,rbp - 8) 5) (jump ,L.__nested.4) (jump ,L.__nested.5)))
                  (define ,L.__nested.4 (jump (,rbp - 0)))
                  (define ,L.__nested.5 (jump ,L.addup.1))
                  (define ,L.addup.1 (jump rcx))
                  (define ,L.addup.1 (jump ,L.addup.2))
                  (define ,L.__nested.3
                    (begin
                      (set! rcx rax)
                      (if (false) (jump ,L.__nested.6) (jump ,L.__nested.7))))
                  (define ,L.__nested.6 (jump (,rbp - 8)))
                  (define ,L.__nested.7 (jump (,rbp - 16))))
                      (frame-base-pointer-register? rbp))))


(let ([x `(module
              (define L.addup.1
                (begin (set! (rbp - 0) 1)
                      (begin (if (true) 
                          (if (false) (set! rax 10) (set! rbx (rbp - 8)))
                          (return-point L.addup.1 (if (< (rbp - 8) 5) 
                                                            (jump (rbp - 0)) 
                                                            (begin (return-point L.addup.1 (jump L.addup.1)) (jump rcx)))))
                       (jump rax))))
               (jump L.addup.1))])
     (test-case "Simple case with begin nesting nested if in effect position"
     (check-match (expose-basic-blocks x)
                 `(module
                  (define ,L.__main.1 (jump ,L.addup.1))
                  (define ,L.addup.1
                    (begin (set! (,rbp - 0) 1) (if (true) (jump ,L.tmp.2) (jump ,L.tmp.3))))
                  (define ,L.tmp.2 (if (false) (jump ,L.tmp.5) (jump ,L.tmp.6)))
                  (define ,L.tmp.5 (begin (set! rax 10) (jump ,L.tmp.7)))
                  (define ,L.tmp.6 (begin (set! rbx (,rbp - 8)) (jump ,L.tmp.7)))
                  (define ,L.tmp.7 (jump ,L.tmp.4))
                  (define ,L.tmp.3 (if (< (,rbp - 8) 5) (jump ,L.__nested.8) (jump ,L.__nested.9)))
                  (define ,L.__nested.8 (jump (,rbp - 0)))
                  (define ,L.__nested.9 (jump ,L.addup.1))
                  (define ,L.addup.1 (jump rcx))
                  (define ,L.addup.1 (jump ,L.tmp.4))
                  (define ,L.tmp.4 (jump rax)))
                  (frame-base-pointer-register? rbp))))


(let ([x `(module
              (define L.addup.1
                (begin (set! (rbp - 8) 1)
                      (begin (if (true) 
                          (if (false) 
                              (return-point L.addup.1 (begin (set! rax (+ rax 8)) (set! (rbp - 0) 8) (jump L.addup.2)))
                              (set! rbx (rbp - 8)))
                          (set! rcx (+ rcx 5)))
                       (jump (rbp - 8)))))
                (jump L.addup.1))])
     (test-case "Simple case with if predicate in tail position"
     (check-match (expose-basic-blocks x)
                 `(module
                  (define ,L.__main.1 (jump ,L.addup.1))
                  (define ,L.addup.1
                    (begin (set! (,rbp - 8) 1) (if (true) (jump ,L.tmp.2) (jump ,L.tmp.3))))
                  (define ,L.tmp.2 (if (false) (jump ,L.tmp.5) (jump ,L.tmp.6)))
                  (define ,L.tmp.5
                    (begin (set! rax (+ rax 8)) (set! (,rbp - 0) 8) (jump ,L.addup.2)))
                  (define ,L.addup.1 (jump ,L.tmp.7))
                  (define ,L.tmp.6 (begin (set! rbx (,rbp - 8)) (jump ,L.tmp.7)))
                  (define ,L.tmp.7 (jump ,L.tmp.4))
                  (define ,L.tmp.3 (begin (set! rcx (+ rcx 5)) (jump ,L.tmp.4)))
                  (define ,L.tmp.4 (jump (,rbp - 8))))
                  (frame-base-pointer-register? rbp))))


(let ([x `(module
            (define L.addup.1
                (if (if (if (true) (false) (true)) (if (false) (true) (true)) (if (true) (false) (true)))
                  (jump (rbp - 8))
                  (begin (jump (rbp - 0)))))
              (if (true) (jump (rbp - 16)) (jump L.addup.1)))])
     (test-case "Simple case with nested if predicate in tail position pred"
     (check-match (expose-basic-blocks x)
                 `(module
                  (define ,L.__main.3 (if (true) (jump ,L.__nested.1) (jump ,L.__nested.2)))
                  (define ,L.__nested.1 (jump (,rbp - 16)))
                  (define ,L.__nested.2 (jump ,L.addup.1))
                  (define ,L.addup.1 (if (true) (jump ,L.tmp.8) (jump ,L.tmp.9)))
                  (define ,L.tmp.8 (if (false) (jump ,L.tmp.6) (jump ,L.tmp.7)))
                  (define ,L.tmp.9 (if (true) (jump ,L.tmp.6) (jump ,L.tmp.7)))
                  (define ,L.tmp.6 (if (false) (jump ,L.tmp.10) (jump ,L.tmp.11)))
                  (define ,L.tmp.10 (if (true) (jump ,L.__nested.4) (jump ,L.__nested.5)))
                  (define ,L.tmp.11 (if (true) (jump ,L.__nested.4) (jump ,L.__nested.5)))
                  (define ,L.tmp.7 (if (true) (jump ,L.tmp.12) (jump ,L.tmp.13)))
                  (define ,L.tmp.12 (if (false) (jump ,L.__nested.4) (jump ,L.__nested.5)))
                  (define ,L.tmp.13 (if (true) (jump ,L.__nested.4) (jump ,L.__nested.5)))
                  (define ,L.__nested.4 (jump (,rbp - 8)))
                  (define ,L.__nested.5 (begin (jump (,rbp - 0)))))
                  (frame-base-pointer-register? rbp))))


(let ([x `(module
            (define L.addup.1
                (if (not (begin (return-point L.rp.1 (begin (return-point L.rp.2 (jump L.addup.1)) (jump L.addup.2))) (< (rbp - 8) 5)))
                  (jump (rbp - 8))
                  (begin (jump (rbp - 16)))))
            (if (false) (jump L.addup.1) (if (true) (jump L.addup.1) (begin (return-point L.rp.3 (jump L.addup.1)) (jump L.addup.2)))))])
     (test-case "Simple case with not predicate in tail position"
     (check-match (expose-basic-blocks x)
                 `(module
                  (define ,L.__main.5 (if (false) (jump ,L.__nested.1) (jump ,L.__nested.2)))
                  (define ,L.__nested.1 (jump ,L.addup.1))
                  (define ,L.__nested.2 (if (true) (jump ,L.__nested.3) (jump ,L.__nested.4)))
                  (define ,L.__nested.3 (jump ,L.addup.1))
                  (define ,L.__nested.4 (jump ,L.addup.1))
                  (define ,L.rp.3 (jump ,L.addup.2))
                  (define ,L.addup.1 (jump ,L.addup.1))
                  (define ,L.rp.2 (jump ,L.addup.2))
                  (define ,L.rp.1 (if (< (,rbp - 8) 5) (jump ,L.__nested.7) (jump ,L.__nested.6)))
                  (define ,L.__nested.6 (jump (,rbp - 8)))
                  (define ,L.__nested.7 (begin (jump (,rbp - 16)))))
                  (frame-base-pointer-register? rbp))))


;; General cases
;; ok
(let ([x `(module
          (define L.swap.1
            (begin
              (set! (rbp - 16) r15)
              (set! r15 (rbp - 0))
              (set! r14 (rbp - 8))
              (if (< r14 r15)
                (begin (set! rax r15) (jump (rbp - 16)))
                (begin
                  (begin
                    (set! rbp (- rbp 24))
                    (return-point L.rp.1
                      (begin
                        (set! (rbp - 32) r15)
                        (set! (rbp - 24) r14)
                        (set! r15 L.rp.1)
                        (jump L.swap.1)))
                    (set! rbp (+ rbp 24)))
                  (set! r15 rax)
                  (set! rax r15)
                  (jump (rbp - 16))))))
          (begin
            (set! r15 r15)
            (set! (rbp - 8) 2)
            (set! (rbp - 0) 1)
            (set! r15 r15)
            (jump L.swap.1)))])
     (test-case "case from textbook"
     (check-match (expose-basic-blocks x)
                 `(module
                  (define ,L.__main.1
                    (begin
                      (set! r15 r15)
                      (set! (,rbp - 8) 2)
                      (set! (,rbp - 0) 1)
                      (set! r15 r15)
                      (jump ,L.swap.1)))
                  (define ,L.swap.1
                    (begin
                      (set! (,rbp - 16) r15)
                      (set! r15 (,rbp - 0))
                      (set! r14 (,rbp - 8))
                      (if (< r14 r15) (jump ,L.__nested.2) (jump ,L.__nested.3))))
                  (define ,L.__nested.2 (begin (set! rax r15) (jump (,rbp - 16))))
                  (define ,L.__nested.3
                    (begin
                      (set! ,rbp (- ,rbp 24))
                      (set! (,rbp - 32) r15)
                      (set! (,rbp - 24) r14)
                      (set! r15 ,L.rp.1)
                      (jump ,L.swap.1)))
                  (define ,L.rp.1
                    (begin
                      (set! ,rbp (+ ,rbp 24))
                      (set! r15 rax)
                      (set! rax r15)
                      (jump (,rbp - 16)))))
                  (frame-base-pointer-register? rbp))))



