#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/2c-run-time
 racket/match
 rackunit)

(require "../component/replace.rkt")

; input: asm-pred-lang-v8/assignment
; output: nested-asm-lang-v8
; purpose: Check if the output of replace-locations matches the output of interrogator 


(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15))))
                 (begin (mset! x.1 8 8) (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace aloc in mset!"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.1 (begin (mset! r15 8 8) (jump r15)))
                (begin (jump L.addup.1))))))


(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15) (x.2 fv0))))
                 (begin (mset! x.2 x.1 8) (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace index in mset!"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.1 (begin (mset! fv0 r15 8) (jump r15)))
                (begin (jump L.addup.1))))))



(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15) (x.2 fv0) (x.3 rcx))))
                 (begin (mset! x.2 x.1 x.3) (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace triv in mset!"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.1 (begin (mset! fv0 r15 rcx) (jump r15)))
                (begin (jump L.addup.1))))))


(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15))))
                 (begin (set! x.1 (mref rax 8)) (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace loc1 in mref"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.1 (begin (set! r15 (mref rax 8)) (jump r15)))
                (begin (jump L.addup.1))))))


(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15))))
                 (begin (set! rax (mref x.1 8)) (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace loc2 in mref"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.1 (begin (set! rax (mref r15 8)) (jump r15)))
                (begin (jump L.addup.1))))))



(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15))))
                 (begin (set! rax (mref rbx x.1)) (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace index in mref"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.1 (begin (set! rax (mref rbx r15)) (jump r15)))
                (begin (jump L.addup.1))))))


(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15) (x.2 r14) (x.3 r13))))
                 (if (begin (mset! x.1 x.2 x.3) (set! x.2 (mref x.3 x.1)) (< x.1 x.2))
                      (jump L.end.1)
                      (jump L.end.2)))
            (begin (jump L.addup.1)))])
    (test-case " Replace in effect in begin as pred"
    (check-equal? (replace-locations x)
                `(module
                  (define L.addup.1
                    (if (begin (mset! r15 r14 r13) (set! r14 (mref r13 r15)) (< r15 r14))
                      (jump L.end.1)
                      (jump L.end.2)))
                  (begin (jump L.addup.1))))))


(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15) (x.2 r15) (x.3 r14))))
                 (begin 
                    (begin
                      (mset! x.1 x.2 x.3)
                      (set! x.2 (mref x.3 x.1))) 
                    (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace in effect in begin as effect"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.1
                  (begin (begin (mset! r15 r15 r14) (set! r15 (mref r14 r15))) (jump r15)))
                (begin (jump L.addup.1))))))


(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15) (x.2 r15) (x.3 r14))))
                 (begin 
                    (if (true) (mset! x.1 x.2 x.3) (set! x.2 (mref x.3 x.1)))
                    (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace in effect in if as effect"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.1
                  (begin
                    (if (true) (mset! r15 r15 r14) (set! r15 (mref r14 r15)))
                    (jump r15)))
                (begin (jump L.addup.1))))))

(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15) (x.2 fv2) (x.3 r14))))
                 (begin (return-point L.rp.1 (begin (mset! x.1 x.2 x.3) (set! x.2 (mref x.3 x.1)) (jump x.1))) (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace in effect in return-point"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.1
                  (begin
                    (return-point
                    L.rp.1
                    (begin (mset! r15 fv2 r14) (set! fv2 (mref r14 r15)) (jump r15)))
                    (jump r15)))
                (begin (jump L.addup.1))))))


(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15) (x.2 fv2) (x.3 fv3))))
                 (if (true) 
                      (if (false) (begin (mset! x.1 x.2 x.3) (jump x.1)) (begin (set! x.2 (mref x.3 x.1)) (jump x.2)))
                      (if (not (true)) (begin (set! x.2 (mref x.3 x.1)) (jump x.2)) (jump L.end.1))))
            (begin (jump L.addup.1)))])
    (test-case " Replace in effect in if"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.1
                  (if (true)
                    (if (false)
                      (begin (mset! r15 fv2 fv3) (jump r15))
                      (begin (set! fv2 (mref fv3 r15)) (jump fv2)))
                    (if (not (true))
                      (begin (set! fv2 (mref fv3 r15)) (jump fv2))
                      (jump L.end.1))))
                (begin (jump L.addup.1))))))

;; --------------------------------------------- old cases --------------------------------------------------------
(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15) (x.2 r14) (x.3 fv0))))
                 (begin (set! x.1 L.addup.2) (return-point L.rp.1 (begin (set! x.2 5) (set! x.3 (bitwise-and x.3 x.2)) (jump x.2))) (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace alocs in begin-statement inside return-point -- bitwise-and"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.1
                  (begin
                    (set! r15 L.addup.2)
                    (return-point
                    L.rp.1
                    (begin (set! r14 5) (set! fv0 (bitwise-and fv0 r14)) (jump r14)))
                    (jump r15)))
                (begin (jump L.addup.1))))))


(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15) (x.2 r14) (x.3 fv0))))
                 (begin (set! x.1 L.addup.2) (return-point L.rp.1 (begin (set! x.2 5) (set! x.3 (bitwise-ior x.3 x.2)) (jump x.2))) (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace alocs in begin-statement inside return-point -- bitwise-ior"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.1
                  (begin
                    (set! r15 L.addup.2)
                    (return-point
                    L.rp.1
                    (begin (set! r14 5) (set! fv0 (bitwise-ior fv0 r14)) (jump r14)))
                    (jump r15)))
                (begin (jump L.addup.1))))))



(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15) (x.2 r14) (x.3 fv0))))
                 (begin (set! x.1 L.addup.2) (return-point L.rp.1 (begin (set! x.2 5) (set! x.3 (bitwise-xor x.3 x.2)) (jump x.2))) (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace alocs in begin-statement inside return-point -- bitwise-ior"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.1
                  (begin
                    (set! r15 L.addup.2)
                    (return-point
                    L.rp.1
                    (begin (set! r14 5) (set! fv0 (bitwise-xor fv0 r14)) (jump r14)))
                    (jump r15)))
                (begin (jump L.addup.1))))))



(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15) (x.2 r14) (x.3 fv0))))
                 (begin (set! x.1 L.addup.2) (return-point L.rp.1 (begin (set! x.2 5) (set! x.3 (arithmetic-shift-right x.3 x.2)) (jump x.2))) (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace alocs in begin-statement inside return-point -- bitwise-ior"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.1
                  (begin
                    (set! r15 L.addup.2)
                    (return-point
                    L.rp.1
                    (begin (set! r14 5) (set! fv0 (arithmetic-shift-right fv0 r14)) (jump r14)))
                    (jump r15)))
                (begin (jump L.addup.1))))))

;; -----------------------------------------------------------------------------
(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ()))
                 (jump L.addup.1))
            (begin (jump L.addup.1)))])
    (test-case "Nothing to replace"
    (check-equal? (replace-locations x)
                `(module (define L.addup.1 (jump L.addup.1)) (begin (jump L.addup.1))))))


(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15))))
                 (begin (set! x.1 L.addup.2) (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace aloc in trg"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.1 (begin (set! r15 L.addup.2) (jump r15)))
                (begin (jump L.addup.1))))))



(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15) (x.2 r14) (x.3 r15))))
                 (begin (set! x.1 L.addup.2) (set! x.2 5) (set! x.3 2) (jump x.1 x.2 x.3)))
            (begin (jump L.addup.1)))])
    (test-case " Drop arguments in jump"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.1
                  (begin (set! r15 L.addup.2) (set! r14 5) (set! r15 2) (jump r15)))
                (begin (jump L.addup.1))))))


(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15) (x.2 r14) (x.3 fv0))))
                 (begin (set! x.1 L.addup.2) (return-point L.rp.1 (jump x.1 x.2 x.3)) (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace alocs in return-point"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.1
                  (begin (set! r15 L.addup.2) (return-point L.rp.1 (jump r15)) (jump r15)))
                (begin (jump L.addup.1))))))


(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15) (x.2 r14) (x.3 fv0))))
                 (begin (set! x.1 L.addup.2) (return-point L.rp.1 (if (< x.2 x.3) (jump x.1) (jump x.2))) (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace alocs in if-statement inside return-point"
    (check-equal? (replace-locations x)
                `(module
                  (define L.addup.1
                    (begin
                      (set! r15 L.addup.2)
                      (return-point L.rp.1 (if (< r14 fv0) (jump r15) (jump r14)))
                      (jump r15)))
                  (begin (jump L.addup.1))))))




(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15) (x.2 r14) (x.3 fv0))))
                 (begin (set! x.1 L.addup.2) (return-point L.rp.1 (begin (set! x.2 5) (set! x.3 (- x.3 x.2)) (jump x.2))) (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace alocs in begin-statement inside return-point"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.1
                  (begin
                    (set! r15 L.addup.2)
                    (return-point
                    L.rp.1
                    (begin (set! r14 5) (set! fv0 (- fv0 r14)) (jump r14)))
                    (jump r15)))
                (begin (jump L.addup.1))))))


(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15) (x.2 r14) (x.3 fv0))))
                 (begin (set! x.1 L.addup.2) (return-point L.rp.1 (begin (set! x.2 5) (begin (set! x.3 (- x.3 x.2)) (jump x.2)))) (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace alocs in nested begin-statement inside return-point"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.1
                  (begin
                    (set! r15 L.addup.2)
                    (return-point
                    L.rp.1
                    (begin (set! r14 5) (begin (set! fv0 (- fv0 r14)) (jump r14))))
                    (jump r15)))
                (begin (jump L.addup.1))))))


(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15) (x.2 r14) (x.3 fv0))))
                 (begin (set! x.1 L.addup.2) 
                        (return-point L.rp.1 (if (if (< x.2 x.3)
                                                      (begin (set! x.2 (- x.2 x.3)) (< x.2 x.3))
                                                      (begin (set! x.3 (- x.3 x.2)) (> x.3 x.2))) 
                                                  (jump x.2) 
                                                  (begin (set! x.3 (- x.3 x.2)) (jump x.2)))) (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace alocs in nested if-statement inside return-point"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.1
                  (begin
                    (set! r15 L.addup.2)
                    (return-point
                    L.rp.1
                    (if (if (< r14 fv0)
                          (begin (set! r14 (- r14 fv0)) (< r14 fv0))
                          (begin (set! fv0 (- fv0 r14)) (> fv0 r14)))
                      (jump r14)
                      (begin (set! fv0 (- fv0 r14)) (jump r14))))
                    (jump r15)))
                (begin (jump L.addup.1))))))


(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15) (x.2 r14) (x.3 fv0))))
                 (begin (set! x.1 L.addup.2) 
                        (return-point L.rp.1 (begin (begin (if (!= x.2 x.3) (set! x.2 x.3) (set! x.3 (- x.3 5))) (jump x.2)))) (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace alocs in nested begin-if-statement inside return-point"
    (check-equal? (replace-locations x)
                `(module
                  (define L.addup.1
                    (begin
                      (set! r15 L.addup.2)
                      (return-point
                      L.rp.1
                      (begin
                        (begin
                          (if (!= r14 fv0) (set! r14 fv0) (set! fv0 (- fv0 5)))
                          (jump r14))))
                      (jump r15)))
                  (begin (jump L.addup.1))))))


(let ([x `(module
                ((assignment ()))
                (define L.addup.1
                ((assignment ((x.1 r15) (x.2 r14) (x.3 fv0))))
                 (begin (set! x.1 L.addup.2) 
                        (return-point L.rp.1 (if (begin (set! x.2 x.3) (set! x.2 (- x.2 x.3)) (< x.2 x.3)) 
                                                  (begin (set! x.1 (+ x.1 x.3)) (jump x.1 x.2 x.3)) 
                                                  (begin (set! x.3 (- x.3 x.2)) (jump x.2)))) (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Replace alocs in nested if-begin-statement inside return-point"
    (check-equal? (replace-locations x)
                `(module
                  (define L.addup.1
                    (begin
                      (set! r15 L.addup.2)
                      (return-point
                      L.rp.1
                      (if (begin (set! r14 fv0) (set! r14 (- r14 fv0)) (< r14 fv0))
                        (begin (set! r15 (+ r15 fv0)) (jump r15))
                        (begin (set! fv0 (- fv0 r14)) (jump r14))))
                      (jump r15)))
                  (begin (jump L.addup.1))))))



(let ([x `(module
          ((locals (tmp-ra.4))
          (undead-out
            ((tmp-ra.4 rbp)
            (tmp-ra.4 fv1 rbp)
            (tmp-ra.4 fv1 fv0 rbp)
            (fv1 fv0 r15 rbp)
            (fv1 fv0 r15 rbp)))
          (conflicts
            ((tmp-ra.4 (fv0 fv1 rbp))
            (rbp (r15 fv0 fv1 tmp-ra.4))
            (fv1 (r15 fv0 rbp tmp-ra.4))
            (fv0 (r15 rbp fv1 tmp-ra.4))
            (r15 (rbp fv0 fv1))))
          (assignment ((tmp-ra.4 fv2))))
          (define L.swap.1
            ((locals (z.3 y.2 x.1))
            (undead-out
              ((fv0 fv1 tmp-ra.1 rbp)
              (fv1 x.1 tmp-ra.1 rbp)
              (y.2 x.1 tmp-ra.1 rbp)
              ((y.2 x.1 tmp-ra.1 rbp)
                ((tmp-ra.1 rax rbp) (rax rbp))
                (((rax tmp-ra.1 rbp)
                  ((y.2 nfv.3 rbp)
                  (nfv.3 nfv.2 rbp)
                  (nfv.3 nfv.2 r15 rbp)
                  (nfv.3 nfv.2 r15 rbp)))
                (z.3 tmp-ra.1 rbp)
                (tmp-ra.1 rax rbp)
                (rax rbp)))))
            (conflicts
              ((y.2 (rbp tmp-ra.1 x.1))
              (x.1 (y.2 rbp tmp-ra.1 fv1))
              (tmp-ra.1 (y.2 x.1 rbp fv1 fv0 rax z.3 nfv.3))
              (z.3 (rbp tmp-ra.1))
              (nfv.3 (rbp tmp-ra.1 rax))
              (nfv.2 ())
              (rax (rbp tmp-ra.1 nfv.3))
              (rbp (y.2 x.1 tmp-ra.1 rax z.3 nfv.3))
              (fv0 (tmp-ra.1))
              (fv1 (x.1 tmp-ra.1))))
            (assignment
              ((tmp-ra.1 fv2) (nfv.2 fv3) (nfv.3 fv4) (x.1 fv0) (y.2 fv1) (z.3 fv0))))
            (begin
              (set! tmp-ra.1 r15)
              (set! x.1 fv0)
              (set! y.2 fv1)
              (if (< y.2 x.1)
                (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
                (begin
                  (begin
                    (set! rbp (- rbp 24))
                    (return-point
                    L.rp.1
                    (begin
                      (set! nfv.3 x.1)
                      (set! nfv.2 y.2)
                      (set! r15 L.rp.1)
                      (jump L.swap.1 rbp r15 nfv.2 nfv.3)))
                    (set! rbp (+ rbp 24)))
                  (set! z.3 rax)
                  (set! rax z.3)
                  (jump tmp-ra.1 rbp rax)))))
          (begin
            (set! tmp-ra.4 r15)
            (set! fv1 2)
            (set! fv0 1)
            (set! r15 tmp-ra.4)
            (jump L.swap.1 rbp r15 fv0 fv1)))])
    (test-case "General case"
    (check-equal? (replace-locations x)
                `(module
                  (define L.swap.1
                    (begin
                      (set! fv2 r15)
                      (set! fv0 fv0)
                      (set! fv1 fv1)
                      (if (< fv1 fv0)
                        (begin (set! rax fv0) (jump fv2))
                        (begin
                          (begin
                            (set! rbp (- rbp 24))
                            (return-point
                            L.rp.1
                            (begin
                              (set! fv4 fv0)
                              (set! fv3 fv1)
                              (set! r15 L.rp.1)
                              (jump L.swap.1)))
                            (set! rbp (+ rbp 24)))
                          (set! fv0 rax)
                          (set! rax fv0)
                          (jump fv2)))))
                  (begin
                    (set! fv2 r15)
                    (set! fv1 2)
                    (set! fv0 1)
                    (set! r15 fv2)
                    (jump L.swap.1))))))


(let ([x `(module
            ((locals ()) (undead-out ()) (conflicts ()) (assignment ()))
            (define L.addup.2
              ((locals (z.1 y.1 x.3))
              (undead-out
                ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
              (conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1))))
              (assignment
                ((x.1 fv0)
                (nfv.1 fv1)
                (nfv.2 fv2)
                (nfv.3 fv3)
                (x.3 fv1)
                (y.1 fv1)
                (z.1 fv1))))
              (begin
                (set! x.1 1)
                (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (- z.1 x.1)))
                (begin
                  (set! rbp (- rbp 8))
                  (return-point
                  L.addup.1
                  (begin (set! x.3 L.addup.2) (jump x.3 z.1 y.1)))
                  (set! rbp (+ rbp 8)))
                (jump L.addup.2 x.1)))
            (jump L.addup.2))])
    (test-case "General case"
    (check-equal? (replace-locations x)
                `(module
                (define L.addup.2
                  (begin
                    (set! fv0 1)
                    (begin (set! fv1 2) (set! fv1 fv1) (set! fv1 (- fv1 fv0)))
                    (begin
                      (set! rbp (- rbp 8))
                      (return-point L.addup.1 (begin (set! fv1 L.addup.2) (jump fv1)))
                      (set! rbp (+ rbp 8)))
                    (jump L.addup.2)))
                (jump L.addup.2)))))
