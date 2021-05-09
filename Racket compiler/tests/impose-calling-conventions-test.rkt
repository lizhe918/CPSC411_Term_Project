#lang racket
(require
  cpsc411/compiler-lib
  cpsc411/2c-run-time
  cpsc411/test-suite/utils
  racket/match
  rackunit)

(require "../component/impose.rkt")

; input: proc-imp-mf-mf-lang-v8
; output: imp-mf-lang-v8
; purpose: Check if the outputs of impose-calling-conventions matches the desired outputs of the interrogator.


(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (mref x.1 8)))
            (call L.addup.1 1))])
  (test-case "Impose conventions to support mref"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames ()))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! ,tmp-ra.1 ,r15)
                                (begin
                                  (set! x.1 ,rdi)
                                  (begin (set! rax (mref x.1 8)) (jump ,tmp-ra.1 ,rbp ,rax)))))
                            (begin
                              (set! ,tmp-ra.2 ,r15)
                              (begin (set! ,rdi 1) (set! ,r15 ,tmp-ra.2) (jump L.addup.1 ,rbp ,r15 ,rdi)))))))



(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (alloc 8)))
            (call L.addup.1 1))])
  (test-case "Impose conventions to support alloc"
             (check-match (impose-calling-conventions x)
                           `(module
                              ((new-frames ()))
                              (define L.addup.1
                                ((new-frames ()))
                                (begin
                                  (set! ,tmp-ra.1 ,r15)
                                  (begin
                                    (set! x.1 ,rdi)
                                    (begin (set! ,rax (alloc 8)) (jump ,tmp-ra.1 ,rbp ,rax)))))
                              (begin
                                (set! ,tmp-ra.2 ,r15)
                                (begin (set! ,rdi 1) (set! ,r15 ,tmp-ra.2) (jump L.addup.1 ,rbp ,r15 ,rdi)))))))


(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (begin
                          (mset! x.2 x.1 8)
                          5)))
            (call L.addup.1 1))])
  (test-case "Impose conventions to support mset! as effect"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames ()))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! ,tmp-ra.1 ,r15)
                                (begin
                                  (set! x.1 ,rdi)
                                  (begin
                                    (mset! x.2 x.1 8)
                                    (begin (set! ,rax 5) (jump ,tmp-ra.1 ,rbp ,rax))))))
                            (begin
                              (set! ,tmp-ra.2 ,r15)
                              (begin (set! ,rdi 1) (set! ,r15 ,tmp-ra.2) (jump L.addup.1 ,rbp ,r15 ,rdi)))))))


;; ------------------------------------ support composite value

(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (begin
                          (set! x.2 (begin (set! x.3 7) (mref x.1 x.3)))
                          5)))
            (call L.addup.1 1))])
  (test-case "Impose conventions to support begin as value"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames ()))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! ,tmp-ra.1 ,r15)
                                (begin
                                  (set! x.1 ,rdi)
                                  (begin
                                    (set! x.2 (begin (set! x.3 7) (mref x.1 x.3)))
                                    (begin (set! ,rax 5) (jump ,tmp-ra.1 ,rbp ,rax))))))
                            (begin
                              (set! ,tmp-ra.2 ,r15)
                              (begin (set! ,rdi 1) (set! r15 ,tmp-ra.2) (jump L.addup.1 ,rbp ,r15 ,rdi)))))))


(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (begin
                          (set! x.3 7)
                          (set! x.2 (if (true) (alloc 8) (mref x.3 x.1)))
                          5)))
            (call L.addup.1 1))])
  (test-case "Impose conventions to support if as value"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames ()))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! ,tmp-ra.1 ,r15)
                                (begin
                                  (set! x.1 ,rdi)
                                  (begin
                                    (set! x.3 7)
                                    (set! x.2 (if (true) (alloc 8) (mref x.3 x.1)))
                                    (begin (set! ,rax 5) (jump ,tmp-ra.1 ,rbp ,rax))))))
                            (begin
                              (set! ,tmp-ra.2 ,r15)
                              (begin (set! ,rdi 1) (set! r15 ,tmp-ra.2) (jump L.addup.1 ,rbp ,r15 ,rdi)))))))


(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (begin
                          (set! x.2 (begin (set! x.3 7) (begin (alloc x.3))))
                          5)))
            (call L.addup.1 1))])
  (test-case "Impose conventions to support nested begin as value"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames ()))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! ,tmp-ra.1 ,r15)
                                (begin
                                  (set! x.1 ,rdi)
                                  (begin
                                    (set! x.2 (begin (set! x.3 7) (begin (alloc x.3))))
                                    (begin (set! ,rax 5) (jump ,tmp-ra.1 ,rbp ,rax))))))
                            (begin
                              (set! ,tmp-ra.2 ,r15)
                              (begin (set! ,rdi 1) (set! r15 ,tmp-ra.2) (jump L.addup.1 ,rbp ,r15 ,rdi)))))))


(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (begin
                          (set! x.3 8)
                          (set! x.2 (if (true) (if (false) (alloc x.3) (mref x.2 8)) (mref x.3 x.1)))
                          5)))
            (call L.addup.1 1))])
  (test-case "Impose conventions to support nested if as value"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames ()))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! ,tmp-ra.1 ,r15)
                                (begin
                                  (set! x.1 ,rdi)
                                  (begin
                                    (set! x.3 8)
                                    (set! x.2
                                      (if (true) (if (false) (alloc x.3) (mref x.2 8)) (mref x.3 x.1)))
                                    (begin (set! ,rax 5) (jump ,tmp-ra.1 ,rbp ,rax))))))
                            (begin
                              (set! ,tmp-ra.2 ,r15)
                              (begin (set! ,rdi 1) (set! r15 ,tmp-ra.2) (jump L.addup.1 ,rbp ,r15 ,rdi)))))))
;; ------------------------------------ support composite effect


(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (begin
                          (set! x.3 8)
                          (begin (set! x.2 8) (set! x.3 (alloc x.1)) (set! x.4 (mref x.3 8)))
                          (mref x.1 8))))
            (call L.addup.1 1))])
  (test-case "Impose conventions to support begin as effect"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames ()))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! ,tmp-ra.1 ,r15)
                                (begin
                                  (set! x.1 ,rdi)
                                  (begin
                                    (set! x.3 8)
                                    (begin (set! x.2 8) (set! x.3 (alloc x.1)) (set! x.4 (mref x.3 8)))
                                    (begin (set! ,rax (mref x.1 8)) (jump ,tmp-ra.1 ,rbp ,rax))))))
                            (begin
                              (set! ,tmp-ra.2 ,r15)
                              (begin (set! ,rdi 1) (set! r15 ,tmp-ra.2) (jump L.addup.1 ,rbp ,r15 ,rdi)))))))


(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (begin
                          (set! x.3 8)
                          (if (true) (set! x.2 8) (mset! x.1 8 (mref x.2 8)))
                          (alloc x.1))))
            (call L.addup.1 1))])
  (test-case "Impose conventions to support if as effect"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames ()))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! ,tmp-ra.1 ,r15)
                                (begin
                                  (set! x.1 ,rdi)
                                  (begin
                                    (set! x.3 8)
                                    (if (true) (set! x.2 8) (mset! x.1 8 (mref x.2 8)))
                                    (begin (set! ,rax (alloc x.1)) (jump ,tmp-ra.1 ,rbp ,rax))))))
                            (begin
                              (set! ,tmp-ra.2 ,r15)
                              (begin (set! ,rdi 1) (set! r15 ,tmp-ra.2) (jump L.addup.1 ,rbp ,r15 ,rdi)))))))

(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (if (begin (mset! x.2 x.1 (alloc 8)) (true))
                            (alloc x.1)
                            (if (false)
                                (alloc 16)
                                (mref x.2 x.1)))))
            (call L.addup.1 1))])
  (test-case "Impose conventions to support begin as pred"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames ()))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! ,tmp-ra.1 ,r15)
                                (begin
                                  (set! x.1 ,rdi)
                                  (if (begin (mset! x.2 x.1 (alloc 8)) (true))
                                    (begin (set! ,rax (alloc x.1)) (jump ,tmp-ra.1 ,rbp ,rax))
                                    (if (false)
                                      (begin (set! ,rax (alloc 16)) (jump ,tmp-ra.1 ,rbp ,rax))
                                      (begin (set! ,rax (mref x.2 x.1)) (jump ,tmp-ra.1 ,rbp ,rax)))))))
                            (begin
                              (set! ,tmp-ra.2 ,r15)
                              (begin (set! ,rdi 1) (set! r15 ,tmp-ra.2) (jump L.addup.1 ,rbp ,r15 ,rdi)))))))

;; ok
(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (begin
                          (set! x.3 8)
                          (set! x.2 (begin (mset! x.1 8 (begin (set! x.1 2) (alloc 8))) (mref x.1 x.1)))
                          (+ x.1 x.3))))
            (call L.addup.1 1))])
  (test-case "Impose conventions to support begin as value"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames ()))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! ,tmp-ra.1 ,r15)
                                (begin
                                  (set! x.1 ,rdi)
                                  (begin
                                    (set! x.3 8)
                                    (set! x.2
                                      (begin
                                        (mset! x.1 8 (begin (set! x.1 2) (alloc 8)))
                                        (mref x.1 x.1)))
                                    (begin (set! ,rax (+ x.1 x.3)) (jump ,tmp-ra.1 ,rbp ,rax))))))
                            (begin
                              (set! ,tmp-ra.2 ,r15)
                              (begin (set! ,rdi 1) (set! r15 ,tmp-ra.2) (jump L.addup.1 ,rbp ,r15 ,rdi)))))))
;; ------------------------------------ old cases -------------------------------------------
(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (bitwise-and x.1 1)))
            (call L.addup.1 1))])
  (test-case "Impose conventions to support tail call -- bitwise-and"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames ()))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! x.1 ,rdi)
                                (begin
                                  (set! ,tmp-ra.1 ,r15)
                                  (begin (set! ,rax (bitwise-and x.1 1)) (jump ,tmp-ra.1 ,rbp ,rax)))))
                            (begin
                              (set! ,tmp-ra.2 r15)
                              (begin (set! ,rdi 1) (set! ,r15 ,tmp-ra.2) (jump ,L.addup.1 ,rbp ,r15 ,rdi))))
                            (and (register? rdi)
                            (eq? (current-return-address-register) r15)
                            (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))

(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (bitwise-ior x.1 1)))
            (call L.addup.1 1))])
  (test-case "Impose conventions to support tail call -- bitwise-ior"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames ()))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! x.1 ,rdi)
                                (begin
                                  (set! ,tmp-ra.1 ,r15)
                                  (begin (set! ,rax (bitwise-ior x.1 1)) (jump ,tmp-ra.1 ,rbp ,rax)))))
                            (begin
                              (set! ,tmp-ra.2 r15)
                              (begin (set! ,rdi 1) (set! ,r15 ,tmp-ra.2) (jump ,L.addup.1 ,rbp ,r15 ,rdi))))
                            (and (register? rdi)
                            (eq? (current-return-address-register) r15)
                            (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))


(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (bitwise-xor x.1 1)))
            (call L.addup.1 1))])
  (test-case "Impose conventions to support tail call -- bitwise-xor"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames ()))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! x.1 ,rdi)
                                (begin
                                  (set! ,tmp-ra.1 ,r15)
                                  (begin (set! ,rax (bitwise-xor x.1 1)) (jump ,tmp-ra.1 ,rbp ,rax)))))
                            (begin
                              (set! ,tmp-ra.2 r15)
                              (begin (set! ,rdi 1) (set! ,r15 ,tmp-ra.2) (jump ,L.addup.1 ,rbp ,r15 ,rdi))))
                            (and (register? rdi)
                            (eq? (current-return-address-register) r15)
                            (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))



(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (arithmetic-shift-right x.1 1)))
            (call L.addup.1 1))])
  (test-case "Impose conventions to support tail call -- arithmetic-shift-right"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames ()))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! x.1 ,rdi)
                                (begin
                                  (set! ,tmp-ra.1 ,r15)
                                  (begin (set! ,rax (arithmetic-shift-right x.1 1)) (jump ,tmp-ra.1 ,rbp ,rax)))))
                            (begin
                              (set! ,tmp-ra.2 r15)
                              (begin (set! ,rdi 1) (set! ,r15 ,tmp-ra.2) (jump ,L.addup.1 ,rbp ,r15 ,rdi))))
                            (and (register? rdi)
                            (eq? (current-return-address-register) r15)
                            (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))
;; -----------------------------------------------------------------------
; 5 more general cases
(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (+ x.1 1)))
            (- 1 2))])
  (test-case "Impose conventions to support negation"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames ()))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! x.1 ,rdi)
                                (begin
                                  (set! ,tmp-ra.1 ,r15)
                                  (begin (set! ,rax (+ x.1 1)) (jump ,tmp-ra.1 ,rbp ,rax)))))
                            (begin
                              (set! ,tmp-ra.2 ,r15)
                              (begin (set! ,rax (- 1 2)) (jump ,tmp-ra.2 ,rbp ,rax))))
                            (and (register? rdi)
                            (eq? (current-return-address-register) r15)
                             (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))

(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (+ x.1 1)))
            (call L.addup.1 1))])
  (test-case "Impose conventions to support tail call"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames ()))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! x.1 ,rdi)
                                (begin
                                  (set! ,tmp-ra.1 ,r15)
                                  (begin (set! ,rax (+ x.1 1)) (jump ,tmp-ra.1 ,rbp ,rax)))))
                            (begin
                              (set! ,tmp-ra.2 r15)
                              (begin (set! ,rdi 1) (set! ,r15 ,tmp-ra.2) (jump ,L.addup.1 ,rbp ,r15 ,rdi))))
                            (and (register? rdi)
                            (eq? (current-return-address-register) r15)
                            (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))

(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (+ x.1 1)))
            (begin (set! x.2 (call L.addup.1 1)) x.2))])
  (test-case "Impose conventions to support non-tail call"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames (())))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! x.1 ,rdi)
                                (begin
                                  (set! ,tmp-ra.1 ,r15)
                                  (begin (set! ,rax (+ x.1 1)) (jump ,tmp-ra.1 ,rbp ,rax)))))
                            (begin
                              (set! ,tmp-ra.2 ,r15)
                              (begin
                                (set! x.2
                                  (return-point
                                  ,L.rp.1
                                  (begin (set! ,rdi 1) (set! ,r15 ,L.rp.1) (jump ,L.addup.1 ,rbp ,r15 ,rdi))))
                                (begin (set! ,rax x.2) (jump ,tmp-ra.2 ,rbp ,rax)))))
                            (and (register? rdi)
                            (label? L.rp.1)
                            (eq? (current-return-address-register) r15)
                             (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))

(let ([x '(module
          (define L.swap.1
            (lambda (x.1 y.2)
              (if (< y.2 x.1) x.1 (begin (set! z.3 (call L.swap.1 y.2 x.1)) z.3))))
          (begin (set! z.4 (call L.swap.1 1 2)) z.4))])
  (test-case "Impose conventions to support non-tail call"
             (check-match (impose-calling-conventions x)
                           `(module
                          ((new-frames (())))
                          (define L.swap.1
                            ((new-frames (())))
                            (begin
                              (set! x.1 ,rdi)
                              (set! y.2 ,rsi)
                              (begin
                                (set! ,tmp-ra.1 ,r15)
                                (if (< y.2 x.1)
                                  (begin (set! ,rax x.1) (jump ,tmp-ra.1 ,rbp ,rax))
                                  (begin
                                    (set! z.3
                                      (return-point
                                      ,L.rp.1
                                      (begin
                                        (set! ,rsi x.1)
                                        (set! ,rdi y.2)
                                        (set! ,r15 ,L.rp.1)
                                        (jump L.swap.1 ,rbp ,r15 ,rdi ,rsi))))
                                    (begin (set! rax z.3) (jump ,tmp-ra.1 ,rbp ,rax)))))))
                          (begin
                            (set! ,tmp-ra.2 ,r15)
                            (begin
                              (set! z.4
                                (return-point
                                L.rp.2
                                (begin
                                  (set! ,rsi 2)
                                  (set! ,rdi 1)
                                  (set! ,r15 ,L.rp.2)
                                  (jump L.swap.1 ,rbp ,r15 ,rdi ,rsi))))
                              (begin (set! rax z.4) (jump ,tmp-ra.2 ,rbp ,rax)))))
                            (and (register? rdi)
                            (register? rsi)
                            (label? L.rp.1)
                            (eq? (current-return-address-register) r15)
                             (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))

(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (+ x.1 1)))
            (begin (set! x.2 (call L.addup.1 1)) x.2))])
  (test-case "Impose conventions to support non-tail call, when param registers are enough"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames (())))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! x.1 ,rdi)
                                (begin
                                  (set! ,tmp-ra.1 ,r15)
                                  (begin (set! ,rax (+ x.1 1)) (jump ,tmp-ra.1 ,rbp ,rax)))))
                            (begin
                              (set! ,tmp-ra.2 ,r15)
                              (begin
                                (set! x.2
                                  (return-point
                                  ,L.rp.1
                                  (begin (set! ,rdi 1) (set! ,r15 ,L.rp.1) (jump ,L.addup.1 ,rbp ,r15 ,rdi))))
                                (begin (set! ,rax x.2) (jump ,tmp-ra.2 ,rbp ,rax)))))
                            (and (register? rdi)
                            (label? L.rp.1)
                            (eq? (current-return-address-register) r15)
                             (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))

                            
(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (+ x.1 1)))
            (begin (set! x.2 (call L.addup.1 1)) x.2))])
  (test-case "Impose conventions to support non-tail call, when param registers are not enough"
             (check-match (parameterize ([current-parameter-registers '()]) (impose-calling-conventions x))
                           `(module
                            ((new-frames ((,nfv.3))))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! x.1 ,fv0)
                                (begin
                                  (set! ,tmp-ra.1 ,r15)
                                  (begin (set! ,rax (+ x.1 1)) (jump ,tmp-ra.1 ,rbp ,rax)))))
                            (begin
                              (set! ,tmp-ra.2 ,r15)
                              (begin
                                (set! x.2
                                  (return-point
                                  ,L.rp.1
                                  (begin
                                    (set! ,nfv.3 1)
                                    (set! ,r15 ,L.rp.1)
                                    (jump ,L.addup.1 ,rbp ,r15 ,nfv.3))))
                                (begin (set! rax x.2) (jump ,tmp-ra.2 ,rbp ,rax)))))
                            (and (fvar? fv0)
                            (label? L.rp.1)
                            (eq? (current-return-address-register) r15)
                             (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))

(let ([x '(module
          (define L.swap.1
            (lambda (x.1 y.2)
              (if (< y.2 x.1) x.1 (begin (set! z.3 (call L.swap.1 y.2 x.1)) z.3))))
          (begin (set! z.4 (call L.swap.1 1 2)) z.4))])
  (test-case "Impose conventions to support non-tail call, when param registers are not enough"
                          (check-match (parameterize ([current-parameter-registers '()]) (impose-calling-conventions x))
                           `(module
                          ((new-frames ((,nfv.5 ,nfv.6))))
                          (define L.swap.1
                            ((new-frames ((,nfv.2 ,nfv.3))))
                            (begin
                              (set! x.1 ,fv0)
                              (set! y.2 ,fv1)
                              (begin
                                (set! ,tmp-ra.1 ,r15)
                                (if (< y.2 x.1)
                                  (begin (set! ,rax x.1) (jump ,tmp-ra.1 ,rbp ,rax))
                                  (begin
                                    (set! z.3
                                      (return-point
                                      ,L.rp.1
                                      (begin
                                        (set! ,nfv.3 x.1)
                                        (set! ,nfv.2 y.2)
                                        (set! ,r15 ,L.rp.1)
                                        (jump L.swap.1 ,rbp ,r15 ,nfv.2 ,nfv.3))))
                                    (begin (set! rax z.3) (jump ,tmp-ra.1 ,rbp ,rax)))))))
                          (begin
                            (set! ,tmp-ra.2 ,r15)
                            (begin
                              (set! z.4
                                (return-point
                                ,L.rp.2
                                (begin
                                  (set! ,nfv.6 2)
                                  (set! ,nfv.5 1)
                                  (set! ,r15 ,L.rp.2)
                                  (jump L.swap.1 ,rbp ,r15 ,nfv.5 ,nfv.6))))
                              (begin (set! rax z.4) (jump ,tmp-ra.2 ,rbp ,rax)))))
                            (and (fvar? fv0) (fvar? fv1)
                            (fvar? fv0) (fvar? fv1)
                            (label? L.rp.1)
                            (eq? (current-return-address-register) r15)
                             (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))

(let ([x '(module
          (define L.addup.1
            (lambda (x.1)
              (begin (set! x.2 (call L.addup.2 x.1 x.2)) x.2)))
          (define L.addup.2
            (lambda (y.1 y.2)
              (+ y.1 y.2)))
          (call L.addup.1 1))])
  (test-case "Impose conventions to nontail call in the procedure"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames ()))
                            (define L.addup.1
                              ((new-frames (())))
                              (begin
                                (set! x.1 ,rdi)
                                (begin
                                  (set! ,tmp-ra.1 ,r15)
                                  (begin
                                    (set! x.2
                                      (return-point
                                      ,L.rp.1
                                      (begin
                                        (set! ,rsi x.2)
                                        (set! ,rdi x.1)
                                        (set! ,r15 ,L.rp.1)
                                        (jump L.addup.2 ,rbp ,r15 ,rdi ,rsi))))
                                    (begin (set! ,rax x.2) (jump ,tmp-ra.1 ,rbp ,rax))))))
                            (define L.addup.2
                              ((new-frames ()))
                              (begin
                                (set! y.1 ,rdi)
                                (set! y.2 ,rsi)
                                (begin
                                  (set! ,tmp-ra.2 ,r15)
                                  (begin (set! ,rax (+ y.1 y.2)) (jump ,tmp-ra.2 ,rbp ,rax)))))
                            (begin
                              (set! ,tmp-ra.3 ,r15)
                              (begin (set! ,rdi 1) (set! ,r15 ,tmp-ra.3) (jump ,L.addup.1 ,rbp ,r15 ,rdi))))
                            (and (register? rdi) (register? rsi)
                            (label? L.rp.1)
                            (eq? (current-return-address-register) r15)
                             (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))
                            

(let ([x '(module
          (define L.addup.1
            (lambda (x.1)
              (begin (set! x.2 (call L.addup.2 x.1 x.2)) x.2)))
          (define L.addup.2
            (lambda (y.1 y.2)
              (+ y.1 y.2)))
          (if (begin (set! z.1 (call L.addup.1 1)) (set! q.1 (call L.addup.2 2 3)) (< z.1 q.1))
              (+ 1 2)
              (call L.addup.1 2)))])
  (test-case "Impose conventions to nontail call in the pred"
             (check-match (parameterize ([current-parameter-registers '()]) (impose-calling-conventions x))
                           `(module
                          ((new-frames ((,nfv.7 ,nfv.8) (,nfv.6))))
                          (define L.addup.1
                            ((new-frames ((,nfv.2 ,nfv.3))))
                            (begin
                              (set! x.1 ,fv0)
                              (begin
                                (set! ,tmp-ra.1 ,r15)
                                (begin
                                  (set! x.2
                                    (return-point
                                    ,L.rp.1
                                    (begin
                                      (set! ,nfv.3 x.2)
                                      (set! ,nfv.2 x.1)
                                      (set! ,r15 ,L.rp.1)
                                      (jump L.addup.2 ,rbp ,r15 ,nfv.2 ,nfv.3))))
                                  (begin (set! ,rax x.2) (jump ,tmp-ra.1 ,rbp ,rax))))))
                          (define L.addup.2
                            ((new-frames ()))
                            (begin
                              (set! y.1 ,fv0)
                              (set! y.2 ,fv1)
                              (begin
                                (set! ,tmp-ra.4 ,r15)
                                (begin (set! ,rax (+ y.1 y.2)) (jump ,tmp-ra.4 ,rbp ,rax)))))
                          (begin
                            (set! ,tmp-ra.5 ,r15)
                            (if (begin
                                  (set! z.1
                                    (return-point
                                    ,L.rp.2
                                    (begin
                                      (set! ,nfv.6 1)
                                      (set! ,r15 ,L.rp.2)
                                      (jump L.addup.1 ,rbp ,r15 ,nfv.6))))
                                  (set! q.1
                                    (return-point
                                    ,L.rp.3
                                    (begin
                                      (set! ,nfv.8 3)
                                      (set! ,nfv.7 2)
                                      (set! ,r15 ,L.rp.3)
                                      (jump L.addup.2 ,rbp ,r15 ,nfv.7 ,nfv.8))))
                                  (< z.1 q.1))
                              (begin (set! ,rax (+ 1 2)) (jump ,tmp-ra.5 ,rbp ,rax))
                              (begin (set! ,fv0 2) (set! ,r15 ,tmp-ra.5) (jump ,L.addup.1 ,rbp ,r15 ,fv0)))))
                            (and (fvar? fv0) (fvar? fv1)
                            (label? L.rp.1) (label? L.rp.2) (label? L.rp.3)
                            (eq? (current-return-address-register) r15)
                             (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))


                          
(let ([x '(module
          (define L.addup.1
            (lambda (x.1)
              (begin (set! x.2 (call L.addup.2 x.1 x.2)) x.2)))
          (define L.addup.2
            (lambda (y.1 y.2)
              (+ y.1 y.2)))
          (if (if (true) (begin (set! x.1 (call L.addup.1 1)) (< x.1 1)) (begin (set! x.2 (call L.addup.1 2)) (> x.2 10)))
              (+ 1 2)
              (call L.addup.1 2)))])
  (test-case "Impose conventions to nontail call in the nested if"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames (() ())))
                            (define L.addup.1
                              ((new-frames (())))
                              (begin
                                (set! x.1 ,rdi)
                                (begin
                                  (set! ,tmp-ra.1 ,r15)
                                  (begin
                                    (set! x.2
                                      (return-point
                                      ,L.rp.1
                                      (begin
                                        (set! ,rsi x.2)
                                        (set! ,rdi x.1)
                                        (set! ,r15 ,L.rp.1)
                                        (jump L.addup.2 ,rbp ,r15 ,rdi ,rsi))))
                                    (begin (set! ,rax x.2) (jump ,tmp-ra.1 ,rbp ,rax))))))
                            (define L.addup.2
                              ((new-frames ()))
                              (begin
                                (set! y.1 ,rdi)
                                (set! y.2 ,rsi)
                                (begin
                                  (set! ,tmp-ra.2 ,r15)
                                  (begin (set! ,rax (+ y.1 y.2)) (jump ,tmp-ra.2 ,rbp ,rax)))))
                            (begin
                              (set! ,tmp-ra.3 ,r15)
                              (if (if (true)
                                    (begin
                                      (set! x.1
                                        (return-point
                                        ,L.rp.2
                                        (begin
                                          (set! ,rdi 1)
                                          (set! ,r15 ,L.rp.2)
                                          (jump L.addup.1 ,rbp ,r15 ,rdi))))
                                      (< x.1 1))
                                    (begin
                                      (set! x.2
                                        (return-point
                                        ,L.rp.3
                                        (begin
                                          (set! ,rdi 2)
                                          (set! ,r15 ,L.rp.3)
                                          (jump L.addup.1 ,rbp ,r15 ,rdi))))
                                      (> x.2 10)))
                                (begin (set! ,rax (+ 1 2)) (jump ,tmp-ra.3 ,rbp ,rax))
                                (begin (set! ,rdi 2) (set! ,r15 ,tmp-ra.3) (jump L.addup.1 ,rbp ,r15 ,rdi)))))
                            (and (register? rdi) (register? rsi)
                            (label? L.rp.1)
                            (eq? (current-return-address-register) r15)
                             (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))

(let ([x `(module 
              (define L.func.0 (lambda (x.0 y.0) 
                  (begin (set! x.0 (call L.func.1 x.0 y.0))
                          (+ y.0 x.1))))
              (define L.func.1 (lambda (x.0) 
                  (begin (set! x.0 (call L.func.0 x.1)) x.0)))
              (begin (set! x.1 (call L.func.1 1)) (+ x.0 x.1)))])
  (test-case "General cases"
             (check-match (parameterize ([current-parameter-registers '()]) (impose-calling-conventions x))
                           `(module
                            ((new-frames ((,nfv.7))))
                            (define L.func.0
                              ((new-frames ((,nfv.2 ,nfv.3))))
                              (begin
                                (set! x.0 ,fv0)
                                (set! y.0 ,fv1)
                                (begin
                                  (set! ,tmp-ra.1 ,r15)
                                  (begin
                                    (set! x.0
                                      (return-point
                                      ,L.rp.1
                                      (begin
                                        (set! ,nfv.3 y.0)
                                        (set! ,nfv.2 x.0)
                                        (set! ,r15 ,L.rp.1)
                                        (jump L.func.1 ,rbp ,r15 ,nfv.2 ,nfv.3))))
                                    (begin (set! ,rax (+ y.0 x.1)) (jump ,tmp-ra.1 ,rbp ,rax))))))
                            (define L.func.1
                              ((new-frames ((,nfv.5))))
                              (begin
                                (set! x.0 ,fv0)
                                (begin
                                  (set! ,tmp-ra.4 ,r15)
                                  (begin
                                    (set! x.0
                                      (return-point
                                      ,L.rp.2
                                      (begin
                                        (set! ,nfv.5 x.1)
                                        (set! ,r15 ,L.rp.2)
                                        (jump L.func.0 ,rbp ,r15 ,nfv.5))))
                                    (begin (set! ,rax x.0) (jump ,tmp-ra.4 ,rbp ,rax))))))
                            (begin
                              (set! ,tmp-ra.6 ,r15)
                              (begin
                                (set! x.1
                                  (return-point
                                  ,L.rp.3
                                  (begin
                                    (set! ,nfv.7 1)
                                    (set! ,r15 ,L.rp.3)
                                    (jump L.func.1 ,rbp ,r15 ,nfv.7))))
                                (begin (set! ,rax (+ x.0 x.1)) (jump ,tmp-ra.6 ,rbp ,rax)))))
                            (and (fvar? fv0) (fvar? fv1)
                            (label? L.rp.1) (label? L.rp.2) (label? L.rp.3)
                            (eq? (current-return-address-register) r15)
                             (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))


(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (+ x.1 1)))
            (begin (set! x.2 (begin (set! x.3 4) (set! x.4 (call L.addup.1 1)) (call L.addup.1 2))) x.2))])
  (test-case "General case"
             (check-match (impose-calling-conventions x)
                           `(module
                            ((new-frames (() ())))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! x.1 ,rdi)
                                (begin
                                  (set! ,tmp-ra.1 ,r15)
                                  (begin (set! ,rax (+ x.1 1)) (jump ,tmp-ra.1 ,rbp ,rax)))))
                            (begin
                              (set! ,tmp-ra.2 ,r15)
                              (begin
                                (set! x.2
                                  (begin
                                    (set! x.3 4)
                                    (set! x.4
                                      (return-point
                                      ,L.rp.1
                                      (begin
                                        (set! ,rdi 1)
                                        (set! ,r15 ,L.rp.1)
                                        (jump L.addup.1 ,rbp ,r15 ,rdi))))
                                    (return-point
                                    ,L.rp.2
                                    (begin
                                      (set! ,rdi 2)
                                      (set! ,r15 ,L.rp.2)
                                      (jump L.addup.1 ,rbp ,r15 ,rdi)))))
                                (begin (set! ,rax x.2) (jump ,tmp-ra.2 ,rbp ,rax)))))
                            (and (register? rdi)
                            (label? L.rp.1) (label? L.rp.2)
                            (eq? (current-return-address-register) r15)
                             (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))

(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (+ x.1 1)))
            (begin (set! x.2 (if (true) (call L.addup.1 1) (begin (set! x.3 3) (call L.addup.1 x.3)))) x.2))])
  (test-case "General case"
             (check-match (parameterize ([current-parameter-registers '()]) (impose-calling-conventions x))
                           `(module
                            ((new-frames ((,nfv.4) (,nfv.3))))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! x.1 ,fv0)
                                (begin
                                  (set! ,tmp-ra.1 ,r15)
                                  (begin (set! ,rax (+ x.1 1)) (jump ,tmp-ra.1 ,rbp ,rax)))))
                            (begin
                              (set! ,tmp-ra.2 ,r15)
                              (begin
                                (set! x.2
                                  (if (true)
                                    (return-point
                                    ,L.rp.1
                                    (begin
                                      (set! ,nfv.3 1)
                                      (set! ,r15 ,L.rp.1)
                                      (jump L.addup.1 ,rbp ,r15 ,nfv.3)))
                                    (begin
                                      (set! x.3 3)
                                      (return-point
                                      ,L.rp.2
                                      (begin
                                        (set! ,nfv.4 x.3)
                                        (set! ,r15 ,L.rp.2)
                                        (jump L.addup.1 ,rbp ,r15 ,nfv.4))))))
                                (begin (set! ,rax x.2) (jump ,tmp-ra.2 ,rbp ,rax)))))
                            (and (fvar? fv0)
                            (label? L.rp.1) (label? L.rp.2)
                            (eq? (current-return-address-register) r15)
                             (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))

(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (begin (set! x.3 6) (set! x.4 (call L.addup.2 x.1 x.3)) (+ x.3 x.4))))
                (define L.addup.2
                    (lambda (y.1 y.2)
                        (+ y.1 y.2)))
            (if (begin (set! q.1 L.addup.1) (set! m.1 (call q.1 2)) (< m.1 2))
                (call L.addup.2 1 2)
                (call L.addup.1 1)))])
  (test-case "General case"
             (check-match (parameterize ([current-parameter-registers '()]) (impose-calling-conventions x))
                           `(module
                            ((new-frames ((,nfv.6))))
                            (define L.addup.1
                              ((new-frames ((,nfv.2 ,nfv.3))))
                              (begin
                                (set! x.1 ,fv0)
                                (begin
                                  (set! ,tmp-ra.1 ,r15)
                                  (begin
                                    (set! x.3 6)
                                    (set! x.4
                                      (return-point
                                      ,L.rp.1
                                      (begin
                                        (set! ,nfv.3 x.3)
                                        (set! ,nfv.2 x.1)
                                        (set! ,r15 ,L.rp.1)
                                        (jump L.addup.2 ,rbp ,r15 ,nfv.2 ,nfv.3))))
                                    (begin (set! ,rax (+ x.3 x.4)) (jump ,tmp-ra.1 ,rbp ,rax))))))
                            (define L.addup.2
                              ((new-frames ()))
                              (begin
                                (set! y.1 ,fv0)
                                (set! y.2 ,fv1)
                                (begin
                                  (set! ,tmp-ra.4 ,r15)
                                  (begin (set! ,rax (+ y.1 y.2)) (jump ,tmp-ra.4 ,rbp ,rax)))))
                            (begin
                              (set! ,tmp-ra.5 ,r15)
                              (if (begin
                                    (set! q.1 ,L.addup.1)
                                    (set! m.1
                                      (return-point
                                      ,L.rp.2
                                      (begin
                                        (set! ,nfv.6 2)
                                        (set! ,r15 ,L.rp.2)
                                        (jump q.1 ,rbp ,r15 ,nfv.6))))
                                    (< m.1 2))
                                (begin
                                  (set! ,fv1 2)
                                  (set! ,fv0 1)
                                  (set! ,r15 ,tmp-ra.5)
                                  (jump L.addup.2 ,rbp ,r15 ,fv0 ,fv1))
                                (begin (set! ,fv0 1) (set! ,r15 ,tmp-ra.5) (jump ,L.addup.1 ,rbp ,r15 ,fv0)))))
                            (and (fvar? fv1) (fvar? fv0)
                            (label? L.rp.1) (label? L.rp.2)
                            (eq? (current-return-address-register) r15)
                             (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))


(let ([x '(module
                (define L.addup.1
                    (lambda (x.1)
                        (+ x.1 1)))
                (define L.addup.2
                    (lambda (y.1 y.2)
                        (+ y.1 y.2)))
            (if (if (true) (begin (set! x.2 1) (set! x.3 (call L.addup.1 x.2)) (< x.3 2)) (begin (set! x.4 5) (if (true) (set! x.5 (call L.addup.1 1)) (set! x.5 (call L.addup.2 2 3))) (false)))
                (call L.addup.2 1 2)
                (call L.addup.1 1)))])
  (test-case "General case"
             (check-match (parameterize ([current-parameter-registers '()]) (impose-calling-conventions x))
                           `(module
                            ((new-frames ((,nfv.6 ,nfv.7) (,nfv.5) (,nfv.4))))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! x.1 ,fv0)
                                (begin
                                  (set! ,tmp-ra.1 ,r15)
                                  (begin (set! ,rax (+ x.1 1)) (jump ,tmp-ra.1 ,rbp ,rax)))))
                            (define L.addup.2
                              ((new-frames ()))
                              (begin
                                (set! y.1 ,fv0)
                                (set! y.2 ,fv1)
                                (begin
                                  (set! ,tmp-ra.2 ,r15)
                                  (begin (set! ,rax (+ y.1 y.2)) (jump ,tmp-ra.2 ,rbp ,rax)))))
                            (begin
                              (set! ,tmp-ra.3 ,r15)
                              (if (if (true)
                                    (begin
                                      (set! x.2 1)
                                      (set! x.3
                                        (return-point
                                        ,L.rp.1
                                        (begin
                                          (set! ,nfv.4 x.2)
                                          (set! ,r15 ,L.rp.1)
                                          (jump L.addup.1 ,rbp ,r15 ,nfv.4))))
                                      (< x.3 2))
                                    (begin
                                      (set! x.4 5)
                                      (if (true)
                                        (set! x.5
                                          (return-point
                                          ,L.rp.2
                                          (begin
                                            (set! ,nfv.5 1)
                                            (set! ,r15 ,L.rp.2)
                                            (jump L.addup.1 ,rbp ,r15 ,nfv.5))))
                                        (set! x.5
                                          (return-point
                                          ,L.rp.3
                                          (begin
                                            (set! ,nfv.7 3)
                                            (set! ,nfv.6 2)
                                            (set! ,r15 ,L.rp.3)
                                            (jump L.addup.2 ,rbp ,r15 ,nfv.6 ,nfv.7)))))
                                      (false)))
                                (begin
                                  (set! ,fv1 2)
                                  (set! ,fv0 1)
                                  (set! ,r15 ,tmp-ra.3)
                                  (jump L.addup.2 ,rbp ,r15 ,fv0 ,fv1))
                                (begin (set! ,fv0 1) (set! ,r15 ,tmp-ra.3) (jump L.addup.1 ,rbp ,r15 ,fv0)))))
                            (and (fvar? fv1) (fvar? fv0)
                            (label? L.rp.1)
                            (eq? (current-return-address-register) r15)
                             (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))

;; cases from sequentialize-let
(let ([x `(module
                (define L.addup1.1
                  (lambda (x.3 y.2 z.1)
                    (begin
                      (set! x.6 (call L.addup2.2 x.3 y.2 z.1))
                      (set! y.5 (call L.addup2.2 x.3 y.2 z.1))
                      (set! z.4 z.1)
                      (+ z.4 y.5))))
                (define L.addup2.2
                  (lambda (x.9 y.8 z.7)
                    (begin
                      (set! addup1.12 x.9)
                      (set! x.11 L.addup1.1)
                      (set! z.10 (call L.addup1.1 x.9 y.8 z.7))
                      (call x.11 addup1.12 y.8 z.10))))
                (define L.addup3.3
                  (lambda (x.15 y.14 z.13)
                    (begin
                      (set! x.17 L.addup1.1)
                      (set! y.16 L.addup2.2)
                      (begin (set! x.19 x.17) (set! q.18 y.16) (call q.18 1 2 3)))))
                (call L.addup3.3 5 6 7))])
  (test-case "General case"
             (check-match (parameterize ([current-parameter-registers '()]) (impose-calling-conventions x))
                           `(module
                            ((new-frames ()))
                            (define L.addup1.1
                              ((new-frames ((,nfv.5 ,nfv.6 ,nfv.7) (,nfv.2 ,nfv.3 ,nfv.4))))
                              (begin
                                (set! x.3 ,fv0)
                                (set! y.2 ,fv1)
                                (set! z.1 ,fv2)
                                (begin
                                  (set! ,tmp-ra.1 ,r15)
                                  (begin
                                    (set! x.6
                                      (return-point
                                      ,L.rp.1
                                      (begin
                                        (set! ,nfv.4 z.1)
                                        (set! ,nfv.3 y.2)
                                        (set! ,nfv.2 x.3)
                                        (set! ,r15 ,L.rp.1)
                                        (jump L.addup2.2 ,rbp ,r15 ,nfv.2 ,nfv.3 ,nfv.4))))
                                    (set! y.5
                                      (return-point
                                      ,L.rp.2
                                      (begin
                                        (set! ,nfv.7 z.1)
                                        (set! ,nfv.6 y.2)
                                        (set! ,nfv.5 x.3)
                                        (set! ,r15 ,L.rp.2)
                                        (jump L.addup2.2 ,rbp ,r15 ,nfv.5 ,nfv.6 ,nfv.7))))
                                    (set! z.4 z.1)
                                    (begin (set! ,rax (+ z.4 y.5)) (jump ,tmp-ra.1 ,rbp ,rax))))))
                            (define L.addup2.2
                              ((new-frames ((,nfv.9 ,nfv.10 ,nfv.11))))
                              (begin
                                (set! x.9 ,fv0)
                                (set! y.8 ,fv1)
                                (set! z.7 ,fv2)
                                (begin
                                  (set! ,tmp-ra.8 ,r15)
                                  (begin
                                    (set! addup1.12 x.9)
                                    (set! x.11 L.addup1.1)
                                    (set! z.10
                                      (return-point
                                      ,L.rp.3
                                      (begin
                                        (set! ,nfv.11 z.7)
                                        (set! ,nfv.10 y.8)
                                        (set! ,nfv.9 x.9)
                                        (set! ,r15 ,L.rp.3)
                                        (jump L.addup1.1 ,rbp ,r15 ,nfv.9 ,nfv.10 ,nfv.11))))
                                    (begin
                                      (set! ,fv2 z.10)
                                      (set! ,fv1 y.8)
                                      (set! ,fv0 addup1.12)
                                      (set! ,r15 ,tmp-ra.8)
                                      (jump x.11 ,rbp ,r15 ,fv0 ,fv1 ,fv2))))))
                            (define L.addup3.3
                              ((new-frames ()))
                              (begin
                                (set! x.15 ,fv0)
                                (set! y.14 ,fv1)
                                (set! z.13 ,fv2)
                                (begin
                                  (set! ,tmp-ra.12 ,r15)
                                  (begin
                                    (set! x.17 L.addup1.1)
                                    (set! y.16 L.addup2.2)
                                    (begin
                                      (set! x.19 x.17)
                                      (set! q.18 y.16)
                                      (begin
                                        (set! ,fv2 3)
                                        (set! ,fv1 2)
                                        (set! ,fv0 1)
                                        (set! ,r15 ,tmp-ra.12)
                                        (jump q.18 ,rbp ,r15 ,fv0 ,fv1 ,fv2)))))))
                            (begin
                              (set! ,tmp-ra.13 ,r15)
                              (begin
                                (set! ,fv2 7)
                                (set! ,fv1 6)
                                (set! ,fv0 5)
                                (set! ,r15 ,tmp-ra.13)
                                (jump L.addup3.3 ,rbp ,r15 ,fv0 ,fv1 ,fv2))))
                            (and (fvar? fv1) (fvar? fv0) (fvar? fv2)
                            (label? L.rp.1) (label? L.rp.2) (label? L.rp.3) 
                            (eq? (current-return-address-register) r15)
                            (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))

(let ([x `(module
                (define L.addup.1
                  (lambda (x.3 y.2 z.1)
                    (begin
                      (set! x.4 x.3)
                      (begin
                        (set! x.7 x.4)
                        (set! y.6 y.2)
                        (set! z.5 z.1)
                        (call L.addup2.2 x.7 y.6 z.5)))))
                (define L.addup2.2
                  (lambda (x.10 y.9 z.8)
                    (begin
                      (set! x.13 z.8)
                      (set! y.12 x.10)
                      (set! z.11 y.9)
                      (if (< x.13 z.11)
                        (+ x.13 y.12)
                        (begin (set! x.14 3) (call L.addup3.3 x.14))))))
                (define L.addup3.3 (lambda (x.15) (+ x.15 3)))
                (call L.addup.1 1 2 3))])
  (test-case "General case"
             (check-match (parameterize ([current-parameter-registers '()]) (impose-calling-conventions x))
                           `(module
                            ((new-frames ()))
                            (define L.addup.1
                              ((new-frames ()))
                              (begin
                                (set! x.3 ,fv0)
                                (set! y.2 ,fv1)
                                (set! z.1 ,fv2)
                                (begin
                                  (set! ,tmp-ra.1 ,r15)
                                  (begin
                                    (set! x.4 x.3)
                                    (begin
                                      (set! x.7 x.4)
                                      (set! y.6 y.2)
                                      (set! z.5 z.1)
                                      (begin
                                        (set! ,fv2 z.5)
                                        (set! ,fv1 y.6)
                                        (set! ,fv0 x.7)
                                        (set! ,r15 ,tmp-ra.1)
                                        (jump L.addup2.2 ,rbp ,r15 ,fv0 ,fv1 ,fv2)))))))
                            (define L.addup2.2
                              ((new-frames ()))
                              (begin
                                (set! x.10 ,fv0)
                                (set! y.9 ,fv1)
                                (set! z.8 ,fv2)
                                (begin
                                  (set! ,tmp-ra.2 ,r15)
                                  (begin
                                    (set! x.13 z.8)
                                    (set! y.12 x.10)
                                    (set! z.11 y.9)
                                    (if (< x.13 z.11)
                                      (begin (set! ,rax (+ x.13 y.12)) (jump ,tmp-ra.2 ,rbp ,rax))
                                      (begin
                                        (set! x.14 3)
                                        (begin
                                          (set! ,fv0 x.14)
                                          (set! ,r15 ,tmp-ra.2)
                                          (jump L.addup3.3 ,rbp ,r15 ,fv0))))))))
                            (define L.addup3.3
                              ((new-frames ()))
                              (begin
                                (set! x.15 ,fv0)
                                (begin
                                  (set! ,tmp-ra.3 ,r15)
                                  (begin (set! ,rax (+ x.15 3)) (jump ,tmp-ra.3 ,rbp ,rax)))))
                            (begin
                              (set! ,tmp-ra.4 ,r15)
                              (begin
                                (set! ,fv2 3)
                                (set! ,fv1 2)
                                (set! ,fv0 1)
                                (set! ,r15 ,tmp-ra.4)
                                (jump L.addup.1 ,rbp ,r15 ,fv0 ,fv1 ,fv2))))
                            (and (fvar? fv1) (fvar? fv0) (fvar? fv2)
                            (eq? (current-return-address-register) r15)
                             (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))


(let ([x `(module
                  (define L.addup1.1
                    (lambda (x.3 y.2 z.1)
                      (begin (set! x.6 x.3) (set! y.5 y.2) (set! z.4 z.1) (+ z.4 y.5))))
                  (define L.addup2.2
                    (lambda (x.9 y.8 z.7)
                      (if (if (begin
                                (set! x.12 (call L.addup2.2 x.9 y.8 z.7))
                                (set! y.11 2)
                                (set! z.10 3)
                                (< x.12 y.11))
                            (if (true)
                              (false)
                              (begin (set! x.14 x.9) (set! y.13 y.8) (not (!= x.14 y.13))))
                            (begin (set! x.16 2) (set! z.15 3) (> x.16 z.15)))
                        (call L.addup1.1 x.9 y.8 z.7)
                        (begin
                          (set! x.19 1)
                          (set! y.18 (call L.addup3.3 x.9 y.8 z.7))
                          (set! z.17 6)
                          (call L.addup1.1 x.19 y.18 z.17)))))
                  (define L.addup3.3
                    (lambda (x.22 y.21 z.20)
                      (begin
                        (set! x.24 1)
                        (set! y.23 (begin (set! z.25 3) z.25))
                        (call L.addup2.2 x.24 y.23 z.20))))
                  (begin
                    (set! x.28 1)
                    (set! y.27 2)
                    (set! z.26 3)
                    (call L.addup3.3 x.28 y.27 z.26)))])
  (test-case "General case"
             (check-match (parameterize ([current-parameter-registers '()]) (impose-calling-conventions x))
                           `(module
                            ((new-frames ()))
                            (define L.addup1.1
                              ((new-frames ()))
                              (begin
                                (set! x.3 ,fv0)
                                (set! y.2 ,fv1)
                                (set! z.1 ,fv2)
                                (begin
                                  (set! ,tmp-ra.1 ,r15)
                                  (begin
                                    (set! x.6 x.3)
                                    (set! y.5 y.2)
                                    (set! z.4 z.1)
                                    (begin (set! ,rax (+ z.4 y.5)) (jump ,tmp-ra.1 ,rbp ,rax))))))
                            (define L.addup2.2
                              ((new-frames ((,nfv.6 ,nfv.7 ,nfv.8) (,nfv.3 ,nfv.4 ,nfv.5))))
                              (begin
                                (set! x.9 ,fv0)
                                (set! y.8 ,fv1)
                                (set! z.7 ,fv2)
                                (begin
                                  (set! ,tmp-ra.2 ,r15)
                                  (if (if (begin
                                            (set! x.12
                                              (return-point
                                              ,L.rp.1
                                              (begin
                                                (set! ,nfv.5 z.7)
                                                (set! ,nfv.4 y.8)
                                                (set! ,nfv.3 x.9)
                                                (set! ,r15 ,L.rp.1)
                                                (jump L.addup2.2 ,rbp ,r15 ,nfv.3 ,nfv.4 ,nfv.5))))
                                            (set! y.11 2)
                                            (set! z.10 3)
                                            (< x.12 y.11))
                                        (if (true)
                                          (false)
                                          (begin (set! x.14 x.9) (set! y.13 y.8) (not (!= x.14 y.13))))
                                        (begin (set! x.16 2) (set! z.15 3) (> x.16 z.15)))
                                    (begin
                                      (set! ,fv2 z.7)
                                      (set! ,fv1 y.8)
                                      (set! ,fv0 x.9)
                                      (set! ,r15 ,tmp-ra.2)
                                      (jump L.addup1.1 ,rbp ,r15 ,fv0 ,fv1 ,fv2))
                                    (begin
                                      (set! x.19 1)
                                      (set! y.18
                                        (return-point
                                        ,L.rp.2
                                        (begin
                                          (set! ,nfv.8 z.7)
                                          (set! ,nfv.7 y.8)
                                          (set! ,nfv.6 x.9)
                                          (set! ,r15 ,L.rp.2)
                                          (jump L.addup3.3 ,rbp ,r15 ,nfv.6 ,nfv.7 ,nfv.8))))
                                      (set! z.17 6)
                                      (begin
                                        (set! ,fv2 z.17)
                                        (set! ,fv1 y.18)
                                        (set! ,fv0 x.19)
                                        (set! ,r15 ,tmp-ra.2)
                                        (jump L.addup1.1 ,rbp ,r15 ,fv0 ,fv1 ,fv2)))))))
                            (define L.addup3.3
                              ((new-frames ()))
                              (begin
                                (set! x.22 ,fv0)
                                (set! y.21 ,fv1)
                                (set! z.20 ,fv2)
                                (begin
                                  (set! ,tmp-ra.9 ,r15)
                                  (begin
                                    (set! x.24 1)
                                    (set! y.23 (begin (set! z.25 3) z.25))
                                    (begin
                                      (set! ,fv2 z.20)
                                      (set! ,fv1 y.23)
                                      (set! ,fv0 x.24)
                                      (set! ,r15 ,tmp-ra.9)
                                      (jump L.addup2.2 ,rbp ,r15 ,fv0 ,fv1 ,fv2))))))
                            (begin
                              (set! ,tmp-ra.10 ,r15)
                              (begin
                                (set! x.28 1)
                                (set! y.27 2)
                                (set! z.26 3)
                                (begin
                                  (set! ,fv2 z.26)
                                  (set! ,fv1 y.27)
                                  (set! ,fv0 x.28)
                                  (set! ,r15 ,tmp-ra.10)
                                  (jump L.addup3.3 ,rbp ,r15 ,fv0 ,fv1 ,fv2)))))
                            (and (fvar? fv1) (fvar? fv0) (fvar? fv2)
                            (label? L.rp.1) (label? L.rp.2)
                            (eq? (current-return-address-register) r15)
                             (eq? (current-return-value-register) rax)
                            (frame-base-pointer-register? rbp)))))


