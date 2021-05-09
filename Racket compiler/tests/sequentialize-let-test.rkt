#lang racket
(require
  cpsc411/compiler-lib
  cpsc411/2c-run-time
  cpsc411/test-suite/utils
  racket/match
  rackunit)

(require "../component/seqn-let.rkt")

; input: values-bit-lang-v8
; output: proc-imp-mf-lang-v8
; purpose: Check if the outputs of sequentialize-let matches the desired outputs of the interrogator.
;; edge cases
(let ([x `(module 5)])
  (test-case "Edge case 1"
             (check-equal? (sequentialize-let x)
                           `(module 5))))

(let ([x `(module (mref x.1 8))])
  (test-case "Support mref as value"
             (check-equal? (sequentialize-let x)
                           `(module (mref x.1 8)))))

(let ([x `(module (alloc 8))])
  (test-case "Edge alloc as value"
             (check-equal? (sequentialize-let x)
                           `(module (alloc 8)))))



(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (let ([x.2 8]) (begin (mset! x.1 x.2 8) x.1))))
            (call L.addup.1 1 2 3))])
  (test-case "Support begin as tail and effect 1" 
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1) (begin (set! x.2 8) (begin (mset! x.1 x.2 8) x.1))))
                            (call L.addup.1 1 2 3)))))


(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (let ([x.2 (begin (let ([x.4 5]) (mset! x.4 8 8)) x.4)]) x.2)))
            (call L.addup.1 1 2 3))])
  (test-case "Support begin as value and effect 2"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (begin (set! x.2 (begin (begin (set! x.4 5) (mset! x.4 8 8)) x.4)) x.2)))
                            (call L.addup.1 1 2 3)))))

(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (if (begin (begin (mset! x.3 y.2 (+ z.1 z.1))) (!= x.3 z.1))
                    x.3
                    (bitwise-and z.1 y.2))))
            (call L.addup.1 1 2 3))])
  (test-case "Support begin as pred and effect 3"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (if (begin (begin (mset! x.3 y.2 (+ z.1 z.1))) (!= x.3 z.1))
                                  x.3
                                  (bitwise-and z.1 y.2))))
                            (call L.addup.1 1 2 3)))))

;; ok
(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (let ([x.2 (mref x.3 8)]) (begin (mset! x.1 x.2 8) x.1))))
            (call L.addup.1 1 2 3))])
  (test-case "Support let bind mref as value"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (begin (set! x.2 (mref x.3 8)) (begin (mset! x.1 x.2 8) x.1))))
                            (call L.addup.1 1 2 3)))))

;; ok
(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (let ([x.2 (alloc 8)]) (begin (mset! x.1 x.2 8) x.1))))
            (call L.addup.1 1 2 3))])
  (test-case "Support let bind alloc as value"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (begin (set! x.2 (alloc 8)) (begin (mset! x.1 x.2 8) x.1))))
                            (call L.addup.1 1 2 3)))))

;; todo all below
(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (let ([x.2 (let ([x.3 (alloc 8)]) (mref x.3 y.2))]) (begin (mset! x.1 x.2 8) x.1))))
            (call L.addup.1 1 2 3))])
  (test-case "Support let use mref as value"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (begin
                                  (set! x.2 (begin (set! x.3 (alloc 8)) (mref x.3 y.2)))
                                  (begin (mset! x.1 x.2 8) x.1))))
                            (call L.addup.1 1 2 3)))))

(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (let ([x.2 (let ([x.3 (mref y.2 8)]) (alloc y.2))]) (begin (mset! x.1 x.2 8) x.1))))
            (call L.addup.1 1 2 3))])
  (test-case "Support let use alloc as value"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (begin
                                  (set! x.2 (begin (set! x.3 (mref y.2 8)) (alloc y.2)))
                                  (begin (mset! x.1 x.2 8) x.1))))
                            (call L.addup.1 1 2 3)))))


(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (let ([x.2 (if (true) (mref y.2 8) (begin (mset! q.1 8 8) (alloc 8)))]) (begin (mset! x.1 x.2 8) x.1))))
            (call L.addup.1 1 2 3))])
  (test-case "Support if use mref and alloc as value"
             (check-equal? (sequentialize-let x)
                           `(module
                              (define L.addup1.1
                                (lambda (x.3 y.2 z.1)
                                  (begin
                                    (set! x.2 (if (true) (mref y.2 8) (begin (mset! q.1 8 8) (alloc 8))))
                                    (begin (mset! x.1 x.2 8) x.1))))
                              (call L.addup.1 1 2 3)))))


(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (let ([x.2 (alloc 8)] [x.4 (mref y.1 8)]) (begin (mset! x.1 x.2 8) (begin (mset! y.2 8 (+ 1 2)) (call L.end.1))))))
            (call L.addup.1 1 2 3))])
  (test-case "Support let begin as tail"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (begin
                                  (set! x.2 (alloc 8))
                                  (set! x.4 (mref y.1 8))
                                  (begin
                                    (mset! x.1 x.2 8)
                                    (begin (mset! y.2 8 (+ 1 2)) (call L.end.1))))))
                            (call L.addup.1 1 2 3)))))


(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (let ([x.2 (begin (mset! x.1 8 8) (let ([x.8 (begin (mset! y.2 8 32) y.2)]) x.8))]) (begin (mset! x.1 x.2 8) x.1))))
            (call L.addup.1 1 2 3))])
  (test-case "Support let begin as value"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (begin
                                  (set! x.2
                                    (begin
                                      (mset! x.1 8 8)
                                      (begin (set! x.8 (begin (mset! y.2 8 32) y.2)) x.8)))
                                  (begin (mset! x.1 x.2 8) x.1))))
                            (call L.addup.1 1 2 3)))))

(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (begin (let ([x.5 (alloc y.2)]) (begin (mset! y.2 8 (alloc 16)) (mset! z.1 z.1 (mref z.1 8)))) (mref z.1 0))))
            (call L.addup.1 1 2 3))])
  (test-case "Support let begin as effect"
             (check-equal? (sequentialize-let x)
                           `(module
                              (define L.addup1.1
                                (lambda (x.3 y.2 z.1)
                                  (begin
                                    (begin
                                      (set! x.5 (alloc y.2))
                                      (begin (mset! y.2 8 (alloc 16)) (mset! z.1 z.1 (mref z.1 8))))
                                    (mref z.1 0))))
                              (call L.addup.1 1 2 3)))))

(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (if (let ([y.2 (alloc 8)]) (not (begin (mset! y.1 y.1 (+ x.3 y.2)) (true))))
                    (+ 1 y.1)
                    (alloc z.1))))
            (call L.addup.1 1 2 3))])
  (test-case "Support let begin as pred"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (if (begin
                                      (set! y.2 (alloc 8))
                                      (not (begin (mset! y.1 y.1 (+ x.3 y.2)) (true))))
                                  (+ 1 y.1)
                                  (alloc z.1))))
                            (call L.addup.1 1 2 3)))))


(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (begin (mset! y.1 8 8) (let ([x.1 (alloc 8)]) (mref z.1 y.2)))))
            (call L.addup.1 1 2 3))])
  (test-case "Support begin with let as tail"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (begin (mset! y.1 8 8) (begin (set! x.1 (alloc 8)) (mref z.1 y.2)))))
                            (call L.addup.1 1 2 3)))))

(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (if (begin (mset! y.1 8 8) (let ([y.3 (mref x.1 8)]) (< y.3 y.2)))
                    (alloc z.1)
                    (mref y.2 x.3))))
            (call L.addup.1 1 2 3))])
  (test-case "Support begin with let as pred"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (if (begin (mset! y.1 8 8) (begin (set! y.3 (mref x.1 8)) (< y.3 y.2)))
                                  (alloc z.1)
                                  (mref y.2 x.3))))
                            (call L.addup.1 1 2 3)))))


(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (begin (begin (let ([y.3 (alloc 8)]) (mset! y.3 y.3 8))) (alloc z.1))))
            (call L.addup.1 1 2 3))])
  (test-case "Support begin with let as effect"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (begin
                                  (begin (begin (set! y.3 (alloc 8)) (mset! y.3 y.3 8)))
                                  (alloc z.1))))
                            (call L.addup.1 1 2 3)))))

(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (let ([x.2 (alloc 8)]) (begin (mset! x.1 x.2 8) x.1))))
            (call L.addup.1 1 2 3))])
  (test-case "Support begin with let as value"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (begin (set! x.2 (alloc 8)) (begin (mset! x.1 x.2 8) x.1))))
                            (call L.addup.1 1 2 3)))))


(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (begin (mset! y.2 y.2 (let ([y.3 (alloc 8)]) (let ([y.4 (mref z.1 8)]) (alloc y.4)))) (mref x.3 32))))
            (call L.addup.1 1 2 3))])
  (test-case "Support let in mset!"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (begin
                                  (mset!
                                  y.2
                                  y.2
                                  (begin
                                    (set! y.3 (alloc 8))
                                    (begin (set! y.4 (mref z.1 8)) (alloc y.4))))
                                  (mref x.3 32))))
                            (call L.addup.1 1 2 3)))))

(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (begin (let ([y.2 (alloc 8)]) (let ([y.3 (mref x.3 32)]) (mset! z.1 z.1 8))) (mref z.1 8))))
            (call L.addup.1 1 2 3))])
  (test-case "Support let nested in effect"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (begin
                                  (begin
                                    (set! y.2 (alloc 8))
                                    (begin (set! y.3 (mref x.3 32)) (mset! z.1 z.1 8)))
                                  (mref z.1 8))))
                            (call L.addup.1 1 2 3)))))

;; -------------------------------------------------- old cases -----------------------------
(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (if (if (let ([x.4 x.3] [q.1 (call L.addup1.1 x.3 2 1)]) (< x.4 x.3)) 
                        (let ([x.5 2] [x.6 (bitwise-and 1 2)]) (!= x.5 x.6))
                        (let ([m.1 y.2] [m.2 (call L.addup1.1 1 2 3)]) (false)))
                    x.3
                    (let ([r.1 5] [r.2 y.2]) (bitwise-and r.1 r.2)))))
            (let ([x.1 5] [b.2 6] [c.3 (call L.addup1.1 x.1 b.2 c.3)]) (call L.addup1.1 x.1 b.2 c.3)))])
  (test-case "Simple case binding in pred -- bitwise-and"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (if (if (begin
                                          (set! x.4 x.3)
                                          (set! q.1 (call L.addup1.1 x.3 2 1))
                                          (< x.4 x.3))
                                      (begin (set! x.5 2) (set! x.6 (bitwise-and 1 2)) (!= x.5 x.6))
                                      (begin (set! m.1 y.2) (set! m.2 (call L.addup1.1 1 2 3)) (false)))
                                  x.3
                                  (begin (set! r.1 5) (set! r.2 y.2) (bitwise-and r.1 r.2)))))
                            (begin
                              (set! x.1 5)
                              (set! b.2 6)
                              (set! c.3 (call L.addup1.1 x.1 b.2 c.3))
                              (call L.addup1.1 x.1 b.2 c.3))))))




(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (if (if (let ([x.4 x.3] [q.1 (call L.addup1.1 x.3 2 1)]) (< x.4 x.3)) 
                        (let ([x.5 2] [x.6 (bitwise-ior 1 2)]) (!= x.5 x.6))
                        (let ([m.1 y.2] [m.2 (call L.addup1.1 1 2 3)]) (false)))
                    x.3
                    (let ([r.1 5] [r.2 y.2]) (bitwise-ior r.1 r.2)))))
            (let ([x.1 5] [b.2 6] [c.3 (call L.addup1.1 x.1 b.2 c.3)]) (call L.addup1.1 x.1 b.2 c.3)))])
  (test-case "Simple case binding in pred -- bitwise-ior"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (if (if (begin
                                          (set! x.4 x.3)
                                          (set! q.1 (call L.addup1.1 x.3 2 1))
                                          (< x.4 x.3))
                                      (begin (set! x.5 2) (set! x.6 (bitwise-ior 1 2)) (!= x.5 x.6))
                                      (begin (set! m.1 y.2) (set! m.2 (call L.addup1.1 1 2 3)) (false)))
                                  x.3
                                  (begin (set! r.1 5) (set! r.2 y.2) (bitwise-ior r.1 r.2)))))
                            (begin
                              (set! x.1 5)
                              (set! b.2 6)
                              (set! c.3 (call L.addup1.1 x.1 b.2 c.3))
                              (call L.addup1.1 x.1 b.2 c.3))))))


(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (if (if (let ([x.4 x.3] [q.1 (call L.addup1.1 x.3 2 1)]) (< x.4 x.3)) 
                        (let ([x.5 2] [x.6 (bitwise-xor 1 2)]) (!= x.5 x.6))
                        (let ([m.1 y.2] [m.2 (call L.addup1.1 1 2 3)]) (false)))
                    x.3
                    (let ([r.1 5] [r.2 y.2]) (bitwise-xor r.1 r.2)))))
            (let ([x.1 5] [b.2 6] [c.3 (call L.addup1.1 x.1 b.2 c.3)]) (call L.addup1.1 x.1 b.2 c.3)))])
  (test-case "Simple case binding in pred -- bitwise-xor"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (if (if (begin
                                          (set! x.4 x.3)
                                          (set! q.1 (call L.addup1.1 x.3 2 1))
                                          (< x.4 x.3))
                                      (begin (set! x.5 2) (set! x.6 (bitwise-xor 1 2)) (!= x.5 x.6))
                                      (begin (set! m.1 y.2) (set! m.2 (call L.addup1.1 1 2 3)) (false)))
                                  x.3
                                  (begin (set! r.1 5) (set! r.2 y.2) (bitwise-xor r.1 r.2)))))
                            (begin
                              (set! x.1 5)
                              (set! b.2 6)
                              (set! c.3 (call L.addup1.1 x.1 b.2 c.3))
                              (call L.addup1.1 x.1 b.2 c.3))))))


(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (if (if (let ([x.4 x.3] [q.1 (call L.addup1.1 x.3 2 1)]) (< x.4 x.3)) 
                        (let ([x.5 2] [x.6 (arithmetic-shift-right 1 2)]) (!= x.5 x.6))
                        (let ([m.1 y.2] [m.2 (call L.addup1.1 1 2 3)]) (false)))
                    x.3
                    (let ([r.1 5] [r.2 y.2]) (arithmetic-shift-right r.1 r.2)))))
            (let ([x.1 5] [b.2 6] [c.3 (call L.addup1.1 x.1 b.2 c.3)]) (call L.addup1.1 x.1 b.2 c.3)))])
  (test-case "Simple case binding in pred -- arithmetic-shift-right"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (if (if (begin
                                          (set! x.4 x.3)
                                          (set! q.1 (call L.addup1.1 x.3 2 1))
                                          (< x.4 x.3))
                                      (begin (set! x.5 2) (set! x.6 (arithmetic-shift-right 1 2)) (!= x.5 x.6))
                                      (begin (set! m.1 y.2) (set! m.2 (call L.addup1.1 1 2 3)) (false)))
                                  x.3
                                  (begin (set! r.1 5) (set! r.2 y.2) (arithmetic-shift-right r.1 r.2)))))
                            (begin
                              (set! x.1 5)
                              (set! b.2 6)
                              (set! c.3 (call L.addup1.1 x.1 b.2 c.3))
                              (call L.addup1.1 x.1 b.2 c.3))))))
;; ------------------------------------------------------------------------------------
(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (+ x.3 y.2)))
            (let ([x.1 5] [b.2 6] [c.3 (call L.addup1.1 x.1 b.2 c.3)]) (call L.addup1.1 x.1 b.2 c.3)))])
  (test-case "Simple case binding in proc call"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1 (lambda (x.3 y.2 z.1) (+ x.3 y.2)))
                            (begin
                              (set! x.1 5)
                              (set! b.2 6)
                              (set! c.3 (call L.addup1.1 x.1 b.2 c.3))
                              (call L.addup1.1 x.1 b.2 c.3))))))

(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (+ x.3 y.2)))
            (if (let ([x.1 5] [b.2 (call L.addup1.1 1 2 3)]) (< x.1 b.2)) 
                    (let ([x.1 5] [b.2 6] [c.3 (call L.addup1.1 x.1 b.2 c.3)]) x.1) 
                    (call L.addup1.1 x.1 b.2 c.3)))])
  (test-case "Simple case binding in if"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1 (lambda (x.3 y.2 z.1) (+ x.3 y.2)))
                            (if (begin (set! x.1 5) (set! b.2 (call L.addup1.1 1 2 3)) (< x.1 b.2))
                              (begin
                                (set! x.1 5)
                                (set! b.2 6)
                                (set! c.3 (call L.addup1.1 x.1 b.2 c.3))
                                x.1)
                              (call L.addup1.1 x.1 b.2 c.3))))))

(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (if (if (let ([x.4 x.3] [q.1 (call L.addup1.1 x.3 2 1)]) (< x.4 x.3)) 
                        (let ([x.5 2] [x.6 (- 1 2)]) (!= x.5 x.6))
                        (let ([m.1 y.2] [m.2 (call L.addup1.1 1 2 3)]) (false)))
                    x.3
                    (let ([r.1 5] [r.2 y.2]) (+ r.1 r.2)))))
            (let ([x.1 5] [b.2 6] [c.3 (call L.addup1.1 x.1 b.2 c.3)]) (call L.addup1.1 x.1 b.2 c.3)))])
  (test-case "Simple case binding in pred"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (if (if (begin
                                          (set! x.4 x.3)
                                          (set! q.1 (call L.addup1.1 x.3 2 1))
                                          (< x.4 x.3))
                                      (begin (set! x.5 2) (set! x.6 (- 1 2)) (!= x.5 x.6))
                                      (begin (set! m.1 y.2) (set! m.2 (call L.addup1.1 1 2 3)) (false)))
                                  x.3
                                  (begin (set! r.1 5) (set! r.2 y.2) (+ r.1 r.2)))))
                            (begin
                              (set! x.1 5)
                              (set! b.2 6)
                              (set! c.3 (call L.addup1.1 x.1 b.2 c.3))
                              (call L.addup1.1 x.1 b.2 c.3))))))

(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (+ x.3 y.2)))
            (let ([x.1 (call L.addup1.1 1 2 3)]) (let ([q.1 x.1] [y.3 y.2] [z.2 z.1]) (call L.addup1.1 q.1 y.2 z.2))))])
  (test-case "Simple case binding in nested let"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1 (lambda (x.3 y.2 z.1) (+ x.3 y.2)))
                            (begin
                              (set! x.1 (call L.addup1.1 1 2 3))
                              (begin
                                (set! q.1 x.1)
                                (set! y.3 y.2)
                                (set! z.2 z.1)
                                (call L.addup1.1 q.1 y.2 z.2)))))))


(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (+ x.3 y.2)))
            (if (true) 
                (if (true) 
                  (let ([x.2 5] [x.3 (call L.addup1.1 1 2 3)]) (+ x.2 x.3))
                  (let ([x.4 6] [x.5 (call L.addup1.1 3 2 1)]) (- x.4 x.5)))
                (if (false)
                  1
                  2)))])
  (test-case "Simple case binding in nested if"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1 (lambda (x.3 y.2 z.1) (+ x.3 y.2)))
                            (if (true)
                              (if (true)
                                (begin (set! x.2 5) (set! x.3 (call L.addup1.1 1 2 3)) (+ x.2 x.3))
                                (begin (set! x.4 6) (set! x.5 (call L.addup1.1 3 2 1)) (- x.4 x.5)))
                              (if (false) 1 2))))))

;; General cases
(let ([x `(module
              (define L.addup1.1
                (lambda (x.3 y.2 z.1)
                  (let ((x.6 (call L.addup2.2 x.3 y.2 z.1))
                        (y.5 (call L.addup2.2 x.3 y.2 z.1))
                        (z.4 z.1))
                    (+ z.4 y.5))))
              (define L.addup2.2
                (lambda (x.9 y.8 z.7)
                  (let ((addup1.12 x.9)
                        (x.11 L.addup1.1)
                        (z.10 (call L.addup1.1 x.9 y.8 z.7)))
                    (call x.11 addup1.12 y.8 z.10))))
              (define L.addup3.3
                (lambda (x.15 y.14 z.13)
                  (let ((x.17 L.addup1.1) (y.16 L.addup2.2))
                    (let ((x.19 x.17) (q.18 y.16)) (call q.18 1 2 3)))))
              (call L.addup3.3 5 6 7))])
  (test-case "General cases"
             (check-equal? (sequentialize-let x)
                           `(module
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
                            (call L.addup3.3 5 6 7)))))

(let ([x `(module
              (define L.addup.1
                (lambda (x.3 y.2 z.1)
                  (let ((x.4 x.3))
                    (let ((x.7 x.4) (y.6 y.2) (z.5 z.1)) (call L.addup2.2 x.7 y.6 z.5)))))
              (define L.addup2.2
                (lambda (x.10 y.9 z.8)
                  (let ((x.13 z.8) (y.12 x.10) (z.11 y.9))
                    (if (< x.13 z.11)
                      (+ x.13 y.12)
                      (let ((x.14 3)) (call L.addup3.3 x.14))))))
              (define L.addup3.3 (lambda (x.15) (+ x.15 3)))
              (call L.addup.1 1 2 3))])
  (test-case "General cases"
             (check-equal? (sequentialize-let x)
                           `(module
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
                            (call L.addup.1 1 2 3)))))


;; 3 more tests from uniquify
(let ([x `(module
                (define L.addup1.1 (lambda (x.3 y.2 z.1) (call L.addup3.3 x.3 y.2 z.1)))
                (define L.addup2.2
                  (lambda (x.6 y.5 z.4)
                    (if (if (let ((x.9 (call L.addup1.1 1 2 3)) (y.8 2) (z.7 3)) (< x.9 y.8))
                          (if (true)
                            (false)
                            (let ((x.11 x.6) (y.10 y.5)) (not (!= x.11 y.10))))
                          (let ((x.13 2) (z.12 3)) (> x.13 z.12)))
                      (call L.addup1.1 x.6 y.5 z.4)
                      (let ((x.16 1) (y.15 2) (z.14 (call L.addup1.1 x.6 y.5 z.4)))
                        (call L.addup1.1 x.16 y.15 z.14)))))
                (define L.addup3.3
                  (lambda (x.19 y.18 z.17)
                    (let ((x.21 1) (y.20 (let ((x.23 1) (y.22 2)) (+ x.23 y.22))))
                      (if (if (let ((x.24 (let ((y.25 1)) y.25))) (< x.24 5))
                            (let ((x.27 2) (q.26 (+ x.21 1))) (> x.27 q.26))
                            (not (let ((x.29 1) (y.28 2)) (!= y.28 2))))
                        (let ((x.31 (call L.addup2.2 x.21 y.20 z.17)) (y.30 y.20))
                          (call L.addup2.2 x.31 y.30 z.17))
                        (let ((z.32 (+ x.21 y.20))) (call L.addup1.1 y.20 z.32 x.21))))))
                (let ((x.34 1)
                      (y.33
                      (if (let ((z.36 3) (q.35 (- 1 2))) (!= z.36 q.35))
                        (let ((z.38 1) (q.37 2)) (+ q.37 z.38))
                        (let ((x.39 (call L.addup1.1 0 1 2))) (+ 1 x.39)))))
                  (call L.addup3.3 x.34 y.33 x.34)))])
  (test-case "General cases"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1 (lambda (x.3 y.2 z.1) (call L.addup3.3 x.3 y.2 z.1)))
                            (define L.addup2.2
                              (lambda (x.6 y.5 z.4)
                                (if (if (begin
                                          (set! x.9 (call L.addup1.1 1 2 3))
                                          (set! y.8 2)
                                          (set! z.7 3)
                                          (< x.9 y.8))
                                      (if (true)
                                        (false)
                                        (begin (set! x.11 x.6) (set! y.10 y.5) (not (!= x.11 y.10))))
                                      (begin (set! x.13 2) (set! z.12 3) (> x.13 z.12)))
                                  (call L.addup1.1 x.6 y.5 z.4)
                                  (begin
                                    (set! x.16 1)
                                    (set! y.15 2)
                                    (set! z.14 (call L.addup1.1 x.6 y.5 z.4))
                                    (call L.addup1.1 x.16 y.15 z.14)))))
                            (define L.addup3.3
                              (lambda (x.19 y.18 z.17)
                                (begin
                                  (set! x.21 1)
                                  (set! y.20 (begin (set! x.23 1) (set! y.22 2) (+ x.23 y.22)))
                                  (if (if (begin (set! x.24 (begin (set! y.25 1) y.25)) (< x.24 5))
                                        (begin (set! x.27 2) (set! q.26 (+ x.21 1)) (> x.27 q.26))
                                        (not (begin (set! x.29 1) (set! y.28 2) (!= y.28 2))))
                                    (begin
                                      (set! x.31 (call L.addup2.2 x.21 y.20 z.17))
                                      (set! y.30 y.20)
                                      (call L.addup2.2 x.31 y.30 z.17))
                                    (begin
                                      (set! z.32 (+ x.21 y.20))
                                      (call L.addup1.1 y.20 z.32 x.21))))))
                            (begin
                              (set! x.34 1)
                              (set! y.33
                                (if (begin (set! z.36 3) (set! q.35 (- 1 2)) (!= z.36 q.35))
                                  (begin (set! z.38 1) (set! q.37 2) (+ q.37 z.38))
                                  (begin (set! x.39 (call L.addup1.1 0 1 2)) (+ 1 x.39))))
                              (call L.addup3.3 x.34 y.33 x.34))))))


(let ([x `(module
                  (define L.addup1.1 (lambda (x.3 y.2 z.1) (call L.addup3.3 x.3 y.2 z.1)))
                  (define L.addup2.2
                    (lambda (x.6 y.5 z.4)
                      (let ((x.8 1)
                            (y.7
                            (if (let ((z.10 (call L.addup1.1 x.6 y.5 z.4)) (q.9 (+ 1 2)))
                                  (!= z.10 q.9))
                              (let ((z.12 1) (q.11 (call L.addup1.1 x.6 y.5 z.4)))
                                (+ q.11 z.12))
                              (let ((x.13 10)) (+ 1 x.13)))))
                        (call L.addup1.1 x.8 y.7 z.4))))
                  (define L.addup3.3
                    (lambda (x.16 y.15 z.14)
                      (let ((x.18 1) (y.17 2))
                        (let ((q.20 (call L.addup2.2 x.18 y.17 z.14)) (z.19 y.17))
                          (let ((w.22 L.addup2.2) (z.21 (call L.addup2.2 q.20 z.19 y.17)))
                            (if (< w.22 q.20)
                              (call w.22 x.18 y.17 z.21)
                              (call L.addup1.1 q.20 z.21 x.18)))))))
                  (let ((x.25 1) (y.24 2) (z.23 3)) (call L.addup2.2 3 2 1)))])
  (test-case "General cases"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1 (lambda (x.3 y.2 z.1) (call L.addup3.3 x.3 y.2 z.1)))
                            (define L.addup2.2
                              (lambda (x.6 y.5 z.4)
                                (begin
                                  (set! x.8 1)
                                  (set! y.7
                                    (if (begin
                                          (set! z.10 (call L.addup1.1 x.6 y.5 z.4))
                                          (set! q.9 (+ 1 2))
                                          (!= z.10 q.9))
                                      (begin
                                        (set! z.12 1)
                                        (set! q.11 (call L.addup1.1 x.6 y.5 z.4))
                                        (+ q.11 z.12))
                                      (begin (set! x.13 10) (+ 1 x.13))))
                                  (call L.addup1.1 x.8 y.7 z.4))))
                            (define L.addup3.3
                              (lambda (x.16 y.15 z.14)
                                (begin
                                  (set! x.18 1)
                                  (set! y.17 2)
                                  (begin
                                    (set! q.20 (call L.addup2.2 x.18 y.17 z.14))
                                    (set! z.19 y.17)
                                    (begin
                                      (set! w.22 L.addup2.2)
                                      (set! z.21 (call L.addup2.2 q.20 z.19 y.17))
                                      (if (< w.22 q.20)
                                        (call w.22 x.18 y.17 z.21)
                                        (call L.addup1.1 q.20 z.21 x.18)))))))
                            (begin (set! x.25 1) (set! y.24 2) (set! z.23 3) (call L.addup2.2 3 2 1))))))


(let ([x `(module
                  (define L.addup1.1
                    (lambda (x.3 y.2 z.1) (let ((x.6 x.3) (y.5 y.2) (z.4 z.1)) (+ z.4 y.5))))
                  (define L.addup2.2
                    (lambda (x.9 y.8 z.7)
                      (if (if (let ((x.12 (call L.addup2.2 x.9 y.8 z.7)) (y.11 2) (z.10 3))
                                (< x.12 y.11))
                            (if (true)
                              (false)
                              (let ((x.14 x.9) (y.13 y.8)) (not (!= x.14 y.13))))
                            (let ((x.16 2) (z.15 3)) (> x.16 z.15)))
                        (call L.addup1.1 x.9 y.8 z.7)
                        (let ((x.19 1) (y.18 (call L.addup3.3 x.9 y.8 z.7)) (z.17 6))
                          (call L.addup1.1 x.19 y.18 z.17)))))
                  (define L.addup3.3
                    (lambda (x.22 y.21 z.20)
                      (let ((x.24 1) (y.23 (let ((z.25 3)) z.25)))
                        (call L.addup2.2 x.24 y.23 z.20))))
                  (let ((x.28 1) (y.27 2) (z.26 3)) (call L.addup3.3 x.28 y.27 z.26)))])
  (test-case "General cases"
             (check-equal? (sequentialize-let x)
                           `(module
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
                              (call L.addup3.3 x.28 y.27 z.26))))))
;; ------------------------------------- old cases -----------------------------------------
(let ([x '(module
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) (+ z.4 y.5)))
            (call L.addup1.1 5 6 7))])
  (test-case "Simple case with no binding in proc declaration"
             (check-equal? (sequentialize-let x)
                           `(module
                              (define L.addup1.1 (lambda (x.3 y.2 z.1) (+ z.4 y.5)))
                              (call L.addup1.1 5 6 7)))))
(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (+ x.3 y.2)))
            (let ([x.1 5] [b.2 6] [c.3 7]) (call L.addup1.1 x.1 b.2 c.3)))])
  (test-case "Simple case binding in proc call"
             (check-equal? (sequentialize-let x)
                           `(module
                              (define L.addup1.1 (lambda (x.3 y.2 z.1) (+ x.3 y.2)))
                              (begin (set! x.1 5) (set! b.2 6) (set! c.3 7) (call L.addup1.1 x.1 b.2 c.3))))))

(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (let ([x.1 x.3] [y.1 y.2]) (+ x.1 y.1))))
            (let ([x.1 5] [b.2 6] [c.3 7]) (call L.addup1.1 x.1 b.2 c.3)))])
  (test-case "Simple case let binding in proc declaration"
             (check-equal? (sequentialize-let x)
                           `(module
                              (define L.addup1.1
                                (lambda (x.3 y.2 z.1) (begin (set! x.1 x.3) (set! y.1 y.2) (+ x.1 y.1))))
                              (begin (set! x.1 5) (set! b.2 6) (set! c.3 7) (call L.addup1.1 x.1 b.2 c.3))))))

(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (let ([x.1 (+ x.3 y.2)] [y.2 (+ y.2 z.1)]) (+ x.1 y.1))))
            (let ([x.1 5] [b.2 6] [c.3 7]) (call L.addup1.1 x.1 b.2 c.3)))])
  (test-case "Simple case let binding in proc declaration"
             (check-equal? (sequentialize-let x)
                           `(module
                              (define L.addup1.1
                                (lambda (x.3 y.2 z.1)
                                  (begin (set! x.1 (+ x.3 y.2)) (set! y.2 (+ y.2 z.1)) (+ x.1 y.1))))
                              (begin (set! x.1 5) (set! b.2 6) (set! c.3 7) (call L.addup1.1 x.1 b.2 c.3))))))

(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) (if (let ([q.1 x.3] [m.1 z.1]) (< q.1 m.1)) (+ x.3 y.1) (+ x.3 z.1))))
            (call L.addup1.1 5 6 7))])
  (test-case "Simple case if binding in proc declaration"
             (check-equal? (sequentialize-let x)
                           `(module
                              (define L.addup1.1
                                (lambda (x.3 y.2 z.1)
                                  (if (begin (set! q.1 x.3) (set! m.1 z.1) (< q.1 m.1))
                                    (+ x.3 y.1)
                                    (+ x.3 z.1))))
                              (call L.addup1.1 5 6 7)))))

(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (let ([n.7 z.1]) (let ([q.4 x.3] [m.6 y.2]) (+ n.1 m.6)))))
            (call L.addup1.1 5 6 7))])
  (test-case "Simple case nested binding in proc declaration"
             (check-equal? (sequentialize-let x)
                           `(module
                              (define L.addup1.1
                                (lambda (x.3 y.2 z.1)
                                  (begin
                                    (set! n.7 z.1)
                                    (begin (set! q.4 x.3) (set! m.6 y.2) (+ n.1 m.6)))))
                              (call L.addup1.1 5 6 7)))))

(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (let ([n.7 (let ([q.4 x.3] [m.6 y.2]) (+ n.1 m.6))]) n.7)))
            (call L.addup1.1 5 6 7))])
  (test-case "Simple case nested let binding in proc declaration"
             (check-equal? (sequentialize-let x)
                           `(module
                              (define L.addup1.1
                                (lambda (x.3 y.2 z.1)
                                  (begin
                                    (set! n.7 (begin (set! q.4 x.3) (set! m.6 y.2) (+ n.1 m.6)))
                                    n.7)))
                              (call L.addup1.1 5 6 7)))))

(let ([x  `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (if (let ([x.1 x.3] [y.3 y.2] [z.3 (+ x.3 y.2)]) (< x.1 z.3)) 
                    (let ([q.1 3] [m.1 y.2]) (* q.1 m.1))
                    (let ([q.2 (* x.3 z.1)] [m.2 (let ([q.3 z.1]) q.3)]) (+ q.2 m.2)))))
            (call L.addup1.1 5 6 7))])
  (test-case "Simple case let nested if binding in proc declaration"
             (check-equal? (sequentialize-let x)
                           `(module
                              (define L.addup1.1
                                (lambda (x.3 y.2 z.1)
                                  (if (begin
                                        (set! x.1 x.3)
                                        (set! y.3 y.2)
                                        (set! z.3 (+ x.3 y.2))
                                        (< x.1 z.3))
                                    (begin (set! q.1 3) (set! m.1 y.2) (* q.1 m.1))
                                    (begin
                                      (set! q.2 (* x.3 z.1))
                                      (set! m.2 (begin (set! q.3 z.1) q.3))
                                      (+ q.2 m.2)))))
                              (call L.addup1.1 5 6 7)))))

(let ([x `(module 
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) 
                (let ([n.7 (if (true) (let ([x.4 y.2] [q.5 y.2]) (+ x.4 q.5)) (+ x.3 y.2))]) n.7)))
            (call L.addup1.1 5 6 7))])
  (test-case "Simple case if nested let binding in proc declaration"
             (check-equal? (sequentialize-let x)
                           `(module
                              (define L.addup1.1
                                (lambda (x.3 y.2 z.1)
                                  (begin
                                    (set! n.7
                                      (if (true)
                                        (begin (set! x.4 y.2) (set! q.5 y.2) (+ x.4 q.5))
                                        (+ x.3 y.2)))
                                    n.7)))
                              (call L.addup1.1 5 6 7)))))
;; General cases x10
(let ([x `(module
          (define L.addup1.1
            (lambda (x.3 y.2 z.1) (let ((x.6 x.3) (y.5 y.2) (z.4 z.1)) (+ z.4 y.5))))
          (define L.addup2.2 (lambda (x.9 y.8 z.7) (let ([y.8 2] [z.7 (let ([x.1 2]) x.1)]) (call L.addup1.1 x.9 y.8 z.7))))
          (define L.addup3.3
            (lambda (x.12 y.11 z.10) (let ([y.11 1] [z.10 2] [x.12 y.11]) (call L.addup2.2 y.11 z.10 x.12))))
          (call L.addup3.3 5 6 7))])
  (test-case "Chained calls"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1
                              (lambda (x.3 y.2 z.1)
                                (begin (set! x.6 x.3) (set! y.5 y.2) (set! z.4 z.1) (+ z.4 y.5))))
                            (define L.addup2.2
                              (lambda (x.9 y.8 z.7)
                                (begin
                                  (set! y.8 2)
                                  (set! z.7 (begin (set! x.1 2) x.1))
                                  (call L.addup1.1 x.9 y.8 z.7))))
                            (define L.addup3.3
                              (lambda (x.12 y.11 z.10)
                                (begin
                                  (set! y.11 1)
                                  (set! z.10 2)
                                  (set! x.12 y.11)
                                  (call L.addup2.2 y.11 z.10 x.12))))
                            (call L.addup3.3 5 6 7)))))
                              
(let ([x '(module
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) (let ((x.6 x.3) (y.5 y.2) (z.4 z.1)) (+ z.4 y.5))))
            (define L.addup2.2
              (lambda (x.9 y.8 z.7)
                (let ((addup1.11 x.9) (x.10 L.addup1.1)) (call x.10 addup1.11 y.8 z.7))))
            (define L.addup3.3
              (lambda (x.14 y.13 z.12)
                (let ((x.16 L.addup1.1) (y.15 L.addup2.2))
                  (let ((x.18 x.16) (q.17 y.15)) (call q.17 1 2 3)))))
            (call L.addup3.3 5 6 7))])
  (test-case "Nested lets calls"
             (check-equal? (sequentialize-let x)
                           `(module
                              (define L.addup1.1
                                (lambda (x.3 y.2 z.1)
                                  (begin (set! x.6 x.3) (set! y.5 y.2) (set! z.4 z.1) (+ z.4 y.5))))
                              (define L.addup2.2
                                (lambda (x.9 y.8 z.7)
                                  (begin
                                    (set! addup1.11 x.9)
                                    (set! x.10 L.addup1.1)
                                    (call x.10 addup1.11 y.8 z.7))))
                              (define L.addup3.3
                                (lambda (x.14 y.13 z.12)
                                  (begin
                                    (set! x.16 L.addup1.1)
                                    (set! y.15 L.addup2.2)
                                    (begin (set! x.18 x.16) (set! q.17 y.15) (call q.17 1 2 3)))))
                              (call L.addup3.3 5 6 7)))))

(let ([x '(module
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) (let ((x.6 x.3) (y.5 y.2) (z.4 z.1)) (+ z.4 y.5))))
            (define L.addup2.2
              (lambda (x.9 y.8 z.7)
                (let ((x.11 1)
                      (y.10
                      (if (let ((z.13 3) (q.12 (+ 1 2))) (!= z.13 q.12))
                        (let ((z.15 1) (q.14 2)) (+ q.14 z.15))
                        (let ((x.16 10)) (+ 1 x.16)))))
                  (+ x.11 y.10))))
            (define L.addup3.3
              (lambda (x.19 y.18 z.17)
                (let ((x.21 L.addup1.1) (y.20 L.addup2.2))
                  (if (false) (call x.21 1 2 3) (call y.20 3 4 5)))))
            (let ((x.23 L.addup3.3) (y.22 L.addup2.2))
              (if (< 1 2) (call x.23 1 2 3) (call L.addup2.2 2 3 4))))])
  (test-case "General cases"
             (check-equal? (sequentialize-let x)
                           `(module
                              (define L.addup1.1
                                (lambda (x.3 y.2 z.1)
                                  (begin (set! x.6 x.3) (set! y.5 y.2) (set! z.4 z.1) (+ z.4 y.5))))
                              (define L.addup2.2
                                (lambda (x.9 y.8 z.7)
                                  (begin
                                    (set! x.11 1)
                                    (set! y.10
                                      (if (begin (set! z.13 3) (set! q.12 (+ 1 2)) (!= z.13 q.12))
                                        (begin (set! z.15 1) (set! q.14 2) (+ q.14 z.15))
                                        (begin (set! x.16 10) (+ 1 x.16))))
                                    (+ x.11 y.10))))
                              (define L.addup3.3
                                (lambda (x.19 y.18 z.17)
                                  (begin
                                    (set! x.21 L.addup1.1)
                                    (set! y.20 L.addup2.2)
                                    (if (false) (call x.21 1 2 3) (call y.20 3 4 5)))))
                              (begin
                                (set! x.23 L.addup3.3)
                                (set! y.22 L.addup2.2)
                                (if (< 1 2) (call x.23 1 2 3) (call L.addup2.2 2 3 4)))))))

(let ([x '(module
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) (let ((x.6 x.3) (y.5 y.2) (z.4 z.1)) (+ z.4 y.5))))
            (define L.addup2.2
              (lambda (x.9 y.8 z.7)
                (if (let ((x.11 1) (y.10 2)) (not (!= x.11 y.10)))
                  (if (false)
                    (let ((x.13 1) (y.12 2))
                      (let ((x.15 x.13) (y.14 y.12)) (+ x.15 y.14)))
                    (let ((x.17 1) (y.16 2)) (+ x.17 y.16)))
                  (if (true)
                    (let ((x.19 1) (y.18 2))
                      (let ((z.21 (+ x.19 y.18)) (x.20 (+ y.18 y.18))) (+ x.20 z.21)))
                    (let ((z.22 3)) (+ z.22 z.22))))))
            (define L.addup3.3
              (lambda (x.25 y.24 z.23)
                (if (not (< x.25 y.24))
                  (let ((addup2.27 x.25) (x.26 L.addup2.2)) (call x.26 addup2.27 2 1))
                  (call L.addup1.1 2 3 4))))
            (let ((x.29 L.addup3.3) (y.28 L.addup2.2))
              (let ((addup3.31 5) (addup2.30 7)) (+ addup3.31 addup2.30))))])
  (test-case "General cases"
             (check-equal? (sequentialize-let x)
                           `(module
                              (define L.addup1.1
                                (lambda (x.3 y.2 z.1)
                                  (begin (set! x.6 x.3) (set! y.5 y.2) (set! z.4 z.1) (+ z.4 y.5))))
                              (define L.addup2.2
                                (lambda (x.9 y.8 z.7)
                                  (if (begin (set! x.11 1) (set! y.10 2) (not (!= x.11 y.10)))
                                    (if (false)
                                      (begin
                                        (set! x.13 1)
                                        (set! y.12 2)
                                        (begin (set! x.15 x.13) (set! y.14 y.12) (+ x.15 y.14)))
                                      (begin (set! x.17 1) (set! y.16 2) (+ x.17 y.16)))
                                    (if (true)
                                      (begin
                                        (set! x.19 1)
                                        (set! y.18 2)
                                        (begin
                                          (set! z.21 (+ x.19 y.18))
                                          (set! x.20 (+ y.18 y.18))
                                          (+ x.20 z.21)))
                                      (begin (set! z.22 3) (+ z.22 z.22))))))
                              (define L.addup3.3
                                (lambda (x.25 y.24 z.23)
                                  (if (not (< x.25 y.24))
                                    (begin
                                      (set! addup2.27 x.25)
                                      (set! x.26 L.addup2.2)
                                      (call x.26 addup2.27 2 1))
                                    (call L.addup1.1 2 3 4))))
                              (begin
                                (set! x.29 L.addup3.3)
                                (set! y.28 L.addup2.2)
                                (begin (set! addup3.31 5) (set! addup2.30 7) (+ addup3.31 addup2.30)))))))


(let ([x '(module
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) (let ((x.6 x.3) (y.5 y.2) (z.4 z.1)) (+ z.4 y.5))))
            (define L.addup2.2
              (lambda (x.9 y.8 z.7)
                (let ((x.12 1)
                      (y.11 2)
                      (z.10
                      (let ((x.15 1)
                            (y.14 2)
                            (z.13 (let ((x.18 2) (y.17 7) (z.16 3)) (+ x.18 y.17))))
                        (+ x.15 z.13))))
                  (call L.addup1.1 x.12 y.11 z.10))))
            (define L.addup3.3
              (lambda (x.21 y.20 z.19)
                (if (not (if (true) (< x.21 y.20) (> y.20 z.19)))
                  (let ((x.23 x.21) (y.22 2)) (call L.addup2.2 x.23 2 1))
                  (call L.addup1.1 2 y.20 4))))
            (let ((x.26 1) (y.25 2) (z.24 3)) (call L.addup3.3 x.26 y.25 z.24)))])
  (test-case "General cases"
             (check-equal? (sequentialize-let x)
                           `(module
                              (define L.addup1.1
                                (lambda (x.3 y.2 z.1)
                                  (begin (set! x.6 x.3) (set! y.5 y.2) (set! z.4 z.1) (+ z.4 y.5))))
                              (define L.addup2.2
                                (lambda (x.9 y.8 z.7)
                                  (begin
                                    (set! x.12 1)
                                    (set! y.11 2)
                                    (set! z.10
                                      (begin
                                        (set! x.15 1)
                                        (set! y.14 2)
                                        (set! z.13
                                          (begin (set! x.18 2) (set! y.17 7) (set! z.16 3) (+ x.18 y.17)))
                                        (+ x.15 z.13)))
                                    (call L.addup1.1 x.12 y.11 z.10))))
                              (define L.addup3.3
                                (lambda (x.21 y.20 z.19)
                                  (if (not (if (true) (< x.21 y.20) (> y.20 z.19)))
                                    (begin (set! x.23 x.21) (set! y.22 2) (call L.addup2.2 x.23 2 1))
                                    (call L.addup1.1 2 y.20 4))))
                              (begin
                                (set! x.26 1)
                                (set! y.25 2)
                                (set! z.24 3)
                                (call L.addup3.3 x.26 y.25 z.24))))))

(let ([x '(module
            (define L.addup1.1
              (lambda (x.3 y.2 z.1) (let ((x.6 x.3) (y.5 y.2) (z.4 z.1)) (+ z.4 y.5))))
            (define L.addup2.2
              (lambda (x.9 y.8 z.7)
                (if (if (let ((x.12 1) (y.11 2) (z.10 3)) (< x.12 y.11))
                      (if (true)
                        (false)
                        (let ((x.14 x.9) (y.13 y.8)) (not (!= x.14 y.13))))
                      (let ((x.16 2) (z.15 3)) (> x.16 z.15)))
                  (call L.addup1.1 x.9 y.8 z.7)
                  (let ((x.19 1) (y.18 2) (z.17 6)) (call L.addup1.1 x.19 y.18 z.17)))))
            (define L.addup3.3
              (lambda (x.22 y.21 z.20)
                (let ((x.24 1) (y.23 (let ((z.25 3)) z.25)))
                  (call L.addup2.2 x.24 y.23 z.20))))
            (let ((x.28 1) (y.27 2) (z.26 3)) (call L.addup3.3 x.28 y.27 z.26)))])
  (test-case "General cases"
             (check-equal? (sequentialize-let x)
                           `(module
                              (define L.addup1.1
                                (lambda (x.3 y.2 z.1)
                                  (begin (set! x.6 x.3) (set! y.5 y.2) (set! z.4 z.1) (+ z.4 y.5))))
                              (define L.addup2.2
                                (lambda (x.9 y.8 z.7)
                                  (if (if (begin (set! x.12 1) (set! y.11 2) (set! z.10 3) (< x.12 y.11))
                                        (if (true)
                                          (false)
                                          (begin (set! x.14 x.9) (set! y.13 y.8) (not (!= x.14 y.13))))
                                        (begin (set! x.16 2) (set! z.15 3) (> x.16 z.15)))
                                    (call L.addup1.1 x.9 y.8 z.7)
                                    (begin
                                      (set! x.19 1)
                                      (set! y.18 2)
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
                                (call L.addup3.3 x.28 y.27 z.26))))))

(let ([x '(module
            (define L.addup1.1 (lambda (x.3 y.2 z.1) (call L.addup3.3 x.3 y.2 z.1)))
            (define L.addup2.2
              (lambda (x.6 y.5 z.4)
                (if (if (let ((x.9 1) (y.8 2) (z.7 3)) (< x.9 y.8))
                      (if (true)
                        (false)
                        (let ((x.11 x.6) (y.10 y.5)) (not (!= x.11 y.10))))
                      (let ((x.13 2) (z.12 3)) (> x.13 z.12)))
                  (call L.addup1.1 x.6 y.5 z.4)
                  (let ((x.16 1) (y.15 2) (z.14 6)) (call L.addup1.1 x.16 y.15 z.14)))))
            (define L.addup3.3
              (lambda (x.19 y.18 z.17)
                (let ((x.21 1) (y.20 (let ((x.23 1) (y.22 2)) (+ x.23 y.22))))
                  (if (if (let ((x.24 (let ((y.25 1)) y.25))) (< x.24 5))
                        (let ((x.27 2) (q.26 (+ x.21 1))) (> x.27 q.26))
                        (not (let ((x.29 1) (y.28 2)) (!= y.28 2))))
                    (let ((x.31 x.21) (y.30 y.20)) (call L.addup2.2 x.31 y.30 z.17))
                    (let ((z.32 (+ x.21 y.20))) (call L.addup1.1 y.20 z.32 x.21))))))
            (let ((x.34 1)
                  (y.33
                  (if (let ((z.36 3) (q.35 (+ 1 2))) (!= z.36 q.35))
                    (let ((z.38 1) (q.37 2)) (+ q.37 z.38))
                    (let ((x.39 10)) (+ 1 x.39)))))
              (call L.addup3.3 x.34 y.33 x.34)))])
  (test-case "General cases"
             (check-equal? (sequentialize-let x)
                           `(module
                              (define L.addup1.1 (lambda (x.3 y.2 z.1) (call L.addup3.3 x.3 y.2 z.1)))
                              (define L.addup2.2
                                (lambda (x.6 y.5 z.4)
                                  (if (if (begin (set! x.9 1) (set! y.8 2) (set! z.7 3) (< x.9 y.8))
                                        (if (true)
                                          (false)
                                          (begin (set! x.11 x.6) (set! y.10 y.5) (not (!= x.11 y.10))))
                                        (begin (set! x.13 2) (set! z.12 3) (> x.13 z.12)))
                                    (call L.addup1.1 x.6 y.5 z.4)
                                    (begin
                                      (set! x.16 1)
                                      (set! y.15 2)
                                      (set! z.14 6)
                                      (call L.addup1.1 x.16 y.15 z.14)))))
                              (define L.addup3.3
                                (lambda (x.19 y.18 z.17)
                                  (begin
                                    (set! x.21 1)
                                    (set! y.20 (begin (set! x.23 1) (set! y.22 2) (+ x.23 y.22)))
                                    (if (if (begin (set! x.24 (begin (set! y.25 1) y.25)) (< x.24 5))
                                          (begin (set! x.27 2) (set! q.26 (+ x.21 1)) (> x.27 q.26))
                                          (not (begin (set! x.29 1) (set! y.28 2) (!= y.28 2))))
                                      (begin
                                        (set! x.31 x.21)
                                        (set! y.30 y.20)
                                        (call L.addup2.2 x.31 y.30 z.17))
                                      (begin
                                        (set! z.32 (+ x.21 y.20))
                                        (call L.addup1.1 y.20 z.32 x.21))))))
                              (begin
                                (set! x.34 1)
                                (set! y.33
                                  (if (begin (set! z.36 3) (set! q.35 (+ 1 2)) (!= z.36 q.35))
                                    (begin (set! z.38 1) (set! q.37 2) (+ q.37 z.38))
                                    (begin (set! x.39 10) (+ 1 x.39))))
                                (call L.addup3.3 x.34 y.33 x.34))))))

(let ([x '(module
          (define L.addup1.1 (lambda (x.3 y.2 z.1) (call L.addup3.3 x.3 y.2 z.1)))
          (define L.addup2.2
            (lambda (x.6 y.5 z.4)
              (if (if (let ((x.9 1) (y.8 2) (z.7 3)) (< x.9 y.8))
                    (if (true) (false) (let ((x.11 2) (y.10 3)) (not (!= x.11 y.10))))
                    (let ((x.13 2) (z.12 3)) (> x.13 z.12)))
                (call L.addup1.1 x.6 y.5 z.4)
                (let ((x.15 1) (y.14 2)) (call L.addup1.1 x.15 x.15 y.14)))))
          (define L.addup3.3
            (lambda (x.18 y.17 z.16)
              (let ((x.20 1)
                    (y.19
                    (if (let ((z.22 3) (q.21 (+ 1 2))) (!= z.22 q.21))
                      (let ((z.24 1) (q.23 2)) (+ q.23 z.24))
                      (let ((x.25 10)) (+ 1 x.25)))))
                (call L.addup2.2 x.20 y.19 z.16))))
          (let ((x.27 1)
                (y.26
                (if (let ((z.29 3) (q.28 (+ 1 2))) (!= z.29 q.28))
                  (let ((z.31 1) (q.30 2)) (+ q.30 z.31))
                  (let ((x.32 10)) (+ 1 x.32)))))
            (call L.addup3.3 x.27 y.26 x.27)))])
  (test-case "General cases"
             (check-equal? (sequentialize-let x)
                           `(module
                                (define L.addup1.1 (lambda (x.3 y.2 z.1) (call L.addup3.3 x.3 y.2 z.1)))
                                (define L.addup2.2
                                  (lambda (x.6 y.5 z.4)
                                    (if (if (begin (set! x.9 1) (set! y.8 2) (set! z.7 3) (< x.9 y.8))
                                          (if (true)
                                            (false)
                                            (begin (set! x.11 2) (set! y.10 3) (not (!= x.11 y.10))))
                                          (begin (set! x.13 2) (set! z.12 3) (> x.13 z.12)))
                                      (call L.addup1.1 x.6 y.5 z.4)
                                      (begin (set! x.15 1) (set! y.14 2) (call L.addup1.1 x.15 x.15 y.14)))))
                                (define L.addup3.3
                                  (lambda (x.18 y.17 z.16)
                                    (begin
                                      (set! x.20 1)
                                      (set! y.19
                                        (if (begin (set! z.22 3) (set! q.21 (+ 1 2)) (!= z.22 q.21))
                                          (begin (set! z.24 1) (set! q.23 2) (+ q.23 z.24))
                                          (begin (set! x.25 10) (+ 1 x.25))))
                                      (call L.addup2.2 x.20 y.19 z.16))))
                                (begin
                                  (set! x.27 1)
                                  (set! y.26
                                    (if (begin (set! z.29 3) (set! q.28 (+ 1 2)) (!= z.29 q.28))
                                      (begin (set! z.31 1) (set! q.30 2) (+ q.30 z.31))
                                      (begin (set! x.32 10) (+ 1 x.32))))
                                  (call L.addup3.3 x.27 y.26 x.27))))))

(let ([x '(module
  (define L.addup1.1 (lambda (x.3 y.2 z.1) (call L.addup3.3 x.3 y.2 z.1)))
  (define L.addup2.2
    (lambda (x.6 y.5 z.4)
      (if (let ((x.8 1) (y.7 2)) (not (!= x.8 y.7)))
        (if (let ((x.10 1) (y.9 2))
              (if (false) (let ((q.12 x.10) (z.11 y.9)) (= q.12 z.11)) (true)))
          (let ((x.14 1) (y.13 2))
            (let ((x.16 x.14) (y.15 y.13)) (+ x.16 y.15)))
          (let ((x.18 1) (y.17 2)) (call L.addup3.3 x.18 y.17 z.4)))
        (if (if (let ((x.20 1) (y.19 2))
                  (if (< x.20 y.19)
                    (let ((q.22 x.20) (y.21 1)) (!= q.22 y.21))
                    (not (< x.20 y.19))))
              (true)
              (let ((x.24 1) (y.23 2)) (> x.24 y.23)))
          (let ((x.26 1) (y.25 2))
            (let ((z.28 (+ x.26 y.25)) (x.27 (+ y.25 y.25))) (+ x.27 z.28)))
          (let ((z.29 3)) (call L.addup1.1 x.6 y.5 z.29))))))
  (define L.addup3.3
    (lambda (x.32 y.31 z.30)
      (let ((x.34 1) (y.33 2))
        (let ((q.36 x.34) (z.35 y.33))
          (let ((w.38 1) (z.37 2))
            (if (< w.38 q.36) (+ w.38 z.37) (+ y.33 q.36)))))))
  (let ((x.41 1) (y.40 2) (z.39 3)) (call L.addup2.2 x.41 y.40 z.39)))])
  (test-case "General cases"
             (check-equal? (sequentialize-let x)
                           `(module
                              (define L.addup1.1 (lambda (x.3 y.2 z.1) (call L.addup3.3 x.3 y.2 z.1)))
                              (define L.addup2.2
                                (lambda (x.6 y.5 z.4)
                                  (if (begin (set! x.8 1) (set! y.7 2) (not (!= x.8 y.7)))
                                    (if (begin
                                          (set! x.10 1)
                                          (set! y.9 2)
                                          (if (false)
                                            (begin (set! q.12 x.10) (set! z.11 y.9) (= q.12 z.11))
                                            (true)))
                                      (begin
                                        (set! x.14 1)
                                        (set! y.13 2)
                                        (begin (set! x.16 x.14) (set! y.15 y.13) (+ x.16 y.15)))
                                      (begin (set! x.18 1) (set! y.17 2) (call L.addup3.3 x.18 y.17 z.4)))
                                    (if (if (begin
                                              (set! x.20 1)
                                              (set! y.19 2)
                                              (if (< x.20 y.19)
                                                (begin (set! q.22 x.20) (set! y.21 1) (!= q.22 y.21))
                                                (not (< x.20 y.19))))
                                          (true)
                                          (begin (set! x.24 1) (set! y.23 2) (> x.24 y.23)))
                                      (begin
                                        (set! x.26 1)
                                        (set! y.25 2)
                                        (begin
                                          (set! z.28 (+ x.26 y.25))
                                          (set! x.27 (+ y.25 y.25))
                                          (+ x.27 z.28)))
                                      (begin (set! z.29 3) (call L.addup1.1 x.6 y.5 z.29))))))
                              (define L.addup3.3
                                (lambda (x.32 y.31 z.30)
                                  (begin
                                    (set! x.34 1)
                                    (set! y.33 2)
                                    (begin
                                      (set! q.36 x.34)
                                      (set! z.35 y.33)
                                      (begin
                                        (set! w.38 1)
                                        (set! z.37 2)
                                        (if (< w.38 q.36) (+ w.38 z.37) (+ y.33 q.36)))))))
                              (begin
                                (set! x.41 1)
                                (set! y.40 2)
                                (set! z.39 3)
                                (call L.addup2.2 x.41 y.40 z.39))))))

(let ([x '(module
            (define L.addup1.1 (lambda (x.3 y.2 z.1) (call L.addup3.3 x.3 y.2 z.1)))
            (define L.addup2.2
              (lambda (x.6 y.5 z.4)
                (let ((x.8 1)
                      (y.7
                      (if (let ((z.10 3) (q.9 (+ 1 2))) (!= z.10 q.9))
                        (let ((z.12 1) (q.11 2)) (+ q.11 z.12))
                        (let ((x.13 10)) (+ 1 x.13)))))
                  (call L.addup1.1 x.8 y.7 z.4))))
            (define L.addup3.3
              (lambda (x.16 y.15 z.14)
                (let ((x.18 1) (y.17 2))
                  (let ((q.20 x.18) (z.19 y.17))
                    (let ((w.22 L.addup2.2) (z.21 2))
                      (if (< w.22 q.20)
                        (call w.22 x.18 y.17 z.21)
                        (call L.addup1.1 q.20 z.21 x.18)))))))
            (let ((x.25 1) (y.24 2) (z.23 3)) (call L.addup2.2 3 2 1)))])
  (test-case "General cases"
             (check-equal? (sequentialize-let x)
                           `(module
                              (define L.addup1.1 (lambda (x.3 y.2 z.1) (call L.addup3.3 x.3 y.2 z.1)))
                              (define L.addup2.2
                                (lambda (x.6 y.5 z.4)
                                  (begin
                                    (set! x.8 1)
                                    (set! y.7
                                      (if (begin (set! z.10 3) (set! q.9 (+ 1 2)) (!= z.10 q.9))
                                        (begin (set! z.12 1) (set! q.11 2) (+ q.11 z.12))
                                        (begin (set! x.13 10) (+ 1 x.13))))
                                    (call L.addup1.1 x.8 y.7 z.4))))
                              (define L.addup3.3
                                (lambda (x.16 y.15 z.14)
                                  (begin
                                    (set! x.18 1)
                                    (set! y.17 2)
                                    (begin
                                      (set! q.20 x.18)
                                      (set! z.19 y.17)
                                      (begin
                                        (set! w.22 L.addup2.2)
                                        (set! z.21 2)
                                        (if (< w.22 q.20)
                                          (call w.22 x.18 y.17 z.21)
                                          (call L.addup1.1 q.20 z.21 x.18)))))))
                              (begin (set! x.25 1) (set! y.24 2) (set! z.23 3) (call L.addup2.2 3 2 1))))))

;;
(let ([x '(module
            (define L.addup1.1 (lambda (x.3 y.2 z.1) (call L.addup3.3 x.3 y.2 z.1)))
            (define L.addup2.2
              (lambda (x.6 y.5 z.4)
                (let ((x.8 1) (y.7 2))
                  (let ((q.10 x.8) (z.9 y.7))
                    (let ((w.12 L.addup3.3) (z.11 2))
                      (if (< w.12 q.10)
                        (call L.addup1.1 q.10 z.11 x.8)
                        (call w.12 q.10 z.11 x.8)))))))
            (define L.addup3.3
              (lambda (x.15 y.14 z.13)
                (if (let ((x.18 1) (y.17 2) (z.16 3)) (< z.16 2))
                  (let ((x.21 1) (y.20 2) (z.19 3)) (if (< x.21 y.20) x.21 y.20))
                  (let ((x.24 3) (y.23 4) (z.22 5))
                    (let ((x.26 2) (y.25 1)) (call L.addup2.2 x.26 y.25 z.22))))))
            (let ((x.29 1) (y.28 2) (z.27 3)) (call L.addup2.2 x.29 y.28 z.27)))])
  (test-case "General cases"
             (check-equal? (sequentialize-let x)
                           `(module
                            (define L.addup1.1 (lambda (x.3 y.2 z.1) (call L.addup3.3 x.3 y.2 z.1)))
                            (define L.addup2.2
                              (lambda (x.6 y.5 z.4)
                                (begin
                                  (set! x.8 1)
                                  (set! y.7 2)
                                  (begin
                                    (set! q.10 x.8)
                                    (set! z.9 y.7)
                                    (begin
                                      (set! w.12 L.addup3.3)
                                      (set! z.11 2)
                                      (if (< w.12 q.10)
                                        (call L.addup1.1 q.10 z.11 x.8)
                                        (call w.12 q.10 z.11 x.8)))))))
                            (define L.addup3.3
                              (lambda (x.15 y.14 z.13)
                                (if (begin (set! x.18 1) (set! y.17 2) (set! z.16 3) (< z.16 2))
                                  (begin
                                    (set! x.21 1)
                                    (set! y.20 2)
                                    (set! z.19 3)
                                    (if (< x.21 y.20) x.21 y.20))
                                  (begin
                                    (set! x.24 3)
                                    (set! y.23 4)
                                    (set! z.22 5)
                                    (begin
                                      (set! x.26 2)
                                      (set! y.25 1)
                                      (call L.addup2.2 x.26 y.25 z.22))))))
                            (begin
                              (set! x.29 1)
                              (set! y.28 2)
                              (set! z.27 3)
                              (call L.addup2.2 x.29 y.28 z.27))))))
;; ----------------------old cases--------------------------------- 

(let ([x `(module (if (true) (+ 1 2) (+ 3 4)))])
  (test-case "Simple case with no binding"
             (check-equal? (sequentialize-let x)
                           `(module (if (true) (+ 1 2) (+ 3 4))))))

(let ([x `(module (let ((x.3 1) (y.2 2) (z.1 3)) z.1))])
  (test-case "binding in tail"
             (check-equal? (sequentialize-let x)
                           `(module (begin (set! x.3 1) (set! y.2 2) (set! z.1 3) z.1)))))

(let ([x `(module (let ((x.3 1) (y.2 2) (z.1 3)) (+ z.1 x.3)))])
  (test-case "Simple case with binding in value"
             (check-equal? (sequentialize-let x)
                           `(module (begin (set! x.3 1) (set! y.2 2) (set! z.1 3) (+ z.1 x.3))))))


(let ([x `(module (if (let ((x.3 1) (y.2 2) (z.1 3)) (< z.1 2)) 5 6))])
  (test-case "Simple case with binding in pred"
             (check-equal? (sequentialize-let x)
                           `(module (if (begin (set! x.3 1) (set! y.2 2) (set! z.1 3) (< z.1 2)) 5 6)))))


;; General cases

(let ([x `(module (let ((x.2 1) (y.1 (let ((z.3 3)) z.3))) (+ x.2 y.1)))])
  (test-case "General case"
             (check-equal? (sequentialize-let x)
                           `(module (begin (set! x.2 1) (set! y.1 (begin (set! z.3 3) z.3)) (+ x.2 y.1))))))

(let ([x `(module
              (let ((x.2 1)
                    (y.1 (if (let ((z.4 3) (q.3 (+ 1 2))) (!= z.4 q.3)) (+ 1 2) 5)))
                (+ x.2 y.1)))])
  (test-case "General case"
             (check-equal? (sequentialize-let x)
                           `(module
                                (begin
                                  (set! x.2 1)
                                  (set! y.1
                                        (if (begin (set! z.4 3) (set! q.3 (+ 1 2)) (!= z.4 q.3)) (+ 1 2) 5))
                                  (+ x.2 y.1))))))

(let ([x `(module
              (let ((x.2 1)
                    (y.1
                     (if (let ((z.4 3) (q.3 (+ 1 2))) (!= z.4 q.3))
                         (let ((z.6 1) (q.5 2)) (+ q.5 z.6))
                         (let ((x.7 10)) (+ 1 x.7)))))
                (+ x.2 y.1)))])
  (test-case "General case"
             (check-equal? (sequentialize-let x)
                           `(module
                                (begin
                                  (set! x.2 1)
                                  (set! y.1
                                        (if (begin (set! z.4 3) (set! q.3 (+ 1 2)) (!= z.4 q.3))
                                            (begin (set! z.6 1) (set! q.5 2) (+ q.5 z.6))
                                            (begin (set! x.7 10) (+ 1 x.7))))
                                  (+ x.2 y.1))))))

(let ([x `(module
              (let ((x.2 1)
                    (y.1
                     (if (let ((z.4 3) (q.3 (+ 1 2))) (!= z.4 q.3))
                         (let ((z.6 (let ((z.8 2) (y.7 1)) (+ y.7 z.8)))
                               (q.5 (let ((x.9 2)) x.9)))
                           (+ q.5 z.6))
                         (let ((x.10 10)) (let ((z.12 1) (y.11 1)) (+ y.11 z.12))))))
                (+ x.2 y.1)))])
  (test-case "General case"
             (check-equal? (sequentialize-let x)
                           `(module
                                (begin
                                  (set! x.2 1)
                                  (set! y.1
                                        (if (begin (set! z.4 3) (set! q.3 (+ 1 2)) (!= z.4 q.3))
                                            (begin
                                              (set! z.6 (begin (set! z.8 2) (set! y.7 1) (+ y.7 z.8)))
                                              (set! q.5 (begin (set! x.9 2) x.9))
                                              (+ q.5 z.6))
                                            (begin
                                              (set! x.10 10)
                                              (begin (set! z.12 1) (set! y.11 1) (+ y.11 z.12)))))
                                  (+ x.2 y.1))))))


(let ([x `(module
              (let ((x.3 1)
                    (y.2 2)
                    (z.1
                     (let ((x.6 1)
                           (y.5 2)
                           (z.4 (let ((x.9 2) (y.8 7) (z.7 3)) (+ x.9 y.8))))
                       (+ x.6 z.4))))
                (+ y.2 z.1)))])
  (test-case "General case"
             (check-equal? (sequentialize-let x)
                           `(module
                                (begin
                                  (set! x.3 1)
                                  (set! y.2 2)
                                  (set! z.1
                                        (begin
                                          (set! x.6 1)
                                          (set! y.5 2)
                                          (set! z.4 (begin (set! x.9 2) (set! y.8 7) (set! z.7 3) (+ x.9 y.8)))
                                          (+ x.6 z.4)))
                                  (+ y.2 z.1))))))



(let ([x `(module
              (if (let ((x.3 1) (y.2 2) (z.1 3)) (< z.1 2))
                  (let ((x.6 1) (y.5 2) (z.4 3)) (if (< x.6 y.5) x.6 y.5))
                  (let ((x.9 3) (y.8 4) (z.7 5))
                    (if (let ((x.11 2) (y.10 1)) (< x.11 y.10)) x.9 y.8))))])
  (test-case "General case"
             (check-equal? (sequentialize-let x)
                           `(module
                                (if (begin (set! x.3 1) (set! y.2 2) (set! z.1 3) (< z.1 2))
                                    (begin (set! x.6 1) (set! y.5 2) (set! z.4 3) (if (< x.6 y.5) x.6 y.5))
                                    (begin
                                      (set! x.9 3)
                                      (set! y.8 4)
                                      (set! z.7 5)
                                      (if (begin (set! x.11 2) (set! y.10 1) (< x.11 y.10)) x.9 y.8)))))))      


(let ([x `(module
              (if (let ((x.3 1) (y.2 2) (z.1 3)) (< z.1 2))
                  (let ((x.6 1) (y.5 2) (z.4 3)) (if (< x.6 y.5) x.6 y.5))
                  (let ((x.9 3) (y.8 4) (z.7 5))
                    (if (let ((z.11 x.9) (q.10 y.8)) (!= z.11 q.10)) x.9 y.8))))])
  (test-case "General case"
             (check-equal? (sequentialize-let x)
                           `(module
                                (if (begin (set! x.3 1) (set! y.2 2) (set! z.1 3) (< z.1 2))
                                    (begin (set! x.6 1) (set! y.5 2) (set! z.4 3) (if (< x.6 y.5) x.6 y.5))
                                    (begin
                                      (set! x.9 3)
                                      (set! y.8 4)
                                      (set! z.7 5)
                                      (if (begin (set! z.11 x.9) (set! q.10 y.8) (!= z.11 q.10)) x.9 y.8)))))))                        

(let ([x `(module
              (let ((x.2 1) (y.1 (let ((x.4 1) (y.3 2)) (+ x.4 y.3))))
                (if (!= x.2 y.1)
                    (let ((x.6 x.2) (y.5 y.1)) (+ x.6 y.5))
                    (let ((z.7 (+ x.2 y.1))) (* z.7 z.7)))))])
  (test-case "General case"
             (check-equal? (sequentialize-let x)
                           `(module
                                (begin
                                  (set! x.2 1)
                                  (set! y.1 (begin (set! x.4 1) (set! y.3 2) (+ x.4 y.3)))
                                  (if (!= x.2 y.1)
                                      (begin (set! x.6 x.2) (set! y.5 y.1) (+ x.6 y.5))
                                      (begin (set! z.7 (+ x.2 y.1)) (* z.7 z.7))))))))     


(let ([x `(module
              (let ((x.2 1) (y.1 (let ((x.4 1) (y.3 2)) (+ x.4 y.3))))
                (if (if (let ((x.5 (let ((y.6 1)) y.6))) (< x.5 5))
                        (let ((x.8 2) (q.7 (+ x.2 1))) (> x.8 q.7))
                        (not (let ((x.10 1) (y.9 2)) (!= y.9 2))))
                    (let ((x.12 x.2) (y.11 y.1)) (+ x.12 y.11))
                    (let ((z.13 (+ x.2 y.1))) (* z.13 z.13)))))])
  (test-case "General case"
             (check-equal? (sequentialize-let x)
                           `(module
                                (begin
                                  (set! x.2 1)
                                  (set! y.1 (begin (set! x.4 1) (set! y.3 2) (+ x.4 y.3)))
                                  (if (if (begin (set! x.5 (begin (set! y.6 1) y.6)) (< x.5 5))
                                          (begin (set! x.8 2) (set! q.7 (+ x.2 1)) (> x.8 q.7))
                                          (not (begin (set! x.10 1) (set! y.9 2) (!= y.9 2))))
                                      (begin (set! x.12 x.2) (set! y.11 y.1) (+ x.12 y.11))
                                      (begin (set! z.13 (+ x.2 y.1)) (* z.13 z.13))))))))   

(let ([x `(module
              (let ((x.2 1) (y.1 2))
                (let ((z.4 x.2) (q.3 y.1)) (let ((x.6 5) (y.5 9)) (+ x.6 q.3)))))])
  (test-case "General case"
             (check-equal? (sequentialize-let x)
                           `(module
                                (begin
                                  (set! x.2 1)
                                  (set! y.1 2)
                                  (begin
                                    (set! z.4 x.2)
                                    (set! q.3 y.1)
                                    (begin (set! x.6 5) (set! y.5 9) (+ x.6 q.3))))))))  

(let ([x `(module
              (let ((x.3 1) (q.2 2) (y.1 (let ((x.5 1) (y.4 2)) (+ x.5 y.4))))
                (if (not (!= x.3 y.1))
                    (let ((x.7 q.2) (y.6 y.1)) (+ q.2 y.6))
                    (let ((z.9 (+ x.3 y.1))
                          (y.8
                           (let ((x.11 1)
                                 (y.10
                                  (if (let ((x.12 1)) (< x.12 5)) (+ x.3 q.2) (+ y.1 q.2))))
                             (+ x.11 y.10))))
                      (* z.9 z.9)))))])
  (test-case "General case"
             (check-equal? (sequentialize-let x)
                           `(module
                                (begin
                                  (set! x.3 1)
                                  (set! q.2 2)
                                  (set! y.1 (begin (set! x.5 1) (set! y.4 2) (+ x.5 y.4)))
                                  (if (not (!= x.3 y.1))
                                      (begin (set! x.7 q.2) (set! y.6 y.1) (+ q.2 y.6))
                                      (begin
                                        (set! z.9 (+ x.3 y.1))
                                        (set! y.8
                                              (begin
                                                (set! x.11 1)
                                                (set! y.10
                                                      (if (begin (set! x.12 1) (< x.12 5)) (+ x.3 q.2) (+ y.1 q.2)))
                                                (+ x.11 y.10)))
                                        (* z.9 z.9)))))))) 
                            
(let ([x `(module
              (if (if (let ((x.3 1) (y.2 2) (z.1 3)) (< x.3 y.2))
                      (if (true) (false) (let ((x.5 2) (y.4 3)) (not (!= x.5 y.4))))
                      (let ((x.7 2) (z.6 3)) (> x.7 z.6)))
                  (+ 1 2)
                  (let ((x.9 1) (y.8 2)) (+ x.9 y.8))))])
  (test-case "General case"
             (check-equal? (sequentialize-let x)
                           `(module
                                (if (if (begin (set! x.3 1) (set! y.2 2) (set! z.1 3) (< x.3 y.2))
                                        (if (true)
                                            (false)
                                            (begin (set! x.5 2) (set! y.4 3) (not (!= x.5 y.4))))
                                        (begin (set! x.7 2) (set! z.6 3) (> x.7 z.6)))
                                    (+ 1 2)
                                    (begin (set! x.9 1) (set! y.8 2) (+ x.9 y.8))))))) 

(let ([x `(module
              (if (let ((x.2 1) (y.1 2)) (not (!= x.2 y.1)))
                  (if (false)
                      (let ((x.4 1) (y.3 2)) (let ((x.6 x.4) (y.5 y.3)) (+ x.6 y.5)))
                      (let ((x.8 1) (y.7 2)) (+ x.8 y.7)))
                  (if (true)
                      (let ((x.10 1) (y.9 2))
                        (let ((z.12 (+ x.10 y.9)) (x.11 (+ y.9 y.9))) (+ x.11 z.12)))
                      (let ((z.13 3)) (+ z.13 z.13)))))])
  (test-case "General case"
             (check-equal? (sequentialize-let x)
                           `(module
                                (if (begin (set! x.2 1) (set! y.1 2) (not (!= x.2 y.1)))
                                    (if (false)
                                        (begin
                                          (set! x.4 1)
                                          (set! y.3 2)
                                          (begin (set! x.6 x.4) (set! y.5 y.3) (+ x.6 y.5)))
                                        (begin (set! x.8 1) (set! y.7 2) (+ x.8 y.7)))
                                    (if (true)
                                        (begin
                                          (set! x.10 1)
                                          (set! y.9 2)
                                          (begin (set! z.12 (+ x.10 y.9)) (set! x.11 (+ y.9 y.9)) (+ x.11 z.12)))
                                        (begin (set! z.13 3) (+ z.13 z.13)))))))) 

(let ([x `(module
              (let ((x.2 1) (y.1 2))
                (let ((q.4 x.2) (z.3 y.1))
                  (let ((w.6 1) (z.5 2)) (if (< w.6 q.4) (+ w.6 z.5) (+ y.1 q.4))))))])
  (test-case "General case"
             (check-equal? (sequentialize-let x)
                           `(module
                                (begin
                                  (set! x.2 1)
                                  (set! y.1 2)
                                  (begin
                                    (set! q.4 x.2)
                                    (set! z.3 y.1)
                                    (begin
                                      (set! w.6 1)
                                      (set! z.5 2)
                                      (if (< w.6 q.4) (+ w.6 z.5) (+ y.1 q.4)))))))))
                                


(let ([x `(module
              (if (let ((x.2 1) (y.1 2)) (not (!= x.2 y.1)))
                  (if (let ((x.4 1) (y.3 2))
                        (if (false) (let ((q.6 x.4) (z.5 y.3)) (= q.6 z.5)) (true)))
                      (let ((x.8 1) (y.7 2)) (let ((x.10 x.8) (y.9 y.7)) (+ x.10 y.9)))
                      (let ((x.12 1) (y.11 2)) (+ x.12 y.11)))
                  (if (if (let ((x.14 1) (y.13 2))
                            (if (< x.14 y.13)
                                (let ((q.16 x.14) (y.15 1)) (!= q.16 y.15))
                                (not (< x.14 y.13))))
                          (true)
                          (let ((x.18 1) (y.17 2)) (> x.18 y.17)))
                      (let ((x.20 1) (y.19 2))
                        (let ((z.22 (+ x.20 y.19)) (x.21 (+ y.19 y.19))) (+ x.21 z.22)))
                      (let ((z.23 3)) (+ z.23 z.23)))))])
  (test-case "General case"
             (check-equal? (sequentialize-let x)
                           `(module
                                (if (begin (set! x.2 1) (set! y.1 2) (not (!= x.2 y.1)))
                                    (if (begin
                                          (set! x.4 1)
                                          (set! y.3 2)
                                          (if (false)
                                              (begin (set! q.6 x.4) (set! z.5 y.3) (= q.6 z.5))
                                              (true)))
                                        (begin
                                          (set! x.8 1)
                                          (set! y.7 2)
                                          (begin (set! x.10 x.8) (set! y.9 y.7) (+ x.10 y.9)))
                                        (begin (set! x.12 1) (set! y.11 2) (+ x.12 y.11)))
                                    (if (if (begin
                                              (set! x.14 1)
                                              (set! y.13 2)
                                              (if (< x.14 y.13)
                                                  (begin (set! q.16 x.14) (set! y.15 1) (!= q.16 y.15))
                                                  (not (< x.14 y.13))))
                                            (true)
                                            (begin (set! x.18 1) (set! y.17 2) (> x.18 y.17)))
                                        (begin
                                          (set! x.20 1)
                                          (set! y.19 2)
                                          (begin
                                            (set! z.22 (+ x.20 y.19))
                                            (set! x.21 (+ y.19 y.19))
                                            (+ x.21 z.22)))
                                        (begin (set! z.23 3) (+ z.23 z.23))))))))

(let ([x `(module
              (if (if (if (if (true)
                              (let ((x.2 1) (y.1 1)) (!= x.2 y.1))
                              (let ((x.4 1) (y.3 2)) (= x.4 y.3)))
                          (let ((x.6 1) (y.5 2)) (< x.6 y.5))
                          (let ((x.8 1) (y.7 2)) (> x.8 y.7)))
                      (let ((x.10 1) (y.9 2)) (< x.10 y.9))
                      (let ((x.12 1) (y.11 2)) (> x.12 y.11)))
                  5
                  (+ 1 2)))])
  (test-case "General case"
             (check-equal? (sequentialize-let x)
                           `(module
                                (if (if (if (if (true)
                                                (begin (set! x.2 1) (set! y.1 1) (!= x.2 y.1))
                                                (begin (set! x.4 1) (set! y.3 2) (= x.4 y.3)))
                                            (begin (set! x.6 1) (set! y.5 2) (< x.6 y.5))
                                            (begin (set! x.8 1) (set! y.7 2) (> x.8 y.7)))
                                        (begin (set! x.10 1) (set! y.9 2) (< x.10 y.9))
                                        (begin (set! x.12 1) (set! y.11 2) (> x.12 y.11)))
                                    5
                                    (+ 1 2))))))