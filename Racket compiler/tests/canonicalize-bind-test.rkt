#lang racket
(require
  cpsc411/compiler-lib
  cpsc411/2c-run-time
  cpsc411/test-suite/utils
  racket/match
  rackunit)

(require "../component/cano-bind.rkt")

; input: imp-mf-lang-v8
; output: imp-cmf-lang-v8
; purpose: Check if the outputs of canonicalize-bind matches the desired outputs of the interrogator.-


(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (mset! rax 8 (begin (set! rcx 1) (set! rbx 2) (+ rcx rbx)))
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support new cano-bind 1"
     (check-match (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin
                        (set! rcx 1)
                        (set! rbx 2)
                        (begin (set! ,tmp.1 (+ rcx rbx)) (mset! rax 8 ,tmp.1)))
                      (jump L.end.1)))
                  (jump L.addup.1)))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (mset! rax 8 (if (true) 8 (+ rcx rbx)))
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support new cano-bind 2"
     (check-match (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (if (true)
                        (mset! rax 8 8)
                        (begin (set! ,tmp.1 (+ rcx rbx)) (mset! rax 8 ,tmp.1)))
                      (jump L.end.1)))
                  (jump L.addup.1)))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (set! x.1 (mref x.1 8))
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support mref as value"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin (set! x.1 (mref x.1 8)) (jump L.end.1)))
                  (jump L.addup.1)))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (set! x.1 (alloc 8))
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support alloc as value"
     (check-equal? (canonicalize-bind x)
                  `(module
                    ((new-frames ()))
                    (define L.addup.1
                      ((new-frames ()))
                      (begin (set! x.1 (alloc 8)) (jump L.end.1)))
                    (jump L.addup.1)))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (mset! rax 8 8)
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support mset! as effect"
     (check-equal? (canonicalize-bind x)
                  `(module
                    ((new-frames ()))
                    (define L.addup.1 ((new-frames ())) (begin (mset! rax 8 8) (jump L.end.1)))
                    (jump L.addup.1)))))

;; --------------------------------- values in different pos
(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (set! x.1 (if (true) (mref x.1 8) (alloc y.1)))
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support new value in if as value"
     (check-equal? (canonicalize-bind x)
                  `(module
                    ((new-frames ()))
                    (define L.addup.1
                      ((new-frames ()))
                      (begin
                        (if (true) (set! x.1 (mref x.1 8)) (set! x.1 (alloc y.1)))
                        (jump L.end.1)))
                    (jump L.addup.1)))))



(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (set! x.1 (return-point L.rp.1 (begin (set! y.1 (alloc 8)) (set! z.1 (mref rax 8)) (jump L.end.1))))
                  (set! x.1 (return-point L.rp.1 (if (false) (begin (set! y.1 (alloc 8)) (jump rax))  (begin (set! z.1 (mref rax 8)) (jump L.end.1)))))
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support new value in return point as value"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin
                        (return-point
                        L.rp.1
                        (begin (set! y.1 (alloc 8)) (set! z.1 (mref rax 8)) (jump L.end.1)))
                        (set! x.1 rax))
                      (begin
                        (return-point
                        L.rp.1
                        (if (false)
                          (begin (set! y.1 (alloc 8)) (jump rax))
                          (begin (set! z.1 (mref rax 8)) (jump L.end.1))))
                        (set! x.1 rax))
                      (jump L.end.1)))
                  (jump L.addup.1)))))



(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (mset! z.1 8 (alloc 8))
                  (mset! z.1 8 (begin (set! x.1 (alloc 8)) (mref rax 8)))
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support new value in mset!"
     (check-match (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin (set! ,tmp.1 (alloc 8)) (mset! z.1 8 ,tmp.1))
                      (begin
                        (set! x.1 (alloc 8))
                        (begin (set! ,tmp.2 (mref rax 8)) (mset! z.1 8 ,tmp.2)))
                      (jump L.end.1)))
                  (jump L.addup.1)))))
                  


;; --------------------------------- effect in different pos
(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (set! x.1 (return-point L.rp.1 (begin (mset! rax 8 (begin (set! rcx 1) (set! rbx 2) (+ rcx rbx))) (jump L.end.1))))
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support new cano-bind 1 in return point"
     (check-match (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin
                        (return-point
                        L.rp.1
                        (begin
                          (begin
                            (set! rcx 1)
                            (set! rbx 2)
                            (begin (set! ,tmp.1 (+ rcx rbx)) (mset! rax 8 ,tmp.1)))
                          (jump L.end.1)))
                        (set! x.1 rax))
                      (jump L.end.1)))
                  (jump L.addup.1)))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (set! x.1 (return-point L.rp.1 (if (false) (begin (mset! rax 8 (if (true) (alloc 8) (mref x.1 8))) (jump rax)) 
                                                              (begin (mset! rax 8 (begin (set! rcx 1) (set! rbx 2) (alloc x.1))) (jump L.end.1)))))
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support new cano-bind 2 in return point"
     (check-match (canonicalize-bind x)
                  `(module
                    ((new-frames ()))
                    (define L.addup.1
                      ((new-frames ()))
                      (begin
                        (begin
                          (return-point
                          L.rp.1
                          (if (false)
                            (begin
                              (if (true)
                                (begin (set! ,tmp.1 (alloc 8)) (mset! rax 8 ,tmp.1))
                                (begin (set! ,tmp.2 (mref x.1 8)) (mset! rax 8 ,tmp.2)))
                              (jump rax))
                            (begin
                              (begin
                                (set! rcx 1)
                                (set! rbx 2)
                                (begin (set! ,tmp.3 (alloc x.1)) (mset! rax 8 ,tmp.3)))
                              (jump L.end.1))))
                          (set! x.1 rax))
                        (jump L.end.1)))
                    (jump L.addup.1)))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (begin (if (true) (mset! rax 8 (begin (set! rcx 1) (set! rbx 2) (+ rcx rbx))) (set! x.1 (mref x.1 32))))
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support new cano-bind 1 in if as effect"
     (check-match (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin
                        (if (true)
                          (begin
                            (set! rcx 1)
                            (set! rbx 2)
                            (begin (set! ,tmp.1 (+ rcx rbx)) (mset! rax 8 ,tmp.1)))
                          (set! x.1 (mref x.1 32))))
                      (jump L.end.1)))
                  (jump L.addup.1)))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (begin (if (true) (mset! rax 8 (if (true) (alloc 8) (mref x.1 8))) (set! x.1 (mref x.1 32))))
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support new cano-bind 2 in if as effect"
     (check-match (canonicalize-bind x)
                  `(module
                    ((new-frames ()))
                    (define L.addup.1
                      ((new-frames ()))
                      (begin
                        (begin
                          (if (true)
                            (if (true)
                              (begin (set! ,tmp.1 (alloc 8)) (mset! rax 8 ,tmp.1))
                              (begin (set! ,tmp.2 (mref x.1 8)) (mset! rax 8 ,tmp.2)))
                            (set! x.1 (mref x.1 32))))
                        (jump L.end.1)))
                    (jump L.addup.1)))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (begin (mset! rax 8 (begin (set! rcx 1) (set! rbx 2) (+ rcx rbx))))
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support new cano-bind 1 in begin as effect"
     (check-match (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin
                        (begin
                          (set! rcx 1)
                          (set! rbx 2)
                          (begin (set! ,tmp.1 (+ rcx rbx)) (mset! rax 8 ,tmp.1))))
                      (jump L.end.1)))
                  (jump L.addup.1)))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (begin (mset! rax 8 (if (true) (alloc 8) (mref x.1 8))))
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support new cano-bind 2 in begin as effect"
     (check-match (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin
                        (if (true)
                          (begin (set! ,tmp.1 (alloc 8)) (mset! rax 8 ,tmp.1))
                          (begin (set! ,tmp.2 (mref x.1 8)) (mset! rax 8 ,tmp.2))))
                      (jump L.end.1)))
                  (jump L.addup.1)))))



(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (if (begin (mset! rax 8 (begin (set! rcx 1) (set! rbx 2) (+ rcx rbx))) (< x.1 y.1))
                      (set! x.1 8)
                      (set! y.1 2))
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support new cano-bind 1 in begin as pred"
     (check-match (canonicalize-bind x)
                  `(module
                    ((new-frames ()))
                    (define L.addup.1
                      ((new-frames ()))
                      (begin
                        (if (begin
                              (begin
                                (set! rcx 1)
                                (set! rbx 2)
                                (begin (set! ,tmp.1 (+ rcx rbx)) (mset! rax 8 ,tmp.1)))
                              (< x.1 y.1))
                          (set! x.1 8)
                          (set! y.1 2))
                        (jump L.end.1)))
                    (jump L.addup.1)))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (if (begin (mset! rax 8 (if (true) (alloc 8) (mref x.1 8))) (< x.1 y.1))
                      (set! x.1 8)
                      (set! y.1 2))
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support new cano-bind 2 in begin as pred"
     (check-match (canonicalize-bind x)
                  `(module
                    ((new-frames ()))
                    (define L.addup.1
                      ((new-frames ()))
                      (begin
                        (if (begin
                              (if (true)
                                (begin (set! ,tmp.1 (alloc 8)) (mset! rax 8 ,tmp.1))
                                (begin (set! ,tmp.2 (mref x.1 8)) (mset! rax 8 ,tmp.2)))
                              (< x.1 y.1))
                          (set! x.1 8)
                          (set! y.1 2))
                        (jump L.end.1)))
                    (jump L.addup.1)))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (if (if (true) 
                          (begin (mset! rax 8 (begin (set! rcx 1) (set! rbx 2) (+ rcx rbx))) (< x.1 y.1))
                          (begin (mset! rax 8 (if (true) (alloc 8) (mref x.1 8))) (< x.1 y.1)))
                      (if (true) (set! x.1 (alloc 8)) 
                                  (set! x.1 (mref x.1 8)))
                      (if (false) (mset! rax 8 (begin (set! rcx 1) (set! rbx 2) (+ rcx rbx)))
                                  (mset! rax 8 (if (true) (alloc 8) (mref x.1 8)))))
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support new cano-bind in nested if"
     (check-match (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (if (if (true)
                            (begin
                              (begin
                                (set! rcx 1)
                                (set! rbx 2)
                                (begin (set! ,tmp.1 (+ rcx rbx)) (mset! rax 8 ,tmp.1)))
                              (< x.1 y.1))
                            (begin
                              (if (true)
                                (begin (set! ,tmp.2 (alloc 8)) (mset! rax 8 ,tmp.2))
                                (begin (set! ,tmp.3 (mref x.1 8)) (mset! rax 8 ,tmp.3)))
                              (< x.1 y.1)))
                        (if (true) (set! x.1 (alloc 8)) (set! x.1 (mref x.1 8)))
                        (if (false)
                          (begin
                            (set! rcx 1)
                            (set! rbx 2)
                            (begin (set! ,tmp.4 (+ rcx rbx)) (mset! rax 8 ,tmp.4)))
                          (if (true)
                            (begin (set! ,tmp.5 (alloc 8)) (mset! rax 8 ,tmp.5))
                            (begin (set! ,tmp.6 (mref x.1 8)) (mset! rax 8 ,tmp.6)))))
                      (jump L.end.1)))
                  (jump L.addup.1)))))


;; -------------------------------- nested cases

(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (mset! rax 8 (begin (mset! rax 8 (if (true) (alloc 8) (mref x.1 8))) (set! rbx 2) (+ rcx rbx)))
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support new cano-bind 1 nested with cano-bind 2"
     (check-match (canonicalize-bind x)
                  `(module
                      ((new-frames ()))
                      (define L.addup.1
                        ((new-frames ()))
                        (begin
                          (begin
                            (if (true)
                              (begin (set! ,tmp.1 (alloc 8)) (mset! rax 8 ,tmp.1))
                              (begin (set! ,tmp.2 (mref x.1 8)) (mset! rax 8 ,tmp.2)))
                            (set! rbx 2)
                            (begin (set! ,tmp.3 (+ rcx rbx)) (mset! rax 8 ,tmp.3)))
                          (jump L.end.1)))
                      (jump L.addup.1)))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (mset! rax 8 (if (true) (begin (mset! rax 8 (begin (set! rcx 1) (set! rbx 2) (+ rcx rbx))) (alloc 8)) (mref x.1 8)))
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support new cano-bind 2 nested with cano-bind 1"
     (check-match (canonicalize-bind x)
                  `(module
                    ((new-frames ()))
                    (define L.addup.1
                      ((new-frames ()))
                      (begin
                        (if (true)
                          (begin
                            (begin
                              (set! rcx 1)
                              (set! rbx 2)
                              (begin (set! ,tmp.1 (+ rcx rbx)) (mset! rax 8 ,tmp.1)))
                            (begin (set! ,tmp.2 (alloc 8)) (mset! rax 8 ,tmp.2)))
                          (begin (set! ,tmp.3 (mref x.1 8)) (mset! rax 8 ,tmp.3)))
                        (jump L.end.1)))
                    (jump L.addup.1)))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (set! x.1 (begin (mset! rax 8 (begin (set! rcx 1) (set! rbx 2) (+ rcx rbx))) (alloc 8)))
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support new cano-bind 1 nested with previous cano-bind"
     (check-match (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin
                        (begin
                          (set! rcx 1)
                          (set! rbx 2)
                          (begin (set! ,tmp.1 (+ rcx rbx)) (mset! rax 8 ,tmp.1)))
                        (set! x.1 (alloc 8)))
                      (jump L.end.1)))
                  (jump L.addup.1)))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin 
                  (set! x.1 (if (true) (begin (mset! rax 8 (begin (set! rcx 1) (set! rbx 2) (+ rcx rbx))) (alloc 8)) 
                                      (begin (mset! rax 8 (if (true) (alloc 8) (mref x.1 8))) (mref x.1 8))))
                  (jump L.end.1)))
              (jump L.addup.1))])
     (test-case "Support new cano-bind 2 nested with previous cano-bind "
     (check-match (canonicalize-bind x)
                  `(module
                    ((new-frames ()))
                    (define L.addup.1
                      ((new-frames ()))
                      (begin
                        (if (true)
                          (begin
                            (begin
                              (set! rcx 1)
                              (set! rbx 2)
                              (begin (set! ,tmp.1 (+ rcx rbx)) (mset! rax 8 ,tmp.1)))
                            (set! x.1 (alloc 8)))
                          (begin
                            (if (true)
                              (begin (set! ,tmp.2 (alloc 8)) (mset! rax 8 ,tmp.2))
                              (begin (set! ,tmp.3 (mref x.1 8)) (mset! rax 8 ,tmp.3)))
                            (set! x.1 (mref x.1 8))))
                        (jump L.end.1)))
                    (jump L.addup.1)))))


;; --------------------------------- old cases -------------------------
(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin (set! x.1 (begin (set! x.2 5) (return-point L.rp.1 (if (begin (set! x.7 (begin (set! q.1 2) (set! v.1 3) (bitwise-and q.1 v.1))) (< x.7 8)) (jump L.rp.1 x.7) (jump L.addup.1 v.1))))) (jump L.addup.2 q.1)))
              (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "Canonicalize the first type in pred -- bitwise-and"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin
                        (set! x.2 5)
                        (begin
                          (return-point
                          L.rp.1
                          (if (begin
                                (begin (set! q.1 2) (set! v.1 3) (set! x.7 (bitwise-and q.1 v.1)))
                                (< x.7 8))
                            (jump L.rp.1 x.7)
                            (jump L.addup.1 v.1)))
                          (set! x.1 rax)))
                      (jump L.addup.2 q.1)))
                  (jump L.addup.1 x.5 x.6 x.7)))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin (set! x.1 (begin (set! x.2 5) (return-point L.rp.1 (if (begin (set! x.7 (begin (set! q.1 2) (set! v.1 3) (bitwise-ior q.1 v.1))) (< x.7 8)) (jump L.rp.1 x.7) (jump L.addup.1 v.1))))) (jump L.addup.2 q.1)))
              (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "Canonicalize the first type in pred -- bitwise-ior"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin
                        (set! x.2 5)
                        (begin
                          (return-point
                          L.rp.1
                          (if (begin
                                (begin (set! q.1 2) (set! v.1 3) (set! x.7 (bitwise-ior q.1 v.1)))
                                (< x.7 8))
                            (jump L.rp.1 x.7)
                            (jump L.addup.1 v.1)))
                          (set! x.1 rax)))
                      (jump L.addup.2 q.1)))
                  (jump L.addup.1 x.5 x.6 x.7)))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin (set! x.1 (begin (set! x.2 5) (return-point L.rp.1 (if (begin (set! x.7 (begin (set! q.1 2) (set! v.1 3) (bitwise-xor q.1 v.1))) (< x.7 8)) (jump L.rp.1 x.7) (jump L.addup.1 v.1))))) (jump L.addup.2 q.1)))
              (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "Canonicalize the first type in pred -- bitwise-xor"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin
                        (set! x.2 5)
                        (begin
                          (return-point
                          L.rp.1
                          (if (begin
                                (begin (set! q.1 2) (set! v.1 3) (set! x.7 (bitwise-xor q.1 v.1)))
                                (< x.7 8))
                            (jump L.rp.1 x.7)
                            (jump L.addup.1 v.1)))
                          (set! x.1 rax)))
                      (jump L.addup.2 q.1)))
                  (jump L.addup.1 x.5 x.6 x.7)))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin (set! x.1 (begin (set! x.2 5) (return-point L.rp.1 (if (begin (set! x.7 (begin (set! q.1 2) (set! v.1 3) (arithmetic-shift-right q.1 v.1))) (< x.7 8)) (jump L.rp.1 x.7) (jump L.addup.1 v.1))))) (jump L.addup.2 q.1)))
              (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "Canonicalize the first type in pred -- arithmetic-shift-right"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin
                        (set! x.2 5)
                        (begin
                          (return-point
                          L.rp.1
                          (if (begin
                                (begin (set! q.1 2) (set! v.1 3) (set! x.7 (arithmetic-shift-right q.1 v.1)))
                                (< x.7 8))
                            (jump L.rp.1 x.7)
                            (jump L.addup.1 v.1)))
                          (set! x.1 rax)))
                      (jump L.addup.2 q.1)))
                  (jump L.addup.1 x.5 x.6 x.7)))))


;; ------------------------------------------------------------------------
; new cases test mainly return-point
; Third type should be added 


(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin (set! x.1 (begin (set! x.3 4) (set! x.4 5) (return-point L.addup.1 (jump L.addup.1 x.3 x.4)))) (jump L.addup.1 x.3)))
              (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "Canonicalize the first type in the proc declaration"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin
                        (set! x.3 4)
                        (set! x.4 5)
                        (begin
                          (return-point L.addup.1 (jump L.addup.1 x.3 x.4))
                          (set! x.1 rax)))
                      (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7)))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin (set! x.1 (begin (set! x.3 4) (set! x.4 5) (return-point L.addup.1 (if (true) (jump L.rp.1 x.1) (jump L.rp.2 fv1))))) (jump L.addup.1 x.3)))
              (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "Canonicalize the first type in the proc declaration"
     (check-equal? (canonicalize-bind x)
                  `(module
                    ((new-frames ()))
                    (define L.addup.1
                      ((new-frames ()))
                      (begin
                        (begin
                          (set! x.3 4)
                          (set! x.4 5)
                          (begin
                            (return-point
                            L.addup.1
                            (if (true) (jump L.rp.1 x.1) (jump L.rp.2 fv1)))
                            (set! x.1 rax)))
                        (jump L.addup.1 x.3)))
                    (jump L.addup.1 x.5 x.6 x.7)))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin (set! x.1 (begin (set! x.3 4) (set! x.4 5) (return-point L.addup.1 (begin (set! x.1 2) (set! fv1 3) (jump L.rp.1 x.1 fv1))))) (jump L.addup.1 x.3)))
              (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "Canonicalize the first type in the proc declaration"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin
                        (set! x.3 4)
                        (set! x.4 5)
                        (begin
                          (return-point
                          L.addup.1
                          (begin (set! x.1 2) (set! fv1 3) (jump L.rp.1 x.1 fv1)))
                          (set! x.1 rax)))
                      (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7)))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin (set! x.1 (if (true) (return-point L.addup.1 (jump L.addup.1 x.1)) (return-point L.rp.1 (jump L.addup.2 x.2)))) (jump L.addup.1 x.3)))
              (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "Canonicalize the second type in the proc declaration"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (if (true)
                        (begin (return-point L.addup.1 (jump L.addup.1 x.1)) (set! x.1 rax))
                        (begin (return-point L.rp.1 (jump L.addup.2 x.2)) (set! x.1 rax)))
                      (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7)))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin (set! x.1 (if (true) (return-point L.addup.1 (begin (set! x.1 2) (set! fv1 3) (jump L.rp.1 x.1 fv1))) (return-point L.rp.1 (begin (set! x.1 2) (set! fv1 3) (jump L.rp.2 x.1 fv1))))) (jump L.addup.1 x.3)))
              (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "Canonicalize the second type in the proc declaration"
     (check-equal? (canonicalize-bind x)
                  `(module
                    ((new-frames ()))
                    (define L.addup.1
                      ((new-frames ()))
                      (begin
                        (if (true)
                          (begin
                            (return-point
                            L.addup.1
                            (begin (set! x.1 2) (set! fv1 3) (jump L.rp.1 x.1 fv1)))
                            (set! x.1 rax))
                          (begin
                            (return-point
                            L.rp.1
                            (begin (set! x.1 2) (set! fv1 3) (jump L.rp.2 x.1 fv1)))
                            (set! x.1 rax)))
                        (jump L.addup.1 x.3)))
                    (jump L.addup.1 x.5 x.6 x.7)))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin (set! x.1 (if (true) (return-point L.addup.1 (if (true) (jump L.rp.1 x.1) (jump L.rp.2 x.1))) (return-point L.rp.1 (if (true) (jump L.rp.1 x.1) (jump L.rp.2 x.1))))) (jump L.addup.1 x.3)))
              (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "Canonicalize the second type in the proc declaration"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (if (true)
                        (begin
                          (return-point
                          L.addup.1
                          (if (true) (jump L.rp.1 x.1) (jump L.rp.2 x.1)))
                          (set! x.1 rax))
                        (begin
                          (return-point L.rp.1 (if (true) (jump L.rp.1 x.1) (jump L.rp.2 x.1)))
                          (set! x.1 rax)))
                      (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7)))))



(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin (set! x.1 (return-point L.addup.1 (jump L.rp.1))) (jump L.addup.1 x.3)))
              (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "Canonicalize the third type in the proc declaration"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin (return-point L.addup.1 (jump L.rp.1)) (set! x.1 rax))
                      (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7)))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin (set! x.1 (return-point L.addup.1 (begin (set! x.1 2) (set! fv1 3) (jump L.rp.1 x.1 fv1)))) (jump L.addup.1 x.3)))
              (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "Canonicalize the third type in the proc declaration"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin
                        (return-point
                        L.addup.1
                        (begin (set! x.1 2) (set! fv1 3) (jump L.rp.1 x.1 fv1)))
                        (set! x.1 rax))
                      (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7)))))



(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin (set! x.1 (return-point L.addup.1 (if (true) (jump L.rp.1 x.1) (jump L.rp.2 x.1)))) (jump L.addup.1 x.3)))
              (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "Canonicalize the third type in the proc declaration"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin
                        (return-point
                        L.addup.1
                        (if (true) (jump L.rp.1 x.1) (jump L.rp.2 x.1)))
                        (set! x.1 rax))
                      (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7)))))


;; nested cases

(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin (set! x.1 
                  (begin (set! x.3 4) (set! x.4 5) 
                    (return-point L.addup.1 (begin (set! x.2 (begin (set! q.1 7) (return-point L.addup.2 (jump L.addup.2 q.1)))) (jump L.addup.1 x.4))))) (jump L.addup.1 x.3)))
              (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "Canonicalize the first type nested begin in the proc declaration"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin
                        (set! x.3 4)
                        (set! x.4 5)
                        (begin
                          (return-point
                          L.addup.1
                          (begin
                            (begin
                              (set! q.1 7)
                              (begin
                                (return-point L.addup.2 (jump L.addup.2 q.1))
                                (set! x.2 rax)))
                            (jump L.addup.1 x.4)))
                          (set! x.1 rax)))
                      (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7)))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin (set! x.1 
                  (begin (set! x.3 4) (set! x.4 5) 
                     (return-point L.addup.1 (begin (set! q.1 (if (true) (return-point L.addup.1 (jump L.addup.1 x.1)) (return-point L.rp.1 (jump L.addup.2 x.2)))) (jump L.rp.2 q.1)))))
                       (jump L.addup.1 x.3)))
              (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "Canonicalize the first type nested begin if in the proc declaration"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                  ((new-frames ()))
                  (begin
                    (begin
                      (set! x.3 4)
                      (set! x.4 5)
                      (begin
                        (return-point
                          L.addup.1
                          (begin
                            (if (true)
                              (begin
                                (return-point L.addup.1 (jump L.addup.1 x.1))
                                (set! q.1 rax))
                              (begin
                                (return-point L.rp.1 (jump L.addup.2 x.2))
                                (set! q.1 rax)))
                            (jump L.rp.2 q.1)))
                        (set! x.1 rax)))
                    (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7)))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin (set! x.1 (begin (set! x.2 5) (return-point L.rp.1 (if (begin (set! x.7 (begin (set! q.1 2) (set! v.1 3) (+ q.1 v.1))) (< x.7 8)) (jump L.rp.1 x.7) (jump L.addup.1 v.1))))) (jump L.addup.2 q.1)))
              (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "Canonicalize the first type in pred"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin
                        (set! x.2 5)
                        (begin
                          (return-point
                          L.rp.1
                          (if (begin
                                (begin (set! q.1 2) (set! v.1 3) (set! x.7 (+ q.1 v.1)))
                                (< x.7 8))
                            (jump L.rp.1 x.7)
                            (jump L.addup.1 v.1)))
                          (set! x.1 rax)))
                      (jump L.addup.2 q.1)))
                  (jump L.addup.1 x.5 x.6 x.7)))))



(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin (set! x.1 (begin (set! x.2 5) (return-point L.rp.1 (if (begin (set! x.7 (if (true) (return-point L.rp.1 (jump L.addup.1 x.1)) (return-point L.rp.2 (jump L.addup.2 q.1)))) (< x.7 8)) (jump L.rp.1 x.7) (jump L.addup.1 v.1))))) (jump L.addup.2 q.1)))
              (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "Canonicalize the second type in pred"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin
                        (set! x.2 5)
                        (begin
                          (return-point
                          L.rp.1
                          (if (begin
                                (if (true)
                                  (begin
                                    (return-point L.rp.1 (jump L.addup.1 x.1))
                                    (set! x.7 rax))
                                  (begin
                                    (return-point L.rp.2 (jump L.addup.2 q.1))
                                    (set! x.7 rax)))
                                (< x.7 8))
                            (jump L.rp.1 x.7)
                            (jump L.addup.1 v.1)))
                          (set! x.1 rax)))
                      (jump L.addup.2 q.1)))
                  (jump L.addup.1 x.5 x.6 x.7)))))

;; small general case
(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin (set! x.1 (begin (set! x.3 4) (set! x.4 5) (return-point L.addup.1 (if (if (true) (false) (true)) 
                                                                                                  (begin (set! fv0 (begin (set! x.1 10) (+ 1 x.1))) (jump L.addup.1 x.5))
                                                                                                  (begin (set! fv0 (if (true) 5 10)) (jump L.addup.1 x.4)))))) (jump L.addup.1 x.3)))
              (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "small general cases"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin
                        (set! x.3 4)
                        (set! x.4 5)
                        (begin
                          (return-point
                          L.addup.1
                          (if (if (true) (false) (true))
                            (begin
                              (begin (set! x.1 10) (set! fv0 (+ 1 x.1)))
                              (jump L.addup.1 x.5))
                            (begin
                              (if (true) (set! fv0 5) (set! fv0 10))
                              (jump L.addup.1 x.4))))
                          (set! x.1 rax)))
                      (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7)))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin (set! x.1 (if (true) (return-point L.addup.1 (jump L.addup.1 x.2)) (return-point L.addup.2 (jump L.addup.2 x.1)))) 
                (set! x.2 (begin (set! x.3 (+ x.3 1)) (return-point L.addup.1 (jump L.addup.1 fv0))))
                (jump L.addup.1 x.3)))
              (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "small general cases"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (if (true)
                        (begin (return-point L.addup.1 (jump L.addup.1 x.2)) (set! x.1 rax))
                        (begin (return-point L.addup.2 (jump L.addup.2 x.1)) (set! x.1 rax)))
                      (begin
                        (set! x.3 (+ x.3 1))
                        (begin (return-point L.addup.1 (jump L.addup.1 fv0)) (set! x.2 rax)))
                      (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7)))))     
;; General case Long cases


(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                ((new-frames ())) 
                (begin (set! x.1                     
                  (begin (set! q.1 5) (set! w.2 7) (begin (set! q.1 (begin (set! x.1 (return-point L.rp.1 (begin (set! q.3 (begin (set! m.1 2) (+ m.1 2))) (jump L.addup.1 x.1)))) x.1)) q.1))) (jump L.addup.2 x.2)))
              (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "Canonicalize the second type in pred"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (begin
                        (set! q.1 5)
                        (set! w.2 7)
                        (begin
                          (begin
                            (begin
                              (return-point
                              L.rp.1
                              (begin
                                (begin (set! m.1 2) (set! q.3 (+ m.1 2)))
                                (jump L.addup.1 x.1)))
                              (set! x.1 rax))
                            (set! q.1 x.1))
                          (set! x.1 q.1)))
                      (jump L.addup.2 x.2)))
                  (jump L.addup.1 x.5 x.6 x.7)))))


(let ([x `(module
               ((new-frames ()))
               (define L.addup.1
                  ((new-frames ()))
                  (begin 
                  (set! x.1 10)
                  (set! x.2 (+ 2 3))
                  (if (not (< 2 3))
                    (set! x.1 100)
                    (begin
                      (set! x.1 10)
                      (if (if (begin (set! x.5 (begin (set! y.6 1) (return-point L.rp.1 (jump L.addup.1 x.1)))) (> x.5 5)) (begin (set! x.2 (begin (set! x.1 1) (set! m.1 10) (+ x.1 m.1))) (> x.1 x.2)) (begin (set! x.2 (if (false) (return-point L.rp.2 (begin (set! r.2 (begin (set! q.1 2) q.1)) (jump L.rp.2 x.1))) x.3)) (= x.2 3)))
                        (set! x.1 1)
                        (set! x.2 2))))
                  (if (if (begin (set! x.1 1) (true)) (>= 1 3) (not (false)))
                    (jump L.addup.1 x.2)
                    (begin (begin (begin (begin
                                          (if (if (begin (set! x.5 (begin (set! y.6 1) y.6)) (> x.5 5)) (begin (set! x.2 (if (false) (+ x.1 x.2) (return-point L.rp.1 (jump L.addup.1 x.1)))) (> x.1 x.2)) (begin (set! x.2 (if (false) x.2 x.3)) (= x.2 3)))
                                            (jump L.addup.1 x.2)
                                            (jump L.addup.2 x.1)))))))))
                (begin (set! x.1 (begin (set! x.3 4) (set! x.4 5) (+ x.3 x.4))) (jump L.addup.1 x.1 rdx fv1)))])
     (test-case "Canonicalize the second type in pred"
     (check-equal? (canonicalize-bind x)
                  `(module
                    ((new-frames ()))
                    (define L.addup.1
                      ((new-frames ()))
                      (begin
                        (set! x.1 10)
                        (set! x.2 (+ 2 3))
                        (if (not (< 2 3))
                          (set! x.1 100)
                          (begin
                            (set! x.1 10)
                            (if (if (begin
                                      (begin
                                        (set! y.6 1)
                                        (begin
                                          (return-point L.rp.1 (jump L.addup.1 x.1))
                                          (set! x.5 rax)))
                                      (> x.5 5))
                                  (begin
                                    (begin (set! x.1 1) (set! m.1 10) (set! x.2 (+ x.1 m.1)))
                                    (> x.1 x.2))
                                  (begin
                                    (if (false)
                                      (begin
                                        (return-point
                                        L.rp.2
                                        (begin
                                          (begin (set! q.1 2) (set! r.2 q.1))
                                          (jump L.rp.2 x.1)))
                                        (set! x.2 rax))
                                      (set! x.2 x.3))
                                    (= x.2 3)))
                              (set! x.1 1)
                              (set! x.2 2))))
                        (if (if (begin (set! x.1 1) (true)) (>= 1 3) (not (false)))
                          (jump L.addup.1 x.2)
                          (begin
                            (begin
                              (begin
                                (begin
                                  (if (if (begin (begin (set! y.6 1) (set! x.5 y.6)) (> x.5 5))
                                        (begin
                                          (if (false)
                                            (set! x.2 (+ x.1 x.2))
                                            (begin
                                              (return-point L.rp.1 (jump L.addup.1 x.1))
                                              (set! x.2 rax)))
                                          (> x.1 x.2))
                                        (begin
                                          (if (false) (set! x.2 x.2) (set! x.2 x.3))
                                          (= x.2 3)))
                                    (jump L.addup.1 x.2)
                                    (jump L.addup.2 x.1)))))))))
                    (begin
                      (begin (set! x.3 4) (set! x.4 5) (set! x.1 (+ x.3 x.4)))
                      (jump L.addup.1 x.1 rdx fv1))))))

;; todo all below
(let ([x `(module
              ((new-frames ()))
                (define L.addup.1
                ((new-frames ()))
                (if (not (begin (set! x.1 (begin (set! x.1 x.2) (set! x.1 (+ x.1 5)) (return-point L.addup.1 (begin (set! fv0 (begin (set! fv1 10) (set! fv2 2) (+ fv1 fv2))) (jump L.addup.1 fv0))))) (= x.1 x.2)))
                  (if (begin (set! x.1 (+ 1 2)) (set! x.2 (* 2 x.2)) (true))
                    (begin (set! x.1 (if (begin (set! x.1 (begin (set! x.1 x.2) (set! x.1 (return-point L.rp.1 (jump L.addup.1 x.1))) (+ x.1 x.2))) (= x.1 x.2)) (return-point L.rp.1 (jump L.addup.2 x.1)) (return-point L.rp.1 (jump L.addup.1 x.2)))) (jump L.addup.1 fv0))
                    (jump L.addup.1 x.1))
                  (if (not (= 1 2))
                    (begin (begin (begin (begin (set! x.1 (begin (set! x.2 2) (set! x.3 (return-point L.rp.1 (jump L.addup.1 x.2))) (+ x.2 x.5))) (set! x.2 (+ 2 x.2)) (jump L.addup.1 x.1)))))
                    (jump L.addup.1 x.1))))
              (jump L.addup.1 x.1 rdx fv1))])
     (test-case "Canonicalize the second type in pred"
     (check-equal? (canonicalize-bind x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (if (not
                        (begin
                          (begin
                            (set! x.1 x.2)
                            (set! x.1 (+ x.1 5))
                            (begin
                              (return-point
                                L.addup.1
                                (begin
                                  (begin (set! fv1 10) (set! fv2 2) (set! fv0 (+ fv1 fv2)))
                                  (jump L.addup.1 fv0)))
                              (set! x.1 rax)))
                          (= x.1 x.2)))
                      (if (begin (set! x.1 (+ 1 2)) (set! x.2 (* 2 x.2)) (true))
                        (begin
                          (if (begin
                                (begin
                                  (set! x.1 x.2)
                                  (begin
                                    (return-point L.rp.1 (jump L.addup.1 x.1))
                                    (set! x.1 rax))
                                  (set! x.1 (+ x.1 x.2)))
                                (= x.1 x.2))
                            (begin (return-point L.rp.1 (jump L.addup.2 x.1)) (set! x.1 rax))
                            (begin (return-point L.rp.1 (jump L.addup.1 x.2)) (set! x.1 rax)))
                          (jump L.addup.1 fv0))
                        (jump L.addup.1 x.1))
                      (if (not (= 1 2))
                        (begin
                          (begin
                            (begin
                              (begin
                                (begin
                                  (set! x.2 2)
                                  (begin
                                    (return-point L.rp.1 (jump L.addup.1 x.2))
                                    (set! x.3 rax))
                                  (set! x.1 (+ x.2 x.5)))
                                (set! x.2 (+ 2 x.2))
                                (jump L.addup.1 x.1)))))
                        (jump L.addup.1 x.1))))
                  (jump L.addup.1 x.1 rdx fv1)))))
