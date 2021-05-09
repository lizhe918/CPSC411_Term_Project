#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/2c-run-time
 cpsc411/test-suite/utils
 racket/match
 rackunit)

(require "../component/uncover.rkt")

; input: asm-pred-lang-v8
; output: asm-pred-lang-v8/locals
; purpose: Check if the output of uncover-locals matches the output of interrogator 

(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                      ((new-frames ()))
                      (begin 
                        (set! x.1 1)
                        (mset! g.1 8 8)
                        (set! x.2 L.addup.1)
                        (jump x.2 x.1)))
               (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1)))])
     (test-case "Uncover aloc in mset!"
     (check-match (uncover-locals x)
                  `(module
                    ((new-frames ()) (locals ,(list-no-order 'm.1 'q.1)))
                    (define L.addup.2
                      ((new-frames ()) (locals ,(list-no-order 'x.1 'x.2 'g.1)))
                      (begin (set! x.1 1) (mset! g.1 8 8) (set! x.2 L.addup.1) (jump x.2 x.1)))
                    (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1))))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                      ((new-frames ()))
                      (begin 
                        (set! x.1 1)
                        (mset! g.1 m.1 8)
                        (set! x.2 L.addup.1)
                        (jump x.2 x.1)))
               (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1)))])
     (test-case "Uncover index in mset!"
     (check-match (uncover-locals x)
                  `(module
                    ((new-frames ()) (locals ,(list-no-order 'm.1 'q.1)))
                    (define L.addup.2
                      ((new-frames ()) (locals ,(list-no-order 'x.1 'x.2 'g.1 'm.1)))
                      (begin (set! x.1 1) (mset! g.1 m.1 8) (set! x.2 L.addup.1) (jump x.2 x.1)))
                    (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1))))))



(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                      ((new-frames ()))
                      (begin 
                        (set! x.1 1)
                        (mset! rax 8 m.1)
                        (set! x.2 L.addup.1)
                        (jump x.2 x.1)))
               (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1)))])
     (test-case "Uncover triv in mset!"
     (check-match (uncover-locals x)
                  `(module
                    ((new-frames ()) (locals ,(list-no-order 'm.1 'q.1)))
                    (define L.addup.2
                      ((new-frames ()) (locals ,(list-no-order 'x.1 'x.2 'm.1)))
                      (begin (set! x.1 1) (mset! rax 8 m.1) (set! x.2 L.addup.1) (jump x.2 x.1)))
                    (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1))))))



(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                      ((new-frames ()))
                      (begin 
                        (set! x.1 1)
                        (set! g.1 (mref rax 8))
                        (set! x.2 L.addup.1)
                        (jump x.2 x.1)))
               (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1)))])
     (test-case "Uncover loc1 in mref"
     (check-match (uncover-locals x)
                  `(module
                  ((new-frames ()) (locals ,(list-no-order 'm.1 'q.1)))
                  (define L.addup.2
                    ((new-frames ()) (locals ,(list-no-order 'g.1 'x.1 'x.2)))
                    (begin
                      (set! x.1 1)
                      (set! g.1 (mref rax 8))
                      (set! x.2 L.addup.1)
                      (jump x.2 x.1)))
                  (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1))))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                      ((new-frames ()))
                      (begin 
                        (set! x.1 1)
                        (set! rax (mref g.1 8))
                        (set! x.2 L.addup.1)
                        (jump x.2 x.1)))
               (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1)))])
     (test-case "Uncover loc2 in mref"
     (check-match (uncover-locals x)
                  `(module
                  ((new-frames ()) (locals ,(list-no-order 'm.1 'q.1)))
                  (define L.addup.2
                    ((new-frames ()) (locals ,(list-no-order 'g.1 'x.1 'x.2)))
                    (begin
                      (set! x.1 1)
                      (set! rax (mref g.1 8))
                      (set! x.2 L.addup.1)
                      (jump x.2 x.1)))
                  (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1))))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                      ((new-frames ()))
                      (begin 
                        (set! x.1 1)
                        (set! rax (mref rcx g.1))
                        (set! x.2 L.addup.1)
                        (jump x.2 x.1)))
               (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1)))])
     (test-case "Uncover index in mref"
     (check-match (uncover-locals x)
                  `(module
                  ((new-frames ()) (locals ,(list-no-order 'm.1 'q.1)))
                  (define L.addup.2
                    ((new-frames ()) (locals ,(list-no-order 'g.1 'x.1 'x.2)))
                    (begin
                      (set! x.1 1)
                      (set! rax (mref rcx g.1))
                      (set! x.2 L.addup.1)
                      (jump x.2 x.1)))
                  (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1))))))


(let ([x `(module
                ((new-frames ()))
                (define L.addup.1
                ((new-frames ()))
                 (if (begin (mset! q.1 q.2 q.3) (set! m.1 (mref m.3 m.2)) (< m.1 q.2))
                      (jump L.end.1)
                      (jump L.end.2)))
            (begin (jump L.addup.1)))])
    (test-case " Uncover in effect in begin as pred"
    (check-match (uncover-locals x)
                `(module
                ((new-frames ()) (locals ()))
                (define L.addup.1
                  ((new-frames ()) (locals ,(list-no-order 'm.3 'm.2 'q.1 'q.3 'q.2 'm.1)))
                  (if (begin (mset! q.1 q.2 q.3) (set! m.1 (mref m.3 m.2)) (< m.1 q.2))
                    (jump L.end.1)
                    (jump L.end.2)))
                (begin (jump L.addup.1))))))


(let ([x `(module
                ((new-frames ()))
                (define L.addup.1
                ((new-frames ()))
                 (begin 
                    (begin
                      (mset! x.1 x.2 x.3)
                      (set! q.1 (mref q.2 q.3))) 
                    (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Uncover in effect in begin as effect"
    (check-match (uncover-locals x)
                `(module
                ((new-frames ()) (locals ()))
                (define L.addup.1
                  ((new-frames ()) (locals ,(list-no-order 'x.2 'x.3 'q.3 'q.2 'q.1 'x.1)))
                  (begin (begin (mset! x.1 x.2 x.3) (set! q.1 (mref q.2 q.3))) (jump x.1)))
                (begin (jump L.addup.1))))))


(let ([x `(module
                ((new-frames ()))
                (define L.addup.1
                ((new-frames ()))
                 (begin 
                    (if (true) (mset! x.1 x.2 x.3) (set! q.1 (mref q.2 q.3)))
                    (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Uncover in effect in if as effect"
    (check-match (uncover-locals x)
                `(module
                  ((new-frames ()) (locals ()))
                  (define L.addup.1
                    ((new-frames ()) (locals ,(list-no-order 'x.2 'x.3 'q.3 'q.2 'q.1 'x.1)))
                    (begin
                      (if (true) (mset! x.1 x.2 x.3) (set! q.1 (mref q.2 q.3)))
                      (jump x.1)))
                  (begin (jump L.addup.1))))))

(let ([x `(module
                ((new-frames ()))
                (define L.addup.1
                ((new-frames ()))
                 (begin (return-point L.rp.1 (begin (mset! x.1 x.2 x.3) (set! q.1 (mref q.2 q.3)) (jump x.1))) (jump x.1)))
            (begin (jump L.addup.1)))])
    (test-case " Uncover in effect in return-point"
    (check-match (uncover-locals x)
                `(module
                ((new-frames ()) (locals ()))
                (define L.addup.1
                  ((new-frames ()) (locals ,(list-no-order 'x.2 'x.3 'q.3 'q.2 'q.1 'x.1)))
                  (begin
                    (return-point
                    L.rp.1
                    (begin (mset! x.1 x.2 x.3) (set! q.1 (mref q.2 q.3)) (jump x.1)))
                    (jump x.1)))
                (begin (jump L.addup.1))))))


(let ([x `(module
                ((new-frames ()))
                (define L.addup.1
                ((new-frames ()))
                 (if (true) 
                      (if (false) (begin (mset! x.1 x.2 x.3) (jump x.1)) (begin (set! q.1 (mref q.2 q.3)) (jump x.2)))
                      (if (not (true)) (begin (set! g.1 (mref g.2 g.3)) (jump x.2)) (jump L.end.1))))
            (begin (jump L.addup.1)))])
    (test-case " Uncover in effect in if"
    (check-match (uncover-locals x)
                `(module
                ((new-frames ()) (locals ()))
                (define L.addup.1
                  ((new-frames ()) (locals ,(list-no-order 'g.1 'g.2 'g.3 'x.2 'x.3 'x.1 'q.1 'q.2 'q.3)))
                  (if (true)
                    (if (false)
                      (begin (mset! x.1 x.2 x.3) (jump x.1))
                      (begin (set! q.1 (mref q.2 q.3)) (jump x.2)))
                    (if (not (true))
                      (begin (set! g.1 (mref g.2 g.3)) (jump x.2))
                      (jump L.end.1))))
                (begin (jump L.addup.1))))))

;; ------------------------------------ old cases -------------------------
(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                ((new-frames ()))
                (begin (set! x.1 1)
                      (begin (set! y.1 2)
                      (set! z.1 (bitwise-and z.1 x.1)))
                      (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3)))
                      (jump y.1)))
               (jump L.addup.2))])
     (test-case "Simple case with nested begins - return-point in begin -- bitwise-and"
     (check-match (uncover-locals x)
                  `(module
                  ((new-frames ()) (locals ()))
                  (define L.addup.2
                    ((new-frames ()) (locals ,(list-no-order 'x.3 'z.1 'x.1 'y.1)))
                    (begin
                      (set! x.1 1)
                      (begin (set! y.1 2) (set! z.1 (bitwise-and z.1 x.1)))
                      (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3)))
                      (jump y.1)))
                  (jump L.addup.2)))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                ((new-frames ()))
                (begin (set! x.1 1)
                      (begin (set! y.1 2)
                      (set! z.1 (bitwise-ior z.1 x.1)))
                      (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3)))
                      (jump y.1)))
               (jump L.addup.2))])
     (test-case "Simple case with nested begins - return-point in begin -- bitwise-ior"
     (check-match (uncover-locals x)
                  `(module
                  ((new-frames ()) (locals ()))
                  (define L.addup.2
                    ((new-frames ()) (locals ,(list-no-order 'x.3 'z.1 'x.1 'y.1)))
                    (begin
                      (set! x.1 1)
                      (begin (set! y.1 2) (set! z.1 (bitwise-ior z.1 x.1)))
                      (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3)))
                      (jump y.1)))
                  (jump L.addup.2)))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                ((new-frames ()))
                (begin (set! x.1 1)
                      (begin (set! y.1 2)
                      (set! z.1 (bitwise-xor z.1 x.1)))
                      (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3)))
                      (jump y.1)))
               (jump L.addup.2))])
     (test-case "Simple case with nested begins - return-point in begin -- bitwise-xor"
     (check-match (uncover-locals x)
                  `(module
                  ((new-frames ()) (locals ()))
                  (define L.addup.2
                    ((new-frames ()) (locals ,(list-no-order 'x.3 'z.1 'x.1 'y.1)))
                    (begin
                      (set! x.1 1)
                      (begin (set! y.1 2) (set! z.1 (bitwise-xor z.1 x.1)))
                      (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3)))
                      (jump y.1)))
                  (jump L.addup.2)))))


                  
(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                ((new-frames ()))
                (begin (set! x.1 1)
                      (begin (set! y.1 2)
                      (set! z.1 (arithmetic-shift-right z.1 x.1)))
                      (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3)))
                      (jump y.1)))
               (jump L.addup.2))])
     (test-case "Simple case with nested begins - return-point in begin -- arithmetic-shift-right"
     (check-match (uncover-locals x)
                  `(module
                  ((new-frames ()) (locals ()))
                  (define L.addup.2
                    ((new-frames ()) (locals ,(list-no-order 'x.3 'z.1 'x.1 'y.1)))
                    (begin
                      (set! x.1 1)
                      (begin (set! y.1 2) (set! z.1 (arithmetic-shift-right z.1 x.1)))
                      (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3)))
                      (jump y.1)))
                  (jump L.addup.2)))))


;; ------------------------------------------------------------------------------

;; test with multiple procs
(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                      ((new-frames ()))
                      (begin 
                        (set! x.1 1)
                        (set! x.2 L.addup.1)
                        (jump x.2 x.1)))
               (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1)))])
     (test-case "Simple case with begin"
     (check-match (uncover-locals x)
                  `(module
                    ((new-frames ()) (locals ,(list-no-order 'm.1 'q.1)))
                    (define L.addup.2
                      ((new-frames ()) (locals ,(list-no-order 'x.1 'x.2)))
                      (begin (set! x.1 1) (set! x.2 L.addup.1) (jump x.2 x.1)))
                    (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1))))))



(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                      ((new-frames ()))
                      (begin
                        (set! q.1 1)
                        (set! x.2 2)
                        (set! b.3 4)
                        (if (true) (jump L.addup.1 q.1) (jump L.addup.1 x.2))))
               (jump L.addup.1 x.1))])
     (test-case "Simple case with if"
     (check-match (uncover-locals x)
                  `(module
                    ((new-frames ()) (locals ,(list-no-order 'x.1)))
                    (define L.addup.1
                      ((new-frames ()) (locals ,(list-no-order 'b.3 'x.2 'q.1)))
                      (begin
                        (set! q.1 1)
                        (set! x.2 2)
                        (set! b.3 4)
                        (if (true) (jump L.addup.1 q.1) (jump L.addup.1 x.2))))
                    (jump L.addup.1 x.1)))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                      ((new-frames ()))
                      (begin (set! x.1 1)
                      (set! x.2 2)
                      (begin (set! x.3 4) (if (true) (set! x.1 x.2) (set! x.3 (+ x.3 q.1))))
                      (jump L.addup.2 x.2)))
               (jump L.addup.1 rax))])
     (test-case "Simple case with jump"
     (check-match (uncover-locals x)
                  `(module
                  ((new-frames ()) (locals ()))
                  (define L.addup.1
                    ((new-frames ()) (locals ,(list-no-order 'x.3 'q.1 'x.1 'x.2)))
                    (begin
                      (set! x.1 1)
                      (set! x.2 2)
                      (begin (set! x.3 4) (if (true) (set! x.1 x.2) (set! x.3 (+ x.3 q.1))))
                      (jump L.addup.2 x.2)))
                  (jump L.addup.1 rax)))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                      ((new-frames ()))
                      (begin (set! x.1 1)
                      (set! q.2 2)
                      (set! z.1 L.addup.1)
                      (begin (jump z.1))))
               (begin (set! z.1 L.addup.1) (set! q.2 10) (jump z.1 q.2)))])
     (test-case "Simple case with jump"
     (check-match (uncover-locals x)
                  `(module
                  ((new-frames ()) (locals ,(list-no-order 'q.2 'z.1)))
                  (define L.addup.1
                    ((new-frames ()) (locals ,(list-no-order 'q.2 'x.1 'z.1)))
                    (begin (set! x.1 1) (set! q.2 2) (set! z.1 L.addup.1) (begin (jump z.1))))
                  (begin (set! z.1 L.addup.1) (set! q.2 10) (jump z.1 q.2))))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                      ((new-frames ()))
                      (begin (begin (begin (begin (jump y.1))))))
               (begin (begin (begin (begin (jump x.1))))))])
     (test-case "Simple case with nested begins - no effect, begin in the tail"
     (check-match (uncover-locals x)
                  `(module
                  ((new-frames ()) (locals (x.1)))
                  (define L.addup.2
                    ((new-frames ()) (locals (y.1)))
                    (begin (begin (begin (begin (jump y.1))))))
                  (begin (begin (begin (begin (jump x.1)))))))))

                  
(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                ((new-frames ()))
                (begin (set! x.1 1)
                      (begin (set! y.1 2)
                      (set! z.1 (- z.1 x.1)))
                      (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3)))
                      (jump y.1)))
               (jump L.addup.2))])
     (test-case "Simple case with nested begins - return-point in begin"
     (check-match (uncover-locals x)
                  `(module
                  ((new-frames ()) (locals ()))
                  (define L.addup.2
                    ((new-frames ()) (locals ,(list-no-order 'x.3 'z.1 'x.1 'y.1)))
                    (begin
                      (set! x.1 1)
                      (begin (set! y.1 2) (set! z.1 (- z.1 x.1)))
                      (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3)))
                      (jump y.1)))
                  (jump L.addup.2)))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.1
                      ((new-frames ()))
                      (begin (set! x.1 1)
                          (begin (set! y.1 2)
                            (return-point L.addup.2 (begin (set! rax 5) (set! x.2 3) (jump L.addup.3 x.2 rax))))
                          (begin (set! q.1 1) (set! m.1 5) (jump rcx q.1 m.1))))
               (begin (set! x.1 5) (jump L.addup.1 x.1)))])
     (test-case "Simple case with nested begins - nested begin in effect"
     (check-match (uncover-locals x)
                  `(module
                  ((new-frames ()) (locals (x.1)))
                  (define L.addup.1
                    ((new-frames ()) (locals ,(list-no-order 'y.1 'x.2 'x.1 'm.1 'q.1)))
                    (begin
                      (set! x.1 1)
                      (begin
                        (set! y.1 2)
                        (return-point
                        L.addup.2
                        (begin (set! rax 5) (set! x.2 3) (jump L.addup.3 x.2 rax))))
                      (begin (set! q.1 1) (set! m.1 5) (jump rcx q.1 m.1))))
                  (begin (set! x.1 5) (jump L.addup.1 x.1))))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                      ((new-frames ()))
                      (begin (set! x.1 1)
                        (set! x.2 L.addup.2)
                        (begin (return-point L.rp.1 (if (not (< q.1 2)) (jump x.2 x.1) (jump x.2 q.1)))
                          (if (true) (set! z.1 10) (set! q.2 (+ q.2 10))))
                        (jump L.addup.2 x.1 z.1)))
                  (begin (set! x.1 1)
                      (set! x.7 L.addup.2)
                      (begin (set! y.1 2)
                        (if (true) (set! z.5 10) (set! z.5 (- z.5 q.2))))
                      (jump x.7 x.1 z.5)))])
     (test-case "Simple case with begin nesting if in effect position"
     (check-match (uncover-locals x)
                  `(module
                    ((new-frames ()) (locals ,(list-no-order 'y.1 'q.2 'z.5 'x.1 'x.7)))
                    (define L.addup.2
                      ((new-frames ()) (locals ,(list-no-order 'q.1 'q.2 'x.2 'z.1 'x.1)))
                      (begin
                        (set! x.1 1)
                        (set! x.2 L.addup.2)
                        (begin
                          (return-point
                          L.rp.1
                          (if (not (< q.1 2)) (jump x.2 x.1) (jump x.2 q.1)))
                          (if (true) (set! z.1 10) (set! q.2 (+ q.2 10))))
                        (jump L.addup.2 x.1 z.1)))
                    (begin
                      (set! x.1 1)
                      (set! x.7 L.addup.2)
                      (begin (set! y.1 2) (if (true) (set! z.5 10) (set! z.5 (- z.5 q.2))))
                      (jump x.7 x.1 z.5))))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                      ((new-frames ()))
                      (begin 
                        (begin
                          (set! x.1 L.addup.1) 
                          (begin (if (< q.1 5) (jump x.1 q.1) 
                                              (begin (set! y.1 10) (return-point L.addup.3 (begin (set! q.2 5) (set! q.3 (- q.3 2)) (jump L.addup.1 q.2 q.3)))
                                                (jump z.1)))))))
              (if (true) (jump x.2) (begin (set! x.3 L.addup.2) (jump x.3))))])
     (test-case "Simple case with begin nesting if in tail position"
     (check-match (uncover-locals x)
                  `(module
                  ((new-frames ()) (locals ,(list-no-order 'x.3 'x.2)))
                  (define L.addup.2
                    ((new-frames ()) (locals ,(list-no-order 'z.1 'y.1 'q.3 'q.2 'x.1 'q.1)))
                    (begin
                      (begin
                        (set! x.1 L.addup.1)
                        (begin
                          (if (< q.1 5)
                            (jump x.1 q.1)
                            (begin
                              (set! y.1 10)
                              (return-point
                              L.addup.3
                              (begin
                                (set! q.2 5)
                                (set! q.3 (- q.3 2))
                                (jump L.addup.1 q.2 q.3)))
                              (jump z.1)))))))
                  (if (true) (jump x.2) (begin (set! x.3 L.addup.2) (jump x.3)))))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                      ((new-frames ()))
                      (begin (set! x.1 1)
                      (if (false) 
                          (if (begin (set! q.1 5) (return-point L.rp.1 (begin (set! q.2 5) (jump L.addup.2 q.2))) (!= q.1 5)) 
                              (jump x.1) 
                              (begin (set! q.1 z.1) (if (false) (jump q.1) (begin (set! m.1 (- m.1 5)) (jump L.addup.1 m.1))))) 
                          (jump L.addup.3 x.1 q.1))))
               (jump x.2 fv0))])
     (test-case "Simple case with begin nesting if in tail position"
     (check-match (uncover-locals x)
                  `(module
                  ((new-frames ()) (locals (x.2)))
                  (define L.addup.2
                    ((new-frames ()) (locals ,(list-no-order 'q.1 'q.2 'x.1 'z.1 'm.1)))
                    (begin
                      (set! x.1 1)
                      (if (false)
                        (if (begin
                              (set! q.1 5)
                              (return-point L.rp.1 (begin (set! q.2 5) (jump L.addup.2 q.2)))
                              (!= q.1 5))
                          (jump x.1)
                          (begin
                            (set! q.1 z.1)
                            (if (false)
                              (jump q.1)
                              (begin (set! m.1 (- m.1 5)) (jump L.addup.1 m.1)))))
                        (jump L.addup.3 x.1 q.1))))
                  (jump x.2 fv0)))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                      ((new-frames ()))
                       (begin (if (true) 
                          (if (false) (set! x.1 10) (set! x.2 x.3))
                          (set! z.1 (+ z.1 5)))
                       (return-point L.rp.1 (if (begin (return-point L.addup.1 (jump L.addup.2)) (set! q.2 5) (= q.2 5)) (jump x.4) (jump L.addup.2)))
                       (jump z.2)))
               (begin (set! x.1 2) (set! x.2 2) (jump x.1)))])
     (test-case "Simple case with begin nesting nested if in effect position"
     (check-match (uncover-locals x)
                  `(module
                  ((new-frames ()) (locals ,(list-no-order 'x.2 'x.1)))
                  (define L.addup.2
                    ((new-frames ()) (locals ,(list-no-order 'q.2 'x.4 'x.2 'x.3 'x.1 'z.1 'z.2)))
                    (begin
                      (if (true)
                        (if (false) (set! x.1 10) (set! x.2 x.3))
                        (set! z.1 (+ z.1 5)))
                      (return-point
                      L.rp.1
                      (if (begin
                            (return-point L.addup.1 (jump L.addup.2))
                            (set! q.2 5)
                            (= q.2 5))
                        (jump x.4)
                        (jump L.addup.2)))
                      (jump z.2)))
                  (begin (set! x.1 2) (set! x.2 2) (jump x.1))))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                ((new-frames ()))
                  (if (if (true) 
                          (begin (set! q.1 5) (return-point L.rp.1 (jump q.2)) (< q.1 x.1)) 
                          (begin (set! q.2 7) (set! q.2 (- q.2 w.1)) (return-point L.rp.2 (jump w.2)) (!= q.2 7)))
                    (jump y.1)
                    (begin (jump x.1))))
                (begin (set! q.1 L.addup.1) (jump q.1)))])
     (test-case "Simple case with if predicate in tail position"
     (check-match (uncover-locals x)
                  `(module
                  ((new-frames ()) (locals (q.1)))
                  (define L.addup.2
                    ((new-frames ()) (locals ,(list-no-order 'y.1 'w.1 'w.2 'q.1 'x.1 'q.2)))
                    (if (if (true)
                          (begin (set! q.1 5) (return-point L.rp.1 (jump q.2)) (< q.1 x.1))
                          (begin
                            (set! q.2 7)
                            (set! q.2 (- q.2 w.1))
                            (return-point L.rp.2 (jump w.2))
                            (!= q.2 7)))
                      (jump y.1)
                      (begin (jump x.1))))
                  (begin (set! q.1 L.addup.1) (jump q.1))))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                 ((new-frames ()))
                  (if (if (if (> q.1 z.1) (begin (set! x.2 5) (return-point L.addup.1 (jump x.3 x.4)) (!= x.2 5)) (true)) (if (false) (< x.1 y.1) (not (> x.3 5))) (if (true) (false) (not (< z.1 y.1))))
                  (jump q.1)
                  (begin (jump x.1))))
               (jump x.2))])
     (test-case "Simple case with nested if predicate in tail position"
     (check-match (uncover-locals x)
                  `(module
                  ((new-frames ()) (locals (x.2)))
                  (define L.addup.2
                    ((new-frames ()) (locals ,(list-no-order 'y.1 'x.1 'x.2 'x.4 'x.3 'z.1 'q.1)))
                    (if (if (if (> q.1 z.1)
                              (begin
                                (set! x.2 5)
                                (return-point L.addup.1 (jump x.3 x.4))
                                (!= x.2 5))
                              (true))
                          (if (false) (< x.1 y.1) (not (> x.3 5)))
                          (if (true) (false) (not (< z.1 y.1))))
                      (jump q.1)
                      (begin (jump x.1))))
                  (jump x.2)))))


(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                      ((new-frames ()))
                      (if (not (begin (set! x.1 1) (return-point L.rp.1 (begin (set! x.2 7) (set! l.1 (- l.1 q.1)) (jump L.addup.1 l.1 x.2))) (not (<= y.1 z.1))))
                        (jump z.1)
                        (begin (set! q.1 (+ q.1 z.1)) (jump L.addup.1 q.1))))
               (jump x.2))])
     (test-case "Simple case with not predicate in tail position"
     (check-match (uncover-locals x)
                  `(module
                  ((new-frames ()) (locals (x.2)))
                  (define L.addup.2
                    ((new-frames ()) (locals ,(list-no-order 'l.1 'x.2 'q.1 'x.1 'z.1 'y.1)))
                    (if (not
                        (begin
                          (set! x.1 1)
                          (return-point
                            L.rp.1
                            (begin
                              (set! x.2 7)
                              (set! l.1 (- l.1 q.1))
                              (jump L.addup.1 l.1 x.2)))
                          (not (<= y.1 z.1))))
                      (jump z.1)
                      (begin (set! q.1 (+ q.1 z.1)) (jump L.addup.1 q.1))))
                  (jump x.2)))))

(let ([x `(module
              ((new-frames ()))
              (define L.addup.2
                      ((new-frames ()))
                      (if (not (not (<= x.1 y.1)))
                      (begin (return-point L.rp.1 (if (not (begin (set! q.1 7) (!= q.1 7))) (jump L.addup.1 q.1) (jump L.addup.2 q.1))) (jump x.3))
                      (begin (jump x.2))))
              (begin (if (true) (set! x.3 5) (set! q.2 (- q.2 7))) (jump L.addup.2 x.3 q.2)))])
     (test-case "Simple case with nested not predicate in tail position"
     (check-match (uncover-locals x)
                  `(module
                  ((new-frames ()) (locals ,(list-no-order 'q.2 'x.3)))
                  (define L.addup.2
                    ((new-frames ()) (locals ,(list-no-order 'x.2 'x.3 'q.1 'y.1 'x.1)))
                    (if (not (not (<= x.1 y.1)))
                      (begin
                        (return-point
                        L.rp.1
                        (if (not (begin (set! q.1 7) (!= q.1 7)))
                          (jump L.addup.1 q.1)
                          (jump L.addup.2 q.1)))
                        (jump x.3))
                      (begin (jump x.2))))
                  (begin
                    (if (true) (set! x.3 5) (set! q.2 (- q.2 7)))
                    (jump L.addup.2 x.3 q.2))))))



(let ([x '(module
          ((new-frames ()))
          (define L.L.L.swap.1.1.2
            ((new-frames (())))
            (begin
              (set! tmp-ra.7 r15)
              (set! x.1.2.5 rdi)
              (set! y.2.1.4 rsi)
              (if (< y.2.1.4 x.1.2.5)
                (begin (set! rax x.1.2.5) (jump tmp-ra.7 rbp rax))
                (begin
                  (return-point L.rp.3
                    (begin
                      (set! rsi x.1.2.5)
                      (set! rdi y.2.1.4)
                      (set! r15 L.rp.3)
                      (jump L.L.L.swap.1.1.2 rbp r15 rdi rsi)))
                  (set! z.3.3.6 rax)
                  (set! rax z.3.3.6)
                  (jump tmp-ra.7 rbp rax)))))
          (begin
            (set! tmp-ra.8 r15)
            (set! rsi 2)
            (set! rdi 1)
            (set! r15 tmp-ra.8)
            (jump L.L.L.swap.1.1.2 rbp r15 rdi rsi)))])
     (test-case "Textbook case"
     (check-match (uncover-locals x)
                 `(module
                  ((new-frames ()) (locals (tmp-ra.8)))
                  (define L.L.L.swap.1.1.2
                    ((new-frames (())) (locals ,(list-no-order 'z.3.3.6 'tmp-ra.7 'x.1.2.5 'y.2.1.4)))
                    (begin
                      (set! tmp-ra.7 r15)
                      (set! x.1.2.5 rdi)
                      (set! y.2.1.4 rsi)
                      (if (< y.2.1.4 x.1.2.5)
                        (begin (set! rax x.1.2.5) (jump tmp-ra.7 rbp rax))
                        (begin
                          (return-point L.rp.3
                            (begin
                              (set! rsi x.1.2.5)
                              (set! rdi y.2.1.4)
                              (set! r15 L.rp.3)
                              (jump L.L.L.swap.1.1.2 rbp r15 rdi rsi)))
                          (set! z.3.3.6 rax)
                          (set! rax z.3.3.6)
                          (jump tmp-ra.7 rbp rax)))))
                  (begin
                    (set! tmp-ra.8 r15)
                    (set! rsi 2)
                    (set! rdi 1)
                    (set! r15 tmp-ra.8)
                    (jump L.L.L.swap.1.1.2 rbp r15 rdi rsi))))))
