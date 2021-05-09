#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/2c-run-time
 cpsc411/test-suite/utils
 racket/match
 rackunit)

(require "../component/undead.rkt")

; input: asm-pred-lang-v8/locals
; output: asm-pred-lang-v8/undead
; purpose: Check if the output of undead-analysis matches the output of interrogator 


(let ([x `(module
                  ((new-frames ()) (locals (y.1 x.1 x.2 x.3 y.3 y.2)))
                  (begin
                    (set! x.1 0)
                    (set! x.2 1)
                    (set! x.3 2)
                    (set! y.1 3)
                    (set! y.2 4)
                    (set! y.3 5)
                    (mset! y.1 y.2 y.3)
                    (jump L.foo.1)))])
     (test-case "Undead-analysis with mset! from piazza"
     (check-match (undead-analysis x)
                  `(module
                  ((new-frames ())
                  (locals ,(list-no-order 'y.1 'x.1 'x.2 'x.3 'y.3 'y.2))
                  (call-undead ())
                  (undead-out (() () () (y.1) ,(list-no-order 'y.2 'y.1) ,(list-no-order 'y.3 'y.2 'y.1) () ())))
                  (begin
                    (set! x.1 0)
                    (set! x.2 1)
                    (set! x.3 2)
                    (set! y.1 3)
                    (set! y.2 4)
                    (set! y.3 5)
                    (mset! y.1 y.2 y.3)
                    (jump L.foo.1))))))



(let ([x `(module
           ((new-frames ())
            (locals (index.2 index.1 x.1 return.1)))
            (begin
              (begin (set! return.1 r12)
                      (set! r12 (+ r12 8)))
           (set! x.1 42)
           (set! index.1 0)
           (mset! return.1 index.1 x.1)
           (set! index.2 0)
           (set! rax (mref return.1 index.2))
           (jump r15 rax)))])
     (test-case "Undead-analysis with mset! mref from piazza"
     (check-match (undead-analysis x)
                  `(module
                  ((new-frames ())
                  (locals ,(list-no-order 'index.2 'index.1 'x.1 'return.1))
                  (call-undead ())
                  (undead-out
                    ((,(list-no-order 'r12 'r15 'return.1) ,(list-no-order 'r15 'return.1))
                    ,(list-no-order 'r15 'x.1 'return.1)
                    ,(list-no-order 'r15 'x.1 'index.1 'return.1)
                    ,(list-no-order 'return.1 'r15)
                    ,(list-no-order 'index.2 'return.1 'r15)
                    ,(list-no-order 'r15 'rax)
                    (rax))))
                  (begin
                    (begin (set! return.1 r12) (set! r12 (+ r12 8)))
                    (set! x.1 42)
                    (set! index.1 0)
                    (mset! return.1 index.1 x.1)
                    (set! index.2 0)
                    (set! rax (mref return.1 index.2))
                    (jump r15 rax))))))



(let ([x `(module
              ((new-frames ()) (locals (m.1 q.1)))
              (define L.addup.2
                ((new-frames ()) (locals (x.1 x.2 g.1)))
                (begin (set! x.1 1) (mset! g.1 8 8) (set! x.2 L.addup.1) (jump x.2 x.1)))
              (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1)))])
     (test-case "Undead-analysis with mset! 1"
     (check-match (undead-analysis x)
                  `(module
                  ((new-frames ())
                  (locals ,(list-no-order 'm.1 'q.1))
                  (call-undead ())
                  (undead-out ((q.1) ,(list-no-order 'm.1 'q.1) ,(list-no-order 'm.1 'q.1))))
                  (define L.addup.2
                    ((new-frames ())
                    (locals ,(list-no-order 'x.1 'x.2 'g.1))
                    (undead-out (,(list-no-order 'x.1 'g.1) (x.1) ,(list-no-order 'x.2 'x.1) (x.1)))
                    (call-undead ()))
                    (begin (set! x.1 1) (mset! g.1 8 8) (set! x.2 L.addup.1) (jump x.2 x.1)))
                  (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1))))))



(let ([x `(module
              ((new-frames ()) (locals (m.1 q.1)))
              (define L.addup.2
                ((new-frames ()) (locals (x.1 x.2 g.1 o.1)))
                (begin (set! x.1 1) (mset! rax g.1 o.1) (set! x.2 L.addup.1) (jump x.2 x.1)))
              (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1)))])
     (test-case "Undead-analysis with mset! 2"
     (check-match (undead-analysis x)
                  `(module
                  ((new-frames ())
                  (locals ,(list-no-order 'm.1 'q.1))
                  (call-undead ())
                  (undead-out ((q.1) ,(list-no-order 'm.1 'q.1) ,(list-no-order 'm.1 'q.1))))
                  (define L.addup.2
                    ((new-frames ())
                    (locals ,(list-no-order 'x.1 'x.2 'g.1 'o.1))
                    (undead-out (,(list-no-order 'x.1 'o.1 'g.1 'rax) (x.1) ,(list-no-order 'x.2 'x.1) (x.1)))
                    (call-undead ()))
                    (begin
                      (set! x.1 1)
                      (mset! rax g.1 o.1)
                      (set! x.2 L.addup.1)
                      (jump x.2 x.1)))
                  (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1))))))


;; revised
(let ([x `(module
                  ((new-frames ()) (locals (m.1 q.1)))
                  (define L.addup.2
                    ((new-frames ()) (locals (g.1 x.1 x.2)))
                    (begin
                      (set! x.1 1)
                      (set! g.1 (mref rax 8))
                      (set! x.2 L.addup.1)
                      (jump x.2 x.1)))
                  (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1)))])
     (test-case "Undead-analysis with mref"
     (check-match (undead-analysis x)
                  `(module
                  ((new-frames ())
                  (locals ,(list-no-order 'm.1 'q.1))
                  (call-undead ())
                  (undead-out ((q.1) ,(list-no-order 'm.1 'q.1) ,(list-no-order 'm.1 'q.1))))
                  (define L.addup.2
                    ((new-frames ())
                    (locals ,(list-no-order 'g.1 'x.1 'x.2))
                    (undead-out (,(list-no-order 'rax 'x.1) (x.1) ,(list-no-order 'x.2 'x.1) (x.1)))
                    (call-undead ()))
                    (begin
                      (set! x.1 1)
                      (set! g.1 (mref rax 8))
                      (set! x.2 L.addup.1)
                      (jump x.2 x.1)))
                  (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1))))))


;; revised
(let ([x  `(module
                ((new-frames ()) (locals ()))
                (define L.addup.1
                  ((new-frames ()) (locals (m.3 m.2 q.1 q.3 q.2 m.1)))
                  (if (begin (mset! q.1 q.2 q.3) (set! m.1 (mref m.3 m.2)) (< m.1 q.2))
                    (jump L.end.1)
                    (jump L.end.2)))
                (begin (jump L.addup.1)))])
     (test-case "Undead-analysis in effect in begin as pred"
     (check-match (undead-analysis x)
                  `(module
                  ((new-frames ()) (locals ()) (call-undead ()) (undead-out (())))
                  (define L.addup.1
                    ((new-frames ())
                    (locals ,(list-no-order 'm.3 'm.2 'q.1 'q.3 'q.2 'm.1))
                    (undead-out ((,(list-no-order 'm.2 'm.3 'q.2) ,(list-no-order 'q.2 'm.1) ()) () ()))
                    (call-undead ()))
                    (if (begin (mset! q.1 q.2 q.3) (set! m.1 (mref m.3 m.2)) (< m.1 q.2))
                      (jump L.end.1)
                      (jump L.end.2)))
                  (begin (jump L.addup.1))))))
;; revised
(let ([x  `(module
                ((new-frames ()) (locals ()))
                (define L.addup.1
                  ((new-frames ()) (locals (x.2 x.3 q.3 q.2 q.1 x.1)))
                  (begin (begin (mset! x.1 x.2 x.3) (set! q.1 (mref q.2 q.3))) (jump x.1)))
                (begin (jump L.addup.1)))])
     (test-case "Undead-analysis in effect in begin as effect"
     (check-match (undead-analysis x)
                  `(module
                  ((new-frames ()) (locals ()) (call-undead ()) (undead-out (())))
                  (define L.addup.1
                    ((new-frames ())
                    (locals ,(list-no-order 'x.2 'x.3 'q.3 'q.2 'q.1 'x.1))
                    (undead-out ((,(list-no-order 'q.3 'q.2 'x.1) (x.1)) ()))
                    (call-undead ()))
                    (begin (begin (mset! x.1 x.2 x.3) (set! q.1 (mref q.2 q.3))) (jump x.1)))
                  (begin (jump L.addup.1))))))


;; revised
(let ([x  `(module
                  ((new-frames ()) (locals ()))
                  (define L.addup.1
                    ((new-frames ()) (locals (x.2 x.3 q.3 q.2 q.1 x.1)))
                    (begin
                      (if (true) (mset! x.1 x.2 x.3) (set! q.1 (mref q.2 q.3)))
                      (jump x.1)))
                  (begin (jump L.addup.1)))])
     (test-case "Undead-analysis in effect in begin as effect"
     (check-match (undead-analysis x)
                  `(module
                    ((new-frames ()) (locals ()) (call-undead ()) (undead-out (())))
                    (define L.addup.1
                      ((new-frames ())
                      (locals ,(list-no-order 'x.2 'x.3 'q.3 'q.2 'q.1 'x.1))
                      (undead-out ((,(list-no-order 'q.2 'q.3 'x.3 'x.2 'x.1) (x.1) (x.1)) ()))
                      (call-undead ()))
                      (begin
                        (if (true) (mset! x.1 x.2 x.3) (set! q.1 (mref q.2 q.3)))
                        (jump x.1)))
                    (begin (jump L.addup.1))))))

;; revised
(let ([x   `(module
                ((new-frames ()) (locals ()))
                (define L.addup.1
                  ((new-frames ()) (locals (x.2 x.3 q.3 q.2 q.1 x.1)))
                  (begin
                    (return-point
                    L.rp.1
                    (begin (mset! x.1 x.2 x.3) (set! q.1 (mref q.2 q.3)) (jump x.1)))
                    (jump x.1)))
                (begin (jump L.addup.1)))])
     (test-case "Undead-analysis in effect in return-point"
     (check-match (undead-analysis x)
                  `(module
                  ((new-frames ()) (locals ()) (call-undead ()) (undead-out (())))
                  (define L.addup.1
                    ((new-frames ())
                    (locals ,(list-no-order 'x.2 'x.3 'q.3 'q.2 'q.1 'x.1))
                    (undead-out (((x.1) (,(list-no-order 'q.3 'q.2 'x.1) (x.1) ())) ()))
                    (call-undead (x.1)))
                    (begin
                      (return-point
                      L.rp.1
                      (begin (mset! x.1 x.2 x.3) (set! q.1 (mref q.2 q.3)) (jump x.1)))
                      (jump x.1)))
                  (begin (jump L.addup.1))))))


;; revised
(let ([x   `(module
                ((new-frames ()) (locals ()))
                (define L.addup.1
                  ((new-frames ()) (locals (g.1 g.2 g.3 x.2 x.3 x.1 q.1 q.2 q.3)))
                  (if (true)
                    (if (false)
                      (begin (mset! x.1 x.2 x.3) (jump x.1))
                      (begin (set! q.1 (mref q.2 q.3)) (jump x.2)))
                    (if (not (true))
                      (begin (set! g.1 (mref g.2 g.3)) (jump x.2))
                      (jump L.end.1))))
                (begin (jump L.addup.1)))])
     (test-case "Undead-analysis in effect in if"
     (check-match (undead-analysis x)
                  `(module
                  ((new-frames ()) (locals ()) (call-undead ()) (undead-out (())))
                  (define L.addup.1
                    ((new-frames ())
                    (locals ,(list-no-order 'g.1 'g.2 'g.3 'x.2 'x.3 'x.1 'q.1 'q.2 'q.3))
                    (undead-out
                      (,(list-no-order 'g.2 'g.3 'q.2 'q.3 'x.3 'x.2 'x.1)
                      (,(list-no-order 'q.2 'q.3 'x.3 'x.2 'x.1) ((x.1) ()) ((x.2) ()))
                      (,(list-no-order 'g.3 'g.2 'x.2) ((x.2) ()) ())))
                    (call-undead ()))
                    (if (true)
                      (if (false)
                        (begin (mset! x.1 x.2 x.3) (jump x.1))
                        (begin (set! q.1 (mref q.2 q.3)) (jump x.2)))
                      (if (not (true))
                        (begin (set! g.1 (mref g.2 g.3)) (jump x.2))
                        (jump L.end.1))))
                  (begin (jump L.addup.1))))))
;; ----------------------------------------- old cases -----------------------------------------------
(let ([x   `(module
            ((new-frames ()) (locals ()))
            (define L.addup.1
              ((new-frames ()) (locals (x.3 q.1 x.1 x.2)))
              (begin
                (set! x.1 1)
                (set! q.1 x.1)
                (set! x.2 2)
                (begin (set! x.3 4) (if (true) (set! x.1 x.2) (set! x.3 (bitwise-and x.3 q.1))))
                (jump L.addup.2 x.2)))
            (begin (set! rax 5) (jump L.addup.1 rax)))])
     (test-case "Simple case with jump -- bitwise-and"
     (check-match (undead-analysis x)
                `(module
                ((new-frames ()) (locals ()) (call-undead ()) (undead-out ((rax) (rax))))
                (define L.addup.1
                  ((new-frames ())
                  (locals ,(list-no-order 'x.3 'q.1 'x.1 'x.2))
                  (undead-out
                    ((x.1)
                    (q.1)
                    ,(list-no-order 'q.1 'x.2)
                    (,(list-no-order 'x.3 'q.1 'x.2) (,(list-no-order 'x.3 'q.1 'x.2) (x.2) (x.2)))
                    (x.2)))
                  (call-undead ()))
                  (begin
                    (set! x.1 1)
                    (set! q.1 x.1)
                    (set! x.2 2)
                    (begin (set! x.3 4) (if (true) (set! x.1 x.2) (set! x.3 (bitwise-and x.3 q.1))))
                    (jump L.addup.2 x.2)))
                (begin (set! rax 5) (jump L.addup.1 rax))))))


(let ([x   `(module
            ((new-frames ()) (locals ()))
            (define L.addup.1
              ((new-frames ()) (locals (x.3 q.1 x.1 x.2)))
              (begin
                (set! x.1 1)
                (set! q.1 x.1)
                (set! x.2 2)
                (begin (set! x.3 4) (if (true) (set! x.1 x.2) (set! x.3 (bitwise-ior x.3 q.1))))
                (jump L.addup.2 x.2)))
            (begin (set! rax 5) (jump L.addup.1 rax)))])
     (test-case "Simple case with jump -- bitwise-ior"
     (check-match (undead-analysis x)
                `(module
                ((new-frames ()) (locals ()) (call-undead ()) (undead-out ((rax) (rax))))
                (define L.addup.1
                  ((new-frames ())
                  (locals ,(list-no-order 'x.3 'q.1 'x.1 'x.2))
                  (undead-out
                    ((x.1)
                    (q.1)
                    ,(list-no-order 'q.1 'x.2)
                    (,(list-no-order 'x.3 'q.1 'x.2) (,(list-no-order 'x.3 'q.1 'x.2) (x.2) (x.2)))
                    (x.2)))
                  (call-undead ()))
                  (begin
                    (set! x.1 1)
                    (set! q.1 x.1)
                    (set! x.2 2)
                    (begin (set! x.3 4) (if (true) (set! x.1 x.2) (set! x.3 (bitwise-ior x.3 q.1))))
                    (jump L.addup.2 x.2)))
                (begin (set! rax 5) (jump L.addup.1 rax))))))


(let ([x   `(module
            ((new-frames ()) (locals ()))
            (define L.addup.1
              ((new-frames ()) (locals (x.3 q.1 x.1 x.2)))
              (begin
                (set! x.1 1)
                (set! q.1 x.1)
                (set! x.2 2)
                (begin (set! x.3 4) (if (true) (set! x.1 x.2) (set! x.3 (bitwise-xor x.3 q.1))))
                (jump L.addup.2 x.2)))
            (begin (set! rax 5) (jump L.addup.1 rax)))])
     (test-case "Simple case with jump -- bitwise-xor"
     (check-match (undead-analysis x)
                `(module
                ((new-frames ()) (locals ()) (call-undead ()) (undead-out ((rax) (rax))))
                (define L.addup.1
                  ((new-frames ())
                  (locals ,(list-no-order 'x.3 'q.1 'x.1 'x.2))
                  (undead-out
                    ((x.1)
                    (q.1)
                    ,(list-no-order 'q.1 'x.2)
                    (,(list-no-order 'x.3 'q.1 'x.2) (,(list-no-order 'x.3 'q.1 'x.2) (x.2) (x.2)))
                    (x.2)))
                  (call-undead ()))
                  (begin
                    (set! x.1 1)
                    (set! q.1 x.1)
                    (set! x.2 2)
                    (begin (set! x.3 4) (if (true) (set! x.1 x.2) (set! x.3 (bitwise-xor x.3 q.1))))
                    (jump L.addup.2 x.2)))
                (begin (set! rax 5) (jump L.addup.1 rax))))))


(let ([x   `(module
            ((new-frames ()) (locals ()))
            (define L.addup.1
              ((new-frames ()) (locals (x.3 q.1 x.1 x.2)))
              (begin
                (set! x.1 1)
                (set! q.1 x.1)
                (set! x.2 2)
                (begin (set! x.3 4) (if (true) (set! x.1 x.2) (set! x.3 (arithmetic-shift-right x.3 q.1))))
                (jump L.addup.2 x.2)))
            (begin (set! rax 5) (jump L.addup.1 rax)))])
     (test-case "Simple case with jump -- arithmetic-shift-right"
     (check-match (undead-analysis x)
                `(module
                ((new-frames ()) (locals ()) (call-undead ()) (undead-out ((rax) (rax))))
                (define L.addup.1
                  ((new-frames ())
                  (locals ,(list-no-order 'x.3 'q.1 'x.1 'x.2))
                  (undead-out
                    ((x.1)
                    (q.1)
                    ,(list-no-order 'q.1 'x.2)
                    (,(list-no-order 'x.3 'q.1 'x.2) (,(list-no-order 'x.3 'q.1 'x.2) (x.2) (x.2)))
                    (x.2)))
                  (call-undead ()))
                  (begin
                    (set! x.1 1)
                    (set! q.1 x.1)
                    (set! x.2 2)
                    (begin (set! x.3 4) (if (true) (set! x.1 x.2) (set! x.3 (arithmetic-shift-right x.3 q.1))))
                    (jump L.addup.2 x.2)))
                (begin (set! rax 5) (jump L.addup.1 rax))))))




;; ------------------------------------------------------------------------------
(let ([x `(module
                ((new-frames ()) (locals (m.1 q.1)))
                (define L.addup.2
                  ((new-frames ()) (locals (x.1 x.2)))
                  (begin (set! x.1 1) (set! x.2 L.addup.1) (jump x.2 x.1)))
                (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1)))])
     (test-case "Simple case with begin"
     (check-match (undead-analysis x)
                `(module
                ((new-frames ())
                (locals ,(list-no-order 'm.1 'q.1))
                (call-undead ())
                (undead-out ((q.1) ,(list-no-order 'm.1 'q.1) ,(list-no-order 'm.1 'q.1))))
                (define L.addup.2
                  ((new-frames ())
                  (locals ,(list-no-order 'x.1 'x.2))
                  (undead-out ((x.1) ,(list-no-order 'x.2 'x.1) (x.1)))
                  (call-undead ()))
                  (begin (set! x.1 1) (set! x.2 L.addup.1) (jump x.2 x.1)))
                (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1))))))


(let ([x `(module
          ((new-frames ()) (locals (x.1)))
          (define L.addup.1
            ((new-frames ()) (locals (b.3 x.2 q.1)))
            (begin
              (set! q.1 1)
              (set! x.2 2)
              (set! b.3 4)
              (if (true) (jump L.addup.1 q.1) (jump L.addup.1 x.2))))
          (begin (set! x.1 5) (jump L.addup.1 x.1)))])
     (test-case "Simple case with if"
     (check-match (undead-analysis x)
                `(module
                ((new-frames ()) (locals (x.1)) (call-undead ()) (undead-out ((x.1) (x.1))))
                (define L.addup.1
                  ((new-frames ())
                  (locals (b.3 x.2 q.1))
                  (undead-out ((q.1) ,(list-no-order 'x.2 'q.1) ,(list-no-order 'x.2 'q.1) (,(list-no-order 'x.2 'q.1) (q.1) (x.2))))
                  (call-undead ()))
                  (begin
                    (set! q.1 1)
                    (set! x.2 2)
                    (set! b.3 4)
                    (if (true) (jump L.addup.1 q.1) (jump L.addup.1 x.2))))
                (begin (set! x.1 5) (jump L.addup.1 x.1))))))

(let ([x   `(module
            ((new-frames ()) (locals ()))
            (define L.addup.1
              ((new-frames ()) (locals (x.3 q.1 x.1 x.2)))
              (begin
                (set! x.1 1)
                (set! q.1 x.1)
                (set! x.2 2)
                (begin (set! x.3 4) (if (true) (set! x.1 x.2) (set! x.3 (+ x.3 q.1))))
                (jump L.addup.2 x.2)))
            (begin (set! rax 5) (jump L.addup.1 rax)))])
     (test-case "Simple case with halt"
     (check-match (undead-analysis x)
                `(module
                ((new-frames ()) (locals ()) (call-undead ()) (undead-out ((rax) (rax))))
                (define L.addup.1
                  ((new-frames ())
                  (locals ,(list-no-order 'x.3 'q.1 'x.1 'x.2))
                  (undead-out
                    ((x.1)
                    (q.1)
                    ,(list-no-order 'q.1 'x.2)
                    (,(list-no-order 'x.3 'q.1 'x.2) (,(list-no-order 'x.3 'q.1 'x.2) (x.2) (x.2)))
                    (x.2)))
                  (call-undead ()))
                  (begin
                    (set! x.1 1)
                    (set! q.1 x.1)
                    (set! x.2 2)
                    (begin (set! x.3 4) (if (true) (set! x.1 x.2) (set! x.3 (+ x.3 q.1))))
                    (jump L.addup.2 x.2)))
                (begin (set! rax 5) (jump L.addup.1 rax))))))


(let ([x   `(module
            ((new-frames ()) (locals (q.2 z.1)))
            (define L.addup.1
              ((new-frames ()) (locals (q.2 x.1 z.1)))
              (begin (set! x.1 1) (set! q.2 2) (set! z.1 L.addup.1) (begin (jump z.1))))
            (begin (set! z.1 L.addup.1) (set! q.2 10) (jump z.1 q.2)))])
     (test-case "Simple case with halt"
     (check-match (undead-analysis x)
                `(module
                ((new-frames ())
                (locals ,(list-no-order 'q.2 'z.1))
                (call-undead ())
                (undead-out ((z.1) ,(list-no-order 'z.1 'q.2) (q.2))))
                (define L.addup.1
                  ((new-frames ())
                  (locals ,(list-no-order 'q.2 'x.1 'z.1))
                  (undead-out (() () (z.1) (())))
                  (call-undead ()))
                  (begin (set! x.1 1) (set! q.2 2) (set! z.1 L.addup.1) (begin (jump z.1))))
                (begin (set! z.1 L.addup.1) (set! q.2 10) (jump z.1 q.2))))))

;; check-equal? is used
(let ([x   `(module
            ((new-frames ()) (locals (x.1)))
            (define L.addup.2
              ((new-frames ()) (locals (y.1)))
              (begin (begin (begin (begin (set! y.1 L.addup.1) (jump y.1))))))
            (begin (begin (begin (begin (set! x.1 L.addup.2) (jump x.1))))))])
     (test-case "Simple case with nested begins - no effect, begin in the tail"
     (check-equal? (undead-analysis x)
                `(module
                ((new-frames ())
                (locals (x.1))
                (call-undead ())
                (undead-out (((((x.1) ()))))))
                (define L.addup.2
                  ((new-frames ())
                  (locals (y.1))
                  (undead-out (((((y.1) ())))))
                  (call-undead ()))
                  (begin (begin (begin (begin (set! y.1 L.addup.1) (jump y.1))))))
                (begin (begin (begin (begin (set! x.1 L.addup.2) (jump x.1)))))))))

(let ([x    `(module
            ((new-frames ()) (locals ()))
            (define L.addup.2
              ((new-frames ()) (locals (x.3 y.1 z.1 x.1)))
              (begin
                (set! x.1 1)
                (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (- z.1 x.1)))
                (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3 z.1)))
                (jump L.addup.2 x.1)))
            (jump L.addup.2))])
     (test-case "Simple case with nested begins - effect in begin"
     (check-match (undead-analysis x)
                `(module
                ((new-frames ()) (locals ()) (call-undead ()) (undead-out ()))
                (define L.addup.2
                  ((new-frames ())
                  (locals ,(list-no-order 'x.3 'y.1 'z.1 'x.1))
                  (undead-out
                    ((x.1) (,(list-no-order 'y.1 'x.1) ,(list-no-order 'z.1 'x.1) ,(list-no-order 'z.1 'x.1)) ((x.1) (,(list-no-order 'x.3 'z.1) (z.1))) (x.1)))
                  (call-undead (x.1)))
                  (begin
                    (set! x.1 1)
                    (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (- z.1 x.1)))
                    (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3 z.1)))
                    (jump L.addup.2 x.1)))
                (jump L.addup.2)))))

(let ([x   `(module
                ((new-frames ()) (locals (x.1)))
                (define L.addup.1
                  ((new-frames ()) (locals (y.1 x.2 x.1 m.1 q.1)))
                  (begin
                    (set! x.1 1)
                    (begin
                      (set! y.1 2)
                      (return-point
                      L.rp.1
                      (begin (set! rax 5) (set! x.2 3) (jump L.addup.3 x.2 rax))))
                    (begin
                      (set! q.1 1)
                      (set! m.1 5)
                      (set! rcx L.addup.1)
                      (jump rcx q.1 m.1))))
                (begin (set! x.1 5) (jump L.addup.1 x.1)))])
     (test-case "Simple case with nested begins - nested begin in effect"
     (check-match (undead-analysis x)
                `(module
                  ((new-frames ()) (locals (x.1)) (call-undead ()) (undead-out ((x.1) (x.1))))
                  (define L.addup.1
                    ((new-frames ())
                    (locals ,(list-no-order 'y.1 'x.2 'x.1 'm.1 'q.1))
                    (undead-out
                      (()
                      (() (() ((rax) ,(list-no-order 'rax 'x.2) ,(list-no-order 'rax 'x.2))))
                      ((q.1) (m.1 q.1) (rcx m.1 q.1) (m.1 q.1))))
                    (call-undead ()))
                    (begin
                      (set! x.1 1)
                      (begin
                        (set! y.1 2)
                        (return-point
                        L.rp.1
                        (begin (set! rax 5) (set! x.2 3) (jump L.addup.3 x.2 rax))))
                      (begin
                        (set! q.1 1)
                        (set! m.1 5)
                        (set! rcx L.addup.1)
                        (jump rcx q.1 m.1))))
                  (begin (set! x.1 5) (jump L.addup.1 x.1))))))

;; long example 
(let ([x   `(module
            ((new-frames ()) (locals (y.1 q.2 z.5 x.1 x.7)))
            (define L.addup.2
              ((new-frames ()) (locals (q.2 x.2 q.1 z.1 x.1)))
              (begin
                (set! x.1 1)
                (set! q.1 2)
                (set! x.2 L.addup.2)
                (begin
                  (return-point
                  L.rp.1
                  (if (not (<= q.1 2)) (jump x.2 x.1) (jump x.2 q.1)))
                  (if (true) (set! z.1 10) (set! q.2 (+ q.2 z.1))))
                (jump L.addup.2 x.1 z.1)))
            (begin
              (set! x.1 1)
              (set! x.7 L.addup.2)
              (set! q.2 7)
              (begin (set! y.1 2) (if (true) (set! z.5 10) (set! z.5 (- z.5 q.2))))
              (jump x.7 x.1 z.5)))])
     (test-case "Simple case with begin nesting if in effect position"
     (check-match (undead-analysis x)
                `(module
                ((new-frames ())
                (locals ,(list-no-order 'y.1 'q.2 'z.5 'x.1 'x.7))
                (call-undead ())
                (undead-out
                  (,(list-no-order 'z.5 'x.1)
                  ,(list-no-order 'z.5 'x.7 'x.1)
                  ,(list-no-order 'z.5 'q.2 'x.7 'x.1)
                  (,(list-no-order 'z.5 'q.2 'x.7 'x.1) (,(list-no-order 'z.5 'q.2 'x.7 'x.1) ,(list-no-order 'x.7 'z.5 'x.1) ,(list-no-order 'x.7 'z.5 'x.1)))
                  ,(list-no-order 'z.5 'x.1))))
                (define L.addup.2
                  ((new-frames ())
                  (locals ,(list-no-order 'q.2 'x.2 'q.1 'z.1 'x.1))
                  (undead-out
                    (,(list-no-order 'z.1 'q.2 'x.1)
                    ,(list-no-order 'q.1 'z.1 'q.2 'x.1)
                    ,(list-no-order 'x.2 'q.1 'z.1 'q.2 'x.1)
                    ((,(list-no-order 'z.1 'q.2 'x.1) (,(list-no-order 'q.1 'x.2 'x.1) (x.1) (q.1)))
                      (,(list-no-order 'z.1 'q.2 'x.1) ,(list-no-order 'z.1 'x.1) ,(list-no-order 'z.1 'x.1)))
                    ,(list-no-order 'z.1 'x.1)))
                  (call-undead ,(list-no-order 'z.1 'q.2 'x.1)))
                  (begin
                    (set! x.1 1)
                    (set! q.1 2)
                    (set! x.2 L.addup.2)
                    (begin
                      (return-point
                      L.rp.1
                      (if (not (<= q.1 2)) (jump x.2 x.1) (jump x.2 q.1)))
                      (if (true) (set! z.1 10) (set! q.2 (+ q.2 z.1))))
                    (jump L.addup.2 x.1 z.1)))
                (begin
                  (set! x.1 1)
                  (set! x.7 L.addup.2)
                  (set! q.2 7)
                  (begin (set! y.1 2) (if (true) (set! z.5 10) (set! z.5 (- z.5 q.2))))
                  (jump x.7 x.1 z.5))))))


(let ([x    `(module
            ((new-frames ()) (locals (x.3 x.2)))
            (define L.addup.2
              ((new-frames ()) (locals (z.1 y.1 q.3 q.2 x.1 q.1)))
              (begin
                (begin
                  (set! x.1 L.addup.1)
                  (set! q.1 4)
                  (set! q.3 2)
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
                        (set! z.1 L.addup.1)
                        (jump z.1)))))))
            (if (true)
              (begin (set! x.2 L.addup.1) (jump x.2))
              (begin (set! x.3 L.addup.2) (jump x.3))))])
     (test-case "Simple case with begin nesting if in tail position"
     (check-match (undead-analysis x)
                `(module
                ((new-frames ())
                (locals ,(list-no-order 'x.3 'x.2))
                (call-undead ())
                (undead-out (() ((x.2) ()) ((x.3) ()))))
                (define L.addup.2
                  ((new-frames ())
                  (locals ,(list-no-order 'z.1 'y.1 'q.3 'q.2 'x.1 'q.1))
                  (undead-out
                    (((x.1)
                      ,(list-no-order 'x.1 'q.1)
                      ,(list-no-order 'q.3 'x.1 'q.1)
                      ((,(list-no-order 'q.3 'x.1 'q.1)
                        (q.1)
                        ((q.3) (() (,(list-no-order 'q.3 'q.2) ,(list-no-order 'q.3 'q.2) ,(list-no-order 'q.3 'q.2))) (z.1) ()))))))
                  (call-undead ()))
                  (begin
                    (begin
                      (set! x.1 L.addup.1)
                      (set! q.1 4)
                      (set! q.3 2)
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
                            (set! z.1 L.addup.1)
                            (jump z.1)))))))
                (if (true)
                  (begin (set! x.2 L.addup.1) (jump x.2))
                  (begin (set! x.3 L.addup.2) (jump x.3)))))))



(let ([x    `(module
            ((new-frames ()) (locals (x.2)))
            (define L.addup.2
              ((new-frames ()) (locals (q.1 q.2 x.1 m.1)))
              (begin
                (set! x.1 1)
                (if (false)
                  (if (begin
                        (set! q.1 5)
                        (return-point L.rp.1 (begin (set! q.2 5) (jump L.addup.2 q.2)))
                        (!= q.1 5))
                    (begin (set! x.1 L.addup.1) (jump x.1))
                    (begin
                      (set! q.1 L.addup.1)
                      (if (false)
                        (jump q.1)
                        (begin (set! m.1 (- m.1 5)) (jump L.addup.1 m.1)))))
                  (begin (set! q.1 6) (jump L.addup.3 x.1 q.1)))))
            (begin (set! x.2 L.addup.2) (set! rax 6) (set! fv0 rax) (jump x.2 fv0)))])
     (test-case "Simple case with begin nesting if in tail position"
     (check-match (undead-analysis x)
                `(module
                ((new-frames ())
                (locals (x.2))
                (call-undead ())
                (undead-out ((x.2) ,(list-no-order 'rax 'x.2) ,(list-no-order 'x.2 'fv0) (fv0))))
                (define L.addup.2
                  ((new-frames ())
                  (locals ,(list-no-order 'q.1 'q.2 'x.1 'm.1))
                  (undead-out
                    (,(list-no-order 'x.1 'm.1)
                    (,(list-no-order 'x.1 'm.1)
                      ((,(list-no-order 'q.1 'm.1) (,(list-no-order 'q.1 'm.1) ((q.2) (q.2))) (m.1))
                      ((x.1) ())
                      (,(list-no-order 'm.1 'q.1) (,(list-no-order 'm.1 'q.1) () ((m.1) (m.1)))))
                      (,(list-no-order 'q.1 'x.1) ,(list-no-order 'q.1 'x.1)))))
                  (call-undead ,(list-no-order 'q.1 'm.1)))
                  (begin
                    (set! x.1 1)
                    (if (false)
                      (if (begin
                            (set! q.1 5)
                            (return-point L.rp.1 (begin (set! q.2 5) (jump L.addup.2 q.2)))
                            (!= q.1 5))
                        (begin (set! x.1 L.addup.1) (jump x.1))
                        (begin
                          (set! q.1 L.addup.1)
                          (if (false)
                            (jump q.1)
                            (begin (set! m.1 (- m.1 5)) (jump L.addup.1 m.1)))))
                      (begin (set! q.1 6) (jump L.addup.3 x.1 q.1)))))
                (begin (set! x.2 L.addup.2) (set! rax 6) (set! fv0 rax) (jump x.2 fv0))))))



(let ([x `(module
          ((new-frames ()) (locals (x.2 x.1)))
          (define L.addup.2
            ((new-frames ()) (locals (q.2 x.4 x.2 x.1 z.1 x.3 z.2)))
            (begin
              (set! x.3 5)
              (set! z.1 x.3)
              (if (true)
                (if (false) (set! x.1 10) (set! x.2 x.3))
                (set! z.1 (+ z.1 5)))
              (set! x.4 L.addup.1)
              (return-point
              L.rp.1
              (if (begin
                    (return-point L.rp.2 (jump L.addup.2))
                    (set! q.2 5)
                    (= q.2 5))
                (jump x.4)
                (jump L.addup.2)))
              (jump z.2)))
          (begin
            (set! x.1 2)
            (set! x.2 2)
            (set! x.2 (- x.2 x.1))
            (set! x.1 L.addup.1)
            (jump x.1)))])
     (test-case "Simple case with begin nesting nested if in effect position"
     (check-match (undead-analysis x)
                `(module
                ((new-frames ())
                (locals ,(list-no-order 'x.2 'x.1))
                (call-undead ())
                (undead-out ((x.1) ,(list-no-order 'x.1 'x.2) () (x.1) ())))
                (define L.addup.2
                  ((new-frames ())
                  (locals ,(list-no-order 'q.2 'x.4 'x.2 'x.1 'z.1 'x.3 'z.2))
                  (undead-out
                    (,(list-no-order 'x.3 'z.2)
                    ,(list-no-order 'z.1 'x.3 'z.2)
                    (,(list-no-order 'z.1 'x.3 'z.2) (,(list-no-order 'x.3 'z.2) (z.2) (z.2)) (z.2))
                    ,(list-no-order 'x.4 'z.2)
                    ((z.2) ((((x.4) ()) ,(list-no-order 'q.2 'x.4) (x.4)) () ()))
                    ()))
                  (call-undead ,(list-no-order 'x.4 'z.2)))
                  (begin
                    (set! x.3 5)
                    (set! z.1 x.3)
                    (if (true)
                      (if (false) (set! x.1 10) (set! x.2 x.3))
                      (set! z.1 (+ z.1 5)))
                    (set! x.4 L.addup.1)
                    (return-point
                    L.rp.1
                    (if (begin
                          (return-point L.rp.2 (jump L.addup.2))
                          (set! q.2 5)
                          (= q.2 5))
                      (jump x.4)
                      (jump L.addup.2)))
                    (jump z.2)))
                (begin
                  (set! x.1 2)
                  (set! x.2 2)
                  (set! x.2 (- x.2 x.1))
                  (set! x.1 L.addup.1)
                  (jump x.1))))))  


(let ([x `(module
          ((new-frames ()) (locals (q.1)))
          (define L.addup.2
            ((new-frames ()) (locals (w.1 w.2 q.1 x.1 q.2)))
            (if (if (true)
                  (begin
                    (set! q.1 5)
                    (set! q.2 L.addup.1)
                    (set! x.1 2)
                    (return-point L.rp.1 (jump q.2))
                    (< q.1 x.1))
                  (begin
                    (set! q.2 7)
                    (set! w.1 q.2)
                    (set! q.2 (- q.2 w.1))
                    (set! w.2 L.addup.1)
                    (return-point L.rp.2 (jump w.2))
                    (!= q.2 7)))
              (jump L.addup.1)
              (begin (set! x.1 L.addup.1) (jump x.1))))
          (begin (set! q.1 L.addup.1) (jump q.1)))])
     (test-case "Simple case with if predicate in tail position"
     (check-match (undead-analysis x)
                `(module
                  ((new-frames ()) (locals (q.1)) (call-undead ()) (undead-out ((q.1) ())))
                  (define L.addup.2
                    ((new-frames ())
                    (locals ,(list-no-order 'w.1 'w.2 'q.1 'x.1 'q.2))
                    (undead-out
                      ((()
                        ((q.1) ,(list-no-order 'q.2 'q.1) ,(list-no-order 'q.2 'x.1 'q.1) (,(list-no-order 'x.1 'q.1) ()) ())
                        ((q.2) ,(list-no-order 'w.1 'q.2) (q.2) ,(list-no-order 'w.2 'q.2) ((q.2) ()) ()))
                      ()
                      ((x.1) ())))
                    (call-undead ,(list-no-order 'q.2 'x.1 'q.1)))
                    (if (if (true)
                          (begin
                            (set! q.1 5)
                            (set! q.2 L.addup.1)
                            (set! x.1 2)
                            (return-point L.rp.1 (jump q.2))
                            (< q.1 x.1))
                          (begin
                            (set! q.2 7)
                            (set! w.1 q.2)
                            (set! q.2 (- q.2 w.1))
                            (set! w.2 L.addup.1)
                            (return-point L.rp.2 (jump w.2))
                            (!= q.2 7)))
                      (jump L.addup.1)
                      (begin (set! x.1 L.addup.1) (jump x.1))))
                  (begin (set! q.1 L.addup.1) (jump q.1))))))





;; pass
(let ([x `(module
        ((new-frames ()) (locals ()))
        (define L.addup.2
          ((new-frames ()) (locals (q.3 y.1 x.1 x.2 x.3 z.1 q.1)))
          (begin
            (set! q.1 2)
            (set! z.1 q.1)
            (set! x.1 5)
            (set! x.3 6)
            (set! y.1 x.3)
            (if (if (if (> q.1 z.1)
                      (begin
                        (set! x.2 5)
                        (set! x.3 L.addup.1)
                        (return-point L.addup.1 (jump x.3 x.2))
                        (!= x.2 5))
                      (true))
                  (if (false) (< x.1 y.1) (not (> x.3 5)))
                  (if (true) (false) (not (< z.1 y.1))))
              (jump L.addup.1)
              (begin (set! q.3 L.addup.2) (jump q.3 q.1 z.1 x.1 x.3)))))
        (jump L.addup.2))])
     (test-case "Simple case with nested if predicate in tail position"
     (check-match (undead-analysis x)
                `(module
                ((new-frames ()) (locals ()) (call-undead ()) (undead-out ()))
                (define L.addup.2
                  ((new-frames ())
                  (locals ,(list-no-order 'q.3 'y.1 'x.1 'x.2 'x.3 'z.1 'q.1))
                  (undead-out
                    ((q.1)
                    ,(list-no-order 'q.1 'z.1)
                    ,(list-no-order 'q.1 'z.1 'x.1)
                    ,(list-no-order 'x.3 'q.1 'z.1 'x.1)
                    ,(list-no-order 'x.3 'y.1 'q.1 'z.1 'x.1)
                    (((,(list-no-order 'x.3 'y.1 'q.1 'z.1 'x.1)
                        (,(list-no-order 'x.2 'y.1 'q.1 'z.1 'x.1)
                        ,(list-no-order 'x.2 'y.1 'q.1 'z.1 'x.1 'x.3)
                        (,(list-no-order 'x.2 'y.1 'q.1 'z.1 'x.1 'x.3) (x.2))
                        ,(list-no-order 'y.1 'q.1 'z.1 'x.1 'x.3))
                        ,(list-no-order 'y.1 'q.1 'z.1 'x.1 'x.3))
                      (,(list-no-order 'y.1 'q.1 'z.1 'x.1 'x.3) ,(list-no-order 'q.1 'z.1 'x.1 'x.3) ,(list-no-order 'q.1 'z.1 'x.1 'x.3))
                      (,(list-no-order 'y.1 'q.1 'z.1 'x.1 'x.3) ,(list-no-order 'q.1 'z.1 'x.1 'x.3) ,(list-no-order 'q.1 'z.1 'x.1 'x.3)))
                      ()
                      (,(list-no-order 'q.3 'x.3 'x.1 'z.1 'q.1) ,(list-no-order 'x.3 'x.1 'z.1 'q.1)))))
                  (call-undead ,(list-no-order 'x.2 'y.1 'q.1 'z.1 'x.1 'x.3)))
                  (begin
                    (set! q.1 2)
                    (set! z.1 q.1)
                    (set! x.1 5)
                    (set! x.3 6)
                    (set! y.1 x.3)
                    (if (if (if (> q.1 z.1)
                              (begin
                                (set! x.2 5)
                                (set! x.3 L.addup.1)
                                (return-point L.addup.1 (jump x.3 x.2))
                                (!= x.2 5))
                              (true))
                          (if (false) (< x.1 y.1) (not (> x.3 5)))
                          (if (true) (false) (not (< z.1 y.1))))
                      (jump L.addup.1)
                      (begin (set! q.3 L.addup.2) (jump q.3 q.1 z.1 x.1 x.3)))))
                (jump L.addup.2)))))


;; pass
(let ([x `(module
          ((new-frames ()) (locals ()))
          (define L.addup.2
            ((new-frames ()) (locals (q.1 x.2 l.1 x.1)))
            (if (not
                (begin
                  (set! x.1 1)
                  (set! l.1 7)
                  (return-point
                    L.rp.1
                    (begin
                      (set! x.2 7)
                      (set! l.1 (- l.1 x.1))
                      (jump L.addup.1 x.1 l.1 x.2)))
                  (not (<= x.1 l.1))))
              (jump L.addup.2 x.1 l.1)
              (begin (set! q.1 (+ q.1 l.1)) (jump L.addup.1 q.1))))
          (jump L.addup.2))])
     (test-case "Simple case with not predicate in tail position"
     (check-match (undead-analysis x)
                `(module
                ((new-frames ()) (locals ()) (call-undead ()) (undead-out ()))
                (define L.addup.2
                  ((new-frames ())
                  (locals ,(list-no-order 'q.1 'x.2 'l.1 'x.1))
                  (undead-out
                    ((,(list-no-order 'q.1 'x.1)
                      ,(list-no-order 'q.1 'l.1 'x.1)
                      (,(list-no-order 'q.1 'l.1 'x.1) (,(list-no-order 'l.1 'x.2 'x.1) ,(list-no-order 'x.2 'l.1 'x.1) ,(list-no-order 'x.2 'l.1 'x.1)))
                      ,(list-no-order 'q.1 'l.1 'x.1))
                    ,(list-no-order 'l.1 'x.1)
                    ((q.1) (q.1))))
                  (call-undead ,(list-no-order 'q.1 'l.1 'x.1)))
                  (if (not
                      (begin
                        (set! x.1 1)
                        (set! l.1 7)
                        (return-point
                          L.rp.1
                          (begin
                            (set! x.2 7)
                            (set! l.1 (- l.1 x.1))
                            (jump L.addup.1 x.1 l.1 x.2)))
                        (not (<= x.1 l.1))))
                    (jump L.addup.2 x.1 l.1)
                    (begin (set! q.1 (+ q.1 l.1)) (jump L.addup.1 q.1))))
                (jump L.addup.2)))))    

(let ([x  `(module
            ((new-frames ()) (locals (q.2 x.3)))
            (define L.addup.2
              ((new-frames ()) (locals (x.2 x.3 q.1 y.1 x.1)))
              (begin
                (set! x.1 7)
                (set! y.1 8)
                (set! x.2 L.addup.2)
                (set! x.3 L.addup.1)
                (if (not (not (<= x.1 y.1)))
                  (begin
                    (return-point
                    L.rp.1
                    (if (not (begin (set! q.1 7) (!= q.1 7)))
                      (jump L.addup.1 q.1)
                      (jump L.addup.2 q.1)))
                    (jump x.3))
                  (begin (jump x.2)))))
            (begin
              (set! q.2 3)
              (if (true) (set! x.3 5) (set! q.2 (- q.2 7)))
              (jump L.addup.2 x.3 q.2)))])
     (test-case "Simple case with nested not predicate in tail position"
     (check-match (undead-analysis x)
                `(module
                ((new-frames ())
                (locals ,(list-no-order 'q.2 'x.3))
                (call-undead ())
                (undead-out (,(list-no-order 'x.3 'q.2) (,(list-no-order 'x.3 'q.2) ,(list-no-order 'q.2 'x.3) ,(list-no-order 'q.2 'x.3)) ,(list-no-order 'q.2 'x.3))))
                (define L.addup.2
                  ((new-frames ())
                  (locals ,(list-no-order 'x.2 'x.3 'q.1 'y.1 'x.1))
                  (undead-out
                    ((x.1)
                    ,(list-no-order 'y.1 'x.1)
                    ,(list-no-order 'y.1 'x.1 'x.2)
                    ,(list-no-order 'y.1 'x.1 'x.2 'x.3)
                    (,(list-no-order 'x.2 'x.3) (((x.3) (((q.1) (q.1)) (q.1) (q.1))) ()) (()))))
                  (call-undead (x.3)))
                  (begin
                    (set! x.1 7)
                    (set! y.1 8)
                    (set! x.2 L.addup.2)
                    (set! x.3 L.addup.1)
                    (if (not (not (<= x.1 y.1)))
                      (begin
                        (return-point
                        L.rp.1
                        (if (not (begin (set! q.1 7) (!= q.1 7)))
                          (jump L.addup.1 q.1)
                          (jump L.addup.2 q.1)))
                        (jump x.3))
                      (begin (jump x.2)))))
                (begin
                  (set! q.2 3)
                  (if (true) (set! x.3 5) (set! q.2 (- q.2 7)))
                  (jump L.addup.2 x.3 q.2))))))     


;; textbook example

(let ([x `(module
          ((new-frames ()) (locals (tmp-ra.2)))
          (define L.swap.1
            ((new-frames (())) (locals (z.3 tmp-ra.1 x.1 y.2)))
            (begin
              (set! tmp-ra.1 r15)
              (set! x.1 rdi)
              (set! y.2 rsi)
              (if (< y.2 x.1)
                (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
                (begin
                  (return-point L.rp.1
                    (begin
                      (set! rsi x.1)
                      (set! rdi y.2)
                      (set! r15 L.rp.1)
                      (jump L.swap.1 rbp r15 rdi rsi)))
                  (set! z.3 rax)
                  (set! rax z.3)
                  (jump tmp-ra.1 rbp rax)))))
          (begin
            (set! tmp-ra.2 r15)
            (set! rsi 2)
            (set! rdi 1)
            (set! r15 tmp-ra.2)
            (jump L.swap.1 rbp r15 rdi rsi)))])
     (test-case "Simple case with if predicate in tail position"
     (check-match (undead-analysis x)
                `(module
                  ((new-frames ())
                  (locals (tmp-ra.2))
                  (call-undead ())
                  (undead-out
                    ((tmp-ra.2 rbp)
                    (tmp-ra.2 rsi rbp)
                    (tmp-ra.2 rsi rdi rbp)
                    (rsi rdi r15 rbp)
                    (rsi rdi r15 rbp))))
                  (define L.swap.1
                    ((new-frames (()))
                    (locals ,(list-no-order 'z.3 'tmp-ra.1 'x.1 'y.2))
                    (undead-out
                      (,(list-no-order 'rdi 'rsi 'tmp-ra.1 'rbp)
                      ,(list-no-order 'rsi 'x.1 'tmp-ra.1 'rbp)
                      ,(list-no-order 'y.2 'x.1 'tmp-ra.1 'rbp)
                      (,(list-no-order 'y.2 'x.1 'tmp-ra.1 'rbp)
                        (,(list-no-order 'tmp-ra.1 'rax 'rbp) ,(list-no-order 'rax 'rbp))
                        ((,(list-no-order 'rax 'tmp-ra.1 'rbp)
                          (,(list-no-order 'y.2 'rsi 'rbp) ,(list-no-order 'rsi 'rdi 'rbp) ,(list-no-order 'rsi 'rdi 'r15 'rbp) ,(list-no-order 'rsi 'rdi 'r15 'rbp)))
                        ,(list-no-order 'z.3 'tmp-ra.1 'rbp)
                        ,(list-no-order 'tmp-ra.1 'rax 'rbp)
                        ,(list-no-order 'rax 'rbp)))))
                    (call-undead (tmp-ra.1)))
                    (begin
                      (set! tmp-ra.1 r15)
                      (set! x.1 rdi)
                      (set! y.2 rsi)
                      (if (< y.2 x.1)
                        (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
                        (begin
                          (return-point
                          L.rp.1
                          (begin
                            (set! rsi x.1)
                            (set! rdi y.2)
                            (set! r15 L.rp.1)
                            (jump L.swap.1 rbp r15 rdi rsi)))
                          (set! z.3 rax)
                          (set! rax z.3)
                          (jump tmp-ra.1 rbp rax)))))
                  (begin
                    (set! tmp-ra.2 r15)
                    (set! rsi 2)
                    (set! rdi 1)
                    (set! r15 tmp-ra.2)
                    (jump L.swap.1 rbp r15 rdi rsi))))))


(let ([x `(module
          ((new-frames ()) (locals ()))
          (define L.addup.2
            ((new-frames ()) (locals ()))
            (if (not
                (begin
                  (set! fv0 1)
                  (set! fv1 7)
                  (return-point
                    L.rp.1
                    (begin
                      (set! fv2 7)
                      (set! fv1 (- fv1 fv0))
                      (jump L.addup.1 fv0 fv1 fv2)))
                  (not (<= fv0 fv1))))
              (jump L.addup.2 fv0 fv1)
              (begin (set! fv3 (+ fv3 fv1)) (jump L.addup.1 fv3))))
          (jump L.addup.2))])
     (test-case "Simple case with not predicate in tail position with fvars only"
     (check-match (undead-analysis x)
                `(module
                ((new-frames ()) (locals ()) (call-undead ()) (undead-out ()))
                (define L.addup.2
                  ((new-frames ())
                  (locals ())
                  (undead-out
                    ((,(list-no-order 'fv3 'fv0)
                      ,(list-no-order 'fv3 'fv1 'fv0)
                      (,(list-no-order 'fv3 'fv1 'fv0) (,(list-no-order 'fv1 'fv2 'fv0) ,(list-no-order 'fv2 'fv1 'fv0) ,(list-no-order 'fv2 'fv1 'fv0)))
                      ,(list-no-order 'fv3 'fv1 'fv0))
                    ,(list-no-order 'fv1 'fv0)
                    ((fv3) (fv3))))
                  (call-undead ,(list-no-order 'fv3 'fv1 'fv0)))
                  (if (not
                      (begin
                        (set! fv0 1)
                        (set! fv1 7)
                        (return-point
                          L.rp.1
                          (begin
                            (set! fv2 7)
                            (set! fv1 (- fv1 fv0))
                            (jump L.addup.1 fv0 fv1 fv2)))
                        (not (<= fv0 fv1))))
                    (jump L.addup.2 fv0 fv1)
                    (begin (set! fv3 (+ fv3 fv1)) (jump L.addup.1 fv3))))
                (jump L.addup.2)))))



(let ([x `(module
       ((new-frames ()) (locals (ra.12)))
       (define L.fact.4
         ((new-frames ((nfv.16)))
          (locals (ra.13 x.9 tmp.14 tmp.15 new-n.10 nfv.16 factn-1.11 tmp.17)))
         (begin
           (set! x.9 fv0)
           (set! ra.13 r15)
           (if (= x.9 0)
               (begin (set! rax 1) (jump ra.13 rbp rax))
               (begin
                 (set! tmp.14 -1)
                 (set! tmp.15 x.9)
                 (set! tmp.15 (+ tmp.15 tmp.14))
                 (set! new-n.10 tmp.15)
                 (return-point
                     L.rp.6
                   (begin
                     (set! nfv.16 new-n.10)
                     (set! r15 L.rp.6)
                     (jump L.fact.4 rbp r15 nfv.16)))
                 (set! factn-1.11 rax)
                 (set! tmp.17 x.9)
                 (set! tmp.17 (* tmp.17 factn-1.11))
                 (set! rax tmp.17)
                 (jump ra.13 rbp rax)))))
       (begin
         (set! ra.12 r15)
         (set! fv0 5)
         (set! r15 ra.12)
         (jump L.fact.4 rbp r15 fv0)))])
     (test-case "Textbook example"
     (check-match (undead-analysis x)
                `(module
                ((new-frames ())
                (locals (ra.12))
                (call-undead ())
                (undead-out (,(list-no-order 'ra.12 'rbp) ,(list-no-order 'ra.12 'fv0 'rbp) ,(list-no-order 'fv0 'r15 'rbp) ,(list-no-order 'fv0 'r15 'rbp))))
                (define L.fact.4
                  ((new-frames ((nfv.16)))
                  (locals ,(list-no-order 'ra.13 'x.9 'tmp.14 'tmp.15 'new-n.10 'nfv.16 'factn-1.11 'tmp.17))
                  (undead-out
                    (,(list-no-order 'r15 'x.9 'rbp)
                    ,(list-no-order 'x.9 'ra.13 'rbp)
                    (,(list-no-order 'x.9 'ra.13 'rbp)
                      (,(list-no-order 'ra.13 'rax 'rbp) ,(list-no-order 'rax 'rbp))
                      (,(list-no-order 'tmp.14 'x.9 'ra.13 'rbp)
                      ,(list-no-order 'tmp.14 'tmp.15 'x.9 'ra.13 'rbp)
                      ,(list-no-order 'tmp.15 'x.9 'ra.13 'rbp)
                      ,(list-no-order 'new-n.10 'x.9 'ra.13 'rbp)
                      (,(list-no-order 'rax 'x.9 'ra.13 'rbp) (,(list-no-order 'nfv.16 'rbp) ,(list-no-order 'nfv.16 'r15 'rbp) ,(list-no-order 'nfv.16 'r15 'rbp)))
                      ,(list-no-order 'x.9 'factn-1.11 'ra.13 'rbp)
                      ,(list-no-order 'factn-1.11 'tmp.17 'ra.13 'rbp)
                      ,(list-no-order 'tmp.17 'ra.13 'rbp)
                      ,(list-no-order 'ra.13 'rax 'rbp)
                      ,(list-no-order 'rax 'rbp)))))
                  (call-undead ,(list-no-order 'x.9 'ra.13)))
                  (begin
                    (set! x.9 fv0)
                    (set! ra.13 r15)
                    (if (= x.9 0)
                      (begin (set! rax 1) (jump ra.13 rbp rax))
                      (begin
                        (set! tmp.14 -1)
                        (set! tmp.15 x.9)
                        (set! tmp.15 (+ tmp.15 tmp.14))
                        (set! new-n.10 tmp.15)
                        (return-point
                        L.rp.6
                        (begin
                          (set! nfv.16 new-n.10)
                          (set! r15 L.rp.6)
                          (jump L.fact.4 rbp r15 nfv.16)))
                        (set! factn-1.11 rax)
                        (set! tmp.17 x.9)
                        (set! tmp.17 (* tmp.17 factn-1.11))
                        (set! rax tmp.17)
                        (jump ra.13 rbp rax)))))
                (begin
                  (set! ra.12 r15)
                  (set! fv0 5)
                  (set! r15 ra.12)
                  (jump L.fact.4 rbp r15 fv0))))))