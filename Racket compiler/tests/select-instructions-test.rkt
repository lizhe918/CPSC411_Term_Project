#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/2c-run-time
 racket/match
 rackunit)

(require "../component/select-instr.rkt")


; input: imp-cmf-lang-v8
; output: asm-pred-lang-v8
; purpose: Check if the output of select-instructions matches the output of interrogator 
;; pass


(let ([x '(module
          ((new-frames ()))
          (define L.addup.1
            ((new-frames ()))
            (begin
              (set! rbx 8)
              (set! rax (mref rbx 32))
              (jump L.end.1)))
          (jump L.addup.1))])
     (test-case "Support new type of value mref (no change)"
     (check-equal? (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (set! rbx 8)
                      (set! rax rbx)
                      (set! rax (mref rax 32))
                      (jump L.end.1)))
                  (jump L.addup.1)))))

(let ([x '(module
          ((new-frames ()))
          (define L.addup.1
            ((new-frames ()))
            (begin
              (set! rbx 8)
              (set! rax (alloc rbx))
              (jump L.end.1)))
          (jump L.addup.1))])
     (test-case "Support new type of value alloc (no change)"
     (check-equal? (select-instructions x)
                  `(module
                    ((new-frames ()))
                    (define L.addup.1
                      ((new-frames ()))
                      (begin (set! rbx 8) (set! rax (alloc rbx)) (jump L.end.1)))
                    (jump L.addup.1)))))


(let ([x '(module
          ((new-frames ()))
          (define L.addup.1
            ((new-frames ()))
            (begin
              (mset! rax 8 32)
              (jump L.end.1)))
          (jump L.addup.1))])
     (test-case "Support new effect (mset! reg int64 int64)"
     (check-equal? (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1 ((new-frames ())) (begin (mset! rax 8 32) (jump L.end.1)))
                  (jump L.addup.1)))))


(let ([x '(module
          ((new-frames ()))
          (define L.addup.1
            ((new-frames ()))
            (begin
              (set! x.1 8)
              (set! x.2 32)
              (mset! fv0 x.1 x.2)
              (jump L.end.1)))
          (jump L.addup.1))])
     (test-case "Support new effect (mset! fv aloc aloc)"
     (check-equal? (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin (set! x.1 8) (set! x.2 32) (mset! fv0 x.1 x.2) (jump L.end.1)))
                  (jump L.addup.1)))))


(let ([x '(module
          ((new-frames ()))
          (define L.addup.1
            ((new-frames ()))
            (begin
              (set! fv0 8)
              (set! fv1 32)
              (mset! x.1 fv0 fv1)
              (jump L.end.1)))
          (jump L.addup.1))])
     (test-case "Support new effect (mset! aloc fv fv)"
     (check-equal? (select-instructions x)
                  `(module
                    ((new-frames ()))
                    (define L.addup.1
                      ((new-frames ()))
                      (begin (set! fv0 8) (set! fv1 32) (mset! x.1 fv0 fv1) (jump L.end.1)))
                    (jump L.addup.1)))))

(let ([x '(module
          ((new-frames ()))
          (define L.addup.1
            ((new-frames ()))
            (begin
              (set! x.1 8)
              (return-point L.rp.1 (begin (set! x.2 (mref x.1 x.1)) (mset! rax x.1 x.2) (jump L.end.1)))
              (jump L.end.1)))
          (jump L.addup.1))])
     (test-case "Support new effect and value in return-point"
     (check-equal? (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (set! x.1 8)
                      (return-point
                      L.rp.1
                      (begin
                        (set! x.2 x.1)
                        (set! x.2 (mref x.2 x.1))
                        (mset! rax x.1 x.2)
                        (jump L.end.1)))
                      (jump L.end.1)))
                  (jump L.addup.1)))))

(let ([x '(module
          ((new-frames ()))
          (define L.addup.1
            ((new-frames ()))
            (begin
              (set! x.1 8)
              (set! fv1 32)
              (if (true)
                (mset! fv0 x.1 x.1)
                (set! fv0 (alloc fv1)))
              (jump L.end.1)))
          (jump L.addup.1))])
     (test-case "Support new effect and value in if as effect"
     (check-equal? (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (set! x.1 8)
                      (set! fv1 32)
                      (if (true) (mset! fv0 x.1 x.1) (set! fv0 (alloc fv1)))
                      (jump L.end.1)))
                  (jump L.addup.1)))))


(let ([x '(module
          ((new-frames ()))
          (define L.addup.1
            ((new-frames ()))
            (begin
              (begin
                (set! fv0 8)
                (set! fv1 fv0)
                (set! rax (mref fv0 fv1))
                (mset! x.1 fv0 32))
              (jump L.end.1)))
          (jump L.addup.1))])
     (test-case "Support new effect and value in begin as effect"
     (check-equal? (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (set! fv0 8)
                      (set! fv1 fv0)
                      (set! rax fv0)
                      (set! rax (mref rax fv1))
                      (mset! x.1 fv0 32)
                      (jump L.end.1)))
                  (jump L.addup.1)))))

(let ([x '(module
          ((new-frames ()))
          (define L.addup.1
            ((new-frames ()))
            (if (begin (set! rax 8) (set! rax (mref rax rax)) (mset! x.1 rax 32) (< 1 2))
                (jump L.end.1)
                (jump L.end.2)))
          (jump L.addup.1))])
     (test-case "Support new effect and value in begin as pred"
     (check-match (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (if (begin
                          (set! rax 8)
                          (set! rax (mref rax rax))
                          (mset! x.1 rax 32)
                          (begin (set! ,tmp.1 1) (< ,tmp.1 2)))
                      (jump L.end.1)
                      (jump L.end.2)))
                  (jump L.addup.1)))))

(let ([x '(module
          ((new-frames ()))
          (define L.addup.1
            ((new-frames ()))
            (if (if (begin (set! rax 8) (set! rax (mref rax 32)) (mset! x.1 rax 32) (true)) (true) (false))
                (if (true)
                    (jump L.end.1)
                    (begin
                      (set! fv0 8)
                      (set! rax (mref fv0 32))
                      (mset! x.1 fv0 32)
                      (jump L.end.2)))
                (if (false)
                    (jump L.end.2)
                    (jump L.end.3))))
          (jump L.addup.1))])
     (test-case "Support new effect and value in nested if"
     (check-equal? (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (if (if (begin
                              (set! rax 8)
                              (set! rax (mref rax 32))
                              (mset! x.1 rax 32)
                              (true))
                          (true)
                          (false))
                      (if (true)
                        (jump L.end.1)
                        (begin
                          (set! fv0 8)
                          (set! rax fv0)
                          (set! rax (mref rax 32))
                          (mset! x.1 fv0 32)
                          (jump L.end.2)))
                      (if (false) (jump L.end.2) (jump L.end.3))))
                  (jump L.addup.1)))))



;; ----------------------------------- old cases ------------------------------------
(let ([x '(module
          ((new-frames ()))
          (define L.addup.1
            ((new-frames ()))
            (begin
              (set! x.1 rdi)
              (begin
                (set! tmp-ra.1 r15)
                (begin (set! rax (bitwise-and x.1 x.2)) (jump tmp-ra.1 rbp rax)))))
          (begin
            (set! tmp-ra.2 r15)
            (begin (set! rax (bitwise-and 1 2)) (jump tmp-ra.2 rbp rax))))])
     (test-case "select halt as binop 1 -- bitwise-and"
     (check-match (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (set! x.1 rdi)
                      (set! tmp-ra.1 r15)
                      (set! ,tmp.1 x.1)
                      (set! ,tmp.1 (bitwise-and ,tmp.1 x.2))
                      (set! rax ,tmp.1)
                      (jump tmp-ra.1 rbp rax)))
                  (begin
                    (set! tmp-ra.2 r15)
                    (set! ,tmp.2 1)
                    (set! ,tmp.2 (bitwise-and ,tmp.2 2))
                    (set! rax ,tmp.2)
                    (jump tmp-ra.2 rbp rax))))))

(let ([x '(module
          ((new-frames ()))
          (define L.addup.1
            ((new-frames ()))
            (begin
              (set! x.1 rdi)
              (begin
                (set! tmp-ra.1 r15)
                (begin (set! rax (bitwise-ior x.1 x.2)) (jump tmp-ra.1 rbp rax)))))
          (begin
            (set! tmp-ra.2 r15)
            (begin (set! rax (bitwise-ior 1 2)) (jump tmp-ra.2 rbp rax))))])
     (test-case "select halt as binop 1 -- bitwise-ior"
     (check-match (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (set! x.1 rdi)
                      (set! tmp-ra.1 r15)
                      (set! ,tmp.1 x.1)
                      (set! ,tmp.1 (bitwise-ior ,tmp.1 x.2))
                      (set! rax ,tmp.1)
                      (jump tmp-ra.1 rbp rax)))
                  (begin
                    (set! tmp-ra.2 r15)
                    (set! ,tmp.2 1)
                    (set! ,tmp.2 (bitwise-ior ,tmp.2 2))
                    (set! rax ,tmp.2)
                    (jump tmp-ra.2 rbp rax))))))


(let ([x '(module
          ((new-frames ()))
          (define L.addup.1
            ((new-frames ()))
            (begin
              (set! x.1 rdi)
              (begin
                (set! tmp-ra.1 r15)
                (begin (set! rax (bitwise-xor x.1 x.2)) (jump tmp-ra.1 rbp rax)))))
          (begin
            (set! tmp-ra.2 r15)
            (begin (set! rax (bitwise-xor 1 2)) (jump tmp-ra.2 rbp rax))))])
     (test-case "select halt as binop 1 -- bitwise-xor"
     (check-match (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (set! x.1 rdi)
                      (set! tmp-ra.1 r15)
                      (set! ,tmp.1 x.1)
                      (set! ,tmp.1 (bitwise-xor ,tmp.1 x.2))
                      (set! rax ,tmp.1)
                      (jump tmp-ra.1 rbp rax)))
                  (begin
                    (set! tmp-ra.2 r15)
                    (set! ,tmp.2 1)
                    (set! ,tmp.2 (bitwise-xor ,tmp.2 2))
                    (set! rax ,tmp.2)
                    (jump tmp-ra.2 rbp rax))))))


(let ([x '(module
          ((new-frames ()))
          (define L.addup.1
            ((new-frames ()))
            (begin
              (set! x.1 rdi)
              (begin
                (set! tmp-ra.1 r15)
                (begin (set! rax (arithmetic-shift-right x.1 x.2)) (jump tmp-ra.1 rbp rax)))))
          (begin
            (set! tmp-ra.2 r15)
            (begin (set! rax (arithmetic-shift-right 1 2)) (jump tmp-ra.2 rbp rax))))])
     (test-case "select halt as binop 1 -- arithmetic-shift-right"
     (check-match (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (set! x.1 rdi)
                      (set! tmp-ra.1 r15)
                      (set! ,tmp.1 x.1)
                      (set! ,tmp.1 (arithmetic-shift-right ,tmp.1 x.2))
                      (set! rax ,tmp.1)
                      (jump tmp-ra.1 rbp rax)))
                  (begin
                    (set! tmp-ra.2 r15)
                    (set! ,tmp.2 1)
                    (set! ,tmp.2 (arithmetic-shift-right ,tmp.2 2))
                    (set! rax ,tmp.2)
                    (jump tmp-ra.2 rbp rax))))))
;; --------------------------------------------------------------------------------------
(let ([x '(module
          ((new-frames ()))
          (define L.addup.1
            ((new-frames ()))
            (begin
              (set! x.1 rdi)
              (begin
                (set! tmp-ra.1 r15)
                (begin (set! rax x.1) (jump tmp-ra.1 rbp rax)))))
          (begin
            (set! tmp-ra.2 r15)
            (begin (set! rax x.1) (jump tmp-ra.2 rbp rax))))])
     (test-case "select halt as aloc in proc declaration (no change)"
     (check-equal? (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (set! x.1 rdi)
                      (set! tmp-ra.1 r15)
                      (set! rax x.1)
                      (jump tmp-ra.1 rbp rax)))
                  (begin (set! tmp-ra.2 r15) (set! rax x.1) (jump tmp-ra.2 rbp rax))))))

(let ([x '(module
          ((new-frames ()))
          (define L.addup.1
            ((new-frames ()))
            (begin
              (set! x.1 rdi)
              (begin
                (set! tmp-ra.1 r15)
                (begin (set! rax 5) (jump tmp-ra.1 rbp rax)))))
          (begin
            (set! tmp-ra.2 r15)
            (begin (set! rax 5) (jump tmp-ra.2 rbp rax))))])
     (test-case "select halt as int64 in proc declaration (no change)"
     (check-equal? (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (set! x.1 rdi)
                      (set! tmp-ra.1 r15)
                      (set! rax 5)
                      (jump tmp-ra.1 rbp rax)))
                  (begin (set! tmp-ra.2 r15) (set! rax 5) (jump tmp-ra.2 rbp rax))))))

(let ([x '(module
          ((new-frames ()))
          (define L.addup.1
            ((new-frames ()))
            (begin
              (set! x.1 rdi)
              (begin
                (set! tmp-ra.1 r15)
                (begin (set! rax (+ x.1 1)) (jump tmp-ra.1 rbp rax)))))
          (begin
            (set! tmp-ra.2 r15)
            (begin (set! rax (- 1 2)) (jump tmp-ra.2 rbp rax))))])
     (test-case "select halt as binop 3"
     (check-match (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (set! x.1 rdi)
                      (set! tmp-ra.1 r15)
                      (set! ,tmp.1 x.1)
                      (set! ,tmp.1 (+ ,tmp.1 1))
                      (set! rax ,tmp.1)
                      (jump tmp-ra.1 rbp rax)))
                  (begin
                    (set! tmp-ra.2 r15)
                    (set! ,tmp.2 1)
                    (set! ,tmp.2 (- ,tmp.2 2))
                    (set! rax ,tmp.2)
                    (jump tmp-ra.2 rbp rax))))))


(let ([x '(module
          ((new-frames ()))
          (define L.addup.1
            ((new-frames ()))
            (begin
              (set! x.1 rdi)
              (begin
                (set! tmp-ra.1 r15)
                (begin (set! rax (+ x.1 x.2)) (jump tmp-ra.1 rbp rax)))))
          (begin
            (set! tmp-ra.2 r15)
            (begin (set! rax (- 1 2)) (jump tmp-ra.2 rbp rax))))])
     (test-case "select halt as binop 1"
     (check-match (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (set! x.1 rdi)
                      (set! tmp-ra.1 r15)
                      (set! ,tmp.1 x.1)
                      (set! ,tmp.1 (+ ,tmp.1 x.2))
                      (set! rax ,tmp.1)
                      (jump tmp-ra.1 rbp rax)))
                  (begin
                    (set! tmp-ra.2 r15)
                    (set! ,tmp.2 1)
                    (set! ,tmp.2 (- ,tmp.2 2))
                    (set! rax ,tmp.2)
                    (jump tmp-ra.2 rbp rax))))))

(let ([x '(module
          ((new-frames ()))
          (define L.addup.1
            ((new-frames ()))
            (begin
              (set! x.1 rdi)
              (begin
                (set! tmp-ra.1 r15)
                (begin (set! rax (+ 1 2)) (jump tmp-ra.1 rbp rax)))))
          (begin
            (set! tmp-ra.2 r15)
            (begin (set! rax (- 1 2)) (jump tmp-ra.2 rbp rax))))])
     (test-case "select halt as binop 2"
     (check-match (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (set! x.1 rdi)
                      (set! tmp-ra.1 r15)
                      (set! ,tmp.1 1)
                      (set! ,tmp.1 (+ ,tmp.1 2))
                      (set! rax ,tmp.1)
                      (jump tmp-ra.1 rbp rax)))
                  (begin
                    (set! tmp-ra.2 r15)
                    (set! ,tmp.2 1)
                    (set! ,tmp.2 (- ,tmp.2 2))
                    (set! rax ,tmp.2)
                    (jump tmp-ra.2 rbp rax))))))


(let ([x `(module
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
                            (if (!= 3 4)
                              (begin
                                (return-point L.addup.1 (if (< 1 2) (jump L.addup.2 x.2) (jump L.addup.1 x.2)))
                                (set! q.1 rax))
                              (begin
                                (return-point L.rp.1 (jump L.addup.2 x.2))
                                (set! q.1 rax)))
                            (jump L.rp.2 q.1)))
                        (set! x.1 rax)))
                    (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "relop both int64 in pred"
     (check-match (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (set! x.3 4)
                      (set! x.4 5)
                      (return-point
                      L.addup.1
                      (begin
                        (if (begin (set! ,tmp.1 3) (!= ,tmp.1 4))
                          (begin
                            (return-point
                              L.addup.1
                              (if (begin (set! ,tmp.2 1) (< ,tmp.2 2))
                                (jump L.addup.2 x.2)
                                (jump L.addup.1 x.2)))
                            (set! q.1 rax))
                          (begin (return-point L.rp.1 (jump L.addup.2 x.2)) (set! q.1 rax)))
                        (jump L.rp.2 q.1)))
                      (set! x.1 rax)
                      (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7)))))

(let ([x `(module
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
                            (if (!= x.3 4)
                              (begin
                                (return-point L.addup.1 (if (< x.4 2) (jump L.addup.2 x.2) (jump L.addup.1 x.2)))
                                (set! q.1 rax))
                              (begin
                                (return-point L.rp.1 (jump L.addup.2 x.2))
                                (set! q.1 rax)))
                            (jump L.rp.2 q.1)))
                        (set! x.1 rax)))
                    (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "relop int64 aloc (no change)"
     (check-match (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (set! x.3 4)
                      (set! x.4 5)
                      (return-point
                      L.addup.1
                      (begin
                        (if (!= x.3 4)
                          (begin
                            (return-point
                              L.addup.1
                              (if (< x.4 2) (jump L.addup.2 x.2) (jump L.addup.1 x.2)))
                            (set! q.1 rax))
                          (begin (return-point L.rp.1 (jump L.addup.2 x.2)) (set! q.1 rax)))
                        (jump L.rp.2 q.1)))
                      (set! x.1 rax)
                      (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7)))))


(let ([x `(module
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
                            (if (!= x.3 4)
                              (begin
                                (return-point L.addup.1 (begin (set! q.1 (+ 1 2)) (jump L.addup.1 q.1)))
                                (set! q.1 rax))
                              (begin
                                (return-point L.rp.1 (jump L.addup.2 x.2))
                                (set! q.1 rax)))
                            (jump L.rp.2 q.1)))
                        (set! x.1 rax)))
                    (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "set aloc with binop expression (binop int64 int64)"
     (check-match (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (set! x.3 4)
                      (set! x.4 5)
                      (return-point
                      L.addup.1
                      (begin
                        (if (!= x.3 4)
                          (begin
                            (return-point
                              L.addup.1
                              (begin
                                (set! ,tmp.1 1)
                                (set! ,tmp.1 (+ ,tmp.1 2))
                                (set! q.1 ,tmp.1)
                                (jump L.addup.1 q.1)))
                            (set! q.1 rax))
                          (begin (return-point L.rp.1 (jump L.addup.2 x.2)) (set! q.1 rax)))
                        (jump L.rp.2 q.1)))
                      (set! x.1 rax)
                      (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7)))))

(let ([x `(module
                  ((new-frames ()))
                  (define L.addup.1
                  ((new-frames ()))
                  (begin
                    (begin
                      (set! x.3 (+ 3 q.1))
                      (set! x.4 (+ 3 3))
                      (begin
                        (return-point
                          L.addup.1
                          (begin
                            (if (!= x.3 4)
                              (begin
                                (return-point L.addup.1 (if (begin (set! q.1 (+ 1 2)) (< 1 2)) (jump L.addup.1 q.1 ) (jump L.addup.1 q.1)))
                                (set! q.1 rax))
                              (begin
                                (return-point L.rp.1 (begin (begin (begin (set! q.1 (+ 3 4)) (jump L.addup.1 q.1)))))
                                (set! q.1 rax)))
                            (jump L.rp.2 q.1)))
                        (set! x.1 rax)))
                    (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "General case"
     (check-match (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (set! ,tmp.1 3)
                      (set! ,tmp.1 (+ ,tmp.1 q.1))
                      (set! x.3 ,tmp.1)
                      (set! ,tmp.2 3)
                      (set! ,tmp.2 (+ ,tmp.2 3))
                      (set! x.4 ,tmp.2)
                      (return-point L.addup.1
                        (begin
                          (if (!= x.3 4)
                            (begin
                              (return-point L.addup.1
                                (if (begin
                                      (set! ,tmp.3 1)
                                      (set! ,tmp.3 (+ ,tmp.3 2))
                                      (set! q.1 ,tmp.3)
                                      (set! ,tmp.4 1)
                                      (< ,tmp.4 2))
                                  (jump L.addup.1 q.1)
                                  (jump L.addup.1 q.1)))
                              (set! q.1 rax))
                            (begin
                              (return-point L.rp.1
                                (begin
                                  (set! ,tmp.5 3)
                                  (set! ,tmp.5 (+ ,tmp.5 4))
                                  (set! q.1 ,tmp.5)
                                  (jump L.addup.1 q.1)))
                              (set! q.1 rax)))
                          (jump L.rp.2 q.1)))
                      (set! x.1 rax)
                      (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7)))))

(let ([x `(module
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
                            (if (!= x.3 4)
                              (begin
                                (return-point L.addup.1 (if (begin (begin (set! q.1 (+ 1 2)) (set! w.1 (+ 3 x.2))) (< 1 2)) (jump L.addup.1 q.1 ) (jump L.addup.1 q.1)))
                                (set! q.1 rax))
                              (begin
                                (return-point L.rp.1 (if (if (begin (set! x.2 (+ 3 x.1)) (set! x.2 (+ 1 2)) (< 3 x.4))
                                                              (> 4 3)
                                                              (< x.1 4))
                                                              (jump L.rp.1 x.4 x.2)
                                                              (begin (set! x.3 (+ 1 2)) (jump L.addup.1 x.3))))
                                (set! q.1 rax)))
                            (jump L.rp.2 q.1)))
                        (set! x.1 rax)))
                    (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7))])
     (test-case "General case"
     (check-match (select-instructions x)
                  `(module
                  ((new-frames ()))
                  (define L.addup.1
                    ((new-frames ()))
                    (begin
                      (set! x.3 4)
                      (set! x.4 5)
                      (return-point L.addup.1
                        (begin
                          (if (!= x.3 4)
                            (begin
                              (return-point L.addup.1
                                (if (begin
                                      (set! ,tmp.1 1)
                                      (set! ,tmp.1 (+ ,tmp.1 2))
                                      (set! q.1 ,tmp.1)
                                      (set! ,tmp.2 3)
                                      (set! ,tmp.2 (+ ,tmp.2 x.2))
                                      (set! w.1 ,tmp.2)
                                      (set! ,tmp.3 1)
                                      (< ,tmp.3 2))
                                  (jump L.addup.1 q.1)
                                  (jump L.addup.1 q.1)))
                              (set! q.1 rax))
                            (begin
                              (return-point L.rp.1
                                (if (if (begin
                                          (set! ,tmp.4 3)
                                          (set! ,tmp.4 (+ ,tmp.4 x.1))
                                          (set! x.2 ,tmp.4)
                                          (set! ,tmp.5 1)
                                          (set! ,tmp.5 (+ ,tmp.5 2))
                                          (set! x.2 ,tmp.5)
                                          (set! ,tmp.6 3)
                                          (< ,tmp.6 x.4))
                                      (begin (set! ,tmp.7 4) (> ,tmp.7 3))
                                      (< x.1 4))
                                  (jump L.rp.1 x.4 x.2)
                                  (begin
                                    (set! ,tmp.8 1)
                                    (set! ,tmp.8 (+ ,tmp.8 2))
                                    (set! x.3 ,tmp.8)
                                    (jump L.addup.1 x.3))))
                              (set! q.1 rax)))
                          (jump L.rp.2 q.1)))
                      (set! x.1 rax)
                      (jump L.addup.1 x.3)))
                  (jump L.addup.1 x.5 x.6 x.7)))))


(let ([x '(module
     ((new-frames (())))
     (define L.make-vector.57.9
       ((new-frames ()))
       (begin
         (set! tmp-ra.79 r15)
         (begin
           (set! c.60 rdi)
           (set! tmp.33 rsi)
           (begin
             (set! make-init-vector.1 (mref c.60 14))
             (if (begin
                   (if (begin
                         (set! tmp.66 (bitwise-and tmp.33 7))
                         (= tmp.66 0))
                     (set! tmp.65 14)
                     (set! tmp.65 6))
                   (!= tmp.65 6))
               (begin
                 (set! rsi tmp.33)
                 (set! rdi make-init-vector.1)
                 (set! r15 tmp-ra.79)
                 (jump L.make-init-vector.1.8 rbp r15 rdi rsi))
               (begin (set! rax 2110) (jump tmp-ra.79 rbp rax)))))))
     (define L.make-init-vector.1.8
       ((new-frames ()))
       (begin
         (set! tmp-ra.80 r15)
         (begin
           (set! c.59 rdi)
           (set! tmp.5 rsi)
           (begin
             (set! vector-init-loop.7 (mref c.59 14))
             (begin
               (begin
                 (begin
                   (begin
                     (begin
                       (begin
                         (set! tmp.70 (arithmetic-shift-right tmp.5 3))
                         (set! tmp.69 (+ 1 tmp.70)))
                       (set! tmp.68 (* tmp.69 8)))
                     (set! tmp.67 (alloc tmp.68)))
                   (set! tmp.61 (+ tmp.67 3)))
                 (begin (mset! tmp.61 -3 tmp.5) (set! tmp.6 tmp.61)))
               (begin
                 (set! rcx tmp.6)
                 (set! rdx 0)
                 (set! rsi tmp.5)
                 (set! rdi vector-init-loop.7)
                 (set! r15 tmp-ra.80)
                 (jump L.vector-init-loop.7.7 rbp r15 rdi rsi rdx rcx)))))))
     (define L.vector-init-loop.7.7
       ((new-frames ()))
       (begin
         (set! tmp-ra.81 r15)
         (begin
           (set! c.58 rdi)
           (set! len.8 rsi)
           (set! i.10 rdx)
           (set! vec.9 rcx)
           (begin
             (set! vector-init-loop.7 (mref c.58 14))
             (if (begin
                   (if (= len.8 i.10) (set! tmp.71 14) (set! tmp.71 6))
                   (!= tmp.71 6))
               (begin (set! rax vec.9) (jump tmp-ra.81 rbp rax))
               (begin
                 (begin
                   (begin
                     (begin
                       (set! tmp.74 (arithmetic-shift-right i.10 3))
                       (set! tmp.73 (* tmp.74 8)))
                     (set! tmp.72 (+ tmp.73 5)))
                   (mset! vec.9 tmp.72 0))
                 (begin
                   (set! tmp.75 (+ i.10 8))
                   (begin
                     (set! rcx vec.9)
                     (set! rdx tmp.75)
                     (set! rsi len.8)
                     (set! rdi vector-init-loop.7)
                     (set! r15 tmp-ra.81)
                     (jump
                      L.vector-init-loop.7.7
                      rbp
                      r15
                      rdi
                      rsi
                      rdx
                      rcx)))))))))
     (begin
       (set! tmp-ra.82 r15)
       (begin
         (begin
           (begin (set! tmp.76 (alloc 24)) (set! tmp.62 (+ tmp.76 2)))
           (begin
             (mset! tmp.62 -2 L.vector-init-loop.7.7)
             (mset! tmp.62 6 24)
             (set! vector-init-loop.7 tmp.62)))
         (begin
           (begin (set! tmp.77 (alloc 24)) (set! tmp.63 (+ tmp.77 2)))
           (begin
             (mset! tmp.63 -2 L.make-init-vector.1.8)
             (mset! tmp.63 6 8)
             (set! make-init-vector.1 tmp.63)))
         (begin
           (begin (set! tmp.78 (alloc 24)) (set! tmp.64 (+ tmp.78 2)))
           (begin
             (mset! tmp.64 -2 L.make-vector.57.9)
             (mset! tmp.64 6 8)
             (set! make-vector.57 tmp.64)))
         (begin
           (mset! vector-init-loop.7 14 vector-init-loop.7)
           (mset! make-init-vector.1 14 vector-init-loop.7)
           (mset! make-vector.57 14 make-init-vector.1)
           (begin
             (begin
               (return-point L.rp.10
                 (begin
                   (set! rsi 0)
                   (set! rdi make-vector.57)
                   (set! r15 L.rp.10)
                   (jump L.make-vector.57.9 rbp r15 rdi rsi)))
               (set! x.1.4 rax))
             (begin (set! rax x.1.4) (jump tmp-ra.82 rbp rax)))))))])
     (test-case "error case"
     (check-match (select-instructions x)
                  '(module
                    ((new-frames (())))
                    (define L.make-vector.57.9
                      ((new-frames ()))
                      (begin
                        (set! tmp-ra.79 r15)
                        (set! c.60 rdi)
                        (set! tmp.33 rsi)
                        (set! make-init-vector.1 (mref c.60 14))
                        (if (begin
                              (if (begin
                                    (begin
                                      (set! tmp.66 tmp.33)
                                      (set! tmp.66 (bitwise-and tmp.66 7)))
                                    (= tmp.66 0))
                                (set! tmp.65 14)
                                (set! tmp.65 6))
                              (!= tmp.65 6))
                          (begin
                            (set! rsi tmp.33)
                            (set! rdi make-init-vector.1)
                            (set! r15 tmp-ra.79)
                            (jump L.make-init-vector.1.8 rbp r15 rdi rsi))
                          (begin (set! rax 2110) (jump tmp-ra.79 rbp rax)))))
                    (define L.make-init-vector.1.8
                      ((new-frames ()))
                      (begin
                        (set! tmp-ra.80 r15)
                        (set! c.59 rdi)
                        (set! tmp.5 rsi)
                        (set! vector-init-loop.7 (mref c.59 14))
                        (set! tmp.70 tmp.5)
                        (set! tmp.70 (arithmetic-shift-right tmp.70 3))
                        (set! tmp.69 1)
                        (set! tmp.69 (+ tmp.69 tmp.70))
                        (set! tmp.68 tmp.69)
                        (set! tmp.68 (* tmp.68 8))
                        (set! tmp.67 (alloc tmp.68))
                        (set! tmp.61 tmp.67)
                        (set! tmp.61 (+ tmp.61 3))
                        (mset! tmp.61 -3 tmp.5)
                        (set! tmp.6 tmp.61)
                        (set! rcx tmp.6)
                        (set! rdx 0)
                        (set! rsi tmp.5)
                        (set! rdi vector-init-loop.7)
                        (set! r15 tmp-ra.80)
                        (jump L.vector-init-loop.7.7 rbp r15 rdi rsi rdx rcx)))
                    (define L.vector-init-loop.7.7
                      ((new-frames ()))
                      (begin
                        (set! tmp-ra.81 r15)
                        (set! c.58 rdi)
                        (set! len.8 rsi)
                        (set! i.10 rdx)
                        (set! vec.9 rcx)
                        (set! vector-init-loop.7 (mref c.58 14))
                        (if (begin
                              (if (= len.8 i.10) (set! tmp.71 14) (set! tmp.71 6))
                              (!= tmp.71 6))
                          (begin (set! rax vec.9) (jump tmp-ra.81 rbp rax))
                          (begin
                            (set! tmp.74 i.10)
                            (set! tmp.74 (arithmetic-shift-right tmp.74 3))
                            (set! tmp.73 tmp.74)
                            (set! tmp.73 (* tmp.73 8))
                            (set! tmp.72 tmp.73)
                            (set! tmp.72 (+ tmp.72 5))
                            (mset! vec.9 tmp.72 0)
                            (set! tmp.75 i.10)
                            (set! tmp.75 (+ tmp.75 8))
                            (set! rcx vec.9)
                            (set! rdx tmp.75)
                            (set! rsi len.8)
                            (set! rdi vector-init-loop.7)
                            (set! r15 tmp-ra.81)
                            (jump L.vector-init-loop.7.7 rbp r15 rdi rsi rdx rcx)))))
                    (begin
                      (set! tmp-ra.82 r15)
                      (set! tmp.76 (alloc 24))
                      (set! tmp.62 tmp.76)
                      (set! tmp.62 (+ tmp.62 2))
                      (mset! tmp.62 -2 L.vector-init-loop.7.7)
                      (mset! tmp.62 6 24)
                      (set! vector-init-loop.7 tmp.62)
                      (set! tmp.77 (alloc 24))
                      (set! tmp.63 tmp.77)
                      (set! tmp.63 (+ tmp.63 2))
                      (mset! tmp.63 -2 L.make-init-vector.1.8)
                      (mset! tmp.63 6 8)
                      (set! make-init-vector.1 tmp.63)
                      (set! tmp.78 (alloc 24))
                      (set! tmp.64 tmp.78)
                      (set! tmp.64 (+ tmp.64 2))
                      (mset! tmp.64 -2 L.make-vector.57.9)
                      (mset! tmp.64 6 8)
                      (set! make-vector.57 tmp.64)
                      (mset! vector-init-loop.7 14 vector-init-loop.7)
                      (mset! make-init-vector.1 14 vector-init-loop.7)
                      (mset! make-vector.57 14 make-init-vector.1)
                      (return-point L.rp.10
                        (begin
                          (set! rsi 0)
                          (set! rdi make-vector.57)
                          (set! r15 L.rp.10)
                          (jump L.make-vector.57.9 rbp r15 rdi rsi)))
                      (set! x.1.4 rax)
                      (set! rax x.1.4)
                      (jump tmp-ra.82 rbp rax))))))