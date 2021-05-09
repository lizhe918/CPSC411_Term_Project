#lang racket
(require
  cpsc411/compiler-lib
  cpsc411/2c-run-time
  cpsc411/test-suite/utils
  racket/match
  rackunit)

(require "../component/assign.rkt")


; input: asm-pred-lang-v7/spilled
; output: asm-pred-lang-v7/assignments
; purpose: Check if the output of assign-frame-variables matches the output of interrogator 


(let ([x   `(module
            ((locals ()) (undead-out ()) (conflicts ()) (assignment ()))
            (define L.addup.2
                ((locals (z.1 y.1 x.3))
                (undead-out
                ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                (conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1))))
                (assignment ((x.1 fv0) (nfv.1 fv1) (nfv.2 fv2) (nfv.3 fv3))))
                (begin
                (set! x.1 1)
                (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (bitwise-and z.1 x.1)))
                (begin
                    (set! rbp (- rbp 8))
                    (return-point
                    L.addup.1
                    (begin (set! x.3 L.addup.2) (jump x.3 z.1 y.1)))
                    (set! rbp (+ rbp 8)))
                (jump L.addup.2 x.1)))
            (jump L.addup.2))])
     (test-case "Preserve allocated new frame variable, need to assign spilled variable, reuse spilled variable -- bitwise-and"
     (check-match (assign-frame-variables x)
                `(module
                (,_ ... (assignment ()) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment
                    ,(list-no-order '(x.1 fv0)
                    '(nfv.1 fv1)
                    '(nfv.2 fv2)
                    '(nfv.3 fv3)
                    '(x.3 fv1)
                    '(y.1 fv1)
                    '(z.1 fv1))) ,_ ...)
                    (begin
                    (set! x.1 1)
                    (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (bitwise-and z.1 x.1)))
                    (begin
                        (set! rbp (- rbp 8))
                        (return-point
                        L.addup.1
                        (begin (set! x.3 L.addup.2) (jump x.3 z.1 y.1)))
                        (set! rbp (+ rbp 8)))
                    (jump L.addup.2 x.1)))
                (jump L.addup.2)))))


(let ([x   `(module
            ((locals ()) (undead-out ()) (conflicts ()) (assignment ()))
            (define L.addup.2
                ((locals (z.1 y.1 x.3))
                (undead-out
                ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                (conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1))))
                (assignment ((x.1 fv0) (nfv.1 fv1) (nfv.2 fv2) (nfv.3 fv3))))
                (begin
                (set! x.1 1)
                (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (bitwise-ior z.1 x.1)))
                (begin
                    (set! rbp (- rbp 8))
                    (return-point
                    L.addup.1
                    (begin (set! x.3 L.addup.2) (jump x.3 z.1 y.1)))
                    (set! rbp (+ rbp 8)))
                (jump L.addup.2 x.1)))
            (jump L.addup.2))])
     (test-case "Preserve allocated new frame variable, need to assign spilled variable, reuse spilled variable -- bitwise-ior"
     (check-match (assign-frame-variables x)
                `(module
                (,_ ... (assignment ()) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment
                    ,(list-no-order '(x.1 fv0)
                    '(nfv.1 fv1)
                    '(nfv.2 fv2)
                    '(nfv.3 fv3)
                    '(x.3 fv1)
                    '(y.1 fv1)
                    '(z.1 fv1))) ,_ ...)
                    (begin
                    (set! x.1 1)
                    (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (bitwise-ior z.1 x.1)))
                    (begin
                        (set! rbp (- rbp 8))
                        (return-point
                        L.addup.1
                        (begin (set! x.3 L.addup.2) (jump x.3 z.1 y.1)))
                        (set! rbp (+ rbp 8)))
                    (jump L.addup.2 x.1)))
                (jump L.addup.2)))))


(let ([x   `(module
            ((locals ()) (undead-out ()) (conflicts ()) (assignment ()))
            (define L.addup.2
                ((locals (z.1 y.1 x.3))
                (undead-out
                ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                (conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1))))
                (assignment ((x.1 fv0) (nfv.1 fv1) (nfv.2 fv2) (nfv.3 fv3))))
                (begin
                (set! x.1 1)
                (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (bitwise-xor z.1 x.1)))
                (begin
                    (set! rbp (- rbp 8))
                    (return-point
                    L.addup.1
                    (begin (set! x.3 L.addup.2) (jump x.3 z.1 y.1)))
                    (set! rbp (+ rbp 8)))
                (jump L.addup.2 x.1)))
            (jump L.addup.2))])
     (test-case "Preserve allocated new frame variable, need to assign spilled variable, reuse spilled variable -- bitwise-xor"
     (check-match (assign-frame-variables x)
                `(module
                (,_ ... (assignment ()) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment
                    ,(list-no-order '(x.1 fv0)
                    '(nfv.1 fv1)
                    '(nfv.2 fv2)
                    '(nfv.3 fv3)
                    '(x.3 fv1)
                    '(y.1 fv1)
                    '(z.1 fv1))) ,_ ...)
                    (begin
                    (set! x.1 1)
                    (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (bitwise-xor z.1 x.1)))
                    (begin
                        (set! rbp (- rbp 8))
                        (return-point
                        L.addup.1
                        (begin (set! x.3 L.addup.2) (jump x.3 z.1 y.1)))
                        (set! rbp (+ rbp 8)))
                    (jump L.addup.2 x.1)))
                (jump L.addup.2)))))


(let ([x   `(module
            ((locals ()) (undead-out ()) (conflicts ()) (assignment ()))
            (define L.addup.2
                ((locals (z.1 y.1 x.3))
                (undead-out
                ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                (conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1))))
                (assignment ((x.1 fv0) (nfv.1 fv1) (nfv.2 fv2) (nfv.3 fv3))))
                (begin
                (set! x.1 1)
                (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (arithmetic-shift-right z.1 x.1)))
                (begin
                    (set! rbp (- rbp 8))
                    (return-point
                    L.addup.1
                    (begin (set! x.3 L.addup.2) (jump x.3 z.1 y.1)))
                    (set! rbp (+ rbp 8)))
                (jump L.addup.2 x.1)))
            (jump L.addup.2))])
     (test-case "Preserve allocated new frame variable, need to assign spilled variable, reuse spilled variable -- arithmetic-shift-right"
     (check-match (assign-frame-variables x)
                `(module
                (,_ ... (assignment ()) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment
                    ,(list-no-order '(x.1 fv0)
                    '(nfv.1 fv1)
                    '(nfv.2 fv2)
                    '(nfv.3 fv3)
                    '(x.3 fv1)
                    '(y.1 fv1)
                    '(z.1 fv1))) ,_ ...)
                    (begin
                    (set! x.1 1)
                    (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (arithmetic-shift-right z.1 x.1)))
                    (begin
                        (set! rbp (- rbp 8))
                        (return-point
                        L.addup.1
                        (begin (set! x.3 L.addup.2) (jump x.3 z.1 y.1)))
                        (set! rbp (+ rbp 8)))
                    (jump L.addup.2 x.1)))
                (jump L.addup.2)))))









;; ---------------------------------------------------------------------
(let ([x   `(module
                ((locals (q.1 m.1))
                (undead-out ((q.1) (m.1 q.1) (m.1 q.1)))
                (conflicts ((q.1 (m.1)) (m.1 (q.1))))
                (assignment ()))
                (define L.addup.2
                  ((locals (x.1 x.2))
                  (undead-out ((x.1) (x.2 x.1) (x.1)))
                  (conflicts ((x.2 (x.1)) (x.1 (x.2))))
                  (assignment ()))
                  (begin (set! x.1 1) (set! x.2 L.addup.1) (jump x.2 x.1)))
                (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1)))])
     (test-case "All alocs need to be assigned to spilled variables"
     (check-match (assign-frame-variables x)
                `(module
                (,_ ... (assignment ,(list-no-order '(m.1 fv0) '(q.1 fv1))) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment ,(list-no-order '(x.2 fv0) '(x.1 fv1))) ,_ ...)
                    (begin (set! x.1 1) (set! x.2 L.addup.1) (jump x.2 x.1)))
                (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1))))))

(let ([x   `(module
                  ((locals ())
                  (undead-out ((q.1) (m.1 q.1) (m.1 q.1)))
                  (conflicts ((q.1 (m.1)) (m.1 (q.1))))
                  (assignment ((m.1 r15) (q.1 r14))))
                  (define L.addup.2
                    ((locals ())
                    (undead-out ((x.1) (x.2 x.1) (x.1)))
                    (conflicts ((x.2 (x.1)) (x.1 (x.2))))
                    (assignment ((x.2 r15) (x.1 r14))))
                    (begin (set! x.1 1) (set! x.2 L.addup.1) (jump x.2 x.1)))
                  (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1)))])
     (test-case "No aloc need to be assigned to spilled variables"
     (check-match (assign-frame-variables x)
                `(module
                (,_ ... (assignment ,(list-no-order '(m.1 r15) '(q.1 r14))) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment ,(list-no-order '(x.2 r15) '(x.1 r14))) ,_ ...)
                    (begin (set! x.1 1) (set! x.2 L.addup.1) (jump x.2 x.1)))
                (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1))))))


(let ([x   `(module
                  ((locals (q.1))
                  (undead-out ((q.1) (m.1 q.1) (m.1 q.1)))
                  (conflicts ((q.1 (m.1)) (m.1 (q.1))))
                  (assignment ((m.1 r15))))
                  (define L.addup.2
                    ((locals (x.1))
                    (undead-out ((x.1) (x.2 x.1) (x.1)))
                    (conflicts ((x.2 (x.1)) (x.1 (x.2))))
                    (assignment ((x.2 r15))))
                    (begin (set! x.1 1) (set! x.2 L.addup.1) (jump x.2 x.1)))
                  (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1)))])
     (test-case "Assign partial alocs to spilled variable"
     (check-match (assign-frame-variables x)
                `(module
                (,_ ... (assignment ,(list-no-order '(m.1 r15) '(q.1 fv0))) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment ,(list-no-order '(x.2 r15) '(x.1 fv0))) ,_ ...)
                    (begin (set! x.1 1) (set! x.2 L.addup.1) (jump x.2 x.1)))
                (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1))))))



(let ([x   `(module
            ((locals ()) (undead-out ()) (conflicts ()) (assignment ()))
            (define L.addup.2
                ((locals ())
                (undead-out
                ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                (conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1))))
                (assignment ((z.1 fv0) (y.1 fv0) (x.1 fv1) (x.3 r15))))
                (begin
                (set! x.1 1)
                (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (- z.1 x.1)))
                (begin
                    (set! rbp (- rbp 16))
                    (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3 z.1)))
                    (set! rbp (+ rbp 16)))
                (jump L.addup.2 x.1)))
            (jump L.addup.2))])
     (test-case "Preserve assigned call-undead variable, no need further assignment"
     (check-match (assign-frame-variables x)
                `(module
                (,_ ... (assignment ()) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment ,(list-no-order '(z.1 fv0) '(y.1 fv0) '(x.1 fv1) '(x.3 r15))) ,_ ...)
                    (begin
                    (set! x.1 1)
                    (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (- z.1 x.1)))
                    (begin
                        (set! rbp (- rbp 16))
                        (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3 z.1)))
                        (set! rbp (+ rbp 16)))
                    (jump L.addup.2 x.1)))
                (jump L.addup.2)))))



(let ([x   `(module
            ((locals ()) (undead-out ()) (conflicts ()) (assignment ()))
            (define L.addup.2
                ((locals (z.1 y.1 x.3))
                (undead-out
                ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                (conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1))))
                (assignment ((x.1 fv0) (nfv.1 fv1) (nfv.2 fv2) (nfv.3 fv3))))
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
     (test-case "Preserve allocated new frame variable, need to assign spilled variable, reuse spilled variable"
     (check-match (assign-frame-variables x)
                `(module
                (,_ ... (assignment ()) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment
                    ,(list-no-order '(x.1 fv0)
                    '(nfv.1 fv1)
                    '(nfv.2 fv2)
                    '(nfv.3 fv3)
                    '(x.3 fv1)
                    '(y.1 fv1)
                    '(z.1 fv1))) ,_ ...)
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
                (jump L.addup.2)))))



(let ([x   `(module
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
            (assignment ()))
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
                (assignment ((tmp-ra.1 fv2) (nfv.2 fv3) (nfv.3 fv4))))
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
     (check-match (assign-frame-variables x)
                `(module
                (,_ ... (assignment ((tmp-ra.4 ,fv2))) ,_ ...)
                (define L.swap.1
                    (,_ ... (assignment
                    ,(list-no-order '(tmp-ra.1 fv2) '(nfv.2 fv3) '(nfv.3 fv4) `(x.1 ,fv0) `(y.2 ,fv1) '(z.3 fv0))) ,_ ...)
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
                    (jump L.swap.1 rbp r15 fv0 fv1)))
                (and (fvar? fv1) (fvar? fv0) (fvar? fv1)))))


