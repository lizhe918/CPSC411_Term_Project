#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/2c-run-time
 cpsc411/test-suite/utils
 racket/match
 rackunit)

(require "../component/assign.rkt")


; input: asm-pred-lang-v7/pre-framed
; output: asm-pred-lang-v7/framed
; purpose: Check if the output of assign-registers matches the output of interrogator 

(let ([x `(module
            ((new-frames ())
            (locals ())
            (call-undead ())
            (undead-out ())
            (conflicts ())
            (assignment ()))
            (define L.addup.2
                ((new-frames ())
                (locals (x.3))
                (undead-out
                ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                (call-undead (x.1 y.1 z.1))
                (conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1))))
                (assignment ((z.1 fv0) (y.1 fv0) (x.1 fv1))))
                (begin
                (set! x.1 1)
                (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (bitwise-and z.1 x.1)))
                (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3 z.1)))
                (jump L.addup.2 x.1)))
            (jump L.addup.2))])
     (test-case "Allocate frames for result-point with more call-undead varibles, and reused fvars (allocate (+ 1 index) frames) -- bitwise-and"
     (check-match (allocate-frames x)
                `(module
                ((locals ()) (undead-out ()) (conflicts ()) (assignment ()))
                (define L.addup.2
                    ,(list-no-order '(locals (x.3))
                    '(undead-out
                    ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                    '(conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1))))
                    '(assignment ((z.1 fv0) (y.1 fv0) (x.1 fv1))))
                    (begin
                    (set! x.1 1)
                    (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (bitwise-and z.1 x.1)))
                    (begin
                        (set! rbp (- rbp 16))
                        (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3 z.1)))
                        (set! rbp (+ rbp 16)))
                    (jump L.addup.2 x.1)))
                (jump L.addup.2)))))


(let ([x `(module
            ((new-frames ())
            (locals ())
            (call-undead ())
            (undead-out ())
            (conflicts ())
            (assignment ()))
            (define L.addup.2
                ((new-frames ())
                (locals (x.3))
                (undead-out
                ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                (call-undead (x.1 y.1 z.1))
                (conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1))))
                (assignment ((z.1 fv0) (y.1 fv0) (x.1 fv1))))
                (begin
                (set! x.1 1)
                (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (bitwise-ior z.1 x.1)))
                (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3 z.1)))
                (jump L.addup.2 x.1)))
            (jump L.addup.2))])
     (test-case "Allocate frames for result-point with more call-undead varibles, and reused fvars (allocate (+ 1 index) frames) -- bitwise-ior"
     (check-match (allocate-frames x)
                `(module
                ((locals ()) (undead-out ()) (conflicts ()) (assignment ()))
                (define L.addup.2
                    ,(list-no-order '(locals (x.3))
                    '(undead-out
                    ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                    '(conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1))))
                    '(assignment ((z.1 fv0) (y.1 fv0) (x.1 fv1))))
                    (begin
                    (set! x.1 1)
                    (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (bitwise-ior z.1 x.1)))
                    (begin
                        (set! rbp (- rbp 16))
                        (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3 z.1)))
                        (set! rbp (+ rbp 16)))
                    (jump L.addup.2 x.1)))
                (jump L.addup.2)))))


(let ([x `(module
            ((new-frames ())
            (locals ())
            (call-undead ())
            (undead-out ())
            (conflicts ())
            (assignment ()))
            (define L.addup.2
                ((new-frames ())
                (locals (x.3))
                (undead-out
                ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                (call-undead (x.1 y.1 z.1))
                (conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1))))
                (assignment ((z.1 fv0) (y.1 fv0) (x.1 fv1))))
                (begin
                (set! x.1 1)
                (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (bitwise-xor z.1 x.1)))
                (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3 z.1)))
                (jump L.addup.2 x.1)))
            (jump L.addup.2))])
     (test-case "Allocate frames for result-point with more call-undead varibles, and reused fvars (allocate (+ 1 index) frames) -- bitwise-xor"
     (check-match (allocate-frames x)
                `(module
                ((locals ()) (undead-out ()) (conflicts ()) (assignment ()))
                (define L.addup.2
                    ,(list-no-order '(locals (x.3))
                    '(undead-out
                    ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                    '(conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1))))
                    '(assignment ((z.1 fv0) (y.1 fv0) (x.1 fv1))))
                    (begin
                    (set! x.1 1)
                    (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (bitwise-xor z.1 x.1)))
                    (begin
                        (set! rbp (- rbp 16))
                        (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3 z.1)))
                        (set! rbp (+ rbp 16)))
                    (jump L.addup.2 x.1)))
                (jump L.addup.2)))))

(let ([x `(module
            ((new-frames ())
            (locals ())
            (call-undead ())
            (undead-out ())
            (conflicts ())
            (assignment ()))
            (define L.addup.2
                ((new-frames ())
                (locals (x.3))
                (undead-out
                ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                (call-undead (x.1 y.1 z.1))
                (conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1))))
                (assignment ((z.1 fv0) (y.1 fv0) (x.1 fv1))))
                (begin
                (set! x.1 1)
                (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (arithmetic-shift-right z.1 x.1)))
                (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3 z.1)))
                (jump L.addup.2 x.1)))
            (jump L.addup.2))])
     (test-case "Allocate frames for result-point with more call-undead varibles, and reused fvars (allocate (+ 1 index) frames) -- arithmetic-shift-right"
     (check-match (allocate-frames x)
                `(module
                ((locals ()) (undead-out ()) (conflicts ()) (assignment ()))
                (define L.addup.2
                    ,(list-no-order '(locals (x.3))
                    '(undead-out
                    ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                    '(conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1))))
                    '(assignment ((z.1 fv0) (y.1 fv0) (x.1 fv1))))
                    (begin
                    (set! x.1 1)
                    (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (arithmetic-shift-right z.1 x.1)))
                    (begin
                        (set! rbp (- rbp 16))
                        (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3 z.1)))
                        (set! rbp (+ rbp 16)))
                    (jump L.addup.2 x.1)))
                (jump L.addup.2)))))
;; ------------------------------------------------------------------------
(let ([x `(module
        ((new-frames ())
        (locals (q.1 m.1))
        (call-undead ())
        (undead-out ((q.1) (m.1 q.1) (m.1 q.1)))
        (conflicts ((q.1 (m.1)) (m.1 (q.1))))
        (assignment ()))
        (define L.addup.2
            ((new-frames ())
            (locals (x.2 x.1))
            (undead-out ((x.1) (x.2 x.1) (x.1)))
            (call-undead ())
            (conflicts ((x.2 (x.1)) (x.1 (x.2))))
            (assignment ()))
            (begin (set! x.1 1) (set! x.2 L.addup.1) (jump x.2 x.1)))
        (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1)))])
     (test-case "Remove new-frames and call-undead only"
     (check-match (allocate-frames x)
                `(module
                ,(list-no-order '(locals (m.1 q.1))
                '(undead-out ((q.1) (m.1 q.1) (m.1 q.1)))
                '(conflicts ((q.1 (m.1)) (m.1 (q.1))))
                '(assignment ()))
                (define L.addup.2
                    ,(list-no-order '(locals (x.1 x.2))
                    '(undead-out ((x.1) (x.2 x.1) (x.1)))
                    '(conflicts ((x.2 (x.1)) (x.1 (x.2))))
                    '(assignment ()))
                    (begin (set! x.1 1) (set! x.2 L.addup.1) (jump x.2 x.1)))
                (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1))))))


(let ([x `(module
            ((new-frames ())
            (locals ())
            (call-undead ())
            (undead-out ())
            (conflicts ())
            (assignment ()))
            (define L.addup.2
                ((new-frames ())
                (locals (z.1 y.1 x.3))
                (undead-out
                ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                (call-undead (x.1))
                (conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1))))
                (assignment ((x.1 fv0))))
                (begin
                (set! x.1 1)
                (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (- z.1 x.1)))
                (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3 z.1)))
                (jump L.addup.2 x.1)))
            (jump L.addup.2))])
     (test-case "Allocate frames for result-point"
     (check-match (allocate-frames x)
                `(module
                ,(list-no-order '(locals ()) '(undead-out ()) '(conflicts ()) '(assignment ()))
                (define L.addup.2
                    ,(list-no-order `(locals ,(list-no-order 'z.1 'y.1 'x.3))
                    '(undead-out
                    ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                    '(conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1))))
                    '(assignment ((x.1 fv0))))
                    (begin
                    (set! x.1 1)
                    (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (- z.1 x.1)))
                    (begin
                        (set! rbp (- rbp 8))
                        (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3 z.1)))
                        (set! rbp (+ rbp 8)))
                    (jump L.addup.2 x.1)))
                (jump L.addup.2)))))



(let ([x `(module
            ((new-frames ())
            (locals ())
            (call-undead ())
            (undead-out ())
            (conflicts ())
            (assignment ()))
            (define L.addup.2
                ((new-frames ())
                (locals (x.3))
                (undead-out
                ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                (call-undead (x.1 y.1 z.1))
                (conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1))))
                (assignment ((z.1 fv0) (y.1 fv0) (x.1 fv1))))
                (begin
                (set! x.1 1)
                (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (- z.1 x.1)))
                (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3 z.1)))
                (jump L.addup.2 x.1)))
            (jump L.addup.2))])
     (test-case "Allocate frames for result-point with more call-undead varibles, and reused fvars (allocate (+ 1 index) frames)"
     (check-match (allocate-frames x)
                `(module
                ((locals ()) (undead-out ()) (conflicts ()) (assignment ()))
                (define L.addup.2
                    ,(list-no-order '(locals (x.3))
                    '(undead-out
                    ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                    '(conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1))))
                    '(assignment ((z.1 fv0) (y.1 fv0) (x.1 fv1))))
                    (begin
                    (set! x.1 1)
                    (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (- z.1 x.1)))
                    (begin
                        (set! rbp (- rbp 16))
                        (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3 z.1)))
                        (set! rbp (+ rbp 16)))
                    (jump L.addup.2 x.1)))
                (jump L.addup.2)))))


(let ([x `(module
            ((new-frames ())
            (locals ())
            (call-undead ())
            (undead-out ())
            (conflicts ())
            (assignment ()))
            (define L.addup.2
                ((new-frames ((nfv.1 nfv.2 nfv.3)))
                (locals (z.1 y.1 x.3))
                (undead-out
                ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                (call-undead (x.1))
                (conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1)) (nfv.1 (nfv.2 nfv.3)) (nfv.2 (nfv.1 nfv.3)) (nfv.3 (nfv.2 nfv.1))))
                (assignment ((x.1 fv0))))
                (begin
                (set! x.1 1)
                (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (- z.1 x.1)))
                (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3 z.1 y.1)))
                (jump L.addup.2 x.1)))
            (jump L.addup.2))])
     (test-case "Allocate frames for new-frame"
     (check-match (allocate-frames x)
                `(module
                ,(list-no-order '(locals ()) '(undead-out ()) '(conflicts ()) '(assignment ()))
                (define L.addup.2
                    ,(list-no-order '(locals (x.3 y.1 z.1))
                    '(undead-out
                    ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                    '(conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1)) (nfv.1 (nfv.2 nfv.3)) (nfv.2 (nfv.1 nfv.3)) (nfv.3 (nfv.2 nfv.1))))
                    '(assignment ((nfv.3 fv3) (nfv.2 fv2) (nfv.1 fv1) (x.1 fv0))))
                    (begin
                    (set! x.1 1)
                    (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (- z.1 x.1)))
                    (begin
                        (set! rbp (- rbp 8))
                        (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3 z.1 y.1)))
                        (set! rbp (+ rbp 8)))
                    (jump L.addup.2 x.1)))
                (jump L.addup.2)))))



(let ([x `(module
            ((new-frames ())
            (locals ())
            (call-undead ())
            (undead-out ())
            (conflicts ())
            (assignment ()))
            (define L.addup.2
                ((new-frames ((nfv.1 nfv.2 nfv.3) (nfv.4) (nfv.5 nfv.6)))
                (locals (z.1 y.1 x.3))
                (undead-out
                ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                (call-undead (x.1))
                (conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1)) (nfv.1 (nfv.2 nfv.3)) (nfv.2 (nfv.1 nfv.3)) (nfv.3 (nfv.1 nfv.2)) (nfv.4 ()) (nfv.5 (nfv.6)) (nfv.6 (nfv.5))))
                (assignment ((x.1 fv0))))
                (begin
                (set! x.1 1)
                (begin (set! y.1 2) (set! z.1 y.1) (set! z.1 (- z.1 x.1)))
                (return-point L.addup.1 (begin (set! x.3 L.addup.2) (jump x.3 z.1 y.1)))
                (jump L.addup.2 x.1)))
            (jump L.addup.2))])
     (test-case "Allocate frames for multiple new-frames list"
     (check-match (allocate-frames x)
                `(module
                ,(list-no-order '(locals ()) '(undead-out ()) '(conflicts ()) '(assignment ()))
                (define L.addup.2
                    ,(list-no-order '(locals (x.3 y.1 z.1))
                    '(undead-out
                    ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                    '(conflicts
                    ((x.1 (x.3 y.1 z.1))
                    (z.1 (x.1))
                    (y.1 (x.1))
                    (x.3 (x.1))
                    (nfv.1 (nfv.2 nfv.3))
                    (nfv.2 (nfv.1 nfv.3))
                    (nfv.3 (nfv.1 nfv.2))
                    (nfv.4 ())
                    (nfv.5 (nfv.6))
                    (nfv.6 (nfv.5))))
                    '(assignment
                    ((nfv.3 fv3)
                    (nfv.2 fv2)
                    (nfv.1 fv1)
                    (nfv.4 fv1)
                    (nfv.6 fv2)
                    (nfv.5 fv1)
                    (x.1 fv0))))
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



;; ------------------ General example -------------

(let ([x `(module
            ((new-frames ((nfv.7 nfv.8) (nfv.6)))
            (locals (q.1 nfv.6 nfv.7 nfv.8))
            (call-undead (z.1 tmp-ra.5))
            (undead-out
                ((tmp-ra.5 rbp)
                (((((rax tmp-ra.5 rbp) ((nfv.6 rbp) (nfv.6 r15 rbp) (nfv.6 r15 rbp)))
                    (z.1 tmp-ra.5 rbp))
                (((rax z.1 tmp-ra.5 rbp)
                    ((nfv.8 rbp)
                    (nfv.8 nfv.7 rbp)
                    (nfv.8 nfv.7 r15 rbp)
                    (nfv.8 nfv.7 r15 rbp)))
                    (q.1 z.1 tmp-ra.5 rbp))
                (tmp-ra.5 rbp))
                ((rax tmp-ra.5 rbp) (tmp-ra.5 rax rbp) (rax rbp))
                ((tmp-ra.5 fv0 rbp) (fv0 r15 rbp) (fv0 r15 rbp)))))
            (conflicts
                ((z.1 (nfv.8 q.1 rbp tmp-ra.5))
                (q.1 (rbp tmp-ra.5 z.1))
                (nfv.6 (rbp tmp-ra.5 rax))
                (nfv.7 (rbp nfv.8))
                (nfv.8 (rbp tmp-ra.5 z.1 rax nfv.7))
                (tmp-ra.5 (rbp nfv.8 q.1 nfv.6 z.1 rax fv0))
                (fv0 (r15 rbp tmp-ra.5))
                (rbp (tmp-ra.5 nfv.8 q.1 nfv.6 z.1 rax r15 fv0))
                (r15 (rbp fv0))
                (rax (nfv.8 nfv.6 rbp tmp-ra.5))))
            (assignment ((tmp-ra.5 fv1) (z.1 fv0))))
            (define L.addup.1
                ((new-frames ((nfv.2 nfv.3)))
                (locals (x.1 nfv.2 nfv.3 x.2))
                (undead-out
                ((fv0 x.2 tmp-ra.1 rbp)
                (x.1 x.2 tmp-ra.1 rbp)
                ((rax tmp-ra.1 rbp)
                    ((x.1 nfv.3 rbp)
                    (nfv.3 nfv.2 rbp)
                    (nfv.3 nfv.2 r15 rbp)
                    (nfv.3 nfv.2 r15 rbp)))
                (x.2 tmp-ra.1 rbp)
                (tmp-ra.1 rax rbp)
                (rax rbp)))
                (call-undead (tmp-ra.1))
                (conflicts
                ((tmp-ra.1 (rax nfv.3 x.1 rbp x.2 fv0))
                (x.1 (rbp tmp-ra.1 x.2))
                (nfv.2 ())
                (nfv.3 (rbp tmp-ra.1 rax))
                (x.2 (rbp x.1 tmp-ra.1))
                (fv0 (tmp-ra.1))
                (rbp (rax x.2 nfv.3 x.1 tmp-ra.1))
                (rax (rbp tmp-ra.1 nfv.3))))
                (assignment ((tmp-ra.1 fv1))))
                        (begin
                        (set! tmp-ra.1 r15)
                        (set! x.1 fv0)
                        (return-point L.rp.1
                            (begin
                            (set! nfv.3 x.2)
                            (set! nfv.2 x.1)
                            (set! r15 L.rp.1)
                            (jump L.addup.2 rbp r15 nfv.2 nfv.3)))
                        (set! x.2 rax)
                        (set! rax x.2)
                        (jump tmp-ra.1 rbp rax)))
            (define L.addup.2
                ((new-frames ())
                (locals (tmp-ra.4 y.1 y.2))
                (undead-out
                ((fv0 fv1 tmp-ra.4 rbp)
                (fv1 y.1 tmp-ra.4 rbp)
                (y.1 y.2 tmp-ra.4 rbp)
                (y.2 rax tmp-ra.4 rbp)
                (tmp-ra.4 rax rbp)
                (rax rbp)))
                (call-undead ())
                (conflicts
                ((tmp-ra.4 (rax y.2 y.1 rbp fv1 fv0))
                (y.1 (y.2 rbp tmp-ra.4 fv1))
                (y.2 (rax rbp tmp-ra.4 y.1))
                (fv0 (tmp-ra.4))
                (fv1 (y.1 tmp-ra.4))
                (rbp (rax y.2 y.1 tmp-ra.4))
                (rax (rbp tmp-ra.4 y.2))))
                (assignment ()))
            (begin
            (set! tmp-ra.4 r15)
            (set! y.1 fv0)
            (set! y.2 fv1)
            (set! rax y.1)
            (set! rax (+ rax y.2))
            (jump tmp-ra.4 rbp rax)))
            (begin
                (set! tmp-ra.5 r15)
                (if (begin
                    (begin
                        (return-point L.rp.2
                        (begin
                            (set! nfv.6 1)
                            (set! r15 L.rp.2)
                            (jump L.addup.1 rbp r15 nfv.6)))
                        (set! z.1 rax))
                    (begin
                        (return-point L.rp.3
                        (begin
                            (set! nfv.8 3)
                            (set! nfv.7 2)
                            (set! r15 L.rp.3)
                            (jump L.addup.2 rbp r15 nfv.7 nfv.8)))
                        (set! q.1 rax))
                    (< z.1 q.1))
                (begin (set! rax 1) (set! rax (+ rax 2)) (jump tmp-ra.5 rbp rax))
                (begin (set! fv0 2) (set! r15 tmp-ra.5) (jump L.addup.1 rbp r15 fv0)))))])
     (test-case "Complex case"
     (check-match (allocate-frames x)
                `(module
                    ,(list-no-order '(locals (q.1))
                    '(undead-out
                        ((tmp-ra.5 rbp)
                        (((((rax tmp-ra.5 rbp) ((nfv.6 rbp) (nfv.6 r15 rbp) (nfv.6 r15 rbp)))
                            (z.1 tmp-ra.5 rbp))
                        (((rax z.1 tmp-ra.5 rbp)
                            ((nfv.8 rbp)
                            (nfv.8 nfv.7 rbp)
                            (nfv.8 nfv.7 r15 rbp)
                            (nfv.8 nfv.7 r15 rbp)))
                            (q.1 z.1 tmp-ra.5 rbp))
                        (tmp-ra.5 rbp))
                        ((rax tmp-ra.5 rbp) (tmp-ra.5 rax rbp) (rax rbp))
                        ((tmp-ra.5 fv0 rbp) (fv0 r15 rbp) (fv0 r15 rbp)))))
                    '(conflicts
                    ((z.1 (nfv.8 q.1 rbp tmp-ra.5))
                    (q.1 (rbp tmp-ra.5 z.1))
                    (nfv.6 (rbp tmp-ra.5 rax))
                    (nfv.7 (rbp nfv.8))
                    (nfv.8 (rbp tmp-ra.5 z.1 rax nfv.7))
                    (tmp-ra.5 (rbp nfv.8 q.1 nfv.6 z.1 rax fv0))
                    (fv0 (r15 rbp tmp-ra.5))
                    (rbp (tmp-ra.5 nfv.8 q.1 nfv.6 z.1 rax r15 fv0))
                    (r15 (rbp fv0))
                    (rax (nfv.8 nfv.6 rbp tmp-ra.5))))
                    '(assignment ((nfv.8 fv3) (nfv.7 fv2) (nfv.6 fv2) (tmp-ra.5 fv1) (z.1 fv0))))
                    (define L.addup.1
                        ,(list-no-order '(locals (x.2 x.1))
                        '(undead-out
                        ((fv0 x.2 tmp-ra.1 rbp)
                        (x.1 x.2 tmp-ra.1 rbp)
                        ((rax tmp-ra.1 rbp)
                            ((x.1 nfv.3 rbp)
                            (nfv.3 nfv.2 rbp)
                            (nfv.3 nfv.2 r15 rbp)
                            (nfv.3 nfv.2 r15 rbp)))
                        (x.2 tmp-ra.1 rbp)
                        (tmp-ra.1 rax rbp)
                        (rax rbp)))
                        '(conflicts
                        ((tmp-ra.1 (rax nfv.3 x.1 rbp x.2 fv0))
                        (x.1 (rbp tmp-ra.1 x.2))
                        (nfv.2 ())
                        (nfv.3 (rbp tmp-ra.1 rax))
                        (x.2 (rbp x.1 tmp-ra.1))
                        (fv0 (tmp-ra.1))
                        (rbp (rax x.2 nfv.3 x.1 tmp-ra.1))
                        (rax (rbp tmp-ra.1 nfv.3))))
                        '(assignment ((nfv.3 fv2) (nfv.2 fv2) (tmp-ra.1 fv1))))
                        (begin
                        (set! tmp-ra.1 r15)
                        (set! x.1 fv0)
                        (begin
                            (set! rbp (- rbp 16))
                            (return-point L.rp.1
                            (begin
                                (set! nfv.3 x.2)
                                (set! nfv.2 x.1)
                                (set! r15 L.rp.1)
                                (jump L.addup.2 rbp r15 nfv.2 nfv.3)))
                            (set! rbp (+ rbp 16)))
                        (set! x.2 rax)
                        (set! rax x.2)
                        (jump tmp-ra.1 rbp rax)))
                    (define L.addup.2
                        ,(list-no-order '(locals (y.2 y.1 tmp-ra.4))
                        '(undead-out
                        ((fv0 fv1 tmp-ra.4 rbp)
                        (fv1 y.1 tmp-ra.4 rbp)
                        (y.1 y.2 tmp-ra.4 rbp)
                        (y.2 rax tmp-ra.4 rbp)
                        (tmp-ra.4 rax rbp)
                        (rax rbp)))
                        '(conflicts
                        ((tmp-ra.4 (rax y.2 y.1 rbp fv1 fv0))
                        (y.1 (y.2 rbp tmp-ra.4 fv1))
                        (y.2 (rax rbp tmp-ra.4 y.1))
                        (fv0 (tmp-ra.4))
                        (fv1 (y.1 tmp-ra.4))
                        (rbp (rax y.2 y.1 tmp-ra.4))
                        (rax (rbp tmp-ra.4 y.2))))
                        '(assignment ()))
                        (begin
                        (set! tmp-ra.4 r15)
                        (set! y.1 fv0)
                        (set! y.2 fv1)
                        (set! rax y.1)
                        (set! rax (+ rax y.2))
                        (jump tmp-ra.4 rbp rax)))
                    (begin
                        (set! tmp-ra.5 r15)
                        (if (begin
                            (begin
                                (begin
                                (set! rbp (- rbp 16))
                                (return-point L.rp.2
                                    (begin
                                    (set! nfv.6 1)
                                    (set! r15 L.rp.2)
                                    (jump L.addup.1 rbp r15 nfv.6)))
                                (set! rbp (+ rbp 16)))
                                (set! z.1 rax))
                            (begin
                                (begin
                                (set! rbp (- rbp 16))
                                (return-point L.rp.3
                                    (begin
                                    (set! nfv.8 3)
                                    (set! nfv.7 2)
                                    (set! r15 L.rp.3)
                                    (jump L.addup.2 rbp r15 nfv.7 nfv.8)))
                                (set! rbp (+ rbp 16)))
                                (set! q.1 rax))
                            (< z.1 q.1))
                        (begin (set! rax 1) (set! rax (+ rax 2)) (jump tmp-ra.5 rbp rax))
                        (begin (set! fv0 2) (set! r15 tmp-ra.5) (jump L.addup.1 rbp r15 fv0))))))))




;; from textbook
                            (let ([x `(module
                            ((new-frames ())
                            (locals (tmp-ra.10))
                            (call-undead ())
                            (undead-out
                                ((tmp-ra.10 rbp)
                                (tmp-ra.10 fv1 rbp)
                                (tmp-ra.10 fv1 fv0 rbp)
                                (fv1 fv0 r15 rbp)
                                (fv1 fv0 r15 rbp)))
                            (conflicts
                                ((tmp-ra.10 (fv0 fv1 rbp))
                                (rbp (r15 fv0 fv1 tmp-ra.10))
                                (fv1 (r15 fv0 rbp tmp-ra.10))
                                (fv0 (r15 rbp fv1 tmp-ra.10))
                                (r15 (rbp fv0 fv1))))
                            (assignment ()))
                            (define L.swap.1
                                ((new-frames ((nfv.8 nfv.9)))
                                (locals (y.2 x.1 z.3 nfv.9 nfv.8))
                                (undead-out
                                ((fv0 fv1 tmp-ra.7 rbp)
                                (fv1 x.1 tmp-ra.7 rbp)
                                (y.2 x.1 tmp-ra.7 rbp)
                                ((y.2 x.1 tmp-ra.7 rbp)
                                    ((tmp-ra.7 rax rbp) (rax rbp))
                                    (((rax tmp-ra.7 rbp)
                                    ((y.2 nfv.9 rbp)
                                    (nfv.9 nfv.8 rbp)
                                    (nfv.9 nfv.8 r15 rbp)
                                    (nfv.9 nfv.8 r15 rbp)))
                                    (z.3 tmp-ra.7 rbp)
                                    (tmp-ra.7 rax rbp)
                                    (rax rbp)))))
                                (call-undead (tmp-ra.7))
                                (conflicts
                                ((y.2 (rbp tmp-ra.7 x.1 nfv.9))
                                (x.1 (y.2 rbp tmp-ra.7 fv1))
                                (tmp-ra.7 (y.2 x.1 rbp fv1 fv0 rax z.3))
                                (z.3 (rbp tmp-ra.7))
                                (nfv.9 (r15 nfv.8 rbp y.2))
                                (nfv.8 (r15 rbp nfv.9))
                                (rbp (y.2 x.1 tmp-ra.7 rax z.3 r15 nfv.8 nfv.9))
                                (r15 (rbp nfv.8 nfv.9))
                                (rax (rbp tmp-ra.7))
                                (fv0 (tmp-ra.7))
                                (fv1 (x.1 tmp-ra.7))))
                                (assignment ((tmp-ra.7 fv2))))
                                (begin
                                (set! tmp-ra.7 r15)
                                (set! x.1 fv0)
                                (set! y.2 fv1)
                                (if (< y.2 x.1)
                                    (begin (set! rax x.1) (jump tmp-ra.7 rbp rax))
                                    (begin
                                    (return-point L.rp.3
                                        (begin
                                        (set! nfv.9 x.1)
                                        (set! nfv.8 y.2)
                                        (set! r15 L.rp.3)
                                        (jump L.swap.1 rbp r15 nfv.8 nfv.9)))
                                    (set! z.3 rax)
                                    (set! rax z.3)
                                    (jump tmp-ra.7 rbp rax)))))
                                    (begin
                                        (set! tmp-ra.10 r15)
                                        (set! fv1 2)
                                        (set! fv0 1)
                                        (set! r15 tmp-ra.10)
                                        (jump L.swap.1 rbp r15 fv0 fv1)))])
                                (test-case "Complex example"
                                (check-match (allocate-frames x)
                                            `(module
                                            ,(list-no-order '(locals (tmp-ra.10))
                                            '(undead-out
                                                ((tmp-ra.10 rbp)
                                                (tmp-ra.10 fv1 rbp)
                                                (tmp-ra.10 fv1 fv0 rbp)
                                                (fv1 fv0 r15 rbp)
                                                (fv1 fv0 r15 rbp)))
                                            '(conflicts
                                                ((tmp-ra.10 (fv0 fv1 rbp))
                                                (rbp (r15 fv0 fv1 tmp-ra.10))
                                                (fv1 (r15 fv0 rbp tmp-ra.10))
                                                (fv0 (r15 rbp fv1 tmp-ra.10))
                                                (r15 (rbp fv0 fv1))))
                                            '(assignment ()))
                                            (define L.swap.1
                                                ,(list-no-order '(locals (z.3 x.1 y.2))
                                                '(undead-out
                                                ((fv0 fv1 tmp-ra.7 rbp)
                                                (fv1 x.1 tmp-ra.7 rbp)
                                                (y.2 x.1 tmp-ra.7 rbp)
                                                ((y.2 x.1 tmp-ra.7 rbp)
                                                    ((tmp-ra.7 rax rbp) (rax rbp))
                                                    (((rax tmp-ra.7 rbp)
                                                    ((y.2 nfv.9 rbp)
                                                    (nfv.9 nfv.8 rbp)
                                                    (nfv.9 nfv.8 r15 rbp)
                                                    (nfv.9 nfv.8 r15 rbp)))
                                                    (z.3 tmp-ra.7 rbp)
                                                    (tmp-ra.7 rax rbp)
                                                    (rax rbp)))))
                                                '(conflicts
                                                ((y.2 (rbp tmp-ra.7 x.1 nfv.9))
                                                (x.1 (y.2 rbp tmp-ra.7 fv1))
                                                (tmp-ra.7 (y.2 x.1 rbp fv1 fv0 rax z.3))
                                                (z.3 (rbp tmp-ra.7))
                                                (nfv.9 (r15 nfv.8 rbp y.2))
                                                (nfv.8 (r15 rbp nfv.9))
                                                (rbp (y.2 x.1 tmp-ra.7 rax z.3 r15 nfv.8 nfv.9))
                                                (r15 (rbp nfv.8 nfv.9))
                                                (rax (rbp tmp-ra.7))
                                                (fv0 (tmp-ra.7))
                                                (fv1 (x.1 tmp-ra.7))))
                                                '(assignment ((nfv.9 fv4) (nfv.8 fv3) (tmp-ra.7 fv2))))
                                                (begin
                                                (set! tmp-ra.7 r15)
                                                (set! x.1 fv0)
                                                (set! y.2 fv1)
                                                (if (< y.2 x.1)
                                                    (begin (set! rax x.1) (jump tmp-ra.7 rbp rax))
                                                    (begin
                                                    (begin
                                                        (set! rbp (- rbp 24))
                                                        (return-point
                                                        L.rp.3
                                                        (begin
                                                        (set! nfv.9 x.1)
                                                        (set! nfv.8 y.2)
                                                        (set! r15 L.rp.3)
                                                        (jump L.swap.1 rbp r15 nfv.8 nfv.9)))
                                                        (set! rbp (+ rbp 24)))
                                                    (set! z.3 rax)
                                                    (set! rax z.3)
                                                    (jump tmp-ra.7 rbp rax)))))
                                            (begin
                                                (set! tmp-ra.10 r15)
                                                (set! fv1 2)
                                                (set! fv0 1)
                                                (set! r15 tmp-ra.10)
                                                (jump L.swap.1 rbp r15 fv0 fv1))))))