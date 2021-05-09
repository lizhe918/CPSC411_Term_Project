#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/2c-run-time
 cpsc411/test-suite/utils
 racket/match
 rackunit)

(require "../component/assign.rkt")

; input: asm-pred-lang-v6/conflicts
; output:  asm-pred-lang-v6/pre-framed
; purpose: Check the correctness of the assign-call-undead-variables

(let ([x `(module
            ((new-frames ()) (locals ()) (call-undead ()) (undead-out ()) (conflicts ()))
            (define L.addup.2
                ((new-frames ())
                (locals ())
                (undead-out
                (((fv3 fv0)
                    (fv3 fv1 fv0)
                    ((fv3 fv1 fv0) ((fv1 fv2 fv0) (fv2 fv1 fv0) (fv2 fv1 fv0)))
                    (fv3 fv1 fv0))
                (fv1 fv0)
                ((fv3) (fv3))))
                (call-undead (x.1 y.1 fv0))
                (conflicts
                ((x.1 ())
                (y.1 ())
                (fv0 ()))))
                (begin (set! fv0 (bitwise-and fv0 5)) (jump L.addup.1 fv3)))
            (jump L.addup.2))])
     (test-case "Mix fvar and aloc in call-undead set -- bitwise-and"
     (check-match (assign-call-undead-variables x)
                `(module
                (,_ ... (assignment ()) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment ,(list-no-order '(y.1 fv0) '(x.1 fv0))) ,_ ...)
                    (begin (set! fv0 (bitwise-and fv0 5)) (jump L.addup.1 fv3)))
                (jump L.addup.2)))))


(let ([x `(module
            ((new-frames ()) (locals ()) (call-undead ()) (undead-out ()) (conflicts ()))
            (define L.addup.2
                ((new-frames ())
                (locals ())
                (undead-out
                (((fv3 fv0)
                    (fv3 fv1 fv0)
                    ((fv3 fv1 fv0) ((fv1 fv2 fv0) (fv2 fv1 fv0) (fv2 fv1 fv0)))
                    (fv3 fv1 fv0))
                (fv1 fv0)
                ((fv3) (fv3))))
                (call-undead (x.1 y.1 fv0))
                (conflicts
                ((x.1 ())
                (y.1 ())
                (fv0 ()))))
                (begin (set! fv0 (bitwise-ior fv0 5)) (jump L.addup.1 fv3)))
            (jump L.addup.2))])
     (test-case "Mix fvar and aloc in call-undead set -- bitwise-ior"
     (check-match (assign-call-undead-variables x)
                `(module
                (,_ ... (assignment ()) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment ,(list-no-order '(y.1 fv0) '(x.1 fv0))) ,_ ...)
                    (begin (set! fv0 (bitwise-ior fv0 5)) (jump L.addup.1 fv3)))
                (jump L.addup.2)))))


(let ([x `(module
            ((new-frames ()) (locals ()) (call-undead ()) (undead-out ()) (conflicts ()))
            (define L.addup.2
                ((new-frames ())
                (locals ())
                (undead-out
                (((fv3 fv0)
                    (fv3 fv1 fv0)
                    ((fv3 fv1 fv0) ((fv1 fv2 fv0) (fv2 fv1 fv0) (fv2 fv1 fv0)))
                    (fv3 fv1 fv0))
                (fv1 fv0)
                ((fv3) (fv3))))
                (call-undead (x.1 y.1 fv0))
                (conflicts
                ((x.1 ())
                (y.1 ())
                (fv0 ()))))
                (begin (set! fv0 (bitwise-xor fv0 5)) (jump L.addup.1 fv3)))
            (jump L.addup.2))])
     (test-case "Mix fvar and aloc in call-undead set -- bitwise-xor"
     (check-match (assign-call-undead-variables x)
                `(module
                (,_ ... (assignment ()) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment ,(list-no-order '(y.1 fv0) '(x.1 fv0))) ,_ ...)
                    (begin (set! fv0 (bitwise-xor fv0 5)) (jump L.addup.1 fv3)))
                (jump L.addup.2)))))


(let ([x `(module
            ((new-frames ()) (locals ()) (call-undead ()) (undead-out ()) (conflicts ()))
            (define L.addup.2
                ((new-frames ())
                (locals ())
                (undead-out
                (((fv3 fv0)
                    (fv3 fv1 fv0)
                    ((fv3 fv1 fv0) ((fv1 fv2 fv0) (fv2 fv1 fv0) (fv2 fv1 fv0)))
                    (fv3 fv1 fv0))
                (fv1 fv0)
                ((fv3) (fv3))))
                (call-undead (x.1 y.1 fv0))
                (conflicts
                ((x.1 ())
                (y.1 ())
                (fv0 ()))))
                (begin (set! fv0 (arithmetic-shift-right fv0 5)) (jump L.addup.1 fv3)))
            (jump L.addup.2))])
     (test-case "Mix fvar and aloc in call-undead set -- arithmetic-shift-right"
     (check-match (assign-call-undead-variables x)
                `(module
                (,_ ... (assignment ()) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment ,(list-no-order '(y.1 fv0) '(x.1 fv0))) ,_ ...)
                    (begin (set! fv0 (arithmetic-shift-right fv0 5)) (jump L.addup.1 fv3)))
                (jump L.addup.2)))))
;; -------------------------------------------------------------------------

;; For simplicity, This test suite uses the single jump statement as body.

(let ([x `(module
            ((new-frames ())
            (locals (m.1 q.1))
            (call-undead ())
            (undead-out ((q.1) (m.1 q.1) (m.1 q.1)))
            (conflicts ((q.1 (m.1)) (m.1 (q.1)))))
            (define L.addup.2
                ((new-frames ())
                (locals (x.1 x.2))
                (undead-out ((x.1) (x.2 x.1) (x.1)))
                (call-undead ())
                (conflicts ((x.2 (x.1)) (x.1 (x.2)))))
                (begin (set! x.1 1) (set! x.2 L.addup.1) (jump x.2 x.1)))
            (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1)))])
     (test-case "Empty call-undead set"
     (check-match (assign-call-undead-variables x)
                `(module
                (,_ ... (assignment ()) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment ()) ,_ ...)
                    (begin (set! x.1 1) (set! x.2 L.addup.1) (jump x.2 x.1)))
                (begin (set! q.1 1) (set! m.1 2) (jump L.addup.2 q.1 m.1))))))



(let ([x `(module
            ((new-frames ()) (locals ()) (call-undead ()) (undead-out ()) (conflicts ()))
            (define L.addup.2
                ((new-frames ())
                (locals (x.3 y.1 z.1 x.1))
                (undead-out
                ((x.1) ((y.1 x.1) (z.1 x.1) (z.1 x.1)) ((x.1) ((x.3 z.1) (z.1))) (x.1)))
                (call-undead (x.1))
                (conflicts ((x.1 (x.3 y.1 z.1)) (z.1 (x.1)) (y.1 (x.1)) (x.3 (x.1)))))
                (jump L.addup.2 x.1))
            (jump L.addup.2))])
     (test-case "Assign one variable in call-undead set"
     (check-match (assign-call-undead-variables x)
                `(module
                (,_ ... (assignment ()) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment ((x.1 fv0))) ,_ ...)
                    (jump L.addup.2 x.1))
                (jump L.addup.2)))))




(let ([x `(module
            ((new-frames ()) (locals ()) (call-undead ()) (undead-out ()) (conflicts ()))
            (define L.addup.2
                ((new-frames ())
                (locals ())
                (undead-out
                (((fv3 fv0)
                    (fv3 fv1 fv0)
                    ((fv3 fv1 fv0) ((fv1 fv2 fv0) (fv2 fv1 fv0) (fv2 fv1 fv0)))
                    (fv3 fv1 fv0))
                (fv1 fv0)
                ((fv3) (fv3))))
                (call-undead (fv3 fv1 fv0))
                (conflicts
                ((fv0 (fv2 fv1 fv3))
                (fv3 (fv2 fv1 fv0))
                (fv1 (fv2 fv0 fv3))
                (fv2 (fv0 fv1 fv3)))))
                (jump L.addup.1 fv3))
            (jump L.addup.2))])
     (test-case "Assign fvars in call-undead set"
     (check-match (assign-call-undead-variables x)
                `(module
                (,_ ... (assignment ()) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment ()) ,_ ...)
                    (jump L.addup.1 fv3))
                (jump L.addup.2)))))



(let ([x `(module
            ((new-frames ()) (locals ()) (call-undead ()) (undead-out ()) (conflicts ()))
            (define L.addup.2
                ((new-frames ())
                (locals ())
                (undead-out
                (((fv3 fv0)
                    (fv3 fv1 fv0)
                    ((fv3 fv1 fv0) ((fv1 fv2 fv0) (fv2 fv1 fv0) (fv2 fv1 fv0)))
                    (fv3 fv1 fv0))
                (fv1 fv0)
                ((fv3) (fv3))))
                (call-undead (x.1 y.1 fv0))
                (conflicts
                ((x.1 ())
                (y.1 ())
                (fv0 ()))))
                (jump L.addup.1 fv3))
            (jump L.addup.2))])
     (test-case "Mix fvar and aloc in call-undead set"
     (check-match (assign-call-undead-variables x)
                `(module
                (,_ ... (assignment ()) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment ,(list-no-order '(y.1 fv0) '(x.1 fv0))) ,_ ...)
                    (jump L.addup.1 fv3))
                (jump L.addup.2)))))


(let ([x `(module
            ((new-frames ()) (locals ()) (call-undead ()) (undead-out ()) (conflicts ()))
            (define L.addup.2
                ((new-frames ())
                (locals ())
                (undead-out
                (((fv3 fv0)
                    (fv3 fv1 fv0)
                    ((fv3 fv1 fv0) ((fv1 fv2 fv0) (fv2 fv1 fv0) (fv2 fv1 fv0)))
                    (fv3 fv1 fv0))
                (fv1 fv0)
                ((fv3) (fv3))))
                (call-undead (x.1 y.1 rbx))
                (conflicts
                ((x.1 ())
                (y.1 ())
                (rbx ()))))
                (jump L.addup.1 fv3))
            (jump L.addup.2))])
     (test-case "Mix reg and aloc in call-undead set"
     (check-match (assign-call-undead-variables x)
                `(module
                (,_ ... (assignment ()) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment ,(list-no-order '(y.1 fv0) '(x.1 fv0))) ,_ ...)
                    (jump L.addup.1 fv3))
                (jump L.addup.2)))))



(let ([x `(module
            ((new-frames ()) (locals ()) (call-undead ()) (undead-out ()) (conflicts ()))
            (define L.addup.2
                ((new-frames ())
                (locals ())
                (undead-out
                (((fv3 fv0)
                    (fv3 fv1 fv0)
                    ((fv3 fv1 fv0) ((fv1 fv2 fv0) (fv2 fv1 fv0) (fv2 fv1 fv0)))
                    (fv3 fv1 fv0))
                (fv1 fv0)
                ((fv3) (fv3))))
                (call-undead (x.1 y.1 fv0))
                (conflicts
                ((x.1 (fv0))
                (y.1 ())
                (fv0 (x.1)))))
                (jump L.addup.1 fv3))
            (jump L.addup.2))])
     (test-case "Mix fvar and aloc in call-undead set with direct incompatibility to fvar"
     (check-match (assign-call-undead-variables x)
                `(module
                (,_ ... (assignment ()) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment ,(list-no-order '(y.1 fv0) '(x.1 fv1))) ,_ ...)
                    (jump L.addup.1 fv3))
                (jump L.addup.2)))))



(let ([x `(module
            ((new-frames ()) (locals ()) (call-undead ()) (undead-out ()) (conflicts ()))
            (define L.addup.2
                ((new-frames ())
                (locals ())
                (undead-out
                (((fv3 fv0)
                    (fv3 fv1 fv0)
                    ((fv3 fv1 fv0) ((fv1 fv2 fv0) (fv2 fv1 fv0) (fv2 fv1 fv0)))
                    (fv3 fv1 fv0))
                (fv1 fv0)
                ((fv3) (fv3))))
                (call-undead (x.1 y.1 fv0))
                (conflicts
                ((x.1 (y.1))
                (y.1 (x.1))
                (fv0 ()))))
                (jump L.addup.1 fv3))
            (jump L.addup.2))])
     (test-case "Mix fvar and aloc in call-undead set with indirect incompatibility "
     (check-match (assign-call-undead-variables x)
                `(module
                (,_ ... (assignment ()) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment ,(list-no-order '(y.1 fv0) '(x.1 fv1))) ,_ ...)
                    (jump L.addup.1 fv3))
                (jump L.addup.2)))))


(let ([x `(module
            ((new-frames ()) (locals ()) (call-undead ()) (undead-out ()) (conflicts ()))
            (define L.addup.2
                ((new-frames ())
                (locals ())
                (undead-out
                (((fv3 fv0)
                    (fv3 fv1 fv0)
                    ((fv3 fv1 fv0) ((fv1 fv2 fv0) (fv2 fv1 fv0) (fv2 fv1 fv0)))
                    (fv3 fv1 fv0))
                (fv1 fv0)
                ((fv3) (fv3))))
                (call-undead (x.1 y.1 rbx))
                (conflicts
                ((x.1 (rbx))
                (y.1 (rbx))
                (rbx (x.1 y.1)))))
                (jump L.addup.1 fv3))
            (jump L.addup.2))])
     (test-case "Mix reg and aloc in call-undead set with incompatibility with reg"
     (check-match (assign-call-undead-variables x)
                `(module
                (,_ ... (assignment ()) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment ,(list-no-order '(y.1 fv0) '(x.1 fv0))) ,_ ...)
                    (jump L.addup.1 fv3))
                (jump L.addup.2)))))




(let ([x `(module
            ((new-frames ())
            (locals (q.1))
            (call-undead ())
            (undead-out ((q.1) ()))
            (conflicts ((q.1 ()))))
            (define L.addup.2
                ((new-frames ())
                (locals (w.1 w.2 q.1 x.1 q.2))
                (undead-out
                ((()
                    ((q.1) (q.2 q.1) (q.2 x.1 q.1) ((x.1 q.1) ()) ())
                    ((q.2) (w.1 q.2) (q.2) (w.2 q.2) ((q.2) ()) ()))
                ()
                ((x.1) ())))
                (call-undead (q.2 x.1 q.1))
                (conflicts
                ((q.2 (x.1 q.1 w.2))
                (x.1 (q.1 q.2))
                (q.1 (x.1 q.2))
                (w.2 (q.2))
                (w.1 ()))))
                (jump L.addup.1 fv3))
            (jump L.addup.2))])
     (test-case "Assign multiple variables in call-undead set"
     (check-match (assign-call-undead-variables x)
                `(module
                (,_ ... (assignment ()) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment ,(list-no-order '(q.1 fv0) '(x.1 fv1) '(q.2 fv2))) ,_ ...)
                    (jump L.addup.1 fv3))
                (jump L.addup.2)))))


;; error
(let ([x `(module
            ((new-frames ()) (locals ()) (call-undead ()) (undead-out ()) (conflicts ()))
            (define L.addup.2
                ((new-frames ())
                (locals (q.3 y.1 x.1 x.2 x.3 z.1 q.1))
                (undead-out
                ((q.1)
                (q.1 z.1)
                (q.1 z.1 x.1)
                (x.3 q.1 z.1 x.1)
                (x.3 y.1 q.1 z.1 x.1)
                ((((x.3 y.1 q.1 z.1 x.1)
                    ((x.2 y.1 q.1 z.1 x.1)
                    (x.2 y.1 q.1 z.1 x.1 x.3)
                    ((x.2 y.1 q.1 z.1 x.1 x.3) (x.2))
                    (y.1 q.1 z.1 x.1 x.3))
                    (y.1 q.1 z.1 x.1 x.3))
                    ((y.1 q.1 z.1 x.1 x.3) (q.1 z.1 x.1 x.3) (q.1 z.1 x.1 x.3))
                    ((y.1 q.1 z.1 x.1 x.3) (q.1 z.1 x.1 x.3) (q.1 z.1 x.1 x.3)))
                    ()
                    ((q.3 x.3 x.1 z.1 q.1) (x.3 x.1 z.1 q.1)))))
                (call-undead (x.2 y.1 q.1 z.1 x.1 x.3))
                (conflicts
                ((q.1 (y.1 x.1 x.3 x.2 q.3))
                (z.1 (y.1 x.1 x.3 x.2 q.3))
                (x.3 (x.1 z.1 q.1 y.1 x.2 q.3))
                (x.2 (x.3 x.1 z.1 q.1 y.1))
                (x.1 (y.1 z.1 q.1 x.3 x.2 q.3))
                (y.1 (x.1 z.1 q.1 x.3 x.2))
                (q.3 (q.1 z.1 x.1 x.3)))))
                (jump L.addup.1 fv3))
            (jump L.addup.2))])
     (test-case "Assign multiple variables in call-undead set, reuse frame"
     (check-match (assign-call-undead-variables x)
                `(module
                (,_ ... (assignment ()) ,_ ...)
                (define L.addup.2
                    (,_ ... (assignment ,(list-no-order '(x.3 fv0) '(x.1 fv1) '(z.1 fv2) '(q.1 fv2) '(y.1 fv3) '(x.2 fv4))) ,_ ...)
                    (jump L.addup.1 fv3))
                (jump L.addup.2)))))