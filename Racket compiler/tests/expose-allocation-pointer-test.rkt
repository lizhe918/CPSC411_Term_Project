#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/ptr-run-time
 racket/match
 rackunit)

(require "../component/expose-allo.rkt")


; input: asm-alloc-lang-v8
; output: asm-pred-lang-v8
; purpose: Check the outputs of expose-allocation-pointer match the output of interrogator

(test-case
    "Support (set! aloc (alloc int32))"
     (let ([x '(module
                 ((new-frames ()))
                 (begin
                 (set! x.1 (alloc 32))
                 (jump L.done.1)))])
       (check-match
        (expose-allocation-pointer x)
        `(module
            ((new-frames ()))
            (begin (begin (set! x.1 ,r12) (set! ,r12 (+ ,r12 32))) (jump L.done.1))))))

(test-case
    "Support (set! aloc (alloc aloc))"
     (let ([x '(module
                 ((new-frames ()))
                 (begin
                 (set! y.1 8)
                 (set! x.1 (alloc y.1))
                 (jump L.done.1)))])
       (check-match
        (expose-allocation-pointer x)
        `(module
        ((new-frames ()))
        (begin
            (set! y.1 8)
            (begin (set! x.1 ,r12) (set! ,r12 (+ ,r12 y.1)))
            (jump L.done.1))))))

(test-case
    "Support (set! reg (alloc int32))"
     (let ([x '(module
                 ((new-frames ()))
                 (begin
                 (set! rax (alloc 32))
                 (jump L.done.1)))])
       (check-match
        (expose-allocation-pointer x)
        `(module
        ((new-frames ()))
        (begin (begin (set! rax ,r12) (set! ,r12 (+ ,r12 32))) (jump L.done.1))))))

(test-case
    "Support (set! reg (alloc aloc))"
     (let ([x '(module
                 ((new-frames ()))
                 (begin
                 (set! y.1 32)
                 (set! rax (alloc y.1))
                 (jump L.done.1)))])
       (check-match
        (expose-allocation-pointer x)
        `(module
            ((new-frames ()))
            (begin
                (set! y.1 32)
                (begin (set! rax ,r12) (set! ,r12 (+ ,r12 y.1)))
                (jump L.done.1))))))

(test-case
    "Support (set! fv (alloc int32))"
     (let ([x '(module
                 ((new-frames ()))
                 (begin
                 (set! fv1 (alloc 32))
                 (jump L.done.1)))])
       (check-match
        (expose-allocation-pointer x)
        `(module
        ((new-frames ()))
        (begin (begin (set! fv1 ,r12) (set! ,r12 (+ ,r12 32))) (jump L.done.1))))))


(test-case
    "Support (set! fv (alloc aloc))"
     (let ([x '(module
                 ((new-frames ()))
                 (begin
                 (set! y.1 32)
                 (set! fv2 (alloc y.1))
                 (jump L.done.1)))])
       (check-match
        (expose-allocation-pointer x)
        `(module
            ((new-frames ()))
            (begin
                (set! y.1 32)
                (begin (set! fv2 ,r12) (set! ,r12 (+ ,r12 y.1)))
                (jump L.done.1))))))

(test-case
    "Support alloc in pred"
     (let ([x '(module
                 ((new-frames ()))
                 (if (begin (set! y.1 32) (set! fv2 (alloc y.1)) (< y.1 32))
                    (jump L.done.1)
                    (jump L.end.1)))])
       (check-match
        (expose-allocation-pointer x)
        `(module
        ((new-frames ()))
        (if (begin
                (set! y.1 32)
                (begin (set! fv2 ,r12) (set! ,r12 (+ ,r12 y.1)))
                (< y.1 32))
            (jump L.done.1)
            (jump L.end.1))))))

(test-case
    "Support alloc in if begin"
     (let ([x '(module
                 ((new-frames ()))
                    (define L.test.1
                        ((new-frames ()))
                        (jump L.done.1))
                 (if (begin (set! y.1 32) (set! fv2 (alloc y.1)) (< y.1 32))
                    (begin
                        (set! y.1 8)
                        (set! x.1 (alloc y.1))
                        (jump L.done.1))
                    (begin
                        (set! rax (alloc 32))
                        (jump L.done.1))))])
       (check-match
        (expose-allocation-pointer x)
        `(module
        ((new-frames ()))
        (define L.test.1 ((new-frames ())) (jump L.done.1))
        (if (begin
                (set! y.1 32)
                (begin (set! fv2 ,r12) (set! ,r12 (+ ,r12 y.1)))
                (< y.1 32))
            (begin
            (set! y.1 8)
            (begin (set! x.1 ,r12) (set! ,r12 (+ ,r12 y.1)))
            (jump L.done.1))
            (begin (begin (set! rax ,r12) (set! ,r12 (+ ,r12 32))) (jump L.done.1)))))))

;; todo unsupported cases
;not supported in interrogator now, write them manually
(test-case
    "Support alloc in begin if"
     (let ([x '(module
                 ((new-frames ()))
                 (begin
                    (set! y.1 8)
                    (if (true)
                        (set! x.1 (alloc y.1))
                        (set! x.1 32))
                    (set! rax (mref rax 32))
                    (jump L.done.1)))])
       (check-match
        (expose-allocation-pointer x)
        `(module
                 ((new-frames ()))
                 (begin
                    (set! y.1 8)
                    (if (true)
                        (begin (set! x.1 ,r12) (set! ,r12 (+ ,r12 y.1))) 
                        (set! x.1 32))
                    (set! rax (mref rax 32))
                    (jump L.done.1))))))

;; not supported
(test-case
    "Support alloc in nested begin"
     (let ([x '(module
                ((new-frames ()))
                    (define L.test.1
                        ((new-frames ()))
                        (jump L.done.1))
                (begin
                    (begin
                        (set! y.1 8)
                        (begin
                            (set! x.1 1)
                            (begin (set! x.1 (alloc 32)) 
                                   (set! y.1 (alloc y.1))))
                        (jump L.done.1))))])
       (check-match
        (expose-allocation-pointer x)
        `(module
                ((new-frames ()))
                    (define L.test.1
                        ((new-frames ()))
                        (jump L.done.1))
                (begin
                    (begin
                        (set! y.1 8)
                        (begin
                            (set! x.1 1)
                            (begin (begin (set! x.1 ,r12) (set! ,r12 (+ ,r12 32))) 
                                   (begin (set! y.1 ,r12) (set! ,r12 (+ ,r12 y.1)))))
                        (jump L.done.1)))))))

;; ok
(test-case
    "Support alloc in nested if"
     (let ([x '(module
                ((new-frames ()))
                (if (if (true) (false) (begin (set! x.1 (alloc 32)) (< x.1 5)))
                    (if (true)
                        (begin 
                            (set! fv0 (alloc 32))
                            (jump rcx))
                        (begin
                            (set! rax (alloc x.1))
                            (jump fv1)))
                    (jump y.1)))])
       (check-match
        (expose-allocation-pointer x)
        `(module
        ((new-frames ()))
        (if (if (true)
                (false)
                (begin (begin (set! x.1 ,r12) (set! ,r12 (+ ,r12 32))) (< x.1 5)))
            (if (true)
            (begin (begin (set! fv0 ,r12) (set! ,r12 (+ ,r12 32))) (jump rcx))
            (begin (begin (set! rax ,r12) (set! ,r12 (+ ,r12 x.1))) (jump fv1)))
            (jump y.1))))))


;; ok
(test-case
    "Support alloc in return point tail begin "
     (let ([x '(module
                ((new-frames ()))
                (define L.test.1
                    ((new-frames ()))
                    (jump L.done.1))
                (begin
                    (return-point L.rp.1 (begin (set! x.1 (alloc 32)) (jump L.test.1)))
                    (jump L.end.1)))])
       (check-match
        (expose-allocation-pointer x)
        `(module
        ((new-frames ()))
        (define L.test.1 ((new-frames ())) (jump L.done.1))
        (begin
            (return-point
            L.rp.1
            (begin (begin (set! x.1 ,r12) (set! ,r12 (+ ,r12 32))) (jump L.test.1)))
            (jump L.end.1))))))

;; ok
(test-case
    "Support alloc in return point tail if"
     (let ([x '(module
                ((new-frames ()))
                (define L.test.1
                    ((new-frames ()))
                    (jump L.done.1))
                (begin
                    (return-point L.rp.1 (if (true) 
                                             (begin (set! x.1 (alloc 32)) (jump L.test.1)) 
                                             (jump L.test.1)))
                    (jump L.end.1)))])
       (check-match
        (expose-allocation-pointer x)
        `(module
        ((new-frames ()))
        (define L.test.1 ((new-frames ())) (jump L.done.1))
        (begin
            (return-point
            L.rp.1
            (if (true)
            (begin (begin (set! x.1 ,r12) (set! ,r12 (+ ,r12 32))) (jump L.test.1))
            (jump L.test.1)))
            (jump L.end.1))))))

