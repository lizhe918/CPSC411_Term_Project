#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/ptr-run-time
 racket/match
 rackunit)

(require "../component/flatten.rkt")

(test-case "Simple case with nested begin -- bitwise-and"
     (check-equal?
      (flatten-program
       '(module
          (define L.x.0
            (begin
              (set! rsi 123)
              (begin (set! rsi (bitwise-and rsi 1))
              (jump L.x.1))))
          (define L.x.1
            (halt rsi))))
      '(begin
        (with-label L.x.0 (set! rsi 123))
        (set! rsi (bitwise-and rsi 1))
        (jump L.x.1)
        (with-label L.x.1 (halt rsi)))))


(test-case "Simple case with jump -- bitwise-ior"
     (check-equal?
      (flatten-program
       '(module
          (define L.x.0
            (begin
              (set! rsi 123)
              (set! rsi (bitwise-ior rsi 1))
              (jump L.x.2)))
          (define L.x.1
            (begin
              (set! rax 2)
              (set! rbx 10)
              (jump L.x.2)))
          (define L.x.2
            (halt rsi))))
      '(begin
        (with-label L.x.0 (set! rsi 123))
        (set! rsi (bitwise-ior rsi 1))
        (jump L.x.2)
        (with-label L.x.1 (set! rax 2))
        (set! rbx 10)
        (jump L.x.2)
        (with-label L.x.2 (halt rsi)))))

(test-case "Simple case with jump-if -- bitwise-xor"
     (check-equal?
      (flatten-program
       '(module
          (define L.x.0
            (begin
              (set! rsi 123)
              (set! rsi (bitwise-xor rsi 1))
              (if (< rsi 100) (jump L.x.2) (jump L.x.1))))
          (define L.x.1
            (begin
              (set! rax 2)
              (set! rbx 10)
              (set! fv1 5)
              (if (>= fv1 6) (jump L.x.2) (jump L.x.1))))
          (define L.x.2
            (halt rsi))))
      '(begin
      (with-label L.x.0 (set! rsi 123))
      (set! rsi (bitwise-xor rsi 1))
      (compare rsi 100)
      (jump-if < L.x.2)
      (jump L.x.1)
      (with-label L.x.1 (set! rax 2))
      (set! rbx 10)
      (set! fv1 5)
      (compare fv1 6)
      (jump-if >= L.x.2)
      (jump L.x.1)
      (with-label L.x.2 (halt rsi)))))


(test-case "Simple case with jump-if -- arithmetic-shift-right"
     (check-equal?
      (flatten-program
       '(module
          (define L.x.0
            (begin
              (set! rsi 123)
              (set! rsi (arithmetic-shift-right rsi 1))
              (if (< rsi rbx) (jump rsi) (jump L.x.1))))
          (define L.x.1
            (begin
              (set! rax 2)
              (set! rbx 10)
              (set! fv1 5)
              (set! fv2 6)
              (if (>= fv1 fv2) (jump fv2) (jump fv1))))
          (define L.x.2
            (halt rsi))))
      '(begin
        (with-label L.x.0 (set! rsi 123))
        (set! rsi (arithmetic-shift-right rsi 1))
        (compare rsi rbx)
        (jump-if < rsi)
        (jump L.x.1)
        (with-label L.x.1 (set! rax 2))
        (set! rbx 10)
        (set! fv1 5)
        (set! fv2 6)
        (compare fv1 fv2)
        (jump-if >= fv2)
        (jump fv1)
        (with-label L.x.2 (halt rsi)))))

;; ---------------------------------------------------------------------------

(test-case "Simple case with jump-if"
     (check-equal?
      (flatten-program
       '(module
          (define L.x.0
            (begin
              (set! rsi 123)
              (set! rsi (+ rsi 1))
              (if (< rsi rbx) (jump L.x.2) (jump L.x.1))))
          (define L.x.1
            (begin
              (set! rax 2)
              (set! rbx 10)
              (set! fv1 5)
              (set! fv2 6)
              (if (>= fv1 fv2) (jump L.x.2) (jump L.x.1))))
          (define L.x.2
            (halt rsi))))
      '(begin
      (with-label L.x.0 (set! rsi 123))
      (set! rsi (+ rsi 1))
      (compare rsi rbx)
      (jump-if < L.x.2)
      (jump L.x.1)
      (with-label L.x.1 (set! rax 2))
      (set! rbx 10)
      (set! fv1 5)
      (set! fv2 6)
      (compare fv1 fv2)
      (jump-if >= L.x.2)
      (jump L.x.1)
      (with-label L.x.2 (halt rsi)))))

(test-case "Simple case with begin halt"
     (check-equal?
      (flatten-program
       '(module
          (define L.x.0
            (begin
              (set! rsi 123)
              (set! rsi (+ rsi 1))
              (jump L.x.1)))
          (define L.x.1
            (begin (halt rsi)))))
      '(begin
      (with-label L.x.0 (set! rsi 123))
      (set! rsi (+ rsi 1))
      (jump L.x.1)
      (with-label L.x.1 (halt rsi)))))

(test-case "Simple case with begin jump-if"
     (check-equal?
      (flatten-program
       '(module
          (define L.x.0
            (begin
              (set! rsi 123)
              (set! rsi (+ rsi 1))
              (begin (jump fv1))))
          (define L.x.1
            (halt rsi))))
      '(begin
      (with-label L.x.0 (set! rsi 123))
      (set! rsi (+ rsi 1))
      (jump fv1)
      (with-label L.x.1 (halt rsi)))))

(test-case "Simple case with begin jump"
     (check-equal?
      (flatten-program
       '(module
          (define L.x.0
            (begin
              (set! rsi L.m.1)
              (set! rsi (+ rsi rax))
              (begin (jump L.x.1))))
          (define L.x.1
            (halt rsi))))
      '(begin
        (with-label L.x.0 (set! rsi L.m.1))
        (set! rsi (+ rsi rax))
        (jump L.x.1)
        (with-label L.x.1 (halt rsi)))))

(test-case "Complex case1"
     (check-equal?
      (flatten-program
       `(module
          (define
            L.tmp.1
            (begin
              (set! rdx 42)
              (if (> rdx 43) (jump rdx) (jump L.tmp.3))))
          (define
            L.tmp.2
            (begin
              (set! rdx 30)
              (jump L.tmp.4)))
          (define L.x.0
            (if (!= fv1 10) (jump L.tmp.3) (jump L.x.0)))
          (define
            L.tmp.3
            (begin
              (set! rdx 0)
              (jump L.tmp.2)))
          (define
            L.tmp.4
            (halt rdx))))
      `(begin
      (with-label L.tmp.1 (set! rdx 42))
      (compare rdx 43)
      (jump-if > rdx)
      (jump L.tmp.3)
      (with-label L.tmp.2 (set! rdx 30))
      (jump L.tmp.4)
      (with-label L.x.0 (compare fv1 10))
      (jump-if != L.tmp.3)
      (jump L.x.0)
      (with-label L.tmp.3 (set! rdx 0))
      (jump L.tmp.2)
      (with-label L.tmp.4 (halt rdx)))))

(test-case "Complex case2"
     (check-equal?
      (flatten-program
       `(module
          (define L.tmp.1
            (begin
              (set! rdx 42)
              (begin
                (set! rdx fv2)
                (begin
                  (set! rdx (+ rdx rdx))
                  (jump L.tmp.0)))))
          (define L.tmp.2
            (begin
              (halt rdx)))
          (define L.x.0
            (halt rbx))
          (define L.x.1
            (if (> rax rbx) (jump L.tmp.2) (jump fv1)))))
      `(begin
      (with-label L.tmp.1 (set! rdx 42))
      (set! rdx fv2)
      (set! rdx (+ rdx rdx))
      (jump L.tmp.0)
      (with-label L.tmp.2 (halt rdx))
      (with-label L.x.0 (halt rbx))
      (with-label L.x.1 (compare rax rbx))
      (jump-if > L.tmp.2)
      (jump fv1))))