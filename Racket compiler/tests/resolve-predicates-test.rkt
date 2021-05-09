#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/2c-run-time
 racket/match
 rackunit)

(require "../component/resolve.rkt")


(test-case
    "No predicate"
    (let ([x  '(module
          (define L.x.0
            (begin
              (set! rsi 123)
              (set! rsi (+ rsi 1))
              (jump L.x.2)))
          (define L.x.1
            (begin
              (set! rax 2)
              (set! rbx 10)
              (jump L.x.2)))
          (define L.x.2
            (halt rsi)))])

      (check-equal?
       (resolve-predicates x)
       `(module
          (define L.x.0
            (begin
              (set! rsi 123)
              (set! rsi (+ rsi 1))
              (jump L.x.2)))
          (define L.x.1
            (begin
              (set! rax 2)
              (set! rbx 10)
              (jump L.x.2)))
          (define L.x.2
            (halt rsi))))))


(test-case
    "No predicate"
    (let ([x '(module
          (define L.x.0
            (begin
              (set! rsi 123)
              (set! rsi (+ rsi 1))
              (if (< rsi 100) (jump L.x.2) (jump L.x.1))))
          (define L.x.1
            (begin
              (set! rax 2)
              (set! rbx 10)
              (set! fv1 5)
              (if (>= fv1 6) (jump L.x.2) (jump L.x.1))))
          (define L.x.2
            (halt rsi)))])

      (check-equal?
       (resolve-predicates x)
       `(module
          (define L.x.0
            (begin
              (set! rsi 123)
              (set! rsi (+ rsi 1))
              (if (< rsi 100) (jump L.x.2) (jump L.x.1))))
          (define L.x.1
            (begin
              (set! rax 2)
              (set! rbx 10)
              (set! fv1 5)
              (if (>= fv1 6) (jump L.x.2) (jump L.x.1))))
          (define L.x.2
            (halt rsi))))))

(test-case
    "True predicate"
    (let ([x '(module
                (define L.main.1
                  (begin (set! rbx 1)
                         (set! rcx 2)
                         (if (true)
                             (jump L.t.1)
                             (jump L.t.2))))
                (define L.t.1
                  (begin (set! rdx 4)
                         (halt rdx)))
                (define L.t.2
                  (begin (set! rdx 8)
                         (halt rdx))))])

      (check-equal?
       (resolve-predicates x)
       `(module
        (define L.main.1 (begin (set! rbx 1) (set! rcx 2) (jump L.t.1)))
        (define L.t.1 (begin (set! rdx 4) (halt rdx)))
        (define L.t.2 (begin (set! rdx 8) (halt rdx)))))))

(test-case
    "False predicate"
    (let ([x '(module
                (define L.main.1
                  (begin (set! rbx 1)
                         (set! rcx 2)
                         (if (false)
                             (jump L.t.1)
                             (jump L.t.2))))
                (define L.t.1
                  (begin (set! rdx 4)
                         (halt rdx)))
                (define L.t.2
                  (begin (set! rdx 8)
                         (halt rdx))))])

      (check-equal?
       (resolve-predicates x)
       `(module
        (define L.main.1 (begin (set! rbx 1) (set! rcx 2) (jump L.t.2)))
        (define L.t.1 (begin (set! rdx 4) (halt rdx)))
        (define L.t.2 (begin (set! rdx 8) (halt rdx)))))))

(test-case
    "relop predicate"
    (let ([x '(module
                (define L.main.1
                  (begin (set! rbx 1)
                         (set! rcx 2)
                         (if (>= rbx rcx)
                             (jump L.t.1)
                             (jump L.t.2))))
                (define L.t.1
                  (begin (set! rdx 4)
                         (halt rdx)))
                (define L.t.2
                  (begin (set! rdx 8)
                         (halt rdx))))])

      (check-equal?
       (resolve-predicates x)
       `(module
        (define L.main.1
          (begin
            (set! rbx 1)
            (set! rcx 2)
            (if (>= rbx rcx) (jump L.t.1) (jump L.t.2))))
        (define L.t.1 (begin (set! rdx 4) (halt rdx)))
        (define L.t.2 (begin (set! rdx 8) (halt rdx)))))))


(test-case
    "NOT True predicate"
    (let ([x '(module
                (define L.main.1
                  (begin (set! rbx 1)
                         (set! rcx 2)
                         (if (not (true))
                             (jump L.t.1)
                             (jump L.t.2))))
                (define L.t.1
                  (begin (set! rdx 4)
                         (halt rdx)))
                (define L.t.2
                  (begin (set! rdx 8)
                         (halt rdx))))])

      (check-equal?
       (resolve-predicates x)
       `(module
        (define L.main.1 (begin (set! rbx 1) (set! rcx 2) (jump L.t.2)))
        (define L.t.1 (begin (set! rdx 4) (halt rdx)))
        (define L.t.2 (begin (set! rdx 8) (halt rdx)))))))
  
  (test-case
    "NOT False predicate"
    (let ([x '(module
                (define L.main.1
                  (begin (set! rbx 1)
                         (set! rcx 2)
                         (if (not (false))
                             (jump L.t.1)
                             (jump L.t.2))))
                (define L.t.1
                  (begin (set! rdx 4)
                         (halt rdx)))
                (define L.t.2
                  (begin (set! rdx 8)
                         (halt rdx))))])

      (check-equal?
       (resolve-predicates x)
       `(module
        (define L.main.1 (begin (set! rbx 1) (set! rcx 2) (jump L.t.1)))
        (define L.t.1 (begin (set! rdx 4) (halt rdx)))
        (define L.t.2 (begin (set! rdx 8) (halt rdx)))))))

(test-case
    "NOT relops predicate"
    (let ([x '(module
                (define L.main.1
                  (begin (set! rbx 1)
                         (set! rcx 2)
                         (if (not (< rbx rcx))
                             (jump L.t.1)
                             (jump L.t.2))))
                (define L.t.1
                  (begin (set! rdx 4)
                         (halt rdx)))
                (define L.t.2
                  (begin (set! rdx 8)
                         (halt rdx))))])

      (check-equal?
       (resolve-predicates x)
       `(module
        (define L.main.1
          (begin
            (set! rbx 1)
            (set! rcx 2)
            (if (< rbx rcx) (jump L.t.2) (jump L.t.1))))
        (define L.t.1 (begin (set! rdx 4) (halt rdx)))
        (define L.t.2 (begin (set! rdx 8) (halt rdx)))))))

;; not sure with this case
(test-case
    "Nested NOT predicate"
    (let ([x '(module
                (define L.main.1
                  (begin (set! rbx 1)
                         (set! rcx 2)
                         (if (not (not (not (not (< rbx rcx)))))
                             (jump L.t.1)
                             (jump L.t.2))))
                (define L.t.1
                  (begin (set! rdx 4)
                         (halt rdx)))
                (define L.t.2
                  (begin (set! rdx 8)
                         (halt rdx))))])

      (check-equal?
       (resolve-predicates x)
       `(module
          (define L.main.1
            (begin (set! rbx 1)
                   (set! rcx 2)
                   (jump L.t.2)))
          (define L.t.1
            (begin (set! rdx 4)
                   (halt rdx)))
          (define L.t.2
            (begin (set! rdx 8)
                   (halt rdx)))))))

;; newly added
(test-case
    "Nested NOT predicate"
    (let ([x '(module
                (define L.main.1
                  (begin (set! rbx 1)
                         (set! rcx 2)
                         (if (not (not (not (true))))
                             (jump L.t.1)
                             (jump L.t.2))))
                (define L.t.1
                  (begin (set! rdx 4)
                         (halt rdx)))
                (define L.t.2
                  (begin (set! rdx 8)
                         (halt rdx))))])

      (check-equal?
       (resolve-predicates x)
       `(module
        (define L.main.1 (begin (set! rbx 1) (set! rcx 2) (jump L.t.2)))
        (define L.t.1 (begin (set! rdx 4) (halt rdx)))
        (define L.t.2 (begin (set! rdx 8) (halt rdx)))))))

(test-case
    "begin with predicate"
    (let ([x '(module
                (define L.main.1
                  (begin  (if (false)
                             (jump L.t.1)
                             (jump L.t.2))))
                (define L.t.1
                  (begin (set! rdx 4)
                         (halt rdx)))
                (define L.t.2
                  (begin (set! rdx 8)
                         (halt rdx))))])

      (check-equal?
       (resolve-predicates x)
       `(module
        (define L.main.1 (begin (jump L.t.2)))
        (define L.t.1 (begin (set! rdx 4) (halt rdx)))
        (define L.t.2 (begin (set! rdx 8) (halt rdx)))))))