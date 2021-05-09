#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/2c-run-time
 rackunit)

(require "../component/interp.rkt")

;; Haven't check with interrogator
;; case-1
(let ([e `(begin
               (set! rdi 15)
               (set! rcx 2)
               (set! rcx (* rcx rdi))
               (set! rax 10)
               (set! rax (+ rax rcx)))])
     (test-case "Simple case without jump"
       (check-equal? (interp-paren-x64 e) 40)))

;; case--2
(let ([e `(begin
               (set! rcx L.case.2)
               (set! rbx 4)
               (jump rcx)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.case.2 (set! rbx 11))
               (with-label L.end.1 (set! rax rbx))
               (set! rax rbx))])
     (test-case "Jump using register"
       (check-equal? (interp-paren-x64 e) 11)))

;; case-3
(let ([e `(begin
               (set! rbx 4)
               (jump L.case.2)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.case.2 (set! rbx 11))
               (with-label L.end.1 (set! rax rbx))
               (set! rax rbx))])
     (test-case "Jump using label"
       (check-equal? (interp-paren-x64 e) 11)))

;; case-3
(let ([e `(begin
               (set! rbx 4)
               (set! rcx L.testing.1)
               (compare rbx 3)
               (jump-if > L.testing.1)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.testing.1 (set! rbx 23))
               (with-label L.end.1 (set! rax rbx)))])
     (test-case "Direct compare with int32"
       (check-equal? (interp-paren-x64 e) 23)))

;; case-4
(let ([e `(begin
               (set! rbx 4)
               (set! rcx 5)
               (compare rcx rbx)
               (jump-if < L.testing.1)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.testing.1 (set! rbx 11))
               (with-label L.end.1 (set! rax rbx)))])
     (test-case "Compare using register" 
       (check-equal? (interp-paren-x64 e) -66)))

;; case-5
(let ([e `(begin
               (set! rbx 4)
               (compare rbx 4)
               (jump L.end.1)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.testing.1 (set! rbx 11))
               (with-label L.end.1 (set! rax rbx)))])
     (test-case "Jump to the end"
       (check-equal? (interp-paren-x64 e) 4)))

;; case-6
(let ([e `(begin
               (set! rbx 4)
               (compare rbx 4)
               (jump-if = L.testing.1)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.end.1 (set! rbx 11))
               (with-label L.testing.1 (set! rbx 55))
               (set! rax rbx))])
     (test-case "Equal conditional jump"
       (check-equal? (interp-paren-x64 e) 55)))

;; case-7
(let ([e `(begin
               (set! rbx 4)
               (compare rbx 4)
               (jump-if = L.testing.1)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.testing.1 (with-label L.testing.2 (with-label L.testing.3 (set! rbx 1))))
               (with-label L.end.1 (set! rax rbx)))])
     (test-case "Nested jump-1"
       (check-equal? (interp-paren-x64 e) 1)))

;; case-8
(let ([e `(begin
               (set! rbx 4)
               (compare rbx 4)
               (jump-if = L.testing.2)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.testing.1 (with-label L.testing.2 (with-label L.testing.3 (set! rbx 1))))
               (with-label L.end.1 (set! rax rbx)))])
     (test-case "Nested jump-2"
       (check-equal? (interp-paren-x64 e) 1)))

;; case-9
(let ([e `(begin
               (set! rbx 4)
               (compare rbx 4)
               (jump-if = L.testing.3)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.testing.1 (with-label L.testing.2 (with-label L.testing.3 (set! rbx 1))))
               (with-label L.end.1 (set! rax rbx)))])
     (test-case "Nested jump-3"
       (check-equal? (interp-paren-x64 e) 1)))

;; case-10
(let ([e `(begin
               (set! rbx 4)
               (compare rbx 4)
               (jump-if <= L.testing.1)
               (set! rbx -66)
               (jump L.testing.2)
               (with-label L.testing.1 (compare rbx 3))
               (jump-if <= L.testing.2)
               (set! rbx -10)
               (with-label L.testing.2 (set! rbx 20))
               (set! rax rbx))])
     (test-case "Jump-if case-1 <="
       (check-equal? (interp-paren-x64 e) 20)))

;; case-11
(let ([e `(begin
               (set! rbx 4)
               (compare rbx 4)
               (jump-if >= L.testing.1)
               (set! rbx -66)
               (jump L.testing.2)
               (with-label L.testing.1 (compare rbx 5))
               (jump-if <= L.testing.2)
               (set! rbx -10)
               (with-label L.testing.2 (set! rbx 200))
               (set! rax rbx))])
     (test-case "Jump-if case-2 >="
       (check-equal? (interp-paren-x64 e) 200)))

;; case-12
(let ([e `(begin
               (set! rbx 4)
               (set! rcx 100)
               (set! rdi 1)
               (compare rbx 4)
               (jump-if != L.testing.1)
               (set! rbx -66)
               (with-label L.testing.1 (set! rbx 11))
               (compare rbx rdi)
               (jump-if != L.end.2)
               (set! rbx 11)
               (with-label L.end.2 (set! rbx 22))
               (set! rbx 50)
               (compare rbx rcx)
               (jump-if != L.end.3)
               (with-label L.end.3 (set! rbx 23))
               (set! rax rbx))])
     (test-case "Jump-if case-3 !="
       (check-equal? (interp-paren-x64 e) 23)))



;; General case x10

;; case-13
(test-case "Moderate single label case with jump"
     (check-equal?
      (interp-paren-x64
       `(begin
          (set! (rbp - 16) 1)
          (set! r10 10)
          (set! rax 2)
          (with-label L.x.1 (set! rax (+ rax (rbp - 16))))
          (compare rax r10)
          (jump-if > L.x.1)))
      3))


;; Produce infinite loop in my (Harry's) interp-x64
;; case-14
;; todo
(test-case "Moderate single label case with jump"
    (check-equal?
      (interp-paren-x64
      `(begin
          (set! (rbp - 8) 6)
          (set! (rbp - 16) 5)
          (set! r10 (rbp - 8))
          (set! rax 0)
          (with-label L.x.1 (with-label L.y.1 (set! rax (+ rax (rbp - 16)))))
          (compare rax r10)
          (jump-if = L.y.1)))
      5))


;; case-15
(test-case "Moderate single label case with jump"
     (check-equal?
      (interp-paren-x64
       '(begin
               (set! (rbp - 0) 21)
               (set! rax 0)
               (set! rsp 1)
               (with-label L.test.1 (set! (rbp - 8) 2))
               (compare rax rsp)
               (jump-if = L.test.1)
               (set! rax (rbp - 0))
               (set! rax (* rax (rbp - 8)))))
      42))

;; case-16
(test-case "Moderate single label case with jump"
     (check-equal?
      (interp-paren-x64
       '(begin
               (set! rdx 4)
               (set! rcx L.testing.4)
               (with-label L.testing.1 (jump L.testing.3))
               (with-label L.testing.2 (jump L.testing.1))
               (with-label L.testing.3 (jump rcx))
               (with-label L.testing.4 (set! rax rdx))))
      4))

;; case-17
(test-case "Moderate single label case with jump"
     (check-equal?
      (interp-paren-x64
       '(begin
               (set! rdx 4)
               (with-label L.x.1 (set! rdx (+ rdx -111)))
               (with-label L.x.2 (set! rdx (+ rdx rdx)))
               (set! rax rdx)))
      -214))

;; case-18
(let ([e '(begin
               (with-label L.cs411main.1
                 (set! r11 20))
               (with-label L.fib.1
                 (set! r9 0))
               (set! r10 1)
               (set! r12 r11)
               (with-label L.fib_iter.1
                 (compare r12 0))
               (jump-if = L.fib_done.1)
               (with-label L.fib_not_done_yet.1
                 (set! r13 r10))
               (set! r10 (+ r10 r9))
               (set! r9 r13)
               (set! r12 (+ r12 -1))
               (jump L.fib_iter.1)
               (with-label L.fib_done.1
                 (set! rdi r10))
               (set! rax rdi))])
     (test-case "Complex case for iteration"
       (check-equal? (interp-paren-x64 e) 10946)))

;; case-19
(let ([e '(begin
               (set! r9 100)
               (set! rax r9)
               (set! rdx 8)
               (with-label L.fib_iter.1
                 (compare rdx 0))
               (jump-if < L.testing.4)
               (set! rax (+ rax -2))
               (set! rdx (+ rdx -1))
               (jump L.fib_iter.1)
               (with-label L.testing.4
                 (compare rdx 0)))])
     (test-case "Complex case for iteration"
       (check-equal? (interp-paren-x64 e) 82)))

;; case-20
;; case-21
(let ([e '(begin
               (set! r9 100)
               (set! rax r9)
               (set! rdx 8)
               (set! rcx 5)
               (with-label L.fib_iter.1
                 (compare rdx 0))
               (jump-if < L.testing.4)
               (set! rdx (+ rdx -1))
               (with-label L.testing.1
                 (compare rcx 0))
               (jump-if < L.fib_iter.1)
               (set! rcx (+ rcx -1))
               (set! rax (+ rax -5))
               (jump L.testing.1)
               (set! rax (+ rax -2))
               (jump L.fib_iter.1)
               (with-label L.testing.4
                 (compare rdx 0)))])
     (test-case "Complex case for double iterations"
       (check-equal? (interp-paren-x64 e) 70)))

;; case-22
(let ([e `(begin
                 (with-label L.main.51 (set! r14 1))
                 (set! r15 5)
                 (with-label L.fact_loop.50 (compare r15 0))
                 (jump-if = L.nested.54)
                 (set! r13 r15)
                 (set! r15 (+ r15 -1))
                 (set! r14 (* r14 r13))
                 (jump L.fact_loop.50)
                 (with-label L.nested.54 (set! rax r14)))])
     (test-case "Complex case for double iterations"
       (check-equal? (interp-paren-x64 e) 120)))

;; new cases -- unchecked -- change to the file 
(let ([e `(begin
                 (set! rdx 9)
                 (set! rcx 18)
                 (set! rsi L.label.1)
                 (compare rdx rcx)
                 (jump-if < L.label.1)
                 (with-label L.label.1
                   (set! rdx 27))
                 (set! rax rdx))])
     (test-case "Complex case for double iterations"
       (check-equal? (interp-paren-x64 e) 27)))

(let ([e `(begin
                 (set! rcx L.case.2)
                 (set! rsi 2)
                 (set! rbx 18)
                 (jump L.case.1)
                 (with-label L.case.2 (set! rbx (+ rbx 1)))
                 (with-label L.case.1 (set! rax rbx))
                 (set! rax (+ rax rsi))
                 (set! rax (+ rax 3))
                 (compare rax 50)
                 (jump-if < L.case.2)
                 (set! rax rax))])
     (test-case ""
       (check-equal? (interp-paren-x64 e) 50)))

;; need to be changed
(let ([e `(begin
                 (set! rcx L.case.2)
                 (set! rsi 20)
                 (set! rax 2)              
                 (with-label L.case.1 (set! rax (* rax rax)))
                 (set! rax (+ rax -1))
                 (with-label L.case.2 (compare rax rsi))
                 (jump-if < L.case.1))])
     (test-case "new case"
       (check-equal? (interp-paren-x64 e) 63)))
  


(let ([e `(begin
                 (set! rcx L.case.2)
                 (set! rsi 120)
                 (set! rax 2)
                 (set! rbx 1)              
                 (with-label L.case.1 (set! rax (* rax 2)))
                 (set! rbx (+ rbx 1))
                 (with-label L.case.2 (compare rax rsi))
                 (jump-if < L.case.1)
                 (set! rax rbx))])
     (test-case "2^7 > 120"
       (check-equal? (interp-paren-x64 e) 7)))


(let ([e `(begin
                 (set! rcx L.case.2)
                 (set! rsi 120)
                 (set! rax 2)
                 (set! rbx 1)              
                 (with-label L.case.1 (set! rax (* rax rax)))
                 (with-label L.case.2 (compare rax rsi))
                 (jump-if < L.case.1))])
     (test-case "new case"
       (check-equal? (interp-paren-x64 e) 256)))

(let ([e `(begin
               (set! rbx 4)
               (set! rcx 100)
               (set! rdi 1)
               (set! rax 0)
               (compare rbx 20)
               (jump-if != L.testing.1)
               (set! rbx -66)
               (with-label L.testing.1 (set! rbx (+ rbx 1)))
               (set! rax (+ rax 1))
               (compare rbx 20)
               (jump-if != L.testing.1))])
     (test-case "new case"
       (check-equal? (interp-paren-x64 e) 16)))

(let ([e `(begin
          (set! (rbp - 16) -1)
          (set! r10 0)
          (set! rax 10)
          (with-label L.x.1 (set! rax (+ rax (rbp - 16))))
          (set! r10 (+ r10 1))
          (compare rax r10)
          (jump-if > L.x.1))])
     (test-case "new case"
       (check-equal? (interp-paren-x64 e) 5)))

(let ([e '(begin
                 (set! rsi L.label.1)
                 (with-label L.label.1
                   (set! rbx 10))
                 (with-label L.label.1
                   (set! rbx (+ rbx 10)))
                 (with-label L.label.1
                   (set! rbx (+ rbx 2)))
                 (with-label L.label.1
                   (set! rbx (+ rbx -15)))
                 (with-label L.label.1
                   (set! rbx (+ rbx 1)))
                 (set! rax rbx))])
     (test-case "Case without jump"
       (check-equal? (interp-paren-x64 e) 8)))

(let ([e `(begin
         (with-label L.tmp.1
           (set! rdx 42))
         (compare rdx 43)
         (jump-if > L.tmp.2)
         (jump L.tmp.3)
         (with-label L.tmp.2
           (set! rdx 30))
         (jump L.tmp.4)
         (with-label L.tmp.3
           (set! rdx 0))
         (jump L.tmp.4)
         (with-label L.tmp.4
           (set! rax rdx)))])
     (test-case "Multiple jumps"
       (check-equal? (interp-paren-x64 e) 0)))

(let ([e `(begin
         (with-label L.tmp.1
           (set! rdx 42))
         (set! rdx 20)
         (set! rdx (+ rdx rdx))
         (jump L.tmp.2)
         (with-label L.tmp.2
           (set! rax rdx)))])
     (test-case "new case"
       (check-equal? (interp-paren-x64 e) 40)))