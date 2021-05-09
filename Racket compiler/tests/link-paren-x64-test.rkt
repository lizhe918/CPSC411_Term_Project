#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/2c-run-time
 rackunit)

(require "../component/link.rkt")



;; case-1
(test-case "Nothing to link"
     (check-equal?
      (link-paren-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (set! (rbp - 8) 8)
          (set! rax (* rax (rbp - 0)))))
      `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (set! (rbp - 8) 8)
          (set! rax (* rax (rbp - 0))))))


;; case-1
(test-case "Single label case"
     (check-equal?
      (link-paren-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (* rax (rbp - 0)))))
      `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (set! (rbp - 8) 8)
          (set! rax (* rax (rbp - 0))))))


;; case-1
(test-case "Single label case with jump label"
     (check-equal?
      (link-paren-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (jump L.test.1)
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (* rax (rbp - 0)))))
      `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (jump 3)
          (set! (rbp - 8) 8)
          (set! rax (* rax (rbp - 0))))))

;; case-1
(test-case "Single label case with jump register"
     (check-equal?
      (link-paren-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (set! rcx L.test.1)
          (jump rcx)
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (* rax (rbp - 0)))))
      `(begin
        (set! (rbp - 16) 2)
        (set! rax (rbp - 8))
        (set! rcx 4)
        (jump rcx)
        (set! (rbp - 8) 8)
        (set! rax (* rax (rbp - 0))))))

;; case-1
(test-case "Single label case with jump-if label"
     (check-equal?
      (link-paren-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (jump-if < L.test.1)
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (* rax (rbp - 0)))))
      `(begin
        (set! (rbp - 16) 2)
        (set! rax (rbp - 8))
        (jump-if < 3)
        (set! (rbp - 8) 8)
        (set! rax (* rax (rbp - 0))))))

;; case-1
(test-case "Single label case with nested label"
     (check-equal?
      (link-paren-x64
       `(begin
               (set! rbx 4)
               (compare rbx 4)
               (jump-if = L.testing.2)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.testing.1 (with-label L.testing.2 (with-label L.testing.3 (set! rbx 1))))
               (with-label L.end.1 (set! rax rbx))))
      `(begin
            (set! rbx 4)
            (compare rbx 4)
            (jump-if = 5)
            (set! rbx -66)
            (jump 6)
            (set! rbx 1)
            (set! rax rbx))))


;; General cases

;; case-1
(test-case "Simple case without jump"
     (check-equal?
      (link-paren-x64
       `(begin
               (set! rdi 15)
               (set! rcx 2)
               (set! rcx (* rcx rdi))
               (set! rax 10)
               (set! rax (+ rax rcx))))
      `(begin
            (set! rdi 15)
            (set! rcx 2)
            (set! rcx (* rcx rdi))
            (set! rax 10)
            (set! rax (+ rax rcx)))))


(test-case "Jump using register"
     (check-equal?
      (link-paren-x64
       `(begin
               (set! rcx L.case.2)
               (set! rbx 4)
               (jump rcx)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.case.2 (set! rbx 11))
               (with-label L.end.1 (set! rax rbx))
               (set! rax rbx)))
      `(begin
                (set! rcx 5)
                (set! rbx 4)
                (jump rcx)
                (set! rbx -66)
                (jump 6)
                (set! rbx 11)
                (set! rax rbx)
                (set! rax rbx))))

(test-case "Jump using label"
     (check-equal?
      (link-paren-x64
        `(begin
               (set! rbx 4)
               (jump L.case.2)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.case.2 (set! rbx 11))
               (with-label L.end.1 (set! rax rbx))
               (set! rax rbx)))
      `(begin
                (set! rbx 4)
                (jump 4)
                (set! rbx -66)
                (jump 5)
                (set! rbx 11)
                (set! rax rbx)
                (set! rax rbx))))            

(test-case "Direct compare with int32"
     (check-equal?
      (link-paren-x64
        `(begin
               (set! rbx 4)
               (set! rcx L.testing.1)
               (compare rbx 3)
               (jump-if > L.testing.1)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.testing.1 (set! rbx 23))
               (with-label L.end.1 (set! rax rbx))))
      `(begin
                (set! rbx 4)
                (set! rcx 6)
                (compare rbx 3)
                (jump-if > 6)
                (set! rbx -66)
                (jump 7)
                (set! rbx 23)
                (set! rax rbx))))    

(test-case "Compare using register" 
     (check-equal?
      (link-paren-x64
        `(begin
               (set! rbx 4)
               (set! rcx 5)
               (compare rcx rbx)
               (jump-if < L.testing.1)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.testing.1 (set! rbx 11))
               (with-label L.end.1 (set! rax rbx))))
      `(begin
                (set! rbx 4)
                (set! rcx 5)
                (compare rcx rbx)
                (jump-if < 6)
                (set! rbx -66)
                (jump 7)
                (set! rbx 11)
                (set! rax rbx))))    


(test-case "Jump to the end"
     (check-equal?
      (link-paren-x64
        `(begin
               (set! rbx 4)
               (compare rbx 4)
               (jump L.end.1)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.testing.1 (set! rbx 11))
               (with-label L.end.1 (set! rax rbx))))
      `(begin
                (set! rbx 4)
                (compare rbx 4)
                (jump 6)
                (set! rbx -66)
                (jump 6)
                (set! rbx 11)
                (set! rax rbx))))  

(test-case "Equal conditional jump"
     (check-equal?
      (link-paren-x64
       `(begin
               (set! rbx 4)
               (compare rbx 4)
               (jump-if = L.testing.1)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.end.1 (set! rbx 11))
               (with-label L.testing.1 (set! rbx 55))
               (set! rax rbx)))
      `(begin
                (set! rbx 4)
                (compare rbx 4)
                (jump-if = 6)
                (set! rbx -66)
                (jump 5)
                (set! rbx 11)
                (set! rbx 55)
                (set! rax rbx))))  

(test-case "Nested jump-1"
     (check-equal?
      (link-paren-x64
       `(begin
               (set! rbx 4)
               (compare rbx 4)
               (jump-if = L.testing.1)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.testing.1 (with-label L.testing.2 (with-label L.testing.3 (set! rbx 1))))
               (with-label L.end.1 (set! rax rbx))))
      `(begin
                (set! rbx 4)
                (compare rbx 4)
                (jump-if = 5)
                (set! rbx -66)
                (jump 6)
                (set! rbx 1)
                (set! rax rbx))))  

(test-case "Nested jump-2"
     (check-equal?
      (link-paren-x64
       `(begin
               (set! rbx 4)
               (compare rbx 4)
               (jump-if = L.testing.2)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.testing.1 (with-label L.testing.2 (with-label L.testing.3 (set! rbx 1))))
               (with-label L.end.1 (set! rax rbx))))
      `(begin
                (set! rbx 4)
                (compare rbx 4)
                (jump-if = 5)
                (set! rbx -66)
                (jump 6)
                (set! rbx 1)
                (set! rax rbx))))  

(test-case "Nested jump-3"
     (check-equal?
      (link-paren-x64
       `(begin
               (set! rbx 4)
               (compare rbx 4)
               (jump-if = L.testing.3)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.testing.1 (with-label L.testing.2 (with-label L.testing.3 (set! rbx 1))))
               (with-label L.end.1 (set! rax rbx))))
      `(begin
                (set! rbx 4)
                (compare rbx 4)
                (jump-if = 5)
                (set! rbx -66)
                (jump 6)
                (set! rbx 1)
                (set! rax rbx))))  


(test-case "Complex case for iteration"
     (check-equal?
      (link-paren-x64
      `(begin
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
            (set! rax rdi)))
      `(begin
            (set! r11 20)
            (set! r9 0)
            (set! r10 1)
            (set! r12 r11)
            (compare r12 0)
            (jump-if = 11)
            (set! r13 r10)
            (set! r10 (+ r10 r9))
            (set! r9 r13)
            (set! r12 (+ r12 -1))
            (jump 4)
            (set! rdi r10)
            (set! rax rdi))))  

(test-case "Complex case for iteration"
     (check-equal?
      (link-paren-x64
      `(begin
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
                (compare rdx 0))))
      `(begin
            (set! r9 100)
            (set! rax r9)
            (set! rdx 8)
            (compare rdx 0)
            (jump-if < 8)
            (set! rax (+ rax -2))
            (set! rdx (+ rdx -1))
            (jump 3)
            (compare rdx 0))))  

(test-case "General case"
     (check-equal?
      (link-paren-x64
       `(begin
                 (set! rcx L.case.2)
                 (set! rsi 20)
                 (set! rax 2)              
                 (with-label L.case.1 (set! rax (* rax rax)))
                 (set! rax (+ rax -1))
                 (with-label L.case.2 (compare rax rsi))
                 (jump-if < L.case.1)))
      `(begin
                (set! rcx 5)
                (set! rsi 20)
                (set! rax 2)
                (set! rax (* rax rax))
                (set! rax (+ rax -1))
                (compare rax rsi)
                (jump-if < 3)))) 

(test-case "General case"
     (check-equal?
      (link-paren-x64
       `(begin
                 (set! rcx L.case.2)
                 (set! rsi 120)
                 (set! rax 2)
                 (set! rbx 1)              
                 (with-label L.case.1 (set! rax (* rax 2)))
                 (set! rbx (+ rbx 1))
                 (with-label L.case.2 (compare rax rsi))
                 (jump-if < L.case.1)
                 (set! rax rbx)))
      `(begin
                (set! rcx 6)
                (set! rsi 120)
                (set! rax 2)
                (set! rbx 1)
                (set! rax (* rax 2))
                (set! rbx (+ rbx 1))
                (compare rax rsi)
                (jump-if < 4)
                (set! rax rbx)))) 

(test-case "General case"
     (check-equal?
      (link-paren-x64
        `(begin
                 (set! rcx L.case.2)
                 (set! rsi 120)
                 (set! rax 2)
                 (set! rbx 1)              
                 (with-label L.case.1 (set! rax (* rax rax)))
                 (with-label L.case.2 (compare rax rsi))
                 (jump-if < L.case.1)))
      `(begin
                (set! rcx 5)
                (set! rsi 120)
                (set! rax 2)
                (set! rbx 1)
                (set! rax (* rax rax))
                (compare rax rsi)
                (jump-if < 4)))) 

(test-case "General case"
     (check-equal?
      (link-paren-x64
        `(begin
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
               (jump-if != L.testing.1)))
      `(begin
                (set! rbx 4)
                (set! rcx 100)
                (set! rdi 1)
                (set! rax 0)
                (compare rbx 20)
                (jump-if != 7)
                (set! rbx -66)
                (set! rbx (+ rbx 1))
                (set! rax (+ rax 1))
                (compare rbx 20)
                (jump-if != 7))))

(test-case "General case"
     (check-equal?
      (link-paren-x64
        `(begin
          (set! (rbp - 16) -1)
          (set! r10 0)
          (set! rax 10)
          (with-label L.x.1 (set! rax (+ rax (rbp - 16))))
          (set! r10 (+ r10 1))
          (compare rax r10)
          (jump-if > L.x.1)))
      `(begin
        (set! (rbp - 16) -1)
        (set! r10 0)
        (set! rax 10)
        (set! rax (+ rax (rbp - 16)))
        (set! r10 (+ r10 1))
        (compare rax r10)
        (jump-if > 3))))

(test-case "General case"
     (check-equal?
      (link-paren-x64
        `(begin
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
            (set! rax rbx)))
      `(begin
            (set! rsi 5)
            (set! rbx 10)
            (set! rbx (+ rbx 10))
            (set! rbx (+ rbx 2))
            (set! rbx (+ rbx -15))
            (set! rbx (+ rbx 1))
            (set! rax rbx))))

(test-case "General case"
     (check-equal?
      (link-paren-x64
        `(begin
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
            (set! rax rdx))))
      `(begin
            (set! rdx 42)
            (compare rdx 43)
            (jump-if > 4)
            (jump 6)
            (set! rdx 30)
            (jump 8)
            (set! rdx 0)
            (jump 8)
            (set! rax rdx))))

(test-case "General case"
     (check-equal?
      (link-paren-x64
        `(begin
            (with-label L.tmp.1
            (set! rdx 42))
            (set! rdx 20)
            (set! rdx (+ rdx rdx))
            (jump L.tmp.2)
            (with-label L.tmp.2
            (set! rax rdx))))
      `(begin
            (set! rdx 42)
            (set! rdx 20)
            (set! rdx (+ rdx rdx))
            (jump 4)
            (set! rax rdx))))