#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/ptr-run-time
 rackunit)

(require "../component/generate.rkt")


; input: paren-x64-v8
; output: x64-instructions
; purpose: Check the output of generate-x64 is same to the output of interrogator


(test-case "Support addr as (set! (reg + int32) int32)"
     (check-equal?
      (generate-x64
       `(begin
          (set! rax 5)
          (set! (rax + 3) 16)
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (- rax (rbp - 0)))))
        "mov rax, 5\nmov QWORD [rax + 3], 16\nL.test.1:\nmov QWORD [rbp - 8], 8\nsub rax, QWORD [rbp - 0]\n"))

(test-case "Support addr as (set! (reg + int32) reg)"
     (check-equal?
      (generate-x64
       `(begin
          (set! rax 16)
          (set! rbx (rbp - 0))
          (set! (rax + 8) rbx)
          (with-label L.test.1 (set! (rbp - 8) 8))))
        "mov rax, 16\nmov rbx, QWORD [rbp - 0]\nmov QWORD [rax + 8], rbx\nL.test.1:\nmov QWORD [rbp - 8], 8\n"))

(test-case "Support addr as (set! (reg + int32) label)"
     (check-equal?
      (generate-x64
       `(begin
          (set! rax 16)
          (set! rbx 6)
          (set! (rax + 8) L.test.1)
          (with-label L.test.1 (set! (rbp - 8) 8))))
        "mov rax, 16\nmov rbx, 6\nmov QWORD [rax + 8], L.test.1\nL.test.1:\nmov QWORD [rbp - 8], 8\n"))

(test-case "Support addr as (set! reg (reg + int32))"
     (check-equal?
      (generate-x64
       `(begin
          (set! rax 16)
          (set! rbx 6)
          (set! rbx (rax + 8))
          (with-label L.test.1 (set! (rbp - 8) 8))))
        "mov rax, 16\nmov rbx, 6\nmov rbx, QWORD [rax + 8]\nL.test.1:\nmov QWORD [rbp - 8], 8\n"))


(test-case "Support addr as (set! reg_1 (binop reg_1 (reg + int32)))"
     (check-equal?
      (generate-x64
       `(begin
          (set! rax 16)
          (set! rbx 6)
          (set! rbx (bitwise-and rbx (rax + 8)))
          (with-label L.test.1 (set! (rbp - 8) 8))))
        "mov rax, 16\nmov rbx, 6\nand rbx, QWORD [rax + 8]\nL.test.1:\nmov QWORD [rbp - 8], 8\n"))








(test-case "Support addr as (set! (reg + reg) int32)"
     (check-equal?
      (generate-x64
       `(begin
          (set! rax 8)
          (set! (rax + rax) 16)
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (bitwise-ior rax (rbp - 0)))))
        "mov rax, 8\nmov QWORD [rax + rax], 16\nL.test.1:\nmov QWORD [rbp - 8], 8\nor rax, QWORD [rbp - 0]\n"))

(test-case "Support addr as (set! (reg + reg) reg)"
     (check-equal?
      (generate-x64
       `(begin
          (set! rax 16)
          (set! rcx 16)
          (set! rbx (rbp - 0))
          (set! (rax + rcx) rbx)
          (with-label L.test.1 (set! (rbp - 8) 8))))
        "mov rax, 16\nmov rcx, 16\nmov rbx, QWORD [rbp - 0]\nmov QWORD [rax + rcx], rbx\nL.test.1:\nmov QWORD [rbp - 8], 8\n"))

(test-case "Support addr as (set! (reg + reg) label)"
     (check-equal?
      (generate-x64
       `(begin
          (set! rax 16)
          (set! rbx 0)
          (set! (rax + rbx) L.test.1)
          (with-label L.test.1 (set! (rbp - 8) 8))))
        "mov rax, 16\nmov rbx, 0\nmov QWORD [rax + rbx], L.test.1\nL.test.1:\nmov QWORD [rbp - 8], 8\n"))

(test-case "Support addr as (set! reg (reg + reg))"
     (check-equal?
      (generate-x64
       `(begin
          (set! rax 16)
          (set! rbx 6)
          (set! rbx (rax + rax))
          (with-label L.test.1 (set! (rbp - 8) 8))))
        "mov rax, 16\nmov rbx, 6\nmov rbx, QWORD [rax + rax]\nL.test.1:\nmov QWORD [rbp - 8], 8\n"))


(test-case "Support addr as (set! reg_1 (binop reg_1 (reg + reg)))"
     (check-equal?
      (generate-x64
       `(begin
          (set! rax 16)
          (set! rbx 6)
          (set! rbx (bitwise-xor rbx (rax + rax)))
          (with-label L.test.1 (set! (rbp - 8) 8))))
        "mov rax, 16\nmov rbx, 6\nxor rbx, QWORD [rax + rax]\nL.test.1:\nmov QWORD [rbp - 8], 8\n"))







;; ---------------------------------------- old cases --------------------------------------------
;; error undefined
(test-case "Support (set! addr int32)"
     (check-equal?
      (generate-x64
       `(begin
          (set! (rbp - 16) ,(max-int 32))
          (set! rax (rbp - 8))
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (- rax (rbp - 0)))))
        "mov QWORD [rbp - 16], 2147483647\nmov rax, QWORD [rbp - 8]\nL.test.1:\nmov QWORD [rbp - 8], 8\nsub rax, QWORD [rbp - 0]\n"))


(test-case "General case 1 support bitwise-and"
     (check-equal?
      (generate-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (bitwise-and rax (rbp - 0)))))
        "mov QWORD [rbp - 16], 2\nmov rax, QWORD [rbp - 8]\nL.test.1:\nmov QWORD [rbp - 8], 8\nand rax, QWORD [rbp - 0]\n"))


(test-case "General case 2 support bitwise-and"
     (check-equal?
      (generate-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (bitwise-and rax ,(max-int 32)))))
        "mov QWORD [rbp - 16], 2\nmov rax, QWORD [rbp - 8]\nL.test.1:\nmov QWORD [rbp - 8], 8\nand rax, 2147483647\n"))


(test-case "General case 3 support bitwise-and"
     (check-equal?
      (generate-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (bitwise-and rax rcx))))
        "mov QWORD [rbp - 16], 2\nmov rax, QWORD [rbp - 8]\nL.test.1:\nmov QWORD [rbp - 8], 8\nand rax, rcx\n"))


(test-case "General case 1 support bitwise-ior"
     (check-equal?
      (generate-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (bitwise-ior rax (rbp - 0)))))
        "mov QWORD [rbp - 16], 2\nmov rax, QWORD [rbp - 8]\nL.test.1:\nmov QWORD [rbp - 8], 8\nor rax, QWORD [rbp - 0]\n"))

(test-case "General case 2 support bitwise-ior"
     (check-equal?
      (generate-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (bitwise-ior rax ,(min-int 32)))))
        "mov QWORD [rbp - 16], 2\nmov rax, QWORD [rbp - 8]\nL.test.1:\nmov QWORD [rbp - 8], 8\nor rax, -2147483648\n"))

(test-case "General case 3 support bitwise-ior"
     (check-equal?
      (generate-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (bitwise-ior rax rcx))))
        "mov QWORD [rbp - 16], 2\nmov rax, QWORD [rbp - 8]\nL.test.1:\nmov QWORD [rbp - 8], 8\nor rax, rcx\n"))

(test-case "General case 1 support bitwise-xor"
     (check-equal?
      (generate-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (bitwise-xor rax (rbp - 0)))))
        "mov QWORD [rbp - 16], 2\nmov rax, QWORD [rbp - 8]\nL.test.1:\nmov QWORD [rbp - 8], 8\nxor rax, QWORD [rbp - 0]\n"))


(test-case "General case 2 support bitwise-xor"
     (check-equal?
      (generate-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (bitwise-xor rax ,(max-int 32)))))
        "mov QWORD [rbp - 16], 2\nmov rax, QWORD [rbp - 8]\nL.test.1:\nmov QWORD [rbp - 8], 8\nxor rax, 2147483647\n"))


(test-case "General case 3 support bitwise-xor"
     (check-equal?
      (generate-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (bitwise-xor rax rcx))))
        "mov QWORD [rbp - 16], 2\nmov rax, QWORD [rbp - 8]\nL.test.1:\nmov QWORD [rbp - 8], 8\nxor rax, rcx\n"))


(test-case "General case 1 support arithmetic-shift-right (sar reg, int)"
     (check-equal?
      (generate-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (arithmetic-shift-right rax 0))))
        "mov QWORD [rbp - 16], 2\nmov rax, QWORD [rbp - 8]\nL.test.1:\nmov QWORD [rbp - 8], 8\nsar rax, 0\n"))



;; ------------------------------------------- old cases ---------------------------------
(test-case "General case 1 support -"
     (check-equal?
      (generate-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (- rax (rbp - 0)))))
        "mov QWORD [rbp - 16], 2\nmov rax, QWORD [rbp - 8]\nL.test.1:\nmov QWORD [rbp - 8], 8\nsub rax, QWORD [rbp - 0]\n"))


(test-case "General case 2 support -"
     (check-equal?
      (generate-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (set! rcx (rbp - 16))
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (- rax rcx))))
        "mov QWORD [rbp - 16], 2\nmov rax, QWORD [rbp - 8]\nmov rcx, QWORD [rbp - 16]\nL.test.1:\nmov QWORD [rbp - 8], 8\nsub rax, rcx\n"))


(test-case "General case 3 support -"
     (check-equal?
      (generate-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (- rax 5))))
        "mov QWORD [rbp - 16], 2\nmov rax, QWORD [rbp - 8]\nL.test.1:\nmov QWORD [rbp - 8], 8\nsub rax, 5\n"))


(test-case "Nested case 4 support -"
     (check-equal?
      (generate-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (with-label L.test.1 (set! rax (- rax 5)))
          (set! rax (- rax (rbp - 0)))))
        "mov QWORD [rbp - 16], 2\nmov rax, QWORD [rbp - 8]\nL.test.1:\nsub rax, 5\nsub rax, QWORD [rbp - 0]\n"))


(test-case "Nested case 5 support -"
     (check-equal?
      (generate-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (set! rcx (rbp - 16))
          (with-label L.test.1 (set! rax (- rax rcx)))
          (set! rax (- rax (rbp - 0)))))
        "mov QWORD [rbp - 16], 2\nmov rax, QWORD [rbp - 8]\nmov rcx, QWORD [rbp - 16]\nL.test.1:\nsub rax, rcx\nsub rax, QWORD [rbp - 0]\n"))

(test-case "Nested case 6 support -"
     (check-equal?
      (generate-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (set! rcx (rbp - 16))
          (with-label L.test.1 (set! rax (- rax (rbp - 0))))
          (set! rax (- rax (rbp - 16)))))
        "mov QWORD [rbp - 16], 2\nmov rax, QWORD [rbp - 8]\nmov rcx, QWORD [rbp - 16]\nL.test.1:\nsub rax, QWORD [rbp - 0]\nsub rax, QWORD [rbp - 16]\n"))

(test-case "Nested case 7 support -"
     (check-equal?
      (generate-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (set! rcx (rbp - 16))
          (with-label L.test.1 (with-label L.test.2 (set! rax (- rax rcx))))
          (set! rax (- rax (rbp - 16)))))
        "mov QWORD [rbp - 16], 2\nmov rax, QWORD [rbp - 8]\nmov rcx, QWORD [rbp - 16]\nL.test.1:\nL.test.2:\nsub rax, rcx\nsub rax, QWORD [rbp - 16]\n"))

(test-case "Nested case 8 support -"
     (check-equal?
      (generate-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (set! rcx (rbp - 16))
          (with-label L.test.1 (with-label L.test.2 (set! rax (- rax (rbp - 0)))))
          (set! rax (- rax (rbp - 16)))))
        "mov QWORD [rbp - 16], 2\nmov rax, QWORD [rbp - 8]\nmov rcx, QWORD [rbp - 16]\nL.test.1:\nL.test.2:\nsub rax, QWORD [rbp - 0]\nsub rax, QWORD [rbp - 16]\n"))


(test-case "Add test"
     (check-equal?
      (generate-x64
       '(begin (with-label L.__main.182 (set! r15 r15)) (set! r14 L.id.6) (set! rdi 5) (set! r15 r15) (jump r14) (with-label L.id.6 (set! r14 rdi)) (set! r15 r15) (set! rax r14) (jump r15)))
        "L.__main.182:\nmov r15, r15\nmov r14, L.id.6\nmov rdi, 5\nmov r15, r15\njmp r14\nL.id.6:\nmov r14, rdi\nmov r15, r15\nmov rax, r14\njmp r15\n"))




;; ---------------------------------- old cases ------------------------------
(test-case "General case 1"
     (check-equal?
      (generate-x64
       `(begin
          (set! (rbp - 16) 2)
          (set! rax (rbp - 8))
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (* rax (rbp - 0)))))
        "mov QWORD [rbp - 16], 2\nmov rax, QWORD [rbp - 8]\nL.test.1:\nmov QWORD [rbp - 8], 8\nimul rax, QWORD [rbp - 0]\n"))

 
(test-case "General case 2"
     (check-equal?
      (generate-x64
      `(begin
               (set! rdi 15)
               (set! rcx 2)
               (set! rcx (* rcx rdi))
               (set! rax 10)
               (set! rax (+ rax rcx))))
        "mov rdi, 15\nmov rcx, 2\nimul rcx, rdi\nmov rax, 10\nadd rax, rcx\n"))      

(test-case "General case 3"
     (check-equal?
      (generate-x64
      `(begin
               (set! rcx L.case.2)
               (set! rbx 4)
               (jump rcx)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.case.2 (set! rbx 11))
               (with-label L.end.1 (set! rax rbx))
               (set! rax rbx)))
        "mov rcx, L.case.2\nmov rbx, 4\njmp rcx\nmov rbx, -66\njmp L.end.1\nL.case.2:\nmov rbx, 11\nL.end.1:\nmov rax, rbx\nmov rax, rbx\n"))   

(test-case "General case 4"
     (check-equal?
      (generate-x64
      `(begin
               (set! rbx 4)
               (jump L.case.2)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.case.2 (set! rbx 11))
               (with-label L.end.1 (set! rax rbx))
               (set! rax rbx)))
        "mov rbx, 4\njmp L.case.2\nmov rbx, -66\njmp L.end.1\nL.case.2:\nmov rbx, 11\nL.end.1:\nmov rax, rbx\nmov rax, rbx\n"))   

(test-case "General case 5"
     (check-equal?
      (generate-x64
        `(begin
               (set! rbx 4)
               (set! rcx L.testing.1)
               (compare rbx 3)
               (jump-if > L.testing.1)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.testing.1 (set! rbx 23))
               (with-label L.end.1 (set! rax rbx))))
        "mov rbx, 4\nmov rcx, L.testing.1\ncmp rbx, 3\njg L.testing.1\nmov rbx, -66\njmp L.end.1\nL.testing.1:\nmov rbx, 23\nL.end.1:\nmov rax, rbx\n"))   

(test-case "General case 6"
     (check-equal?
      (generate-x64
       `(begin
               (set! rbx 4)
               (set! rcx 5)
               (compare rcx rbx)
               (jump-if < L.testing.1)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.testing.1 (set! rbx 11))
               (with-label L.end.1 (set! rax rbx))))
        "mov rbx, 4\nmov rcx, 5\ncmp rcx, rbx\njl L.testing.1\nmov rbx, -66\njmp L.end.1\nL.testing.1:\nmov rbx, 11\nL.end.1:\nmov rax, rbx\n"))   

(test-case "General case 7"
     (check-equal?
      (generate-x64
       `(begin
               (set! rbx 4)
               (compare rbx 4)
               (jump L.end.1)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.testing.1 (set! rbx 11))
               (with-label L.end.1 (set! rax rbx))))
        "mov rbx, 4\ncmp rbx, 4\njmp L.end.1\nmov rbx, -66\njmp L.end.1\nL.testing.1:\nmov rbx, 11\nL.end.1:\nmov rax, rbx\n"))   

(test-case "General case 8"
     (check-equal?
      (generate-x64
      `(begin
               (set! rbx 4)
               (compare rbx 4)
               (jump-if = L.testing.1)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.end.1 (set! rbx 11))
               (with-label L.testing.1 (set! rbx 55))
               (set! rax rbx)))
        "mov rbx, 4\ncmp rbx, 4\nje L.testing.1\nmov rbx, -66\njmp L.end.1\nL.end.1:\nmov rbx, 11\nL.testing.1:\nmov rbx, 55\nmov rax, rbx\n"))   
   

(test-case "General case 9"
     (check-equal?
      (generate-x64
     `(begin
               (set! rbx 4)
               (compare rbx 4)
               (jump-if = L.testing.2)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.testing.1 (with-label L.testing.2 (with-label L.testing.3 (set! rbx 1))))
               (with-label L.end.1 (set! rax rbx))))
        "mov rbx, 4\ncmp rbx, 4\nje L.testing.2\nmov rbx, -66\njmp L.end.1\nL.testing.1:\nL.testing.2:\nL.testing.3:\nmov rbx, 1\nL.end.1:\nmov rax, rbx\n"))     
 
(test-case "General case 10"
     (check-equal?
      (generate-x64
     `(begin
               (set! rbx 4)
               (compare rbx 4)
               (jump-if = L.testing.3)
               (set! rbx -66)
               (jump L.end.1)
               (with-label L.testing.1 (with-label L.testing.2 (with-label L.testing.3 (set! rbx 1))))
               (with-label L.end.1 (set! rax rbx))))
        "mov rbx, 4\ncmp rbx, 4\nje L.testing.3\nmov rbx, -66\njmp L.end.1\nL.testing.1:\nL.testing.2:\nL.testing.3:\nmov rbx, 1\nL.end.1:\nmov rax, rbx\n"))


(test-case "General case 11"
     (check-equal?
      (generate-x64
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
        "L.cs411main.1:\nmov r11, 20\nL.fib.1:\nmov r9, 0\nmov r10, 1\nmov r12, r11\nL.fib_iter.1:\ncmp r12, 0\nje L.fib_done.1\nL.fib_not_done_yet.1:\nmov r13, r10\nadd r10, r9\nmov r9, r13\nadd r12, -1\njmp L.fib_iter.1\nL.fib_done.1:\nmov rdi, r10\nmov rax, rdi\n"))

 (test-case "General case 12"
     (check-equal?
      (generate-x64
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
        "mov r9, 100\nmov rax, r9\nmov rdx, 8\nL.fib_iter.1:\ncmp rdx, 0\njl L.testing.4\nadd rax, -2\nadd rdx, -1\njmp L.fib_iter.1\nL.testing.4:\ncmp rdx, 0\n"))       

(test-case "General case 13"
     (check-equal?
      (generate-x64
     `(begin
        (set! rcx L.case.2)
        (set! rsi 20)
        (set! rax 2)              
        (with-label L.case.1 (set! rax (* rax rax)))
        (set! rax (+ rax -1))
        (with-label L.case.2 (compare rax rsi))
        (jump-if < L.case.1)))
        "mov rcx, L.case.2\nmov rsi, 20\nmov rax, 2\nL.case.1:\nimul rax, rax\nadd rax, -1\nL.case.2:\ncmp rax, rsi\njl L.case.1\n")) 

(test-case "General case 14"
     (check-equal?
      (generate-x64
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
        "mov rcx, L.case.2\nmov rsi, 120\nmov rax, 2\nmov rbx, 1\nL.case.1:\nimul rax, 2\nadd rbx, 1\nL.case.2:\ncmp rax, rsi\njl L.case.1\nmov rax, rbx\n"))   

(test-case "General case 15"
     (check-equal?
      (generate-x64
      `(begin
                 (set! rcx L.case.2)
                 (set! rsi 120)
                 (set! rax 2)
                 (set! rbx 1)              
                 (with-label L.case.1 (set! rax (* rax rax)))
                 (with-label L.case.2 (compare rax rsi))
                 (jump-if < L.case.1)))
        "mov rcx, L.case.2\nmov rsi, 120\nmov rax, 2\nmov rbx, 1\nL.case.1:\nimul rax, rax\nL.case.2:\ncmp rax, rsi\njl L.case.1\n"))        

(test-case "General case 16"
     (check-equal?
      (generate-x64
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
        "mov rbx, 4\nmov rcx, 100\nmov rdi, 1\nmov rax, 0\ncmp rbx, 20\njne L.testing.1\nmov rbx, -66\nL.testing.1:\nadd rbx, 1\nadd rax, 1\ncmp rbx, 20\njne L.testing.1\n"))  

(test-case "General case 17"
     (check-equal?
      (generate-x64
     `(begin
          (set! (rbp - 16) -1)
          (set! r10 0)
          (set! rax 10)
          (with-label L.x.1 (set! rax (+ rax (rbp - 16))))
          (set! r10 (+ r10 1))
          (compare rax r10)
          (jump-if > L.x.1)))
        "mov QWORD [rbp - 16], -1\nmov r10, 0\nmov rax, 10\nL.x.1:\nadd rax, QWORD [rbp - 16]\nadd r10, 1\ncmp rax, r10\njg L.x.1\n"))        


(test-case "General case 18"
     (check-equal?
      (generate-x64
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
        "mov rsi, L.label.1\nL.label.1:\nmov rbx, 10\nL.label.1:\nadd rbx, 10\nL.label.1:\nadd rbx, 2\nL.label.1:\nadd rbx, -15\nL.label.1:\nadd rbx, 1\nmov rax, rbx\n"))    

(test-case "General case 19"
     (check-equal?
      (generate-x64
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
        "L.tmp.1:\nmov rdx, 42\ncmp rdx, 43\njg L.tmp.2\njmp L.tmp.3\nL.tmp.2:\nmov rdx, 30\njmp L.tmp.4\nL.tmp.3:\nmov rdx, 0\njmp L.tmp.4\nL.tmp.4:\nmov rax, rdx\n"))   

(test-case "General case 20"
     (check-equal?
      (generate-x64
       `(begin
            (with-label L.tmp.1
            (set! rdx 42))
            (set! rdx 20)
            (set! rdx (+ rdx rdx))
            (jump L.tmp.2)
            (with-label L.tmp.2
            (set! rax rdx))))
        "L.tmp.1:\nmov rdx, 42\nmov rdx, 20\nadd rdx, rdx\njmp L.tmp.2\nL.tmp.2:\nmov rax, rdx\n"))      