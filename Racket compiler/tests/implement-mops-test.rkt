#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/ptr-run-time
 rackunit)



; input: paren-x64-mops-v8
; output: paren-x64-v8
; purpose: Check the output of implement-mops is same to the output of interrogator

(require "../component/impl-mop.rkt")


(test-case "Support (set! reg_1 (mref reg_1 int32))"
     (check-equal?
      (implement-mops
       `(begin
          (set! rax 8)
          (set! rax (mref rax 8))
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (- rax (rbp - 0)))))
        `(begin
            (set! rax 8)
            (set! rax (rax + 8))
            (with-label L.test.1 (set! (rbp - 8) 8))
            (set! rax (- rax (rbp - 0))))))

(test-case "Support (set! reg_1 (mref reg_1 reg))"
     (check-equal?
      (implement-mops
       `(begin
          (set! rax 8)
          (set! rbx 8)
          (set! rax (mref rax rbx))
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (- rax (rbp - 0)))))
        `(begin
            (set! rax 8)
            (set! rbx 8)
            (set! rax (rax + rbx))
            (with-label L.test.1 (set! (rbp - 8) 8))
            (set! rax (- rax (rbp - 0))))))

(test-case "Support (mset! reg_1 int32 int64)"
     (check-equal?
      (implement-mops
       `(begin
          (set! rax 8)
          (set! rbx 8)
          (mset! rax 8 16)
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (- rax (rbp - 0)))))
        `(begin
            (set! rax 8)
            (set! rbx 8)
            (set! (rax + 8) 16)
            (with-label L.test.1 (set! (rbp - 8) 8))
            (set! rax (- rax (rbp - 0))))))

(test-case "Support (mset! reg_1 int32 reg)"
     (check-equal?
      (implement-mops
       `(begin
          (set! rax 8)
          (set! rbx 8)
          (mset! rax 8 rax)
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (- rax (rbp - 0)))))
        `(begin
            (set! rax 8)
            (set! rbx 8)
            (set! (rax + 8) rax)
            (with-label L.test.1 (set! (rbp - 8) 8))
            (set! rax (- rax (rbp - 0))))))

(test-case "Support (mset! reg_1 int32 label)"
     (check-equal?
      (implement-mops
       `(begin
          (set! rax 8)
          (set! rbx 8)
          (mset! rax 8 L.test.1)
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (- rax (rbp - 0)))))
        `(begin
            (set! rax 8)
            (set! rbx 8)
            (set! (rax + 8) L.test.1)
            (with-label L.test.1 (set! (rbp - 8) 8))
            (set! rax (- rax (rbp - 0))))))


(test-case "Support (mset! reg_1 reg int64)"
     (check-equal?
      (implement-mops
       `(begin
          (set! rax 8)
          (set! rbx 8)
          (mset! rax rbx 64)
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (- rax (rbp - 0)))))
        `(begin
            (set! rax 8)
            (set! rbx 8)
            (set! (rax + rbx) 64)
            (with-label L.test.1 (set! (rbp - 8) 8))
            (set! rax (- rax (rbp - 0))))))
            
(test-case "Support (mset! reg_1 reg reg)"
     (check-equal?
      (implement-mops
       `(begin
          (set! rax 5)
          (set! rbx 16)
          (set! rcx 8)
          (mset! rax rbx rcx)
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (- rax (rbp - 0)))))
        `(begin
            (set! rax 5)
            (set! rbx 16)
            (set! rcx 8)
            (set! (rax + rbx) rcx)
            (with-label L.test.1 (set! (rbp - 8) 8))
            (set! rax (- rax (rbp - 0))))))

(test-case "Support (mset! reg_1 reg label)"
     (check-equal?
      (implement-mops
       `(begin
          (set! rax 8)
          (set! rbx 8)
          (mset! rax rbx L.test.1)
          (with-label L.test.1 (set! (rbp - 8) 8))
          (set! rax (- rax (rbp - 0)))))
        `(begin
            (set! rax 8)
            (set! rbx 8)
            (set! (rax + rbx) L.test.1)
            (with-label L.test.1 (set! (rbp - 8) 8))
            (set! rax (- rax (rbp - 0))))))


(test-case "Support new instr inside with-label 1"
     (check-equal?
      (implement-mops
       `(begin
          (set! rax 8)
          (set! rbx 8)
          (mset! rax rbx L.test.1)
          (with-label L.test.1 (mset! rax rbx rcx))
          (set! rax (- rax (rbp - 0)))))
        `(begin
            (set! rax 8)
            (set! rbx 8)
            (set! (rax + rbx) L.test.1)
            (with-label L.test.1 (set! (rax + rbx) rcx))
            (set! rax (- rax (rbp - 0))))))

(test-case "Support new instr inside with-label 2"
     (check-equal?
      (implement-mops
       `(begin
          (set! rax 8)
          (set! rbx 8)
          (mset! rax rbx L.test.1)
          (with-label L.test.1 (set! rax (mref rax rbx)))
          (set! rax (- rax (rbp - 0)))))
        `(begin
            (set! rax 8)
            (set! rbx 8)
            (set! (rax + rbx) L.test.1)
            (with-label L.test.1 (set! rax (rax + rbx)))
            (set! rax (- rax (rbp - 0))))))