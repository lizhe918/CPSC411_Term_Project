#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/ptr-run-time
 racket/match
 rackunit)

(require "../component/patch.rkt")


; input: para-asm-lang-v8
; output: paren-x64-v8
; purpose: Check the outputs of patch-instructions match the output of interrogator



(test-case
    "Support (set! reg_1 (mref reg_1 int64))"
     (let ([x '(begin
                 (set! rax 8)
                 (set! rax (mref rax 8)))])
       (check-equal?
        (patch-instructions x)
        `(begin (set! rax 8) (set! rax (mref rax 8))))))

(test-case
    "Support (set! reg_1 (mref reg_1 reg))"
     (let ([x '(begin
                 (set! rax 8)
                 (set! rbx 8)
                 (set! rax (mref rax 8)))])
       (check-equal?
        (patch-instructions x)
        `(begin (set! rax 8) (set! rbx 8) (set! rax (mref rax 8))))))

(test-case
    "Support (set! reg_1 (mref reg_1 addr))"
     (let ([x '(begin
                 (set! rax 8)
                 (set! (rbp - 8) 8)
                 (set! rax (mref rax (rbp - 8))))])
       (check-match
        (patch-instructions x)
        `(begin
            (set! rax 8)
            (set! (rbp - 8) 8)
            (set! ,r10 (rbp - 8))
            (set! rax (mref rax ,r10))))))


;; ----
(test-case
    "Support (set! addr (mref addr int64))"
     (let ([x '(begin
                 (set! (rbp - 8) 8)
                 (set! (rbp - 8) (mref (rbp - 8) 8)))])
       (check-match
        (patch-instructions x)
        `(begin
            (set! (rbp - 8) 8)
            (set! ,r10 (rbp - 8))
            (set! ,r10 (mref ,r10 8))
            (set! (rbp - 8) ,r10)))))

(test-case
    "Support (set! addr (mref addr reg))"
     (let ([x '(begin
                 (set! (rbp - 8) 8)
                 (set! rax 8)
                 (set! (rbp - 8) (mref (rbp - 8) rax)))])
       (check-match
        (patch-instructions x)
        `(begin
            (set! (rbp - 8) 8)
            (set! rax 8)
            (set! ,r10 (rbp - 8))
            (set! ,r10 (mref ,r10 rax))
            (set! (rbp - 8) ,r10)))))

(test-case
    "Support (set! addr (mref addr addr))"
     (let ([x '(begin
                 (set! (rbp - 8) 8)
                 (set! (rbp - 16) 8)
                 (set! (rbp - 8) (mref (rbp - 8) (rbp - 16))))])
       (check-match
        (patch-instructions x)
        `(begin
            (set! (rbp - 8) 8)
            (set! (rbp - 16) 8)
            (set! ,r10 (rbp - 8))
            (set! ,r11 (rbp - 16))
            (set! ,r10 (mref ,r10 ,r11))
            (set! (rbp - 8) ,r10)))))


;; -----
(test-case
    "Support (mset! reg_1 int64 int64)"
     (let ([x '(begin
                 (mset! rax 8 16))])
       (check-equal?
        (patch-instructions x)
        `(begin (mset! rax 8 16)))))

(test-case
    "Support (mset! reg_1 int64 reg)"
     (let ([x '(begin
                 (set! rbx 16)
                 (mset! rax 8 rbx))])
       (check-equal?
        (patch-instructions x)
        `(begin (set! rbx 16) (mset! rax 8 rbx)))))


(test-case
    "Support (mset! reg_1 int64 addr)"
     (let ([x '(begin
                 (set! (rbp - 8) 16)
                 (mset! rax 8 (rbp - 8)))])
       (check-match
        (patch-instructions x)
        `(begin (set! (rbp - 8) 16) (set! ,r10 (rbp - 8)) (mset! rax 8 ,r10)))))

;; ----
(test-case
    "Support (mset! reg_1 reg int64)"
     (let ([x '(begin
                 (set! rbx 16)
                 (mset! rax rbx 16))])
       (check-equal?
        (patch-instructions x)
        `(begin (set! rbx 16) (mset! rax rbx 16)))))

(test-case
    "Support (mset! reg_1 reg reg)"
     (let ([x '(begin
                 (set! rbx 16)
                 (set! rcx 8)
                 (mset! rax rbx rcx))])
       (check-equal?
        (patch-instructions x)
        `(begin (set! rbx 16) (set! rcx 8) (mset! rax rbx rcx)))))


(test-case
    "Support (mset! reg_1 reg addr)"
     (let ([x '(begin
                 (set! rbx 16)
                 (set! (rbp - 8) 8)
                 (mset! rax rbx (rbp - 8)))])
       (check-match
        (patch-instructions x)
        `(begin
            (set! rbx 16)
            (set! (rbp - 8) 8)
            (set! ,r10 (rbp - 8))
            (mset! rax rbx ,r10)))))

;; ---
(test-case
    "Support (mset! reg_1 addr int64)"
     (let ([x '(begin
                 (set! (rbp - 8) 8)
                 (mset! rax (rbp - 8) 8))])
       (check-match
        (patch-instructions x)
        `(begin (set! (rbp - 8) 8) (set! ,r10 (rbp - 8)) (mset! rax ,r10 8)))))

(test-case
    "Support (mset! reg_1 addr reg)"
     (let ([x '(begin
                 (set! rbx 16)
                 (set! (rbp - 8) 8)
                 (mset! rax (rbp - 8) rbx))])
       (check-match
        (patch-instructions x)
        `(begin
            (set! rbx 16)
            (set! (rbp - 8) 8)
            (set! ,r10 (rbp - 8))
            (mset! rax ,r10 rbx)))))


(test-case
    "Support (mset! reg_1 addr addr)"
     (let ([x '(begin
                 (set! (rbp - 16) 16)
                 (set! (rbp - 8) 8)
                 (mset! rax (rbp - 8) (rbp - 16)))])
       (check-match
        (patch-instructions x)
        `(begin
            (set! (rbp - 16) 16)
            (set! (rbp - 8) 8)
            (set! ,r10 (rbp - 16))
            (set! ,r11 (rbp - 8))
            (mset! rax ,r11 ,r10)))))


;; ----

(test-case
    "Support (mset! addr addr int64)"
     (let ([x '(begin
                 (set! (rbp - 8) 8)
                 (mset! (rbp - 16) (rbp - 8) 8))])
       (check-match
        (patch-instructions x)
        `(begin
            (set! (rbp - 8) 8)
            (set! ,r10 (rbp - 16))
            (set! ,r11 (rbp - 8))
            (mset! ,r10 ,r11 8)))))

(test-case
    "Support (mset! addr addr reg)"
     (let ([x '(begin
                 (set! (rbp - 8) 8)
                 (set! rbx 8)
                 (mset! (rbp - 16) (rbp - 8) rbx))])
       (check-match
        (patch-instructions x)
        `(begin
            (set! (rbp - 8) 8)
            (set! rbx 8)
            (set! ,r10 (rbp - 16))
            (set! ,r11 (rbp - 8))
            (mset! ,r10 ,r11 rbx)))))


(test-case
    "Support (mset! addr addr addr)"
     (let ([x '(begin
                 (set! (rbp - 8) 8)
                 (set! (rbp - 24) 8)
                 (mset! (rbp - 16) (rbp - 8) (rbp - 24)))])
       (check-match
        (patch-instructions x)
        `(begin
            (set! (rbp - 8) 8)
            (set! (rbp - 24) 8)
            (set! ,r10 (rbp - 16))
            (set! ,r11 (rbp - 8))
            (set! ,r10 (+ ,r10 ,r11))
            (set! ,r11 (rbp - 24))
            (mset! ,r10 0 ,r11)))))

(test-case
    "Support (mset! addr reg int64)"
     (let ([x '(begin
                 (set! (rbp - 8) 8)
                 (set! rbx 8)
                 (mset! (rbp - 16) rbx 64))])
       (check-match
        (patch-instructions x)
        `(begin
            (set! (rbp - 8) 8)
            (set! rbx 8)
            (set! ,r10 (rbp - 16))
            (mset! ,r10 rbx 64)))))

(test-case
    "Support (mset! addr reg reg)"
     (let ([x '(begin
                 (set! rax 8)
                 (set! rbx 8)
                 (mset! (rbp - 16) rbx rax))])
       (check-match
        (patch-instructions x)
        `(begin (set! rax 8) (set! rbx 8) (set! ,r10 (rbp - 16)) (mset! ,r10 rbx rax)))))


(test-case
    "Support (mset! addr reg addr)"
     (let ([x '(begin
                 (set! (rbp - 8) 8)
                 (set! rbx 8)
                 (mset! (rbp - 16) rbx (rbp - 8)))])
       (check-match
        (patch-instructions x)
        `(begin
            (set! (rbp - 8) 8)
            (set! rbx 8)
            (set! ,r10 (rbp - 8))
            (set! ,r11 (rbp - 16))
            (mset! ,r11 rbx ,r10)))))

;; ---
(test-case
    "Support (mset! addr int64 int64)"
     (let ([x '(begin
                 (mset! (rbp - 16) 8 8))])
       (check-match
        (patch-instructions x)
        `(begin (set! ,r10 (rbp - 16)) (mset! ,r10 8 8)))))


(test-case
    "Support (mset! addr int64 reg)"
     (let ([x '(begin
                 (set! rax 8)
                 (mset! (rbp - 16) 8 rax))])
       (check-match
        (patch-instructions x)
        `(begin (set! rax 8) (set! ,r10 (rbp - 16)) (mset! ,r10 8 rax)))))


(test-case
    "Support (mset! addr int64 addr)"
     (let ([x '(begin
                 (set! (rbp - 8) 8)
                 (set! rax 8)
                 (mset! (rbp - 16) 8 (rbp - 8)))])
       (check-match
        (patch-instructions x)
        `(begin
            (set! (rbp - 8) 8)
            (set! rax 8)
            (set! ,r10 (rbp - 8))
            (set! ,r11 (rbp - 16))
            (mset! ,r11 8 ,r10)))))



(test-case
    "Support new instructions inside with-label"
     (let ([x '(begin
                 (with-label L.case.2 (mset! (rbp - 16) 8 (rbp - 8))))])
       (check-match
        (patch-instructions x)
        `(begin
            (with-label L.case.2 (set! ,r10 (rbp - 8)))
            (set! ,r11 (rbp - 16))
            (mset! ,r11 8 ,r10)))))


(test-case
    "Support new instructions inside with-label"
     (let ([x '(begin
                 (with-label L.case.2 (set! (rbp - 8) (mref (rbp - 8) (rbp - 16)))))])
       (check-match
        (patch-instructions x)
        `(begin
            (with-label L.case.2 (set! ,r10 (rbp - 8)))
            (set! ,r11 (rbp - 16))
            (set! ,r10 (mref ,r10 ,r11))
            (set! (rbp - 8) ,r10)))))





;; ---------------------------------- old cases -------------------------------
(test-case
    "Support (set! addr int64)"
     (let ([x '(begin
                 (set! (rbp - 16) 2147483648))])
       (check-match
        (patch-instructions x)
        `(begin (set! ,r10 2147483648) (set! (rbp - 16) ,r10)))))

(test-case
    "Support (set! addr addr)"
     (let ([x '(begin
                 (set! (rbp - 16) 15)
                 (set! (rbp - 24) 12)
                 (set! (rbp - 16) (rbp - 24)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! (rbp - 16) 15)
        (set! (rbp - 24) 12)
        (set! ,r10 (rbp - 24))
        (set! (rbp - 16) ,r10)))))

;; ------------- and
(test-case
    "Binop support and -- case (binop reg int64)"
     (let ([x '(begin
                 (set! (rbp - 0) 15)
                 (set! rax (bitwise-and rax 9223372036854775807)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! (rbp - 0) 15)
        (set! ,r11 9223372036854775807)
        (set! rax (bitwise-and rax ,r11))))))


(test-case
    "Binop support and -- case (binop addr int64)"
     (let ([x '(begin
                 (set! (rbp - 0) 15)
                 (set! (rbp - 0) (bitwise-and (rbp - 0) 9223372036854775807)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! (rbp - 0) 15)
        (set! ,r10 (rbp - 0))
        (set! ,r11 9223372036854775807)
        (set! ,r10 (bitwise-and ,r10 ,r11))
        (set! (rbp - 0) ,r10)))))


(test-case
    "Binop support and -- case (binop addr int32)"
     (let ([x '(begin
                 (set! (rbp - 0) 15)
                 (set! (rbp - 0) (bitwise-and (rbp - 0) 2147483647)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! (rbp - 0) 15)
        (set! ,r10 (rbp - 0))
        (set! ,r10 (bitwise-and ,r10 2147483647))
        (set! (rbp - 0) ,r10)))))

(test-case
    "Binop support and -- case (binop addr reg)"
     (let ([x '(begin
                 (set! rax 10)
                 (set! (rbp - 0) 15)
                 (set! (rbp - 0) (bitwise-and (rbp - 0) rax)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! rax 10)
        (set! (rbp - 0) 15)
        (set! ,r10 (rbp - 0))
        (set! ,r10 (bitwise-and ,r10 rax))
        (set! (rbp - 0) ,r10)))))

(test-case
    "Binop support and -- case (binop addr addr)"
     (let ([x '(begin
                 (set! (rbp - 0) 15)
                 (set! (rbp - 8) 12)
                 (set! (rbp - 0) (bitwise-and (rbp - 0) (rbp - 8))))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! (rbp - 0) 15)
        (set! (rbp - 8) 12)
        (set! ,r10 (rbp - 0))
        (set! ,r10 (bitwise-and ,r10 (rbp - 8)))
        (set! (rbp - 0) ,r10)))))


;;    ------------------- ior 
(test-case
    "Binop support ior -- case (binop reg int64)"
     (let ([x '(begin
                 (set! rax 15)
                 (set! rax (bitwise-ior rax -9223372036854775808)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! rax 15)
        (set! ,r11 -9223372036854775808)
        (set! rax (bitwise-ior rax r11))))))

(test-case
    "Binop support ior -- case (binop addr int64)"
     (let ([x '(begin
                 (set! (rbp - 0) 15)
                 (set! (rbp - 0) (bitwise-ior (rbp - 0) -9223372036854775808)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! (rbp - 0) 15)
        (set! ,r10 (rbp - 0))
        (set! ,r11 -9223372036854775808)
        (set! ,r10 (bitwise-ior ,r10 ,r11))
        (set! (rbp - 0) ,r10)))))


(test-case
    "Binop support ior -- case (binop addr int32)"
     (let ([x '(begin
                 (set! (rbp - 0) 15)
                 (set! (rbp - 0) (bitwise-ior (rbp - 0) 2147483647)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! (rbp - 0) 15)
        (set! ,r10 (rbp - 0))
        (set! ,r10 (bitwise-ior ,r10 2147483647))
        (set! (rbp - 0) ,r10)))))

(test-case
    "Binop support ior -- case (binop addr reg)"
     (let ([x '(begin
                 (set! rax 10)
                 (set! (rbp - 0) 15)
                 (set! (rbp - 0) (bitwise-ior (rbp - 0) rax)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! rax 10)
        (set! (rbp - 0) 15)
        (set! ,r10 (rbp - 0))
        (set! ,r10 (bitwise-ior ,r10 rax))
        (set! (rbp - 0) ,r10)))))

(test-case
    "Binop support ior -- case (binop addr addr)"
     (let ([x '(begin
                 (set! (rbp - 0) 15)
                 (set! (rbp - 8) 12)
                 (set! (rbp - 0) (bitwise-ior (rbp - 0) (rbp - 8))))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! (rbp - 0) 15)
        (set! (rbp - 8) 12)
        (set! ,r10 (rbp - 0))
        (set! ,r10 (bitwise-ior ,r10 (rbp - 8)))
        (set! (rbp - 0) ,r10)))))


;; ------------------------- xor
(test-case
    "Binop support xor -- case (binop reg int64)"
     (let ([x '(begin
                 (set! rax 15)
                 (set! rax (bitwise-xor rax -9223372036854775808)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! rax 15)
        (set! ,r11 -9223372036854775808)
        (set! rax (bitwise-xor rax r11))))))

(test-case
    "Binop support xor -- case (binop addr int64)"
     (let ([x '(begin
                 (set! (rbp - 0) 15)
                 (set! (rbp - 0) (bitwise-xor (rbp - 0) -9223372036854775808)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! (rbp - 0) 15)
        (set! ,r10 (rbp - 0))
        (set! ,r11 -9223372036854775808)
        (set! ,r10 (bitwise-xor ,r10 ,r11))
        (set! (rbp - 0) ,r10)))))


(test-case
    "Binop support xor -- case (binop addr int32)"
     (let ([x '(begin
                 (set! (rbp - 0) 15)
                 (set! (rbp - 0) (bitwise-xor (rbp - 0) 2147483647)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! (rbp - 0) 15)
        (set! ,r10 (rbp - 0))
        (set! ,r10 (bitwise-xor ,r10 2147483647))
        (set! (rbp - 0) ,r10)))))

(test-case
    "Binop support xor -- case (binop addr reg)"
     (let ([x '(begin
                 (set! rax 10)
                 (set! (rbp - 0) 15)
                 (set! (rbp - 0) (bitwise-xor (rbp - 0) rax)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! rax 10)
        (set! (rbp - 0) 15)
        (set! ,r10 (rbp - 0))
        (set! ,r10 (bitwise-xor ,r10 rax))
        (set! (rbp - 0) ,r10)))))

(test-case
    "Binop support xor -- case (binop addr addr)"
     (let ([x '(begin
                 (set! (rbp - 0) 15)
                 (set! (rbp - 8) 12)
                 (set! (rbp - 0) (bitwise-xor (rbp - 0) (rbp - 8))))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! (rbp - 0) 15)
        (set! (rbp - 8) 12)
        (set! ,r10 (rbp - 0))
        (set! ,r10 (bitwise-xor ,r10 (rbp - 8)))
        (set! (rbp - 0) ,r10)))))
        
;; ------------------- sar ; sar does not support second opand to be a loc
(test-case
    "Binop support arithmetic-shift-right -- case (binop reg 0-63)"
     (let ([x '(begin
                 (set! rax 15)
                 (set! rax (arithmetic-shift-right rax 63)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! rax 15)
        (set! rax (arithmetic-shift-right rax 63))))))


(test-case
    "Binop support arithmetic-shift-right -- case (binop addr 0-63)"
     (let ([x '(begin
                 (set! (rbp - 0) 15)
                 (set! (rbp - 0) (arithmetic-shift-right (rbp - 0) 0)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! (rbp - 0) 15)
        (set! ,r10 (rbp - 0))
        (set! ,r10 (arithmetic-shift-right ,r10 0))
        (set! (rbp - 0) ,r10)))))




;; ------------------- old cases --------------------------
(test-case
    "No instruction"
     (let ([x '(begin)])
       (check-equal?
        (patch-instructions x)
            '(begin))))

(test-case
    "No addr is used"
     (let ([x '(begin
                 (set! rcx L.case.2)
                 (set! rsi 2)
                 (set! rbx 18)
                 (with-label L.case.2 (set! rbx 3))
                 (set! rax rbx)
                 (jump L.case.2)
                 (set! rax (+ rax rsi))
                 (set! rax (+ rax 2)))])
       (check-equal?
        (patch-instructions x)
            '(begin
            (set! rcx L.case.2)
            (set! rsi 2)
            (set! rbx 18)
            (with-label L.case.2 (set! rbx 3))
            (set! rax rbx)
            (jump L.case.2)
            (set! rax (+ rax rsi))
            (set! rax (+ rax 2))))))


(test-case
    "Set a addr with another addr"
     (let ([x '(begin
                 (set! (rbp - 16) 10)
                 (set! (rbp - 8) 11)
                 (set! (rbp - 8) (rbp - 16)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! (rbp - 16) 10)
        (set! (rbp - 8) 11)
        (set! ,r10 (rbp - 16))
        (set! (rbp - 8) ,r10)))))


(test-case
    "Set a addr with reg"
     (let ([x '(begin
                 (set! rcx 10)
                 (set! (rbp - 8) 11)
                 (set! (rbp - 8) rcx))])
       (check-match
        (patch-instructions x)
        `(begin (set! rcx 10) (set! (rbp - 8) 11) (set! ,r10 rcx) (set! (rbp - 8) ,r10)))))




(test-case
    "Binop on a addr when opand is int64"
     (let ([x '(begin
                 (set! (rbp - 8) 15)
                 (set! (rbp - 8) (+ (rbp - 8) 32)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! (rbp - 8) 15)
        (set! ,r10 (rbp - 8))
        (set! ,r10 (+ ,r10 32))
        (set! (rbp - 8) ,r10)))))


(test-case
    "Binop on a addr when opand is addr"
     (let ([x '(begin
                 (set! (rbp - 8) 7)
                 (set! (rbp - 0) 15)
                 (set! (rbp - 0) (- (rbp - 0) (rbp - 8))))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! (rbp - 8) 7)
        (set! (rbp - 0) 15)
        (set! ,r10 (rbp - 0))
        (set! ,r10 (- ,r10 (rbp - 8)))
        (set! (rbp - 0) ,r10)))))


(test-case
    "Binop on a addr when opand is addr"
     (let ([x '(begin
                 (set! (rbp - 8) 7)
                 (set! (rbp - 0) 15)
                 (set! rax (- rax (rbp - 8))))])
       (check-equal?
        (patch-instructions x)
        `(begin (set! (rbp - 8) 7) (set! (rbp - 0) 15) (set! rax (- rax (rbp - 8)))))))


(test-case
    "Jump to label store in a addr"
     (let ([x '(begin
                 (set! (rbp - 8) L.label.1)
                 (jump (rbp - 8))
                 (with-label L.label.1
                   (set! rbx 18)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! ,r10 L.label.1)
        (set! (rbp - 8) ,r10)
        (set! ,r10 (rbp - 8))
        (jump ,r10)
        (with-label L.label.1 (set! rbx 18))))))

(test-case
    "Jump to label store in reg"
     (let ([x '(begin
                 (set! rcx L.label.1)
                 (jump rcx)
                 (with-label L.label.1
                   (set! rbx 18)))])
       (check-equal?
        (patch-instructions x)
        `(begin (set! rcx L.label.1) (jump rcx) (with-label L.label.1 (set! rbx 18))))))

(test-case
    "Compare a addr with a int64 opand"
     (let ([x '(begin
                 (set! (rbp - 8) 2)
                 (compare (rbp - 8) 3)
                 (jump-if > L.label.1)
                 (set! (rbp - 8) 10)
                 (with-label L.label.1
                   (set! rbx 18)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! (rbp - 8) 2)
        (set! ,r10 (rbp - 8))
        (compare ,r10 3)
        (jump-if > L.label.1)
        (set! (rbp - 8) 10)
        (with-label L.label.1 (set! rbx 18))))))


(test-case
    "Compare a addr with a addr opand"
     (let ([x '(begin
                 (set! (rbp - 8) 2)
                 (set! (rbp - 0) 3)
                 (compare (rbp - 0) (rbp - 8))
                 (jump-if > L.label.1)
                 (with-label L.label.1
                   (set! rbx 18)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! (rbp - 8) 2)
        (set! (rbp - 0) 3)
        (set! ,r11 (rbp - 8))
        (set! ,r10 (rbp - 0))
        (compare ,r10 ,r11)
        (jump-if > L.label.1)
        (with-label L.label.1 (set! rbx 18))))))

(test-case
    "Compare a addr with a reg opand"
     (let ([x '(begin
                 (set! (rbp - 8) 2)
                 (set! (rbp - 0) 3)
                 (set! rcx 3)
                 (compare (rbp - 0) rcx)
                 (jump-if > L.label.1)
                 (with-label L.label.1
                   (set! rbx 18)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! (rbp - 8) 2)
        (set! (rbp - 0) 3)
        (set! rcx 3)
        (set! ,r10 (rbp - 0))
        (compare ,r10 rcx)
        (jump-if > L.label.1)
        (with-label L.label.1 (set! rbx 18))))))


(test-case
    "Jump-if to a label stored in addr"
     (let ([x '(begin
                 (set! rbx 10)
                 (compare rbx 5)
                 (set! (rbp - 8) L.label.1)
                 (jump-if > (rbp - 8))
                 (with-label L.label.1
                   (set! rbx 18)))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! rbx 10)
        (compare rbx 5)
        (set! ,r10 L.label.1)
        (set! (rbp - 8) ,r10)
        (set! ,r10 (rbp - 8))
        (jump-if <= ,L.tmp.1)
        (jump ,r10)
        (with-label ,L.tmp.1 (set! ,r10 ,r10))
        (with-label L.label.1 (set! rbx 18))))))


(test-case
    "Patch instructions in nested with-label"
     (let ([x '(begin
                 (set! rbx 10)
                 (compare rbx 5)
                 (set! (rbp - 8) 6)
                 (set! rax L.label.1)
                 (jump-if > rax)
                 (with-label L.label.1
                   (with-label L.cs411main.1
                      (with-label L.end.1 (set! (rbp - 8) (rbp - 0))))))])
       (check-match
        (patch-instructions x)
        `(begin
        (set! rbx 10)
        (compare rbx 5)
        (set! (rbp - 8) 6)
        (set! rax L.label.1)
        (jump-if <= ,L.tmp.1)
        (jump rax)
        (with-label ,L.tmp.1 (set! ,r10 ,r10))
        (with-label
        L.label.1
        (with-label L.cs411main.1 (with-label L.end.1 (set! ,r10 (rbp - 0)))))
        (set! (rbp - 8) ,r10)))))

