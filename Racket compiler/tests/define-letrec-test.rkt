#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/ptr-run-time
 racket/match
 rackunit)

(require "../component/def-rec.rkt")

; input: exprs-unsafe-lang-v9
; output: just-exprs-lang-v9
; purpose: Check if the outputs of define->letrec matches the desired outputs of the interrogator.

(let ([x '(module
            (define fun1.1
                (lambda (x.1 y.1 z.1) (unsafe-procedure-call fun1.1 y.1 z.1 x.1)))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))])
  (test-case "define-letrec for single proc"
             (check-equal? (define->letrec x)
                           `(module
                            (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call fun1.1 y.1 z.1 x.1))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))

(let ([x '(module
            (define fun1.1
                (lambda (x.1 y.1 z.1) (unsafe-procedure-call fun1.1 y.1 z.1 x.1)))
            (define fun2.2
                (lambda (x.2 y.2) (unsafe-fx+ x.2 y.2)))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))])
  (test-case "define-letrec for more procs"
             (check-equal? (define->letrec x)
                           `(module
                            (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call fun1.1 y.1 z.1 x.1)))
                                    (fun2.2 (lambda (x.2 y.2) (unsafe-fx+ x.2 y.2))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))

(let ([x '(module
            (define fun1.1
                (lambda (x.1 y.1 z.1) (unsafe-procedure-call fun2.2 y.1 z.1)))
            (define fun2.2
                (lambda (x.2 y.2) (unsafe-procedure-call fun1.1 1 x.2 y.2)))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))])
  (test-case "define-letrec for mutually rec procs"
             (check-equal? (define->letrec x)
                           `(module
                            (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call fun2.2 y.1 z.1)))
                                    (fun2.2
                                        (lambda (x.2 y.2) (unsafe-procedure-call fun1.1 1 x.2 y.2))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))

(let ([x '(module
            (define fun1.1
                (lambda (x.1 y.1 z.1) (unsafe-procedure-call fun2.2 y.1 z.1)))
            (define fun2.2
                (lambda (x.2 y.2) (unsafe-procedure-call fun1.1 1 x.2 y.2)))
            (define fun.4
                (lambda (x.3 y.3) (unsafe-procedure-call (lambda (x.4 y.4) x.4) x.3)))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))])
  (test-case "define-letrec for three procs 1"
             (check-equal? (define->letrec x)
                           `(module
                            (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call fun2.2 y.1 z.1)))
                                    (fun2.2 (lambda (x.2 y.2) (unsafe-procedure-call fun1.1 1 x.2 y.2)))
                                    (fun.4
                                        (lambda (x.3 y.3)
                                        (unsafe-procedure-call (lambda (x.4 y.4) x.4) x.3))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))

(let ([x '(module
            (define fun1.1
                (lambda (x.1 y.1 z.1) (unsafe-procedure-call fun2.2 y.1 z.1)))
            (define fun2.2
                (lambda (x.2 y.2) (unsafe-procedure-call fun1.1 1 x.2 y.2)))
            (define fun.4
                (lambda (x.3 y.3) (lambda (x.4 y.4) x.4)))
            (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))])
  (test-case "define-letrec for three procs 2"
             (check-equal? (define->letrec x)
                           `(module
                            (letrec ((fun1.1
                                        (lambda (x.1 y.1 z.1) (unsafe-procedure-call fun2.2 y.1 z.1)))
                                    (fun2.2 (lambda (x.2 y.2) (unsafe-procedure-call fun1.1 1 x.2 y.2)))
                                    (fun.4 (lambda (x.3 y.3) (lambda (x.4 y.4) x.4))))
                                (if (unsafe-procedure-call fun1.1 1 2 3) #t #f))))))