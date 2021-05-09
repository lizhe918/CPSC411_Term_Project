#lang racket
(require
  cpsc411/compiler-lib
  cpsc411/ptr-run-time
  cpsc411/test-suite/utils
  cpsc411/langs/a9
  racket/match
  rackunit) 

(require "../compiler.rkt")


; input: asm-pred-lang-v7/framed
; output: asm-pred-lang-v7/spilled
; purpose: Check if the output of assign-registers matches the output of interrogator 

(parameterize ([current-pass-list (list
 hoist-lambdas
 implement-closures
 specify-representation
 remove-complex-opera*
 sequentialize-let
 impose-calling-conventions
 canonicalize-bind
 select-instructions
 expose-allocation-pointer
 uncover-locals
 undead-analysis
 conflict-analysis
 assign-call-undead-variables
 allocate-frames
 assign-registers
 assign-frame-variables
 replace-locations
 optimize-predicates
 implement-fvars
 expose-basic-blocks
 resolve-predicates
 flatten-program
 patch-instructions
 implement-mops
 generate-x64)])
    
    (compile    '(module
     (letrec ((L.+.71.7
               (lambda (c.74 tmp.32 tmp.33)
                 (let ()
                   (if (fixnum? tmp.33)
                     (if (fixnum? tmp.32) (unsafe-fx+ tmp.32 tmp.33) (error 2))
                     (error 2)))))
              (L.cons.70.8
               (lambda (c.75 tmp.63 tmp.64) (let () (cons tmp.63 tmp.64))))
              (L.-.69.9
               (lambda (c.76 tmp.34 tmp.35)
                 (let ()
                   (if (fixnum? tmp.35)
                     (if (fixnum? tmp.34) (unsafe-fx- tmp.34 tmp.35) (error 3))
                     (error 3)))))
              (L.eq?.68.10
               (lambda (c.77 tmp.65 tmp.66) (let () (eq? tmp.65 tmp.66))))
              (L.zero?.4.11
               (lambda (c.78 n.8)
                 (let ((eq?.68 (closure-ref c.78 0)))
                   (call L.eq?.68.10 eq?.68 n.8 0))))
              (L.sub1.5.12
               (lambda (c.79 n.9)
                 (let ((|-.69| (closure-ref c.79 0)))
                   (call L.-.69.9 |-.69| n.9 1))))
              (L.curry.6.13
               (lambda (c.80 f.11 x.10)
                 (let ()
                   (letrec ((L.lam.72.15
                             (lambda (c.81 y.12)
                               (let ((x.10 (closure-ref c.81 0))
                                     (f.11 (closure-ref c.81 1)))
                                 (if (procedure? f.11)
                                   (if (eq? (unsafe-procedure-arity f.11) 2)
                                     (closure-call f.11 f.11 x.10 y.12)
                                     (error 42))
                                   (error 43))))))
                     (cletrec
                      ((lam.72 (make-closure L.lam.72.15 1 x.10 f.11)))
                      lam.72)))))
              (L.build-list.7.14
               (lambda (c.82 f.14 n.13)
                 (let ((sub1.5 (closure-ref c.82 0))
                       (build-list.7 (closure-ref c.82 1))
                       (cons.70 (closure-ref c.82 2))
                       (zero?.4 (closure-ref c.82 3)))
                   (if (call L.zero?.4.11 zero?.4 n.13)
                     empty
                     (call
                      L.cons.70.8
                      cons.70
                      (if (procedure? f.14)
                        (if (eq? (unsafe-procedure-arity f.14) 1)
                          (closure-call f.14 f.14 n.13)
                          (error 42))
                        (error 43))
                      (call
                       L.build-list.7.14
                       build-list.7
                       f.14
                       (call L.sub1.5.12 sub1.5 n.13))))))))
       (cletrec
        ((|+.71| (make-closure L.+.71.7 2))
         (cons.70 (make-closure L.cons.70.8 2))
         (|-.69| (make-closure L.-.69.9 2))
         (eq?.68 (make-closure L.eq?.68.10 2))
         (zero?.4 (make-closure L.zero?.4.11 1 eq?.68))
         (sub1.5 (make-closure L.sub1.5.12 1 |-.69|))
         (curry.6 (make-closure L.curry.6.13 2))
         (build-list.7
          (make-closure
           L.build-list.7.14
           2
           sub1.5
           build-list.7
           cons.70
           zero?.4)))
        (call
         L.cons.70.8
         cons.70
         (call
          L.build-list.7.14
          build-list.7
          (letrec ((L.lam.73.16 (lambda (c.83 n.15) (let () 1))))
            (cletrec ((lam.73 (make-closure L.lam.73.16 1))) lam.73))
          5)
         (call
          L.build-list.7.14
          build-list.7
          (call L.curry.6.13 curry.6 |+.71| 1)
          5)))))))




