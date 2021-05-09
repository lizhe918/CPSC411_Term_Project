#lang racket
(require
 cpsc411/compiler-lib
 cpsc411/2c-run-time
 cpsc411/test-suite/utils
 racket/match
 rackunit)

(require "../component/optimize.rkt")

;; no sure the result-

; input: nested-asm-lang-v4
; output: nested-asm-lang-v4
; purpose: Check the correctness the output of optimize-predicates 
(let ([x `(module
               (begin (set! fv0 1)
                      (set! fv1 2)
                      (set! fv0 (+ fv0 fv1))
                      (halt fv0)))])
     (test-case "Nothing to optimize"
     (check-equal? (optimize-predicates x)
                  `(module
                    (begin (set! fv0 1)
                            (set! fv1 2)
                            (set! fv0 (+ fv0 fv1))
                            (halt fv0))))))

(let ([x `(module
               (begin (set! fv0 1)
                      (set! fv1 2)
                      (if (not (false))
                        (halt fv0)
                        (halt fv1))))])
     (test-case "Optimize not false predicate"
     (check-equal? (optimize-predicates x)
                  `(module
                      (begin (set! fv0 1)
                              (set! fv1 2)
                              (if (true)
                                (halt fv0)
                                (halt fv1)))))))

(let ([x `(module
               (begin (set! fv0 1)
                      (set! fv1 2)
                      (if (not (true))
                        (halt fv0)
                        (halt fv1))))])
     (test-case "Optimize not true predicate"
     (check-equal? (optimize-predicates x)
                  `(module
                      (begin (set! fv0 1)
                              (set! fv1 2)
                              (if (false)
                                (halt fv0)
                                (halt fv1)))))))

(let ([x `(module
               (begin (set! fv0 1)
                      (set! fv1 2)
                      (if (begin (set! fv0 2) (< fv0 (max-int 64)))
                        (halt fv0)
                        (halt fv1))))])
     (test-case "Optimize predicate when comparing with max-int"
     (check-equal? (optimize-predicates x)
                  `(module
                      (begin (set! fv0 1)
                              (set! fv1 2)
                              (if (begin (set! fv0 2) (true))
                                (halt fv0)
                                (halt fv1)))))))

(let ([x `(module
               (begin (set! fv0 1)
                      (set! fv1 2)
                      (if (begin (set! fv0 2) (> fv0 (min-int 64)))
                        (halt fv0)
                        (halt fv1))))])
     (test-case "Optimize predicate when comparing with min-int"
     (check-equal? (optimize-predicates x)
                  `(module
                      (begin (set! fv0 1)
                              (set! fv1 2)
                              (if (begin (set! fv0 2) (true))
                                (halt fv0)
                                (halt fv1)))))))

(let ([x `(module
               (begin (set! fv0 1)
                      (set! fv1 2)
                      (if (begin (set! rax fv1) (= rax fv1))
                        (halt fv1)
                        (halt fv0))))])
     (test-case "Optimize = relop predicate"
     (check-equal? (optimize-predicates x)
                  `(module
                  (begin (set! fv0 1)
                          (set! fv1 2)
                          (if (begin (set! rax fv1) (true))
                            (halt fv1)
                            (halt fv0)))))))

(let ([x `(module
               (begin (set! fv0 1)
                      (set! fv1 2)
                      (if (begin (set! fv1 2) (< fv1 0))
                        (halt fv1)
                        (halt fv0))))])
     (test-case "Optimize < relop predicate"
     (check-equal? (optimize-predicates x)
                  `(module
                    (begin (set! fv0 1)
                            (set! fv1 2)
                            (if (begin (set! fv1 2) (false))
                              (halt fv1)
                              (halt fv0)))))))

(let ([x `(module
               (begin (set! fv0 1)
                      (set! fv1 2)
                      (if (begin (set! fv1 2) (> fv1 0))
                        (halt fv1)
                        (halt fv0))))])
     (test-case "Optimize > relop predicate"
     (check-equal? (optimize-predicates x)
                 `(module
                    (begin (set! fv0 1)
                            (set! fv1 2)
                            (if (begin (set! fv1 2) (true))
                              (halt fv1)
                              (halt fv0)))))))

(let ([x `(module
               (begin (set! fv0 1)
                      (set! fv1 2)
                      (if (begin (set! fv1 2) (>= fv1 2))
                        (halt fv1)
                        (halt fv0))))])
     (test-case "Optimize >= relop predicate"
     (check-equal? (optimize-predicates x)
                  `(module
                    (begin (set! fv0 1)
                            (set! fv1 2)
                            (if (begin (set! fv1 2) (true))
                              (halt fv1)
                              (halt fv0)))))))

(let ([x `(module
               (begin (set! fv0 1)
                      (set! fv1 2)
                      (if (begin (set! fv1 2) (<= fv1 2))
                        (halt fv1)
                        (halt fv0))))])
     (test-case "Optimize <= relop predicate"
     (check-equal? (optimize-predicates x)
                  `(module
                    (begin (set! fv0 1)
                            (set! fv1 2)
                            (if (begin (set! fv1 2) (true))
                              (halt fv1)
                              (halt fv0)))))))

; (let ([x `(module
;                (begin (set! fv0 1)
;                       (set! fv1 2)
;                       (if (begin (set! fv1 2) (!= fv1 2))
;                         (halt fv1)
;                         (halt fv0))))])
;      (test-case "Optimize != relop predicate"
;      (check-equal? (optimize-predicates x)
;                   `(module
;                     (begin (set! fv0 1)
;                             (set! fv1 2)
;                             (if (false)
;                               (halt fv1)
;                               (halt fv0)))))))

;; General cases

(let ([x `(module
               (begin (set! fv0 1)
                      (set! fv1 2)
                      (if (not (not (not (true))))
                        (halt fv0)
                        (halt fv1))))])
     (test-case "Optimize nested not"
     (check-equal? (optimize-predicates x)
                  `(module
                    (begin (set! fv0 1)
                            (set! fv1 2)
                            (if (false)
                              (halt fv0)
                              (halt fv1)))))))

; (let ([x `(module
;                 (begin (set! fv0 1)
;                         (set! fv1 2)
;                         (if (begin (begin (begin (set! fv1 2))) (set! fv1 3) (= fv1 3))
;                           (halt fv0)
;                           (halt fv1))))])
;      (test-case "Optimize nested begin predicate"
;      (check-equal? (optimize-predicates x)
;                   `(module
;                     (begin (set! fv0 1)
;                             (set! fv1 2)
;                             (if (begin (set! fv3 fv0) (true))
;                               (halt fv0)
;                               (halt fv1)))))))

; (let ([x `(module
;                 (begin (set! fv0 1)
;                         (set! fv1 2)
;                         (if (begin (begin (begin (set! fv1 2))) (set! fv1 3) (true))
;                           (halt fv0)
;                           (halt fv1))))])
;      (test-case "Optimize begin predicate nested not"
;      (check-equal? (optimize-predicates x)
;                   `(module
;                     (begin (set! fv0 1)
;                             (set! fv1 2)
;                             (if (begin (set! fv3 2) (true))
;                               (halt fv0)
;                               (halt fv1)))))))

(let ([x `(module
                (begin (set! fv0 1)
                        (set! fv1 2)
                        (if (if (begin (set! fv3 2) (= fv3 2)) (begin (set! fv0 fv1) (= fv0 fv1)) (begin (set! rax 1) (> rax 0)))
                          (halt fv0)
                          (halt fv1))))])
     (test-case "Optimize begin predicate nested if "
     (check-equal? (optimize-predicates x)
                  `(module
                    (begin (set! fv0 1)
                            (set! fv1 2)
                            (if (if (begin (set! fv3 2) (true)) (begin (set! fv0 fv1) (true)) (begin (set! rax 1) (true)))
                              (halt fv0)
                              (halt fv1)))))))