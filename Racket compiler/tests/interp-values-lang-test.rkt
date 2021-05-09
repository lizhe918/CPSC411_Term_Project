#lang racket
(require
  cpsc411/compiler-lib
  cpsc411/2c-run-time
  cpsc411/test-suite/utils
  racket/match
  rackunit)

(require "../compiler.rkt")

; input: values-lang-v4
; output: int64
; purpose: Check if the output of values-lang-v4 programs matches the desired outputs. Also provide end-to-end tests which
; the result of executing compiled programs should be equal to the result of interpretation.

(define pass-list (current-pass-list))

(define passes
    (list
     uniquify
     sequentialize-let
     canonicalize-bind
     select-instructions
     uncover-locals
     undead-analysis
     conflict-analysis
     assign-registers
     replace-locations
     optimize-predicates
     expose-basic-blocks
     resolve-predicates
     flatten-program
     patch-instructions
     implement-fvars
     generate-x64
     wrap-x64-run-time
     wrap-x64-boilerplate))

  (test-suite
   "a2 values-lang tests"
   #:before
   (thunk
    (current-pass-list passes))
   #:after
   (thunk
    (current-pass-list pass-list))

(let ([x `(module
              (if (true)
                  (+ 1 2)
                  (+ 3 4)))])
  (test-case "Simple case with no binding"
              (test-confluent?/upto (execute x) (interp-values-lang x) 3)))

(let ([x `(module
              (let ([x 1] [y 2] [z 3]) z))])
  (test-case "binding in tail"
              (test-confluent?/upto (execute x) (interp-values-lang x) 3)))

(let ([x `(module
              (let ([x 1] [y 2] [z 3]) (+ z x)))])
  (test-case "Simple case with binding in value"
              (test-confluent?/upto (execute x) (interp-values-lang x) 4)))

(let ([x `(module
                (if (let ([x 1] [y 2] [z 3]) (< z 2))
                    5
                    6))])
  (test-case "Simple case with binding in pred"
              (test-confluent?/upto (execute x) (interp-values-lang x) 6)))

(let ([x `(module
              (let ([x 1] 
                    [y (let ([z 3]) z)])
                (+ x y)))])
  (test-case "General case"
              (test-confluent?/upto (execute x) (interp-values-lang x) 4)))

(let ([x `(module
               (let ([x 1] 
                     [y (if (let ([z 3] [q (+ 1 2)]) (!= z q))
                            (+ 1 2)
                            5)])
                 (+ x y)))])
  (test-case "General case"
              (test-confluent?/upto (execute x) (interp-values-lang x) 6)))

(let ([x `(module
               (let ([x 1] 
                     [y (if (let ([z 3] [q (+ 1 2)]) (!= z q))
                            (let ([z 1] [q 2]) (+ q z))
                            (let ([x 10]) (+ 1 x)))])
                 (+ x y)))])
  (test-case "General case"
              (test-confluent?/upto (execute x) (interp-values-lang x) 12)))

(let ([x  `(module
               (let ([x 1] 
                     [y (if (let ([z 3] [q (+ 1 2)]) (!= z q))
                            (let ([z (let ([z 2] [y 1]) (+ y z))] [q (let ([x 2]) x)]) (+ q z))
                            (let ([x 10]) (let ([z 1] [y 1]) (+ y z))))])
                 (+ x y)))])
  (test-case "General case"
              (test-confluent?/upto (execute x) (interp-values-lang x) 3)))


(let ([x  `(module
              (let ([x 1] 
                    [y 2] 
                    [z (let ([x 1] 
                             [y 2] 
                             [z (let ([x 2] [y 7] [z 3]) (+ x y))]) (+ x z))])
                (+ y z)))])
  (test-case "General case"
              (test-confluent?/upto (execute x) (interp-values-lang x) 12)))


(let ([x  `(module
              (if (let ([x 1] [y 2] [z 3]) (< z 2))
                  (let ([x 1] [y 2] [z 3]) (if (< x y) x y))
                  (let ([x 3] [y 4] [z 5]) (if (let ([x 2] [y 1]) (< x y)) x y))))])
  (test-case "General case"
              (test-confluent?/upto (execute x) (interp-values-lang x) 4)))


(let ([x  `(module
              (if (let ([x 1] [y 2] [z 3]) (< z 2))
                  (let ([x 1] [y 2] [z 3]) (if (< x y) x y))
                  (let ([x 3] [y 4] [z 5]) (if (let ([z x] [q y]) (!= z q)) x y))))])
  (test-case "General case"
              (test-confluent?/upto (execute x) (interp-values-lang x) 3)))

(let ([x  `(module
              (let ([x 1] [y (let ([x 1] [y 2]) (+ x y))])
                (if (!= x y)
                    (let ([x x] [y y]) (+ x y))
                    (let ([z (+ x y)]) (* z z)))))])
  (test-case "General case"
              (test-confluent?/upto (execute x) (interp-values-lang x) 4)))


(let ([x   `(module
              (let ([x 1] [y (let ([x 1] [y 2]) (+ x y))])
                (if (if (let ([x (let ([y 1]) y)]) (< x 5)) (let ([x 2] [q (+ x 1)]) (> x q)) (not (let ([x 1] [y 2]) (!= y 2))))
                    (let ([x x] [y y]) (+ x y))
                    (let ([z (+ x y)]) (* z z)))))])
  (test-case "General case"
              (test-confluent?/upto (execute x) (interp-values-lang x) 16)))


(let ([x   `(module
              (let ([x 1] [y 2])
                (let ([z x] [q y])
                  (let ([x 5] [y 9])
                    (+ x q)))))])
  (test-case "General case"
              (test-confluent?/upto (execute x) (interp-values-lang x) 7)))

(let ([x  `(module
              (let ([x 1] [q 2] [y (let ([x 1] [y 2]) (+ x y))])
                (if (not (!= x y))
                    (let ([x q] [y y]) (+ q y))
                    (let ([z (+ x y)] [y (let ([x 1] [y (if (let ([x 1]) (< x 5)) (+ x q) (+ y q))]) (+ x y))]) (* z z)))))])
  (test-case "General case"
              (test-confluent?/upto (execute x) (interp-values-lang x) 16)))

(let ([x `(module
              (if (if (let ([x 1] [y 2] [z 3]) (< x y)) (if (true) (false) (let ([x 2] [y 3]) (not (!= x y)))) (let ([x 2] [z 3]) (> x z)))
                  (+ 1 2)
                  (let ([x 1] [y 2]) (+ x y))))])
  (test-case "General case"
              (test-confluent?/upto (execute x) (interp-values-lang x) 3)))

(let ([x `(module
              (if (let ([x 1] [y 2]) (not (!= x y)))
                  (if (false) 
                      (let ([x 1] [y 2]) (let ([x x] [y y]) (+ x y)))
                      (let ([x 1] [y 2]) (+ x y)))
                  (if (true)
                      (let ([x 1] [y 2]) (let ([z (+ x y)] [x (+ y y)]) (+ x z)))
                      (let ([z 3]) (+ z z)))))])
  (test-case "General case"
              (test-confluent?/upto (execute x) (interp-values-lang x) 7)))

(let ([x `(module
              (let ([x 1] [y 2]) (let ([q x] [z y]) (let ([w 1] [z 2])
                                                      (if (< w q)
                                                          (+ w z)
                                                          (+ y q))))))])
  (test-case "General case"
              (test-confluent?/upto (execute x) (interp-values-lang x) 3)))


(let ([x `(module
              (if (let ([x 1] [y 2]) (not (!= x y)))
                  (if (let ([x 1] [y 2]) (if (false) (let ([q x] [z y]) (= q z)) (true))) 
                      (let ([x 1] [y 2]) (let ([x x] [y y]) (+ x y)))
                      (let ([x 1] [y 2]) (+ x y)))
                  (if (if (let ([x 1] [y 2]) (if (< x y) (let ([q x] [y 1]) (!= q y)) (not (< x y)))) (true) (let ([x 1] [y 2]) (> x y)))
                      (let ([x 1] [y 2]) (let ([z (+ x y)] [x (+ y y)]) (+ x z)))
                      (let ([z 3]) (+ z z)))))])
  (test-case "General case"
              (test-confluent?/upto (execute x) (interp-values-lang x) 6)))

(let ([x `(module
              (if (if (if (if (true) (let ([x 1] [y 1]) (!= x y)) (let ([x 1] [y 2]) (= x y)))
                          (let ([x 1] [y 2]) (< x y))
                          (let ([x 1] [y 2]) (> x y)))
                      (let ([x 1] [y 2]) (< x y))
                      (let ([x 1] [y 2]) (> x y)))
                  5
                  (+ 1 2)))])
  (test-case "General case"
              (test-confluent?/upto (execute x) (interp-values-lang x) 3))))