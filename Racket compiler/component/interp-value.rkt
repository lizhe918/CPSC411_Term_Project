#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require rackunit)
(provide (all-defined-out))

; values-lang-v4? -> int64
; Interpret the Values-lang v4 program p as a value.
(define (interp-values-lang p)
  (define (interp-p p env)
    (match p
      [`(module ,tail)
       (interp-tail tail env)]))

  (define (interp-pred p env)
    (match p
      [`(,relop ,triv1 ,triv2)
       (cond [(equal? relop `<) (< (interp-triv triv1 env)
                                  (interp-triv triv2 env))]
             [(equal? relop `<=) (<= (interp-triv triv1 env)
                                    (interp-triv triv2 env))]
             [(equal? relop `=) (= (interp-triv triv1 env)
                                  (interp-triv triv2 env))]
             [(equal? relop `>=) (>= (interp-triv triv1 env)
                                    (interp-triv triv2 env))]
             [(equal? relop `>) (> (interp-triv triv1 env)
                                  (interp-triv triv2 env))]
             [(equal? relop `!=) (not (= (interp-triv triv1 env)
                                        (interp-triv triv2 env)))])]
      [`(true)
       true]
      [`(false)
       false]
      [`(not ,pred)
       (not (interp-pred pred env))]
      [`(let ([,xs ,values] ...) ,pred)
       (interp-pred pred (for/fold ([env0 env])
                                   ([x xs]
                                    [v values])
                           (dict-set env0 x (interp-value v env))))]
      [`(if ,pred ,predt ,predf)
       (if (interp-pred pred env)
           (interp-pred predt env)
           (interp-pred predf env))]))

  (define (interp-tail t env)
    (match t
      [`(let ([,xs ,values] ...) ,tail)
       (interp-tail tail (for/fold ([env0 env])
                                   ([x xs]
                                    [v values])
                           (dict-set env0 x (interp-value v env))))]
      [`(if ,pred ,tailt ,tailf)
       (if (interp-pred pred env)
           (interp-tail tailt env)
           (interp-tail tailf env))]
      [else (interp-value t env)]))

  (define (interp-value v env)
    (match v
      [`(let ([,xs ,values] ...) ,value)
       (interp-value value (for/fold ([env env])
                                     ([x xs]
                                      [vi values])
                             (dict-set env x (interp-value vi env))))]
      [`(,binop ,triv1 ,triv2)
       (let* ([triv1-result (interp-triv triv1 env)]
              [tirv2-result (interp-triv triv2 env)])
         (interp-binop binop triv1-result tirv2-result))]
      [`(if ,pred ,valuet ,valuef)
       (if (interp-pred pred env)
           (interp-value valuet env)
           (interp-value valuef env))]
      [else (interp-triv v env)]))

  (define (interp-binop b v1 v2)
    (if (symbol=? b '+)
        (+ v1 v2)
        (* v1 v2)))

  (define (interp-triv t env)
    (cond [(int64? t) t]
          [else (dict-ref env t)]))
  
  (interp-p p '()))