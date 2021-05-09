#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require "../utils/utils.rkt")
(provide (all-defined-out))


(define stub `())
(define (ploc? e) (or (register? e) (fvar? e)))

(define (sort-conf conf)
  (sort conf (λ (a b) (< (length (second a)) (length (second b))))))
  
(define (filter-conf conf)
  (filter (λ (x) (not (register? (first x)))) conf))

(define (allo-fvar-base num used)
  (let ([m (foldr max -1 (map (λ (x) (if (fvar? x) (fvar->index x) 0)) used))])
    (reverse (map (λ (x) (make-fvar x)) (range (+ 1 m) (+ 1 m num))))))

(define (allo-fvar num)
  (reverse (map make-fvar (range num))))


(define (has-conflict node conf assign)
  (if (list-dict-has conf node)
      (ormap (λ (x)
              (if (ploc? x)
                  (eq? x (list-dict-ref assign node))
                  (if (list-dict-has assign x) 
                      (eq? (list-dict-ref assign x) (list-dict-ref assign node)) 
                      #f)))
             (list-dict-ref conf node))
      #f))

(define (try-assign node ploc a-ploc var conf assign)
  (if (list-dict-has assign node)
      assign
      (let ([try-ass (list-dict-set assign node ploc)])
        (if (has-conflict node conf try-ass) 
            assign
            (mark var a-ploc conf try-ass)))))
              
(define (mark var a-ploc conf assign)
  (if (subset? var (map first assign))
      assign
      (let ([node (first (remove* (map first assign) var))])
        (foldr (λ (ploc ass) (try-assign node ploc a-ploc var conf ass))
               assign
               a-ploc))))
  

     
; Input: 
; Output: 
; Purpose: 
(define (assign-call-undead-variables p)
  (debug-log p "assign-call")

  (define (assign-info i)
    (let* ([conf (list-dict-ref i 'conflicts)]
           [c-undead (list-dict-ref i 'call-undead)]
           [assign (mark (reverse (filter aloc? c-undead))
                         (allo-fvar (+ 1 (length conf)))
                         (sort-conf (filter-conf conf))
                         `())])
      (list-dict-set i 'assignment assign)))
      
  (define (assign-fun f)
    (match f
      [`(define ,label ,info ,tail)
       `(define ,label ,(assign-info info) ,tail)]))

  (define (assign-p p)
    (match p
      [`(module ,info ,fun ... ,tail)
       `(module ,(assign-info info) ,@(map (λ (f) (assign-fun f)) fun) ,tail)]))
  
  (assign-p p))

  
; Input: 
; Output: 
; Purpose: 
(define (allocate-frames p)
  (debug-log p "allo-frames")

  (define cbr (current-frame-base-pointer-register))

  (define (allo-info i)
    (let* ([frame (list-dict-ref i 'new-frames)]
           [var (list-dict-ref i 'locals)]
           [conf (list-dict-ref i 'conflicts)]
           [assign (list-dict-ref i 'assignment)]
           [new-var (remove* (apply append frame) (reverse var))]
           [new-assign (foldr (λ (fm ass)
                                (mark fm 
                                      (allo-fvar-base (+ 1 (length conf)) (map second assign))
                                      (sort-conf (filter-conf conf))
                                      ass))
                              assign
                              frame)]
           [new-info (list-dict-set (list-dict-set i 'locals new-var) 
                                    'assignment 
                                    new-assign)])
      (filter (λ (x) (not (or (eq? (first x) 'new-frames) (eq? (first x) 'call-undead)))) 
              new-info)))

  (define (calculate-nb assign)
    (* (current-word-size-bytes)
       (+ 1 (foldr max 0 (map (λ (x) (if (fvar? x) (fvar->index x) 0)) (map second assign))))))
    
  (define (allo-e nb e)
    (match e
      [`(return-point ,rp ,tail)
       `(begin (set! ,cbr (- ,cbr ,nb))
               (return-point ,rp ,tail)
               (set! ,cbr (+ ,cbr ,nb)))]
      [_ (if (list? e) (map (λ (x) (allo-e nb x)) e) e)]))
      
  (define (allo-fun f)
    (match f
      [`(define ,label ,info ,tail)
       (let ([new-info (allo-info info)])
         `(define ,label 
                  ,new-info 
                  ,(allo-e (calculate-nb (list-dict-ref info 'assignment)) tail)))]))

  (define (allo-p p)
    (match p
      [`(module ,info ,fun ... ,tail)
       (let ([new-info (allo-info info)])
         `(module ,new-info 
                  ,@(map (λ (f) (allo-fun f)) fun) 
                  ,(allo-e (calculate-nb (list-dict-ref info 'assignment)) tail)))]))
       
  (allo-p p))

; Input: 
; Output: 
; Purpose: 
(define (assign-registers p)
  (debug-log p "assign-reg")

  (define car (current-assignable-registers))
  
  (define (assign-info i)
    (let* ([var (list-dict-ref i 'locals)]
           [conf (list-dict-ref i 'conflicts)]
           [assign (list-dict-ref i 'assignment)]
           [a-ploc `(,@(range (- (+ 1 (length conf)) (length car))) ,@car)]
           [temp-assign (mark (reverse var)
                              a-ploc
                              (sort-conf (filter-conf conf))
                              assign)]
           [new-assign (filter (λ (x) (not (number? (second x)))) temp-assign)])
      (list-dict-set i 'assignment (reverse new-assign))))

  (define (assign-fun f)
    (match f
      [`(define ,label ,info ,tail)
       `(define ,label ,(assign-info info) ,tail)]))

  (define (assign-p p)
    (match p
      [`(module ,info ,fun ... ,tail)
       `(module ,(assign-info info) ,@(map (λ (f) (assign-fun f)) fun) ,tail)]))
                
  (assign-p p))
  
; Input: 
; Output: 
; Purpose: 
(define (assign-frame-variables p)
  (debug-log p "assign-frame")
  
  (define (assign-info i)
    (let* ([var (list-dict-ref i 'locals)]
           [undead (list-dict-ref i 'undead-out)]
           [conf (list-dict-ref i 'conflicts)]
           [assign (list-dict-ref i 'assignment)]
           [new-assign (mark (reverse var)
                             (allo-fvar (+ 1 (length conf)))
                             (sort-conf (filter-conf conf))
                             assign)])
      `((assignment ,new-assign))))
    
  (define (assign-fun f)
    (match f
      [`(define ,label ,info ,tail)
       `(define ,label ,(assign-info info) ,tail)]))

  (define (assign-p p)
    (match p
      [`(module ,info ,fun ... ,tail)
       `(module ,(assign-info info) ,@(map (λ (f) (assign-fun f)) fun) ,tail)]))
  
  (assign-p p))