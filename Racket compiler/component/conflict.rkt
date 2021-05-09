#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(require cpsc411/graph-lib)
(require cpsc411/2c-run-time)
(require "../utils/utils.rkt")
(provide (all-defined-out))

(define (conflict-analysis p)
  (debug-log p "conflict")

  (define graph (new-graph))
  (define vertices `())
  (define stub `())
  
  (define (loc? x) 
    (if (label? x) false (or (aloc? x) (register? x) (fvar? x))))
  (define (add-source vtx) vtx)
  (define (apply-conflict vtx1 vtx2)
    (if (and (loc? vtx1) (loc? vtx2) (not (eq? vtx1 vtx2)))
        (set! graph (add-edge graph vtx1 vtx2))
        stub))
  (define (apply-conflict* vtx vs)
    (for ([v vs]) (apply-conflict vtx v)))

  (define (conflict-effect undead e)
    (match e
      [`(mset! ,loc ,index ,triv)
       stub]
      [`(set! ,loc1 (mref ,loc2 ,index))
       (apply-conflict* loc1 undead)]
      [`(set! ,aloc (,binop ,aloc ,triv))
       (for ([u undead]) (apply-conflict u aloc))]
      [`(set! ,aloc ,triv)
       (for ([u (set-remove undead triv)]) 
         (apply-conflict u aloc))]
      [`(begin ,effect ...)
       (for ([u undead] [e effect]) (conflict-effect u e))]
      [`(if ,pred ,effect1 ,effect2)
       (conflict-pred (first undead) pred)
       (conflict-effect (second undead) effect1)
       (conflict-effect (third undead) effect2)]
      [`(return-point ,label ,tail)
       (conflict-tail (second undead) tail)]))

  (define (conflict-pred undead e)
    (match e
      [`(not ,pred)
       (conflict-pred undead pred)]
      [`(if ,pred ,pred1 ,pred2)
       (conflict-pred (first undead) pred)
       (conflict-pred (second undead) pred1)
       (conflict-pred (third undead) pred2)]
      [`(begin ,effect ... ,pred)
       (conflict-effect (ex-last undead) `(begin ,@effect))
       (conflict-pred (last undead) pred)]
      [_ stub]))

  (define (conflict-tail undead t)
    (match t
      [`(halt ,triv)
       #:when (loc? triv) 
       (add-source triv)]
      [`(if ,pred ,tail1 ,tail2)
       (conflict-pred (first undead) pred)
       (conflict-tail (second undead) tail1)
       (conflict-tail (third undead) tail2)]
      [`(begin ,effect ... ,tail)
       (conflict-effect (ex-last undead) `(begin ,@effect))
       (conflict-tail (last undead) tail)]
      [`(jump ,trg ,loc ...)
      ;  (apply-conflict* trg loc)
      ;  (for ([l loc]) (apply-conflict* l loc))
      ;  (apply-conflict* trg undead)
      ;  (for ([u undead]) (apply-conflict* u (set-intersection undead loc)))
      stub
       ]
      [_ stub]))

  (define (conflict-info i tail)
    (let* ([var (list-dict-ref i 'locals)]
           [frame (list-dict-ref i 'new-frames)]
           [undead (list-dict-ref i 'undead-out)]
           [c-undead (list-dict-ref i 'call-undead)]
           [conf (begin 
                   (set! graph (new-graph var))
                  ;  (for ([c c-undead]) (apply-conflict* c c-undead))
                   (for ([f-set frame]) (for ([f f-set]) (apply-conflict* f f-set)))
                   (conflict-tail undead tail) 
                   graph)])
      (list-dict-set i 'conflicts conf)))
  
  (define (conflict-fun f)
    (match f
      [`(define ,label ,info ,tail)
       `(define ,label ,(conflict-info info tail) ,tail)]))
              
  (define (conflict-p p)
    (match p
      [`(module ,info ,fun ... ,tail)
       `(module ,(conflict-info info tail) ,@(map conflict-fun fun) ,tail)]))
               
  (conflict-p p))