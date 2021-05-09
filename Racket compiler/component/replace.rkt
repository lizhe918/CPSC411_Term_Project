#lang racket
(require cpsc411/compiler-lib)
(require racket/pretty)
(provide (all-defined-out))
(require "../utils/utils.rkt")

; input: asm-lang-v5/assignments
; output: nested-asm-lang-v5
; purpose: replace abstract locations with physical locations
(define (replace-locations p)
  (debug-log p "replace")

  (define (loc? x) (if (label? x) false (aloc? x)))
  
  (define (replace-locations-e assign e)
    (match e
      [`(jump ,trg ,loc ...)
       (if (loc? trg) `(jump ,(list-dict-ref assign trg)) `(jump ,trg))]
      [_ 
       (if (loc? e)
           (list-dict-ref assign e)
           (if (list? e)
               (map (lambda (x) (replace-locations-e assign x)) e)
               e))]))

  (define (replace-fun f)
    (match f
      [`(define ,label ,info ,tail)
       (let ([assign (list-dict-ref info 'assignment)])
         `(define ,label ,(replace-locations-e assign tail)))]))
          
  (match p
    [`(module ,info ,fun ... ,tail)
     (let ([assign (list-dict-ref info 'assignment)])
       `(module ,@(map replace-fun fun) ,(replace-locations-e assign tail)))]))
