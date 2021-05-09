#lang racket
(require cpsc411/compiler-lib)

(provide 
  fresh-int
  contain
  ex-last
  build-check
  debug-log
  format-int
  dict-ref-exist
  list-dict-has
  list-dict-ref
  list-dict-set
  run-seqn
  debug-run-seqn)

(define is-debug #f)

(define count -1)
(define (fresh-int)
  (set! count (+ count 1))
  count)

(define (contain l x)
  (ormap (lambda (e) (eq? x e)) l))

(define (ex-last l) 
  (reverse (rest (reverse l))))

(define (build-check valid? item)
  (lambda (x) (if (valid? x)
                  x
                  (error "Expected " item " ,got " x))))

(define (debug-log e info)
  (if is-debug
      (begin 
        (print (string-append "d: " info))
        ; (println (string-append "debug: " info))
        ; (if (eq? info "impl-mops") (println e) `())
        (if (eq? info "check-val") (begin (println " ") (println e)) `())
        ; (if (eq? info "uniquify") (begin (println " ") (println e)) `())
        ; (println e) 
        e)
       e))

(define (format-int i)
  (match i
    [(list 'min-int x) (min-int x)]
    [(list 'max-int x) (max-int x)]
    [_ i]))

(define (dict-ref-exist d x)
  (if (contain (dict-keys d) x)
      (dict-ref d x)
      x))

(define (list-dict-ref d x)
  (if (empty? d)
      (error "Element with key " x " not found.")
      (if (eq? (first (first d)) x)
          (second (first d))
          (list-dict-ref (rest d) x))))

(define (list-dict-set d x v)
  (if (ormap (lambda (y) (eq? x (first y))) d)
      (if (eq? (first (first d)) x)
          (append (list (list x v)) (rest d))
          (append (list (first d)) (list-dict-set (rest d) x v)))
      (append (list (list x v)) d)))

(define (list-dict-has d x)
  (ormap (λ (y) (eq? x y)) (map first d)))

(define (run-seqn p fun-list)
  (foldl (lambda (fun p) (fun p))
         p
         fun-list))
         
(define (debug-run-seqn p fun-list)
  (foldl (lambda (fun p) ((debug-log fun "function") (debug-log p "input")))
         p
         fun-list))