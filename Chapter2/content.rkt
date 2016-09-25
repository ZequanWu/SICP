;; Chapter 2
#lang sicp

(define (length l)
  (if (null? l)
      0
      (+ 1 (length (cdr l)))))

(define (count-leaves x)
  (cond [(null? x) 0]
        [(not (pair? x)) 1]
        [else (+ (count-leaves (car x))
                 (count-leaves (cdr x)))]))

(define (scale-tree tree factor)
  (cond [(null? tree) nil]
        [(not (pair? tree)) (* tree factor)]
        [else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor))]))

