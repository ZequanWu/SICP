;; Chapter 2
#lang sicp

(define (square x) (* x x))
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

(define (fibs n)
  (define (iter a b count)
    (if (= count n)
        b
        (iter (+ a b) a (+ count 1))))
  (iter 1 0 0))

(define (sum-odd-squares tree)
  (cond [(null? tree) nil]
        [(not (pair? tree))
         (if (odd? tree) (square tree) 0)]
        [else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree)))]))
      
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ([x (fibs k)])
          (if (even? x)
              (cons x (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define (filter predicate sequence)
  (cond [(null? sequence) nil]
        [(predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence)))]
        [else (filter predicate (cdr sequence))]))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond [(null? tree) nil]
        [(not (pair? tree)) (list tree)]
        [else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree)))]))

(define (sum-odd-squares-v2 tree)
  (accumulate + 0
              (map square
                   (filter odd? (enumerate-tree tree)))))

(define (even-fibs-v2 n)
  (accumulate cons nil
              (filter even?
                      (map fibs (enumerate-interval 0 n)))))

(accumulate append
            nil
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n)))
