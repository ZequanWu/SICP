#lang sicp
;; Exercise 1.11
;; iterative exponentiation process
(define (fast-exp b n)
  (define (iter exponent a)
    (cond [(= exponent 0) 1]
          [(even? exponent) (iter (/ exponent 2)
                                  (* a (square b)))]
          [else (iter (- exponent 1)
                      (* b a))]))
  (iter n 1))
