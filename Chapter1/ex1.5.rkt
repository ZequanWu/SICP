#lang sicp
;; Exercise 1.5
;; new good-enough? this run faster than original good-enough?
(define (sqrt x)
  (define (sqrt-iter old-guess new-guess x)
    (define (good-enough? old-guess new-guess)
      (< (abs (- old-guess new-guess)) 0.001))
    (if (good-enough? old-guess new-guess)
        new-guess
        (sqrt-iter new-guess
                   (improve new-guess x)
                   x)))
  (sqrt-iter x 1 (* x 1.0)))
