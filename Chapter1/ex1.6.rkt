#lang sicp
;; Exercise 1.6
(define (cube x) (* x x x))
(define (cube-root-iter old-guess new-guess x)
  (define (improve x y) (/ (+ (/ x
                               (* y y))
                            (* 2 y))
                           3))
  (define (good-enough? old-guess new-guess)
    (< (abs (- old-guess new-guess)) 0.001))
  (if (good-enough? old-guess new-guess)
      new-guess
      (cube-root-iter new-guess
                      (improve x new-guess)
                      x)))
(define (cube-root x)
  (cube-root-iter x 1 (* x 1.0)))
