#lang sicp
;; Exercise 1.3
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))
;; In applicative-order evaluation, it will never terminate,
;; since (p) will evaluated forever.
;; In normal-order evaluation, (test 0 (p)) => (if (= 0 0) 0 (p)) => 0,
;; so the result will be evaluated to 0.
