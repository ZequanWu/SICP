#lang sicp
;; Exercise 1.7
;; 1+ add 1 to its argument, -1+ subtract 1 from its argument
;; version 1
(define (+ a b)
  (if (= a 0)
      b
      (1+ (+ (-1+ a) b))))
;; version 2
(define (+ a b)
  (if (= a 0)
      b
      (+ (-1+ a) (1+ b))))
;; difference when executing (+ 4 5)
;; version 1 is recursive process
;; version 2 is iterative process, tail-recursive
