;; 1.1.7 Square Roots by Newton's Method
#lang sicp
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (improve guess x) (average guess (/ x guess)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
;; start with guess as 1:
(define (sqrt x)
  (sqrt-iter 1 (* x 1.0)))


;; sqrt version 2: all helper functions are inside sqrt-v2
;; This is block structure.
(define (sqrt-v2 x)
  (define (square x) (* x x))
  (define (average x y) (/ (+ x y) 2))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1))
