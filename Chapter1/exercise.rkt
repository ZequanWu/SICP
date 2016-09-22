;; Exercises in chapter 1
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

;; Exercise 1.4
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; helper functions
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (improve guess x) (average guess (/ x guess)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;; if new-if procedure is used in following procedure:
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
;; Because applicative-order evaluation is actually used,
;; the "new-if" procedure will actually evaluate its 3 arguments before its body is evaluated.
;; Therefore, infinite recursion will occur due to else-clause part call itself forever.
;; (sqrt-iter (improve guess x)
;;            x)))
;; this part will call itself forever.

;; Exercise 1.5
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (improve guess x) (average guess (/ x guess)))
;; new good-enough? this run faster than original good-enough?
(define (good-enough? old-guess new-guess)
  (< (abs (- old-guess new-guess)) 0.001))
(define (sqrt-iter old-guess new-guess x)
  (if (good-enough? old-guess new-guess)
      new-guess
      (sqrt-iter new-guess
                 (improve new-guess x)
                 x)))
(define (sqrt x)
  (sqrt-iter x 1 (* x 1.0)))

;; Exercise 1.6
(define (cube x) (* x x x))
(define (improve x y) (/ (+ (/ x
                               (* y y))
                            (* 2 y))
                         3))
(define (good-enough? old-guess new-guess)
  (< (abs (- old-guess new-guess)) 0.001))
(define (cube-root-iter old-guess new-guess x)
  (if (good-enough? old-guess new-guess)
      new-guess
      (cube-root-iter new-guess
                      (improve x new-guess)
                      x)))
(define (cube-root x)
  (cube-root-iter x 1 (* x 1.0)))

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

;; Exercise 1.9
;; iterative process (I don't know!!!)
;; (define (count-change2 amount)
;;   (define (cc amount kinds-of-coints n)
;;     (if (= amount 0)
;;         n
;;         (cc amount

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
