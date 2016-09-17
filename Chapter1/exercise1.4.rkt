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
;; Therefore, the "sqrt-iter" will call itself forever.
;; (sqrt-iter (improve guess x)
;;            x)))
;; this part will call itself forever.
