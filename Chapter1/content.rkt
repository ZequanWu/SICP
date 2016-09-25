;; 1.1.7 Square Roots by Newton's Method
#lang sicp
(#%require (only racket/base random))


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

;; factorial function
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial2 n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;; fibonacci numbers (tree-recursive process)
(define (fib n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (fib (- n 1))
                 (fib (- n 2)))]))

;; iterative process
(define (fib2 n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

;; count change (tree-recursive process)
(define (first-denomination kinds-of-coins)
  (cond [(= kinds-of-coins 1) 1]
        [(= kinds-of-coins 2) 5]
        [(= kinds-of-coins 3) 10]
        [(= kinds-of-coins 4) 25]
        [(= kinds-of-coins 5) 50]))

(define (count-change amount)
  (define (cc amount kinds-of-coins)
    (cond [(= amount 0) 1]
          [(or (< amount 0) (= kinds-of-coins 0)) 0]
          [else (+ (cc (- amount
                          (first-denomination kinds-of-coins))
                       (kinds-of-coins))
                   (cc amount
                       (- kinds-of-coins 1)))]))
  (cc amount 5))

;; exponentiation
;; linear recursive process
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;; linear iteration
(define (expt2 b n)
  (define (exp-iter counter product)
    (if (= counter 0)
        product
        (exp-iter (- counter 1)
                  (* b product))))
  (exp-iter n 1))

(define (fast-exp b n)
  (cond [(= n 0) 1]
        [(even? n) (square (fast-exp b (/ n 2)))]
        [else (* b (fast-exp b (- n 1)))]))

;; find the greatest common divisor bwtween a and b
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n) n]
        [(= (remainder n test-divisor) 0) test-divisor]
        [else (find-divisor n (+ test-divisor 1))]))
(define (prime? n)
  (= (smallest-divisor n) n))

;; fermat test
(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m)]
        [else (remainder (* base (expmod base (- exp 1) m))
                         m)]))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
;; if n fails fermat test, then n is not prime.
(define (fast-prime? n times)
  (cond [(= times 0) true]
        [(fermat-test n) (fast-prime? n (- times 1))]
        [else false]))

;; helf-interval method for finding roots of equation
(define (search f neg-point pos-point)
  (let ([midpoint (average neg-point pos-point)]
        [close-enough? (lambda (x y) (< (abs (- x y)) 0.001))])
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ([test-value (f midpoint)])
          (cond [(positive? test-value)
                 (search f neg-point midpoint)]
                [(negative? test-value)
                 (search f midpoint pos-point)]
                [else midpoint])))))
(define (half-interval f a b)
  (let ([left (f a)]
        [right (f b)])
    (cond [(and (positive? left) (negative? right))
           (search f b a)]
          [(and (positive? right) (negative? left))
           (search f a b)])))

;; find fixed points of functions
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))
  (define (try guess)
    (let ([next (f guess)])
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
      
(define (sqrt-v3 x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

;; return the average of x and f(x)
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (sqrt-v4 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

;; newton's method
(define (newton-transform g)
  (define dx 0.00001)
  (define (deriv g)
    (lambda (x)
      (/ (- (g (+ x dx)) (g x))
         dx)))
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; implementation of cons, cdr, car
(define (cons x y)
  (lambda (m)
    (cond [(= m 0) x]
          [(= m 1) y])))
(define (car z) (z 0))
(define (cdr z) (z 1))
