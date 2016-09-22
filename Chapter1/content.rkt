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
