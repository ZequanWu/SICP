#lang sicp
;; Exercise 1.11 rkt

;; recursive process
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

;; 从网上看到的，想不出来
;; 这种解法从树的底部往上走，counter 的初始值为2，因为最底部的a=f(2)=2。每往上走一层，counter++
;; iterative process
(define (f2 n)
  (define (iter a b c counter)
    (cond [(< n 3) n]
          [(= n counter) a]
          [else (iter (+ a (* 2 b) (* 3 c)) a b
                      (+ counter 1))]))
  (iter 2 1 0 2))
        
