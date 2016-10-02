#lang sicp

(define withdraw
  (let ([balance 100])
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define (make-account balance)
  (let* ([withdraw (lambda (amount)
                     (if (>= balance amount)
                         (begin (set! balance (- balance amount))
                                balance)
                         "Insufficient funds"))]
         [deposit (lambda (amount)
                    (set! balance (+ balance amount))
                    balance)]
         [dispatch (lambda (m)
                     (cond [(eq? m 'withdraw) withdraw]
                           [(eq? m 'deposit) deposit]
                           [else "Unknown request -- MAKE-ACCOUNT"]))])
    dispatch))
