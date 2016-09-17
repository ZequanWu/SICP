;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |diagonal(not finished)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Number -> List-of List-of Number
;; given a number , produce a list of list of numbers
;; each inside list consists of 1 and 0, 
(define (diagonal n)
  (cond
    [(= 0 n) '()]
    [else (local (;; Number -> List-of List-of Number
                  ;; 
                  (define (create-lol num row)
                    (cond
                      [(= row num) '()]
                      [else (append ()
                                    ())]))
                  (define (create-lon pos num-of-0)
                    (cond
                      [(zero? num-of-0) (list 1)]
                      [(pos ;I have no idea how to solve this problem
            (...))]))