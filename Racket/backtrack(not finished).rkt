;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |backtrack(not finished)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/abstraction)

(define QUEENS 8)

; QP is (make-posn CI CI)
; CI is a natural number in [0,QUEENS)
; interpretation a CI denotes a row or column index for a chess board, 
; (make-posn r c) specifies the square in the r-th row and the c-th column

(define (threatening? QP1 QP2)
  (local ((define x1 (posn-x QP1))
          (define x2 (posn-x QP2))
          (define y1 (posn-y QP1))
          (define y2 (posn-y QP2)))
    (or (= x1 x2)
        (= y1 y2)
        (= (abs (- x1 x2)) (abs (- y1 y2))))))


(define (render-queens n l i)
  (cond
    [(empty? l) (empty-scene (* n (image-width i))
                             (* n (image-height i)))]
    [else (place-image i
                       (+ (* (posn-x (first l))
                             (image-width i))
                          (/ (image-width i) 2))
                       (+ (* (posn-y (first l))
                             (image-height i))
                          (/ (image-height i) 2))
                       (render-queens n (rest l) i))]))

(define (n-queens-solution? s)
  (for*/or ((x s) (y s))
    (cond
      [(equal? x y) true]
      [else (not (threatening? x y))])))
(define (

(define loqp (list (make-posn 2 0)
                   (make-posn 0 1)
                   (make-posn 3 2)
                   (make-posn 1 3)))
(render-queens 4 loqp (triangle 20 "solid" "red"))