#lang sicp
;; Exercise 1.12

;; recursive process
(define (triangle row col)
  (cond [(or (= col 1) (= col row)) 1]
        [(or (< row 1) (< col 1) (> col row)) (error "no number in that position")]
        [else (+ (triangle (- row 1) col)
                 (triangle (- row 1) (- col 1))))))

;; iterative process (i have no idea)
;; (define (triangle2 row col)
;;   (define (iter c-row c-col num l-num ll-num r-num)
;;     (cond [(and (= c-row row) (= c-col col)) num]
;;           [(= c-col col) (iter (+ c-row 1) (+ c-col 1) (+ num l-num) (+ l-num
;;   (cond [(or (= row 1) (= col 1) (= col row)) 1]
;;         [(or (< row 1) (< col 1) (> col row)) (error "no number in that position")]
;;         [else (iter 1 1 1 0 0)])
