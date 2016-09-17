;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname render-poly) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(define MT (empty-scene 50 50))
(define (render-line im p q)
  (scene+line
   im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "black"))
(define (render-poly p)
  (render-line (connect-dots p) (first p) (last p)))
(define (render-poly2 p)
  (connect-dots (cons (last p) p)))
(define (render-poly3 p)
  (connect-dots (add-at-end p (first p))))
(define (add-at-end l item)
  (cond
    [(empty? l) (list item)]
    [else (cons (first l)
                (add-at-end (rest l) item))]))
(define (last p)
  (cond
    [(empty? (rest p)) (first p)]
    [else (last (rest p))]))
(define (connect-dots p)
  (cond
    [(empty? (rest p)) MT]
    [else
     (render-line (connect-dots (rest p))
                  (first p)
                  (second p))]))
(define l (list (make-posn 30 30)
                (make-posn 10 10)
                (make-posn 20 10)
                (make-posn 20 20)
                (make-posn 10 20)))
(render-poly l)
(render-poly2 l)
(render-poly3 l)

     