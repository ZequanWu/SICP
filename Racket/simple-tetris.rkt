;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname simple-tetris) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;physical constants
(define WIDTH 10) ; the maximal number of blockss horizontally

;graphical constants
(define SIZE 10)
(define BLOCK
  (overlay (rectangle (- SIZE 1) (- SIZE 1) "solid" "red")
           (rectangle SIZE SIZE "outline" "black")))
(define SCENE-SIZE (* WIDTH SIZE))
(define BACKGROUND (empty-scene SCENE-SIZE
                                SCENE-SIZE))
(define-struct tetris [block landscape])
(define-struct block [x y])
(define-struct ws [tetris rate])

; a Block is:
; (make-block [Number Number])
; a Tetris is:
; (make-tetris [Block List-of-block]
; a WorldState is:
; (make-ws [Tetris Number])

;; WorldState -> WorldState
;; launches the program from initial WorldState
(define (tetris-main ws)
  (big-bang (ws-tetris ws)
            (to-draw tetris-render)
            (on-tick move (ws-rate ws))
            (on-key control)
            (stop-when over?)))

(define (over? t)
  (cond
    [(empty? t) #f]
    [else (reach-height? (tetris-landscape t))]))
(define (reach-height? lob)
  (cond
    [(empty? lob) #f]
    [else (or (= (block-y (first lob)) 0)
              (reach-height? (rest lob)))]))

(define (check-move t dx)
  (cond
    [(= dx -1) (cond
                 [(= (block-x (tetris-block t)) 0) #f]
                 [else
                  (not (member? (make-block
                                 (sub1 (block-x (tetris-block t)))
                                 (block-y (tetris-block t)))
                                (tetris-landscape t)))])]
    [(= dx 1) (cond
                [(= (block-x (tetris-block t)) (sub1 WIDTH)) #f]
                 [else
                  (not (member? (make-block
                                 (add1 (block-x (tetris-block t)))
                                 (block-y (tetris-block t)))
                                (tetris-landscape t)))])]))
(define (control t ke)
  (cond
    [(and (key=? ke "left")
          (> (block-x (tetris-block t)) 0)
          (check-move t -1))
     (make-tetris (make-block (sub1 (block-x (tetris-block t)))
                              (block-y (tetris-block t)))
                  (tetris-landscape t))]
    [(and (key=? ke "right")
          (< (block-x (tetris-block t)) (sub1 WIDTH))
          (check-move t 1))
     (make-tetris (make-block (add1 (block-x (tetris-block t)))
                              (block-y (tetris-block t)))
                  (tetris-landscape t))]
    [else t]))

(define (tetris-render t)
  (place-image BLOCK
               (+ (* WIDTH
                     (block-x (tetris-block t)))
                  (/ WIDTH 2))
               (+ (* WIDTH
                     (block-y (tetris-block t)))
                  (/ WIDTH 2))
               (landscape-render (tetris-landscape t))))
(define (landscape-render l)
  (cond
    [(empty? l) BACKGROUND]
    [else
     (place-image BLOCK
                  (+ (* WIDTH
                        (block-x (first l)))
                     (/ WIDTH 2))
                  (+ (* WIDTH
                        (block-y (first l)))
                     (/ WIDTH 2))
                  (landscape-render (rest l)))]))

(define (move t)
  (if (check-stop (tetris-block t) (tetris-landscape t))
      (block-generate t)
      (make-tetris (make-block (block-x (tetris-block t))
                               (add1 (block-y (tetris-block t))))
                   (tetris-landscape t))))
(define (check-stop b l)
  (cond
    [(= (block-y b) (sub1 WIDTH)) #t]
    [else (member? (make-block (block-x b)
                               (add1 (block-y b)))
                   l)]))
(define (block-generate t)
  (make-tetris (make-block (if (< (block-x (tetris-block t))
                                  (- WIDTH 1))
                               (add1 (block-x (tetris-block t)))
                               0)
                           0)
               (cons (tetris-block t) (tetris-landscape t))))
(tetris-main (make-ws (make-tetris (make-block 0 0)
                                   (list ))
                      0.2))