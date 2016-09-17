;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname worm-game) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 310)
(define HEIGHT 310)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define WORM (circle (/ WIDTH 62) "solid" "red"))
(define GAME-OVER-TEXT (text "GAME OVER" 24 "olive"))
(define FOOD (circle (/ (image-width WORM) 2) "solid" "green"))

;worm is a structure, x, y are both number indicate the position
;direc is a string
(define-struct worm [x y direc])
;worms is a list of worm
;food is a posn
(define-struct ws [worms food])

;worms is either:
;- (cons worm '())
;- (cons worm worms)

;s is a worms
(define (main s)
  (big-bang s
            [on-tick change (/ 1 5)]
            [on-key control]
            [to-draw draw]
            [stop-when collide? collided-image]))
;----------------------------------------------------------
(define (food-create p)
  (food-check-create
   p
   (make-posn (generate-random WIDTH)
              (generate-random HEIGHT))))
(define (generate-random range)
  (* (+ (* (round (/ (quotient (random range)
                               (/ (image-width WORM) 2))
                     2))
           2)
        1)
     (/ (image-width WORM) 2)))
; Posn Posn -> Posn 
; generative recursion 
; make sure that the food will not appear
; in the same location next time.
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))
;----------------------------------------------------------
(define (collide? ws)
  (collide?2 (ws-worms ws)))
(define (collided-image s)
  (place-image GAME-OVER-TEXT
               (/ WIDTH 2)
               (/ HEIGHT 2)
               (draw s)))
(define (collide?2 s)
  (or (< (worm-x (first s)) (/ (image-width WORM) 2))
      (> (worm-x (first s)) (- WIDTH (/ (image-width WORM) 2)))
      (< (worm-y (first s)) (/ (image-height WORM) 2))
      (> (worm-y (first s)) (- HEIGHT (/ (image-height WORM) 2)))
      (eat-self?1 (first s) (rest s))))
(define (eat-self?1 w s)
  (cond
    [(or (empty? s) (empty? (rest s))) #f]
    [else (eat-self?2 w (rest s))]))
(define (eat-self?2 w s)
  (cond
    [(empty? (rest s)) (close? w (first s))]
    [else (or (close? w (first s))
              (eat-self?2 w (rest s)))]))

(define (close? w w2)
  (and (< (abs (- (worm-x w)
                 (worm-x w2)))
         (/ (image-width WORM) 3))
      (< (abs (- (worm-y w)
                 (worm-y w2)))
         (/ (image-height WORM) 3))))
;----------------------------------------------------------
(define (change ws)
  (if (and (= (posn-x (ws-food ws))
              (worm-x (first (ws-worms ws))))
           (= (posn-y (ws-food ws))
              (worm-y (first (ws-worms ws)))))
      (make-ws (add-head (move (ws-worms ws)))
               (food-create (ws-food ws)))
      (make-ws (move (ws-worms ws))
               (ws-food ws))))
(define (move s)
  (remove-last (add-head s)))

(define (add-head s)
  (append (list
           (make-worm
            (cond
              [(string=? (worm-direc (first s)) "left")
               (- (worm-x (first s)) (image-width WORM))]
              [(string=? (worm-direc (first s)) "right")
               (+ (worm-x (first s)) (image-width WORM))]
              [else (worm-x (first s))])
            (cond
              [(string=? (worm-direc (first s)) "up")
               (- (worm-y (first s)) (image-height WORM))]
              [(string=? (worm-direc (first s)) "down")
               (+ (worm-y (first s)) (image-height WORM))]
              [else (worm-y (first s))])
            (worm-direc (first s))))
          s))

(define (remove-last l)
  (cond
    [(empty? (rest l)) '()]
    [else (cons (first l)
                (remove-last (rest l)))]))
;----------------------------------------------------------
(define (control ws ke)
  (make-ws (control2 (ws-worms ws) ke)
           (ws-food ws)))
(define (opposite? str1 str2)
  (or (and (string=? str1 "left")
           (string=? str2 "right"))
      (and (string=? str2 "left")
           (string=? str1 "right"))
      (and (string=? str2 "up")
           (string=? str1 "down"))
      (and (string=? str1 "up")
           (string=? str2 "down"))))

(define (control2 s ke)
  (cond
    [(opposite? ke (worm-direc (first s))) s]
    [(or (key=? ke "left")
         (key=? ke "right")
         (key=? ke "up")
         (key=? ke "down"))
     (append (list (make-worm (worm-x (first s))
                              (worm-y (first s))
                              ke))
             (rest s))]
    [else s]))
;----------------------------------------------------------
(define (draw w)
  (place-image FOOD
               (posn-x (ws-food w))
               (posn-y (ws-food w))
               (draw2 (ws-worms w))))

(define (draw2 s)
  (cond
    [(empty? s) BACKGROUND]
    [else (place-image WORM
                       (worm-x (first s))
                       (worm-y (first s))
                       (draw2 (rest s)))]))
;----------------------------------------------------------
(define x (list (make-worm (/ WIDTH 2)
                           (/ HEIGHT 2)
                           "right")
                 (make-worm (- (/ WIDTH 2) (image-width WORM))
                            (/ HEIGHT 2)
                            "right")))
(define ws1 (make-ws x
                     (food-create (make-posn 200 200))))
(main ws1)