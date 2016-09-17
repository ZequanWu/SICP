;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |text-editor(list)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define HEIGHT 20)
(define WIDTH 200)
(define FONT-SIZE 16)
(define FONT-COLOR "black")
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))
(define-struct editor [pre post])

;editor-kh deals with the key input
(define (editor-kh ed ke)
  (cond
    [(key=? ke "left") (editor-lft ed)]
    [(key=? ke "right") (editor-rgt ed)]
    [(key=? ke "\b") (editor-del ed)]
    [(key=? ke "\t") ed]
    [(key=? ke "\r") ed]
    [(= (string-length ke) 1) (editor-ins ed ke)]
    [else ed]))
;following functions operating key input
(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed))
               (editor-post ed)))
(define (editor-lft ed)
  (cond
    [(empty? (editor-pre ed)) ed]
    [else
     (make-editor (rest (editor-pre ed))
                  (cons (first (editor-pre ed))
                        (editor-post ed)))]))
(define (editor-rgt ed)
  (cond
    [(empty? (editor-post ed)) ed]
    [else
     (make-editor (rev (add-at-end (rev (editor-pre ed))
                                   (first (editor-post ed))))
                  (rest (editor-post ed)))]))
(define (editor-del ed)
  (cond
    [(empty? (editor-pre ed)) ed]
    [else
     (make-editor (rest (editor-pre ed))
                  (editor-post ed))]))
;editor-text consumes list of 1string and outputs text image
(define (editor-text l)
  (cond
    [(empty? l) (text "" FONT-SIZE FONT-COLOR)]
    [else (beside (text (first l) FONT-SIZE FONT-COLOR)
                  (editor-text (rest l)))]))
;editor-render conpose the text image to graphic editor
(define (editor-render ed)
  (place-image/align
   (beside (editor-text (rev (editor-pre ed)))
           CURSOR
           (editor-text (editor-post ed)))
   1 1 "left" "top" MT))

;str->los converts a string to a list of 1string
(define (str->los str)
  (cond
    [(string=? "" str) '()]
    [else (cons (string-ith str 0)
                (str->los (substring str 1)))]))
;create-editor consumes 2 strigns (left and right)
;to make a editor structure
(define (create-editor left-str right-str)
  (make-editor (str->los left-str)
               (str->los right-str)))

(define (main s)
  (big-bang (create-editor s "")
            [on-key editor-kh]
            [to-draw editor-render]))
;rev reverses a list
(define (rev l)
  (cond
    [(empty? l) '()]
    [else
     (add-at-end (rev (rest l))
                 (first l))]))
;add-at-end add item at the end of a l
(define (add-at-end l item)
  (cond
    [(empty? l) (cons item '())]
    [else
     (cons (first l)
           (add-at-end (rest l) item))]))
