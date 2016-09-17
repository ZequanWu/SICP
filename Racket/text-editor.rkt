;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname text-editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/universe)

;text editor
(define WIDTH 400)
(define HEIGHT 20)
(define-struct editor [pre post])

(define (combine pre post)
  (overlay/align "left" "center"
                 (beside (text pre 16 "black")
                         (rectangle 1 16 "solid" "red")
                         (text post 16 "black"))
                 (empty-scene WIDTH HEIGHT)))
;add an function that deal with a string contain \b \r \t ...

(define (render edt)
  (combine (editor-pre edt) (editor-post edt)))
;auxiliary functions
(define (string-first str)
  (substring str 0 1))
(define (string-last str)
  (substring str (- (string-length str) 1)))
(define (string-remove-first str)
  (substring str 1))
(define (string-remove-last str)
  (substring str 0 (- (string-length str) 1)))
;edit function deal with the add character and move problem
(define (edit edt ke)
  (cond
    [(and (= (editor-pre edt) 0) (string=? ke "\b"))
     edt]
    ;need to modify. 
    [(and (= (string-length ke) 1))
     (make-editor (string-append (editor-pre edt) ke)
                  (editor-post edt))]
    [(string=? "left" ke)
     (make-editor (string-remove-last (editor-pre edt))
                                       (string-append (string-last (editor-pre edt))
                                                      (editor-post edt)))]
    [(string=? "right" ke)
     (make-editor (string-append (editor-pre edt)
                                 (string-first (editor-post edt)))
                  (string-remove-first (editor-post edt)))]
    [else edt]))
(define (run edt)
  (big-bang edt
            [to-draw render]
            [on-key edit]))
