;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname invasion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/universe)

(define WIDTH 400)
(define HEIGHT 400)
(define BACK (empty-scene WIDTH HEIGHT))
(define TANK (above (rectangle 5 10 "solid" "red")
                    (rectangle 30 10 "solid" "red")))
(define TANK-HEIGHT (image-height TANK))
(define UFO (overlay (circle 5 "solid" "green")
           (rectangle 30 5 "solid" "green")))
(define MISSILE (regular-polygon 5 3 "solid" "blue"))
(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])
(define-struct tank [loc vel])
