;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname BSL-var-exp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; A BSL-fun-expr is one of:
; 

(define-struct add [left right])
(define-struct mul [left right])
(define WRONG "wrong kind of S-expression")

(define (atom? ex)
  (or (number? ex) (symbol? ex) (string? ex)))

(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (symbol=? x ex)
                      v
                      ex)]
    [(add? ex) (make-add (subst (add-left ex) x v)
                         (subst (add-right ex) x v))]
    [(mul? ex) (make-mul (subst (mul-left ex) x v)
                         (subst (mul-right ex) x v))]
    [else (error WRONG)]))

(define (numberic? ex)
  (cond
    [(empty? ex) #t]
    [(list? ex) (and (numberic? (first ex))
                     (numberic? (rest ex)))]
    [(atom? ex) #t]
    [else #f]))

(define (eval-variable ex)
  (cond
    [(numberic? ex)
     (cond
       [(number? ex) ex]
       [(list? ex) (cond
                     [(eq? '+ (first ex))
                      (make-add (second ex) (third ex))]
                     [(eq? '* (first ex))
                      (make-mul (second ex) (third ex))]
                     [else (error "invalid input")])]
       [(string? ex) ex])]
    [else (error "invalid input")]))

(define (lookup-con da x)
  (for/or ((i da))
    (if (= (first i) x)
        (second i)
        #f)))
(define (eval-var-lookup ex da)
  (cond
    [(list? ex) (for*/list ((i ex))
                  (if (symbol? i)
                      (for]
    [(atom? ex) ex]
    [else (error "invalid input")]))