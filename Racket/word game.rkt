;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |word game|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(define DICTIONARY-LOCATION "/usr/share/dict/words")
(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))

;a Word is either:
;- '()
;- (cons 1String Word)
(define (strings->words s)
  (cond
    [(string=? s "") '()]
    [(= 1 (string-length s)) (list s)]
    [else
     (cons (string-ith s 0)
           (strings->words (substring s 1)))]))
;Word->string
(define (words->strings word)
  (cond
    [(empty? word) ""]
    [else (string-append (first word)
                         (words->strings (rest word)))]))
(define (low->los low)
  (cond
    [(empty? low) '()]
    [else (append (list (words->strings (first low)))
                  (low->los (rest low)))]))

;string->list of string
(define (alternative-words s)
  (in-dictionary (low->los (arrangements (strings->words s)))))
;list of string->list of strings
;purpose statement: consumes a list of strings, output a list of
;strings that those strings also occur in the dictionary
(define (in-dictionary los)
  (cond
   [(empty? los) '()]
   [else (if (member? (first los) DICTIONARY-AS-LIST)
             (cons (first los)
                   (in-dictionary (rest los)))
             (in-dictionary (rest los)))]))
;a list-of-words is
;- (list words ...)

;list-of-words->list-of-words
;purpose statement: consumes a Word s, output a list of all possible
;re-arrangements of the letters
(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w)
                                          (arrangements (rest w)))]))
;list-of-words->list-of-words
(define (insert-everywhere/in-all-words 1s low)
  (cond
    [(empty? (rest low)) (insert-words 1s (first low) 0)]
    [else (append (insert-words 1s (first low) 0)
                  (insert-everywhere/in-all-words 1s (rest low)))]))
(check-expect
 (insert-everywhere/in-all-words "d"
                                 (cons (list "e" "r")
                                       (cons (list "r" "e")
                                             '())))
 (cons (list "d" "e" "r")
       (cons (list "e" "d" "r")
             (cons (list "e" "r" "d")
                   (cons (list "d" "r" "e")
                         (cons (list "r" "d" "e")
                               (cons (list "r" "e" "d")
                                     '())))))))
;Word->list-of-Wrod
;consumes a 1string and a word
;output a list of all possiable words that combined with 1string
(define (insert-words 1s w pos)
  (cond
    [(= (length w) pos)
     (list (append w (list 1s)))]
    [else (cons (append (left-list w pos 0)
                        (list 1s)
                        (list (list-ref w pos))
                        (right-list w pos))
                (insert-words 1s w (add1 pos)))]))

(check-expect (insert-words "d" (list "e" "r") 0)
              (cons (list "d" "e" "r")
                    (cons (list "e" "d" "r")
                          (cons (list "e" "r" "d")
                                '()))))
(define (left-list l end start)
  (cond
    [(or (= 0 end) (= start end))'()]
    [else (cons (first l)
                (left-list (rest l) end (add1 start)))]))

    
(define (right-list l pos)
  (cond
    [(= pos (- (length l) 1)) '()]
    [else (cons (list-ref l (add1 pos))
                (right-list l (add1 pos)))]))
(define x (list 1 2 3 4 5 6 7))