#lang racket

;; empty environment
(define env0 '())

;; extend env by putting the pair `(,x . ,v) in the head of env
(define ext-env
  (lambda (x v env)
    (cons `(,x . ,v) env)))

;; look up the value of x in the environment env.
(define lookup
  (lambda (x env)
    (let ([p (assq x env)])
      (cond
        [(not p) #f]
        [else (cdr p)]))))

(struct Closure (f env) #:transparent)

;; unquote to a variable means to match anything
(define interp
  (lambda (exp env)
    (match exp
      [(? symbol? x)
       (let ([v (lookup x env)])
         (cond
           [(not v) (error "undefined variable" x)]
           [else v]))]
      [(? number? x) x]
      [`(lambda (,x) ,e)
       (Closure exp env)]  ;; notice: exp actually is a list in the Closure struct
      [`(let ([,x ,e1]) ,e2)
       (let ([v1 (interp e1 env)])
         (interp e2 (ext-env x v1 env)))]
      [`(,e1 ,e2) ;; e1 is a lambda function. e2 is a parameter
       (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match v1
           [(Closure `(lambda (,x) ,e) old-env)
            (interp e (ext-env x v2 old-env))]))]
      [`(,op ,e1 ,e2)
       (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))])))

(define r2
  (lambda (exp)
    (interp exp env0)))
         

