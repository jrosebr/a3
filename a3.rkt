#lang racket
;Problem 1

(define index
  (λ (var ls)
    (match ls
      (`() var)
      (`(,a . ,_) #:when (eqv? var a) 0)
      (`(,_ . ,d) (let ([idx (index var d)])
                    (cond
                      ((number? idx) (add1 idx))
                      ((symbol? idx) var)))))))

(define lex
  (λ (exp acc)
    (match exp
      (`,y #:when (symbol? y)
           (let ([idx (index y acc)])
             (cond
               ((symbol? idx) idx)
               ((number? idx) `(var ,idx)))))
      (`(lambda (,x) ,body)
       #:when (symbol? x)
       `(lambda ,(lex body (cons x acc))))
      (`(,rator ,rand)
       `(,(lex rator acc) ,(lex rand acc))))))

;(lex '(lambda (x) x) '())
;(lex '(lambda (y) (lambda (x) y)) '())
;(lex '(lambda (y) (lambda (x) (x y))) '())
;(lex '(lambda (x) (lambda (x) (x x))) '())
;(lex '(lambda (x) (lambda (x) (y x))) '())

;Problem 2
(require racket/trace)
;Representation Dependent
(define value-of
  (λ (e env)
    (match e
      (`,n #:when (number? n) n)
      
      (`(* ,e1 ,e2)
       (* (value-of e1 env)
          (value-of e2 env)))
      
      (`,n #:when (boolean? n) n)
      
      (`(sub1 ,n)
       (sub1 (value-of n env)))
      
      (`(zero? ,n)
       (zero? (value-of n env)))
      
      (`(if ,cond ,then ,else)
       (if (value-of cond env) (value-of then env) (value-of else env)))
      
      (`(let ([,var-name ,var-expression])
          ,body)     
         (value-of body (λ (y)
                          (cond
                            ((eqv? y var-name) (value-of var-expression env))
                            (else (env y))))))
      
      ; Lookup the symbol y in environment
      (`,y #:when (symbol? y)
           (env y))
      
      ; Return function
      (`(lambda (,x) ,body)
       #:when (symbol? x)
       (λ (arg)
         (value-of body (λ (y)
                          (cond
                            ((eqv? y x) arg)
                            (else (env y)))))))
      
      ; Application should apply and of course,
      ; natural recursion
      (`(,rator ,rand)
       ((value-of rator env) (value-of rand env))))))

#|
(value-of
 '((lambda (x) (if (zero? x)
                   #t
                   #f))
   0)
 (lambda (y) (error 'value-of "unbound variable ~s" y)))

(value-of
 '((lambda (x) (if (zero? x)
                   12
                   47))
   0)
 (lambda (y) (error 'value-of "unbound variable ~s" y)))

(value-of
 '(let ([y (* 3 4)])
    ((lambda (x) (* x y)) (sub1 6)))
 (lambda (y) (error 'value-of "unbound variable ~s" y)))

(value-of
 '(let ([x (* 2 3)])
    (let ([y (sub1 x)])
      (* x y)))
 (lambda (y) (error 'value-of "unbound variable ~s" y)))

(value-of
 '(let ([x (* 2 3)])
    (let ([x (sub1 x)])
      (* x x)))
 (lambda (y) (error 'value-of "unbound variable ~s" y)))

(value-of
 '(let ((! (lambda (x) (* x x))))
    (let ((! (lambda (n)
               (if (zero? n) 1 (* n (! (sub1 n)))))))
      (! 5)))
 (lambda (y) (error 'value-of "unbound variable ~s" y)))

(value-of
 '(((lambda (f)
      (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
    (lambda (f)
      (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
   5)
 (lambda (y) (error 'value-of "unbound variable ~s" y)))
|#

;Representation Independent
(define value-of-fn
  (λ (e env)
    (match e
      (`,n #:when (number? n) n)
      
      (`(* ,e1 ,e2)
       (* (value-of-fn e1 env)
          (value-of-fn e2 env)))
      
      (`,n #:when (boolean? n) n)
      
      (`(sub1 ,n)
       (sub1 (value-of-fn n env)))
      
      (`(zero? ,n)
       (zero? (value-of-fn n env)))
      
      (`(if ,cond ,then ,else)
       (if (value-of-fn cond env) (value-of-fn then env) (value-of-fn else env)))
      
      (`(let ([,var-name ,var-expression])
          ,body)
       (value-of-fn body (extend-env-fn var-name (value-of-fn var-expression env) env)))
      
      ; Lookup the symbol y in environment
      (`,y #:when (symbol? y)
           (apply-env-fn env y))
      
      ; Return function
      (`(lambda (,x) ,body)
       #:when (symbol? x)
       (make-clos-fn x body env))
      
      ; Application should apply and of course,
      ; natural recursion
      (`(,rator ,rand)
       (apply-clos-fn rator rand env))
      )))

(define value-of-fn-fn 
  (λ (x env)
    (value-of-fn x (empty-env-fn))))

(define empty-env-fn
  (λ ()
    (λ (y)
      (error 'value-of-fn "unbound ~a" y))))

(define extend-env-fn
  (λ (x arg env)
    (λ (y)
      (cond
        ((eqv? y x) arg)
        (else (apply-env-fn env y))))))

(define apply-env-fn
  (λ (env y)
    (env y)))

(define make-clos-fn
  (λ (x body env)
    (λ (arg)
      ; We also need to extend the environment
      (value-of-fn body (extend-env-fn x arg env)))))

(define apply-clos-fn
  (λ (rator rand env)
    ((value-of-fn rator env) (value-of-fn rand env))))

(value-of-fn
   '((lambda (x) (if (zero? x)
                     #t
                     #f))
     0)
   (empty-env-fn))

(value-of-fn
   '((lambda (x) (if (zero? x)
                     12
                     47))
     0)
   (empty-env-fn))

(value-of-fn
   '(let ([y (* 3 4)])
      ((lambda (x) (* x y)) (sub1 6)))
   (empty-env-fn))

(value-of-fn
   '(let ([x (* 2 3)])
      (let ([y (sub1 x)])
        (* x y)))
   (empty-env-fn))

(value-of-fn
   '(let ([x (* 2 3)])
      (let ([x (sub1 x)])
        (* x x)))
   (empty-env-fn))

(value-of-fn
   '(((lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
      (lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
     5)
   (empty-env-fn))