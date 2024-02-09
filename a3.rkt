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

(displayln (lex '(lambda (x) x)
                '()))
(displayln (lex '(lambda (y) (lambda (x) y))
                '()))
(displayln (lex '(lambda (y) (lambda (x) (x y)))
                '()))
(displayln (lex '(lambda (x) (lambda (x) (x x)))
                '()))
(display (lex '(lambda (x) (lambda (x) (y x)))
              '()))

;Problem 2

(define val-of
  (λ (e env)
    (match e
      (`,n #:when (number? n) n)
      (`(+ ,e1 ,e2)
       (+ (val-of e1 env)
          (val-of e2 env)))
      ; Lookup the symbol y in environment
      (`,y #:when (symbol? y)
           (apply-env-fn env y))
      ; Return function
      (`(λ (,x) ,body)
       #:when (symbol? x)
       (λ (arg)
         ; We also need to extend the environment
         (val-of body (extend-env-fn x arg env))))
      ; Application should apply and of course,
      ; natural recursion
      (`(,rator ,rand)
       ((val-of rator env) (val-of rand env))))))


(define value-of-fn
  (λ (x)
    x))

(define empty-env-fn
  (λ ()
    (λ (y)
      (error 'val-of "unbound ~a" y))))

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
      (val-of body (extend-env-fn x arg env)))))



(displayln (value-of
            '((lambda (x) (if (zero? x)
                              #t
                              #f))
              0)
            (lambda (y) (error 'value-of "unbound variable ~s" y))))
(displayln (value-of
            '((lambda (x) (if (zero? x)
                              12
                              47))
              0)
            (lambda (y) (error 'value-of "unbound variable ~s" y))))
(displayln (value-of
            '(let ([y (* 3 4)])
               ((lambda (x) (* x y)) (sub1 6)))
            (lambda (y) (error 'value-of "unbound variable ~s" y))))
