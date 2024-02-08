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
(define value-of
  (λ (x)))
(define value-of-fn
  (λ (x)))
(define empty-env-fn
  (λ (x)))
(define extend-env-fn
  (λ (x)))
(define apply-env-fn
  (λ (x)))
(define apply-clos-fn
  (λ (x)))
(define make-clos-fn
  (λ (x)))


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