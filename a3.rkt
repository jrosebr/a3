#lang racket
;Problem 1

(define index
  (λ (var ls)
    (match ls
      (`() (error "not found"))
      (`(,a . ,_) #:when (eqv? var a) 0)
      (`(,_ . ,d) (add1 (index var d))))))

(define lex
  (λ (exp acc)
    (match exp
      (`,y #:when (symbol? y) (index y acc))
      (`(lambda (,x) ,body)
       #:when (symbol? x)
       `(lambda ,(lex body (cons x acc))))
      (`(,rator ,rand)
       `(,(lex rator acc) ,(lex rand acc)))))) 