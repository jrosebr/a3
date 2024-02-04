#lang racket
;Problem 1
(define (lex expr env)
  (cond
    ((null? expr) '())
    ((symbol? expr)
     (if (assoc expr env)
         (list 'var (cdr (assoc expr env))) 
         expr))
    ((list? expr)
     (let ((first (car expr))
           (rest (cdr expr)))
       (cond
         ((eq? first 'lambda)
          (let ((param (cadr expr))
                (body (caddr expr)))
            (list 'lambda
                  (lex body (cons (cons param 0)
                                  (map (lambda (e) (cons (car e) (+ 1 (cdr e)))) env))))))
         (else
          (cons (lex first env) (lex rest env))))))))
  
;Problem 2

