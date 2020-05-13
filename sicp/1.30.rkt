#lang racket

(define (sum term a next b)
  (letrec ((iter (lambda (na counter)
                   (if (> na b)
                       counter
                       (iter (next na) (+ counter (term na)))))))                       
    (iter a 0)))

(sum (lambda (x) x)
     1
     (lambda (x) (+ x 1))
     100)