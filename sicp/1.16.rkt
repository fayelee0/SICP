#lang racket

(define fast-expt
  (lambda (b n)

    (define square (lambda (x) (* x x)))

    (define iter
      (lambda (b n a)
        (cond ((= n 0) a)
              ((even? n) (iter (square b) (/ n 2) a))
              (else (iter b (- n 1) (* a b))))))
    
    (iter b n 1)))

(= (fast-expt 2 10) 1024)