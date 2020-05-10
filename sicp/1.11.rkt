#lang racket

(define f
  (lambda (n)
    (if (< n 3)
        n
        (+ (f (- n 1))
           (f (- n 2))
           (f (- n 3))))))

(define f-
  (lambda (n)

    (define iter
      (lambda (a b c x)
        (if (< x 3)
            c
            (iter b c (+ c (* 2 b) (* 3 a)) (- x 1)))))
    
    (if (< n 3)
        n
        (iter 0 1 2 n))))

(= (f- 0) (f 0))
(= (f- 3) (f 3))