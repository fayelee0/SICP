#lang racket

(define cubic
  (lambda (a b c)
    (lambda (x)
      (+ (* x x x)
         (* a x x)
         (* b x)
         c))))