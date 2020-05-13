#lang racket

(define (repeated f n)
  (lambda (x)
    (if (= n 1)
        (f x)
        (f ((repeated f (- n 1)) x)))))

(define square (lambda (x) (* x x)))

((repeated square 2) 5)

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated.v2 f n)
  (lambda (x)
    (if (= n 1)
        (f x)
        ((compose f (repeated.v2 f (- n 1))) x))))

((repeated.v2 square 2) 5)