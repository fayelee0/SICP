#lang racket

(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (+ x dx))
          (f x)
          (f (- x dx)))
       3.0)))

(define (smooth-n f n)
  (repeated (smooth f) n))

(define (repeated f n)
  (lambda (x)
    (if (= n 1)
        (f x)
        ((compose f (repeated f (- n 1))) x))))

(define (square x) (* x x))

((smooth square) 5)

((smooth-n square 1) 5)