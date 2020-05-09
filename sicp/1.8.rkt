#lang racket

(define cube-root
  (lambda (x)
    (cube-root-iter 1.0 x)))

(define cube-root-iter
  (lambda (guess x)
    (if (good-enough? guess x)
        guess
        (cube-root-iter (improve guess x) x))))

(define improve
  (lambda (guess x)
    (/ (+ (/ x (square guess))
          (* 2 guess))
       3)))

(define good-enough?
  (lambda (guess x)
    (< (abs (- (* guess guess guess) x)) 0.001)))

(define square
  (lambda (x)
    (* x x)))

(cube-root 3)
(cube-root 27)