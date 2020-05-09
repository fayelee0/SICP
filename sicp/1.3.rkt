#lang racket

(define sum-of-squares-of-the-two-larger-numbers
  (lambda (x y z)
    (if (and (> x z) (> y z))
        (sum-of-squares x y)
        (if (> x y)
            (sum-of-squares z x)
            (sum-of-squares z y)))))

(define square
  (lambda (x)
    (* x x)))

(define sum-of-squares
  (lambda (x y)
    (+ (square x) (square y))))

(sum-of-squares-of-the-two-larger-numbers 3 4 5)
;; 41