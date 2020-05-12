#lang racket

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (square q) (* 2 q p))
                   (/ count 2)))
        (else
         (fib-iter (+ (* b q) (* a q) (* a p))
                   (+ (* b q) (* a q))
                   p
                   q
                   (- count 1)))))

(define (square x) (* x x))

(fib 10)
(fib 0)
(fib 1)
(fib 5)
(fib 40)
(fib 64)