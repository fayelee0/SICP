#lang racket

(define (smallest-divisor n)
  (find-divisor n 2))

; O(n^1/2) steps
(define (find-divisor n test-divisor)
  (let ((square (lambda (x) (* x x))))
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1))))))

(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)