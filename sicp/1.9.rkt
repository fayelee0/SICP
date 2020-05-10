#lang racket

(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define (p+ a b)
  (if (= a 0)
      b
      (inc (p+ (dec a) b))))

(define (q+ a b)
  (if (= a 0)
      b
      (q+ (dec a) (inc b))))

(p+ 4 5)
(inc (p+ 3 5))
(inc (inc (p+ 2 5)))
(inc (inc (inc (p+ 1 5))))
(inc (inc (inc (inc (p+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

(q+ 4 5)
(q+ 3 6)
(q+ 2 7)
(q+ 1 8)
(q+ 0 9)
9