#lang racket

(define pascal-triangle
  (lambda (i j)
    (cond ((= j 1) 1)
          ((= i j) 1)
          (else (+ (pascal-triangle (- i 1) (- j 1))
                   (pascal-triangle (- i 1) j))))))

(pascal-triangle 1 1)
(pascal-triangle 5 3)
(pascal-triangle 5 4)