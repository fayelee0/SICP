#lang racket

(define (tan-cf x k)

  (define (rec n)
    (if (= n k)
        (- (* 2 n) 1)
        (/ (if (= n 1) x (* x x))
           (- (- (* 2 n) 1)
              (rec (+ n 1))))))

  (rec 1))

(tan-cf (/ pi 4) 100)
