#lang racket

(define average (lambda (x y) (/ (+ x y) 2.0)))

(define (fixed-point f guess)
  (let ((close-enough? (lambda (v1 v2) (< (abs (- v1 v2)) 0.00001))))
    (letrec ((try (lambda (g)
                    (let ((next (f g)))
                      (newline)
                      (display g)
                      (if (close-enough? g next)
                          next
                          (try next))))))
      (try guess))))

(define v1
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
               2.0))

(newline)

(define v2
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
               2.0))

