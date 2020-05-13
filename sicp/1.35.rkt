#lang racket

(define (close-enough? a b) (= a b))

(define (fixed-point f guess)
  (letrec ((try (lambda (g)
                  (let ((next (f g)))
                    (if (close-enough? g next)
                        next
                        (try next))))))
    (try guess)))

; φ ~ x -> 1 + 1/x
(define φ
  (fixed-point (lambda (x) (+ 1.0 (/ 1.0 x)))
               1.0))

(display φ)