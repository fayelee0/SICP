#lang racket

(define (cube x)
  (* x x x))

(define (p x)
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; 1
(sine 12.15)
; 5
; 12.15 - 4.05 - 1.35 - 0.45 - 0.15 - 0.05

;; 2
; growth in space: O(n)
; number of steps: O(lg n) ~ O(lg 1/3 n)