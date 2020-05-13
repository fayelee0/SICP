#lang racket

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;; (fast-expt base exp) result is bigger