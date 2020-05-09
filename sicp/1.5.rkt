#lang racket

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

;; application order
;;
;; 1. evaluate <x = 0> <y = (p)>
;; 2. loop

;; normal-order
;;
;; 1. evaluate <if-predicate (= x 0)> is #t
;; 2. return 0