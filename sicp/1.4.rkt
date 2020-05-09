#lang racket

(define a-plus-b
  (lambda (a b)
    ((if (> b 0) + -) a b)))

;; a + |b|
        