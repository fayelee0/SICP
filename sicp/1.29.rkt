#lang racket

(define (integral f a b n)
  (let ((h (/ (- b a) n 1.0)))
    (let ((term (lambda (k)
                  (cond ((= k 0) (f a))
                        ((= k n) (f b))
                        (else (* (if (even? k) 2 4)
                                 (f (+ a (* k h)))))))))                      
    (* (/ h 3.0)
       (sum term 0 inc n)))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (cube n) (* n n n))

(integral cube 0 1 100)
(integral cube 0 1 1000)