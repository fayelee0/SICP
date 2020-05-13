#lang racket

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(product (lambda (x) x)
         1
         (lambda (x) (+ x 1))
         10)

(define (factorial n)
  (* 4.0 (product (lambda (i)
                    (if (even? i)
                        (/ (+ 2.0 i) (+ 1.0 i))
                        (/ (+ 1.0 i) (+ 2.0 i))))
                  1
                  (lambda (i) (+ i 1))
                  n)))

(factorial 100000)

(define (product.v2 term a next b)
  (letrec ((iter (lambda (na p)
                   (if (> na b)
                       p
                       (iter (next na) (* p (term na)))))))
    (iter a 1)))

(product.v2 (lambda (x) x)
            1
            (lambda (x) (+ x 1))
            10)

(define (factorial.v2 n)
  (* 4.0 (product.v2 (lambda (i)
                       (if (even? i)
                           (/ (+ 2.0 i) (+ 1.0 i))
                           (/ (+ 1.0 i) (+ 2.0 i))))
                     1
                     (lambda (i) (+ i 1))
                     n)))
(factorial.v2 1000000)