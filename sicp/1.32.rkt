#lang racket

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) b))))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (identity n) n)
(define (inc n) (+ n 1))

(define (sum.v2 a b)
  (accumulate + 0 identity a inc b))

(sum.v2 1 100)

(define (accumulate.v2 combiner null-value term a next b)
  (letrec ((iter (lambda (na ret)
                   (if (> na b)
                       ret
                       (iter (next na) (combiner ret (term na)))))))
    (iter a null-value)))

(define (sum.v3 a b)
  (accumulate.v2 + 0 identity a inc b))

(sum.v3 1 100)
      