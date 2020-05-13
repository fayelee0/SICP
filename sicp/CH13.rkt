#lang racket

;; Formulating Abstractions with Higher-Order Procedurres

(define (cube x) (* x x x))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(sum-integers 1 100)

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))

(sum-cubes 1 100)

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

(* 8.0 (pi-sum 1 100000))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (identity n) n)

(define (sum-integers- a b) (sum identity a inc b))
(= (sum-integers 1 100) (sum-integers- 1 100))

(define (sum-cubes- a b) (sum cube a inc b))
(= (sum-cubes 1 100) (sum-cubes- 1 100))

(define (pi-sum- a b)
  (let ((pi-term (lambda (x)
                   (/ 1.0 (* x (+ x 2)))))
        (pi-next (lambda (x)
                   (+ x 4))))
    (sum pi-term a pi-next b)))

(= (pi-sum 1 100000) (pi-sum- 1 100000))

;; integral of a function f
(define (integral f a b dx)
  (let ((add-dx (lambda (x) (+ x dx))))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
       dx)))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)