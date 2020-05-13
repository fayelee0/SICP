#lang racket

(define (cont-frac n d k)
  (letrec
      ((f (lambda (rt cc)
            (if (= cc 0)
                rt
                (f (/ (n cc) (+ (d cc) rt)) (- cc 1))))))
    (f (/ (n k) (d k)) (- k 1))))

(define N (lambda (i) 1.0))

(define D (lambda (i)
            (if (= (remainder i 3) 2)
                (* 2 (/ (+ i 1) 3)) ; / 会得到分数 !!!
                1)))

(define print-range
  (lambda (a b f)
    (if (> a b)
        (begin (newline) (display "done"))
        (begin (newline) (display (f a)) (print-range (+ a 1) b f)))))

(define E
  (+ 2.0 (cont-frac N D 100)))
 
(newline)
(display E)
