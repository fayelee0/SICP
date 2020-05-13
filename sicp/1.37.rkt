#lang racket

(define (cont-frac n d k)
  (letrec
      ((f (lambda (c)
            (if (= c k)
                (/ (n c) (d c))
                (/ (n c)
                   (+ (d c)
                      (f (+ c 1))))))))
    (f 1)))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)

(define (cont-frac.v2 n d k)
  (letrec
      ((iter (lambda (rt cc)
               (if (= cc 0)
                   rt
                   (iter (/ (n cc) (+ (d cc) rt))
                         (- cc 1))))))
    (iter (/ (n k) (d k)) (- k 1))))

(cont-frac.v2 (lambda (i) 1.0)
              (lambda (i) 1.0)
              11)