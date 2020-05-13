#lang racket

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-mul (double a) (halve b)))
        (else (+ a (fast-mul a (- b 1))))))

(= (fast-mul 2 10) 20)

(define (fast-mul- a b)

  (define (iter a b c)
    (cond ((= b 0) c)
          ((even? b) (iter (double a) (halve b) c))
          (else (iter a (- b 1) (+ a c)))))
  
  (iter a b 0))

(= (fast-mul- 2 10) 20)