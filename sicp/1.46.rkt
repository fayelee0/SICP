#lang racket

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (let ((next (improve guess)))
      (if (good-enough? guess next)
          next
          ((iterative-improve good-enough? improve) next)))))

(define (average x y) (/ (+ x y) 2.0))

(define (sqrt x)
  (let ((improve
         (lambda (guess)
           (average guess (/ x guess))))
        (good-enough?
         (lambda (v1 v2)
           (= v1 v2))))
    (letrec
        ((iter
          (lambda (guess)
            (let ((next (improve guess)))
              (if (good-enough? guess next)
                  next
                  (iter next))))))
      (iter 1.0))))

(define (sqrt.v2 x)
  (let ((improve
         (lambda (guess)
           (average guess (/ x guess))))
        (good-enough?
         (lambda (v1 v2)
           (= v1 v2))))
    ((iterative-improve good-enough? improve) 1.0)))

(sqrt 2)
(sqrt.v2 2)

(define (fixed-point f guess)
  (let ((close-enough?
         (lambda (v1 v2)
           (< (abs (- v1 v2)) 0.001))))
    (letrec
        ((try (lambda (g)
                (let ((next (f g)))
                  (if (close-enough? g next)
                      next
                      (try next))))))
      (try guess))))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))
                
(define (sqrt.v3 x)
  (fixed-point
   (average-damp
    (lambda (y) (/ x y)))
   1.0))

(sqrt.v3 2)

(define (sqrt.v4 x)
  (let ((close-enough?
         (lambda (v1 v2)
           (< (abs (- v1 v2)) 0.001))))
    ((iterative-improve close-enough?
                        (average-damp (lambda (y) (/ x y))))
     1.0)))

(sqrt.v4 2)
