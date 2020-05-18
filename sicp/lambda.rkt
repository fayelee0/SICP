#lang racket

(define average (lambda (x y) (/ (+ x y) 2.0)))

(define square (lambda (x) (* x x)))

(display "(sqrt    2) = ")
(sqrt 2)

(define sqrt.v1
  (lambda (x)
    (let ((good-enough? (lambda (guess)
                          (< (abs (- guess (/ x guess))) 0.00001)))
          (improve (lambda (guess)
                     (average guess (/ x guess)))))
      (letrec
          ((iter (lambda (guess)
                   (if (good-enough? guess)
                       guess
                       (iter (improve guess))))))
        (iter 2.0)))))

(newline)
(display "(sqrt.v1 2) = ")
(sqrt.v1 2)

(define sqrt.v2
  (lambda (x)
    (let ((improve (lambda (guess) (average guess (/ x guess))))
          (good-enough? (lambda (guess next) (= guess next))))
      (letrec
          ((iter (lambda (guess)
                   (let ((next (improve guess)))
                     (if (good-enough? guess next)
                         next
                         (iter next))))))
        (iter 2.0)))))

(newline)
(display "(sqrt.v2 2) = ")
(sqrt.v2 2)

(define fixed-point
  (lambda (f guess)
    (let ((close-enough?
           (lambda (guess next)
             (< (abs (- guess next)) 0.00001))))
      (letrec
          ((iter (lambda (g)
                   (let ((next (average g (f g))))
                     (if (close-enough? g next)
                         next
                         (iter next))))))
        (iter guess)))))

(define sqrt.v3
  (lambda (x)
    (fixed-point (lambda (y) (/ x y)) 2.0)))

(newline)
(display "(sqrt.v3 2) = ")
(sqrt.v3 2)

(define average-damp
  (lambda (f)
    (lambda (x)
      (average x (f x)))))

(define sqrt.v4
  (lambda (x)
    
    (define fixed-point
      (lambda (f guess)
        (let ((close-enough? (lambda (g n) (< (abs (- g n)) 0.00001))))
          (letrec
              ((iter (lambda (g)
                       (let ((next ((average-damp f) g)))
                         (if (close-enough? g next)
                             next
                             (iter next))))))
            (iter guess)))))
    
      (fixed-point (lambda (y) (/ x y)) 2.0)))

(newline)
(display "(sqrt.v4 2) = ")
(sqrt.v4 2)

(define dx 0.001)

(define deriv
  (lambda (f)
    (lambda (x)
      (/ (- (f (+ x dx)) (f x)) dx))))

(define newtown-transform
  (lambda (g)
    (lambda (x)
      (- x (/ (g x) ((deriv g) x))))))

(define newtown-method
  (lambda (f guess)
    (fixed-point (newtown-transform f) guess)))

(define sqrt.v5
  (lambda (x)
    (newtown-method (lambda (y) (- x (square y))) 2.0)))

(newline)
(display "(sqrt.v5 2) = ")
(sqrt.v5 2)

(define fixed-point-transform
  (lambda (f transform guess)
    (fixed-point (transform f) guess)))

(define sqrt.v6
  (lambda (x)
    (fixed-point-transform
     (lambda (y) (/ x y))
     average-damp
     2.0)))

(define sqrt.v7
  (lambda (x)
    (fixed-point-transform
     (lambda (y) (- x (square y)))
     newtown-transform
     2.0)))

(newline)
(display "(sqrt.v6 2) = ")
(sqrt.v6 2)

(newline)
(display "(sqrt.v7 2) = ")
(sqrt.v7 2)