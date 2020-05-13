#lang racket

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (average x y)
  (/ (+ x y) 2.0))

(define (fixed-point f guess)
  (let ((close-enough?
         (lambda (v1 v2)
           (< (abs (- v1 v2)) 0.00001))))
    (letrec
        ((try (lambda (g)
                (let ((next (f g)))
                  (if (close-enough? g next)
                      next
                      (try next))))))
      (try 1.0))))

(define (sqrt x)
  (fixed-point
   (average-damp (lambda (y) (/ x y)))
   1.0))

(sqrt 2)

(define (cube-root x)
  (fixed-point
   (average-damp (lambda (y) (/ x (* y y))))
   1.0))

(cube-root 2)

(define (root-nth x n)
  (fixed-point
   (repeated
    (average-damp (lambda (y) (/ x (nth y (- n 1)))))
    (repeated-times n))
   1.0))

(define (nth y n)
  (if (= n 1)
      y
      (* y (nth y (- n 1)))))

(define (repeated f n)
  (lambda (x)
    (if (= n 1)
        (f x)
        (f ((repeated f (- n 1)) x)))))

(define (repeated-times n)
  (floor (/ (log n) (log 2))))

;; 2 1th
;; 3 1th
;; 4 2th
;; 8 3th
;; 9 3th
;
; floor (log 2 n)
(root-nth 2 9)

(repeated-times 2)
(repeated-times 4)
(repeated-times 5)
