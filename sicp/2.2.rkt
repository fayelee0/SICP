#lang racket

;; points
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

;; segments
(define (make-segment sp ep) (cons sp ep))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (average x y) (/ (+ x y) 2.0))

(define (mid-point-segment seg)
  (let ((sp (start-segment seg))
        (ep (end-segment seg)))
    (make-point
     (average (x-point sp) (x-point ep))
     (average (y-point sp) (y-point ep)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ".")
  (display (y-point p))
  (display ")"))

(print-point
 (mid-point-segment
  (make-segment
   (make-point 1 1)
   (make-point 4 4))))

(print-point
 (mid-point-segment
  (make-segment
   (make-point -3 -5)
   (make-point 4 7))))