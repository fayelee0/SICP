#lang racket

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

;; v1
(define (make-rect.v1 left-bottom-point right-top-point)
  (cons left-bottom-point right-top-point))
(define (left-bottom-point rect) (car rect))
(define (right-top-point rect) (cdr rect))

(define (high.v1 rect)
  (- (y-point (right-top-point rect))
     (y-point (left-bottom-point rect))))
(define (width.v1 rect)
  (- (x-point (right-top-point rect))
     (x-point (left-bottom-point rect))))

(define (perimeter.v1 rect)
  (* 2 (+ (high.v1 rect)
          (width.v1 rect))))
(define (area.v1 rect)
  (* (high.v1 rect) (width.v1 rect)))

(define rect.v10 (make-rect.v1 (make-point 1 1)
                              (make-point 4 4)))

(perimeter.v1 rect.v10)
(area.v1 rect.v10)

(define rect.v11 (make-rect.v1 (make-point -10 -10)
                               (make-point 3 7)))

(perimeter.v1 rect.v11)
(area.v1 rect.v11)

;; v2
(define (make-rect.v2 left-bottom width height)
  (cons left-bottom (cons width height)))
(define (point.v2 rect) (car rect))
(define (width.v2 rect) (car (cdr rect)))
(define (high.v2 rect) (cdr (cdr rect)))

(define (perimeter.v2 rect)
  (* 2 (+ (high.v2 rect)
          (width.v2 rect))))
(define (area.v2 rect)
  (* (high.v2 rect)
     (width.v2 rect)))

(define rect.v20 (make-rect.v2 (make-point 1 1) 3 3))

(perimeter.v2 rect.v20)
(area.v2 rect.v20)

(define rect.v21 (make-rect.v2 (make-point -10 -10) 13 17))

(perimeter.v2 rect.v21)
(area.v2 rect.v21)

;; cons.v1
(define (cons.v1 x y)
  (lambda (m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m)))))
(define (car.v1 z) (cons.v1 0))
(define (cdr.v1 z) (cons.v1 1))