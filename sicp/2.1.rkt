#lang racket

(define (gcd n d)
  (if (= d 0)
      n
      (gcd d (remainder n d))))

(define (make-rat n d)
  (let ((nx (abs n))
        (dx (abs d))
        (sg (if (or (and (positive? n) (positive? d))
                    (and (negative? n) (negative? d)))
                +
                -)))
    (let ((g (gcd nx dx)))
      (cons (sg (/ nx g)) (/ dx g)))))

(make-rat 1 3)
(make-rat 3 6)
(make-rat -3 6)
(make-rat 3 -6)