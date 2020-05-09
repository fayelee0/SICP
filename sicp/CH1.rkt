#lang racket

;;;; 1. Building Abstractions with Procedures

;; The acts of the mind
;;
;; 1. combine
;; 2. relation
;; 3. abstract


;;; 1.1 The Elements of Programming

;; Three mechanisms
;;
;; 1. primitive expressions
;; 2. means of combination
;; 3. means of abstraction


(define size 2)
(define pi 3.14159)
(define radius 10)

(* pi (* radius radius))

(define circumference (* 2 pi radius))

circumference


;; To evaluate a combination, do the following:
;;
;; 1. Evaluate the sub-expressions of thee combination
;; 2. Apply the procedure that is the value of thee leftmost
;;    sub-expression (the operator) to the arguments that the
;;    values of the other sub-expressions (the operands).

;; Primitive
;;
;; 1. the values of numerals are the numbers that they name
;; 2. the values of build-in operators are the machine instruction
;;    sequences that carry out the corresponding operations
;; 3. the values of other names aree the objects associated with
;;    those names in the environment

;; Special form
;;
;; define
;; if
;; and
;; or
;;
;; <if>
;; To evaluate an if expression, the interpreter starts by evaluating
;; the <predicate> part of the expression.
;; If the <predicate> evaluates to a #t value, the interpreters then
;; evaluates the <consequent> and returns its value.
;; Otherwise it evaluates the <alternative> and returns its value.
;;
;; <and> <or>
;; The interpreter evaluates the expressions one at a time, in left-to-right order.

(define square (lambda (x) (* x x)))

(square 21)
(square (+ 2 5))
(square (square 3))

(define sum-of-squares
  (lambda (x y)
    (+ (square x) (square y))))

(sum-of-squares 3 4)

(define f
  (lambda (a)
    (sum-of-squares (+ a 1) (* a 2))))

(f 5)

;; Case analysis
;;
;; cond = conditional

(define abs-cond
  (lambda (x)
    (cond ((> x 0) x)
          ((= x 0) 0)
          ((< x 0) (- x)))))

(define abs-else
  (lambda (x)
    (cond ((< x 0) (- x))
          (else x))))

(define abs-if
  (lambda (x)
    (if (< x 0)
        (- x)
        x)))

;; sqrt

(define sqrt-iter
  (lambda (guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x))))

(define improve
  (lambda (guess x)
    (average guess (/ x guess))))

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

(define good-enough?
  (lambda (guess x)
    (< (abs (- (square guess) x)) 0.001)))

(define sqrt
  (lambda (x)
    (sqrt-iter 1.0 x)))

(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(sqrt (sqrt 1000))
