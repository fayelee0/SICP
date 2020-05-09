#lang racket

(define new-if
  (lambda (predicate then-clause else-clause)
    (cond (predicate then-clause)
          (else else-clause))))

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

(define sqrt-iter
  (lambda (guess x)
    (new-if (good-enough? guess x)
            guess
            (sqrt-iter (improve guess x) x))))

;; if use <new-if>
;;
;; 1. evaluate <predicate> <then-clause> <else-clause>
;; 2. infinite loop <else-clause>