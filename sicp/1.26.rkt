#lang racket

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder
                      (* (expmod base (/ exp 2) m)  ;; origin evaluate once, O(lg n) steps
                         (expmod base (/ exp 2) m)) ;; this one evaluate twice, O(n) steps
                      m))
        (else
         (remainder (* base
                       (expmod base (- exp 1) m))
                    m))))
                    