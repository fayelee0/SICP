#lang racket

;; 1. n is a prime
;; 2. a > 0 && a < n
;; 3. a^(n-1) % n = 1
;
;  1. random number a < n
;  2. a^(n-1) % n
;  3. v = (square ...), v != 1 && v != (n - 1) and v % n = 1, v is not prime

(define square (lambda (x) (* x x)))

(define (nontrivial v n)
  (and (not (= v 1))
       (not (= v (- n 1)))
       (= (remainder (square v) n) 1)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let ((v (expmod base (/ exp 2) m)))
           (if (nontrivial v m)
               0
               (remainder (square v) m))))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (mr-test n)
  (let ((try-it (lambda (a)
                  (= (expmod a (- n 1) n) 1))))
    (try-it (+ 1 (random (- n 1))))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((mr-test n) (fast-prime? n (- times 1)))
        (else #f)))

; fault number
(not (fast-prime? 561  4))
(not (fast-prime? 1105 4))
(not (fast-prime? 1729 4))
(not (fast-prime? 2465 4))
(not (fast-prime? 2821 4))
(not (fast-prime? 6601 4))

(fast-prime?  3 4)
(fast-prime? 11 4)