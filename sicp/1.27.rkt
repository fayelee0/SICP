#lang racket

;; 561
;; 1105
;; 1729
;; 2465
;; 2821
;; 6601

(define (expmod base exp m)
  (let ((square (lambda (x) (* x x))))
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder (square (expmod base (/ exp 2) m)) m))
          (else
           (remainder (* base (expmod base (- exp 1) m)) m)))))

(define (fermat-test n)
  (let ((try-it (lambda (a)
                  (= (expmod a n n) a))))
    (try-it (+ 1 (random (- n 1))))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

; fault number
(fast-prime? 561 4)
(fast-prime? 1105 4)
(fast-prime? 1729 4)
(fast-prime? 2465 4)
(fast-prime? 2821 4)
(fast-prime? 6601 4)