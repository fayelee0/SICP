#lang racket

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      #f))

(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time)
  #t)

(define (runtime)
  (current-inexact-milliseconds))

(define (prime? n)
  (let ((square (lambda (x) (* x x)))
        (divides? (lambda (a b) (= (remainder b a) 0))))
    (letrec ((find-divisor
              (lambda (n test-divisor)
                (let ((square (lambda (x) (* x x))))
                  (cond ((> (square test-divisor) n) n)
                        ((divides? test-divisor n) test-divisor)
                        (else (find-divisor n (+ test-divisor 1))))))))
      (let ((smallest-divisor
             (lambda (n) (find-divisor n 2))))
        (= n (smallest-divisor n))))))

(define (search-for-primes n)
  (define iter
    (lambda (n c)
      (if (timed-prime-test n)
          (and (< (+ c 1) 3) (iter (+ n 1) (+ c 1)) (newline))
          (iter (+ n 1) c))))
  (iter n 0))

(search-for-primes 1000) (newline)
(search-for-primes 10000) (newline)
(search-for-primes 100000) (newline)
(search-for-primes 1000000) (newline)


;1009***0.002197265625
;1013***0.0029296875
;1019***0.0029296875

;10007***0.008056640625
;10009***0.007080078125
;10037***0.007080078125

;100003***0.02392578125
;100019***0.02197265625
;100043***0.031982421875

;1000003***0.069091796875
;1000033***0.06787109375
;1000037***0.068115234375

