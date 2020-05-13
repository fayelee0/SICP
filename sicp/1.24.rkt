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

(define (prime? n)
  (fast-prime? n 4))

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

;1009***0.006103515625
;1013***0.007080078125
;1019***0.006103515625

;10007***0.008056640625
;10009***0.0078125
;10037***0.008056640625

;100003***0.010986328125
;100019***0.009033203125
;100043***0.010986328125

;1000003***0.009033203125
;1000033***0.010009765625
;1000037***0.010009765625