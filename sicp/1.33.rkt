#lang racket

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (filtered-accumulate predicate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (predicate (term a)) (term a) null-value)
                (filtered-accumulate predicate
                                     combiner
                                     null-value
                                     term
                                     (next a)
                                     next
                                     b))))

(define (filtered-accumulate.v2 predicate combiner null-value term a next b)
  (letrec ((iter (lambda (na rt)
                   (if (> na b)
                       rt
                       (iter (next na)
                             (combiner (if (predicate (term na)) (term na) null-value) rt))))))
    (iter a null-value)))

(filtered-accumulate even?
                     +
                     0
                     (lambda (x) x)
                     1
                     (lambda (x) (+ x 1))
                     100)

(filtered-accumulate.v2 even?
                        +
                        0
                        (lambda (x) x)
                        1
                        (lambda (x) (+ x 1))
                        100)

;; 1
(define (prime? n)

  (define (fast-prime? times)
    (cond ((= times 0) #t)
          ((mrt) (fast-prime? (- times 1)))
          (else #f)))

  (define (mrt)
    (let ((try (lambda (a)
                 (= (expmod a (- n 1)) 1))))
      (try (+ (random n) 1))))

  (define (expmod base exp)
    (cond ((= exp 0) 1)
          ((even? exp)
           (let ((v (expmod base (/ exp 2))))
             (if (nontrivial v)
                 0
                 (remainder (square v) n))))
          (else
           (remainder (* base (expmod base (- exp 1))) n))))

  (define (nontrivial v)
    (and (not (= v 1))
         (not (= v (- n 1)))
         (= (remainder (square v) n) 1)))
  
  (fast-prime? 4))

(define (square x) (* x x))

(define (f1 a b)
  (filtered-accumulate prime?
                       (lambda (x rt) (+ (square x) rt))
                       0
                       (lambda (x) x)
                       a
                       (lambda (x) (+ x 1))
                       b))

(f1 1 100)

;; 2
(define (gcd x y)
  (if (= y 0)
      x
      (gcd y (remainder x y))))

(= (gcd 206 40) 2)

(define (f2 n)
  (filtered-accumulate (lambda (x) (= (gcd x n) 1))
                       *
                       1
                       (lambda (x) x)
                       1
                       (lambda (x) (+ x 1))
                       (- n 1)))

(f2 10)