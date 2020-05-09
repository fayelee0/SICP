#lang racket


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

(define square
  (lambda (x)
    (* x x)))

(define sqrt
  (lambda (x)
    (sqrt-iter 1.0 x)))

(define sqrt-plus
  (lambda (x)

    (define sqrt-iter
      (lambda (guess)
        (if (good-enough? guess)
            (improve guess)
            (sqrt-iter (improve guess)))))

    (define improve
      (lambda (guess)
        (average guess (/ x guess))))

    (define good-enough?
      (lambda (guess)
        (= guess (improve guess))))
    
    (sqrt-iter 1.0)))


(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(sqrt (sqrt 1000))

(sqrt-plus 9)
(sqrt-plus (+ 100 37))
(sqrt-plus (+ (sqrt-plus 2) (sqrt-plus 3)))
(sqrt-plus (sqrt-plus 1000))

;; 最小精度是 0.001 无法计算最小精度
(sqrt 0.001)        
(sqrt-plus 0.001)

;; 编译器的编译精度 guess 和 (improve guess) 差值无法降低到精度已下
; (sqrt 1e13)
(sqrt-plus 1e13)