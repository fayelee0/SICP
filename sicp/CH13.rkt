#lang racket

;; Formulating Abstractions with Higher-Order Procedurres

(define (cube x) (* x x x))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(sum-integers 1 100)

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))

(sum-cubes 1 100)

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

(* 8.0 (pi-sum 1 100000))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (identity n) n)

(define (sum-integers- a b) (sum identity a inc b))
(= (sum-integers 1 100) (sum-integers- 1 100))

(define (sum-cubes- a b) (sum cube a inc b))
(= (sum-cubes 1 100) (sum-cubes- 1 100))

(define (pi-sum- a b)
  (let ((pi-term (lambda (x)
                   (/ 1.0 (* x (+ x 2)))))
        (pi-next (lambda (x)
                   (+ x 4))))
    (sum pi-term a pi-next b)))

(= (pi-sum 1 100000) (pi-sum- 1 100000))

;; integral of a function f
(define (integral f a b dx)
  (let ((add-dx (lambda (x) (+ x dx))))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
       dx)))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

;; Using let to create local variables

(define square (lambda (x) (* x x)))

(define (f.v1 x y)

  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))

  (f-helper (+ 1 (* x y))
            (- 1 y)))

(define (f.v2 x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f.v3 x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(= (f.v1 1 2)
   (f.v2 1 2))

(= (f.v3 1 2)
   (f.v2 1 2))

;; Procedures as General Methods

(define average (lambda (x y) (/ (+ x y) 2.0)))

(define close-enough? (lambda (x y) (< (abs (- x y)) 0.001)))

(define (search f np pp)
  (let ((mp (average np pp)))
    (if (close-enough? np pp)
        mp
        (let ((tv (f mp)))
          (cond ((positive? tv)
                 (search f np mp))
                ((negative? tv)
                 (search f mp pp))
                (else mp))))))

(define half-interval-method
  (lambda (f a b)
    (let ((av (f a))
          (bv (f b)))
      (cond ((and (negative? av) (positive? bv)) (search f a b))
            ((and (negative? bv) (positive? av)) (search f b a))
            (else (error "Values are not of opposite sign" a b))))))

(half-interval-method sin 2.0 4.0)

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0)

;; Finding fixed points of functions
;  (= x (f x))
;  (= x (f (f x)))
;  ...

(define tolerance 0.00001)

(define (fixed-point f guess)

  (define (try g)
    (let ((next (f g)))
      (if (close-enough? g next)
          next
          (try next))))

  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  
  (try guess))

(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

; (define (sqrt x)
;   (fixed-point (lambda (y) (/ x y))
;                1.0))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(sqrt 2)