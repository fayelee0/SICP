#lang racket

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; normal order, evaluate in (if (= b 0) ...) and in (else ...) not evaluate
;;
(gcd 206 40)

(gcd 40
     (remainder 206 40)) ; +1 for if

(gcd (remainder 206 40)
     (remainder 40 (remainder 206 40))) ; +2 for if

(gcd (remainder 40 (remainder 206 40))
     (remainder (remainder 206 40)
                (remainder 40 (remainder 206 40)))) ; +4 for if

(gcd (remainder (remainder 206 40)
                (remainder 40 (remainder 206 40)))
     (remainder (remainder 40 (remainder 206 40))
                (remainder (remainder 206 40)
                           (remainder 40 (remainder 206 40))))) ; +7 for if
; +7 for ret
; 1 + 2 + 4 + 7 + 7 = 21

; n, (f n) = (f (- n 1)) + (f (- n 2)) + 1

;; applicative order
; 4
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)
(gcd 4 (remainder 6 4))
(gcd 4 2)
(gcd 2 (remainder 4 2))
(gcd 2 0)
2