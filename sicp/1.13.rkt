#lang racket

;; solution
;; https://codology.net/post/sicp-solution-exercise-1-13/

φ 
ψ

;; 1. 证明 fib = (φ^n - ψ^n) / 5^.5
;;
;; 从这里推导出 fib(n) = fib(n-1) + fib(n-2)
;;
;; 2. 证明 fib ~ ψ^5 / 5^.5
;;
;; fib(n) < fib(n) + ψ^n/5^.5 < fib(n) + 1