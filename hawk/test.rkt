#lang hawk

;(define-type foo ((sum-type Symbol Num)))
(define-type test ((list-type num?  (lambda (x) (> x 5))  )))
(define foo (Test) '(6 6 6))
