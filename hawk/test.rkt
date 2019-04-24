#lang hawk

;(define-type foo ((sum-type Symbol Num)))
(define-type test ((sum-type num? symbol?)))
(define foo (Test) 'f)

;(sum-type num? symbol?)