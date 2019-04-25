#lang hawk

(define-type foo ((lambda (x) (and (num? (car x)) (num? (cdr x)) (> (cdr x) (car x))))))
(define foo (Foo) '(3 . 5))