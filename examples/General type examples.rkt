#lang hawk

(define-type list-num>5 ((list-type num? (lambda (x) (> x 5)))))
(define brett (List-Num>5) '(6 7 8 6 10))

(define-type num-or-symbol ((sum-type num? symbol?)))
(define anna-maria (Num-Or-Symbol) 10)

(define-type num-symbol-pair ((pair-type (num? (lambda (x) (> x 5))) (symbol?))))
(define allison (Num-Symbol-Pair) '(6 . d))

(define-type num-and-int (int? num?))
(define nick (Num-And-Int) 3)