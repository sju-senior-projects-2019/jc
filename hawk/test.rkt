#lang hawk

(define (f g x)
  (-> Real (-> Real Real) Real)
  (g x))