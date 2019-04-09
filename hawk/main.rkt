#lang br

(define (read-syntax path port)
  (define src-lines (port->lines port))
  0)

(define (count-whitespace lst)
  (if (or (null? lst) (not (eq? (car lst) #\space)))
      0
      (+ (count-whitespace (cdr lst)) 1)))

(define (.? lst)
  (if (and (not (null? lst)) (eq? (car lst) #\.))
      #t
      #f))
