#lang racket/base


(require racket (for-syntax racket/syntax syntax/parse))
(provide (except-out (all-from-out racket) define)
         define~
         Real)


(define-syntax Real
    (lambda (stx)
      (syntax-case stx ()
        [Real (identifier? #'Real) #'real?])))

(define-syntax (define~ stx)
  (syntax-parse stx
    [(_ (name:id args:id ...)
        typedef:expr
        body)
     #'(define/contract
         (name args ...)
         typedef
         body)]

    ))
