#lang racket


(require racket (for-syntax racket/syntax syntax/parse))
(provide (except-out (all-from-out racket) define)
         (rename-out [define~ define])
         define-type
         Real)

;Test macro 
(define-syntax Real
    (lambda (stx)
      (syntax-case stx ()
        [Real (identifier? #'Real) #'real?])))

;Implements dynamic typechecking
(define-syntax (define~ stx)
  (syntax-parse stx
    [(_ name:id type:expr x:expr)
     #'(define/contract name type x)]

    [(_ (name:id args:id ...)
        typedef:expr
        body)
     #'(define/contract
         (name args ...)
         typedef
         body)]))

(define-syntax (define-type stx)
  (syntax-parse stx
    [(_ name:id (pred ...))
     (with-syntax
        ([pred-name
          (datum->syntax
           #'name
           (string->symbol
            (string-downcase
             (format "~a?" (syntax->datum #'name)))))])
     #''(begin
         (define (pred-name arg)
           (and (pred arg) ...))))]))



(define-type NUM (number? real?))
