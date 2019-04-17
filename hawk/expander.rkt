#lang racket


(require racket (for-syntax racket/syntax syntax/parse))
(provide (except-out (all-from-out racket) define)
         (rename-out [define~ define])
         )

(define-syntax (define-base-type stx)
  (syntax-parse stx
    [(_ name pred)
     #'(define-syntax name
         (lambda (stx)
           (syntax-case stx ()
             [name #'pred])))]))

(define-base-type Re real?)


;(define-syntax Real
;    (lambda (stx)
;      (syntax-case stx ()
;        [Real (identifier? #'Real) #'real?])))

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
     (define (rename case form)
       (datum->syntax
        #'name
        (string->symbol
         (case
          (format form
                  (syntax->datum
                   #'name))))))
        (with-syntax
         ([pred-name (rename string-downcase "~a?")]
          [macr-name (rename string-titlecase "~a")])
     #'(begin
         (define (pred-name arg)
           (and (pred arg) ...))
         (define-syntax macr-name
           (lambda (stx-m)
             (syntax-case stx-m ()
               [macr-name #'pred-name])))))]))
               



