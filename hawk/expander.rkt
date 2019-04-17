#lang racket


(require (for-syntax racket/syntax syntax/parse)
         (rename-in racket [boolean? bool?]
                           [number? num?  ]
                           [exact-integer? int?]))
(provide (except-out (all-from-out racket) define)
         (rename-out [define~ define])
          define-type)

;Macro to create the simple types. It is wasteful to put the formating restraints on types I define
(define-syntax (define-base-type stx)
  (syntax-parse stx
    [(_ name pred)
     #'(begin
        (define-syntax name
         (lambda (stx)
           (syntax-case stx ()
             [name #'pred])))
        (provide name pred))]))

(define-base-type Real real?)
(define-base-type Bool bool?)
(define-base-type Num num?)
(define-base-type List list?)
(define-base-type Pair pair?)
(define-base-type Symbol symbol?)
(define-base-type String string?)
(define-base-type Vector vector?)
(define-base-type Hash hash?)
       

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
 
               




