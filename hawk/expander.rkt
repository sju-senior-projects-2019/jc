#lang racket


(require (for-syntax racket/syntax syntax/parse)
         (rename-in racket [boolean? bool?]
                           [number? num?  ]
                           [exact-integer? int?]))
(provide (except-out (all-from-out racket) define)
         (rename-out [define/contract define])
          define-type
          sum-type
          or
          and
          list-type
          pair-type
          Any)

(define (elem? a l)
  (cond [(null? l) #f]
        [(eq? (car l) a) #t]
        [else (elem? a (cdr l))]))

;First class and, or so that they can be used as function and macros
(define-syntax (or stx)
    (syntax-parse stx
     [(_:id)
      #'#f]
     [(_:id a b ...)
      #'(if a a (or b ...))]
     [_:id
      #'(lambda arg
          (elem? #t arg))]))

(define-syntax (and stx)
  (syntax-parse stx
    [(_:id)
     #'#t]
    [(_:id a b ...)
     #'(if a (and b ...) a)]
    [_:id
     #'(lambda arg
         (not (elem? #f arg)))]))

;Macro to create the simple types. It is wasteful to put the formating enforcers on types I define
(define-syntax (define-base-type stx)
  (syntax-parse stx
    [(_ name pred)
     #'(begin
        (define-syntax name
         (lambda (stx)
           (syntax-case stx ()
             [name #'pred])))
        (provide name pred))]))

(define-base-type Int int?)
(define-base-type Real real?)
(define-base-type Bool bool?)
(define-base-type Num num?)
(define-base-type List list?)
(define-base-type Pair pair?)
(define-base-type Symbol symbol?)
(define-base-type String string?)


;Alters enviorment variables on define so all type created predicates and type names are formated correctly 
(define-for-syntax (alter name case form)
  (datum->syntax
     name
       (string->symbol
        (case
         (format form
                 (syntax->datum
                   name))))))


(define-syntax (define-type stx)
  (syntax-parse stx
    [(_ name:id (pred ...))
        (with-syntax
         ([pred-name (alter #'name string-downcase "~a?")]
          [macr-name (alter #'name string-titlecase "~a")])
     #'(begin
         (define (pred-name arg)
           (and (pred arg) ...))
         (define-syntax macr-name
           (lambda (stx-m)
             (syntax-case stx-m ()
               [macr-name #'pred-name])))))]))

(define-syntax (sum-type stx)
  (syntax-parse stx
    [(_ pred ...)
     #'(lambda (x) (or (pred x) ...))]))

(define-syntax (list-type stx)
  (syntax-parse stx
    [(_ pred ...)
     #'(lambda (lst) (apply and (map (lambda (x) (and (pred x) ...)) lst )))]))

(define-syntax (pair-type stx)
  (syntax-parse stx
    [(_ (pred1 ...) (pred2 ...))
     #'(lambda (x) (and (pred1 (car x)) ... (pred2 (cdr x)) ...))]))

(define-type any ((lambda (x) #t)))