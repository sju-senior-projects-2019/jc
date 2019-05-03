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
          Any
          any?
          product-type)

;Helper function, just answers if a thing is a member of a list
(define (elem? a l) 
  (cond [(null? l) #f]
        [(eq? (car l) a) #t]
        [else (elem? a (cdr l))]))

;First class and, or so that they can be used as higher order functions
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

;Macro to create the simple types. It is wasteful to put the formating enforcers on types I define.
;First creates the Identifier macto and then provides that predicate to the hawk langauge namespace
(define-syntax (define-base-type stx)
  (syntax-parse stx
    [(_ name pred)
     #'(begin
        (define-syntax name
         (lambda (stx)
           (syntax-case stx ()
             [name #'pred])))
        (provide name pred))]))

;Creating simple types
(define-base-type Int int?)
(define-base-type Real real?)
(define-base-type Bool bool?)
(define-base-type Num num?)
(define-base-type List list?)
(define-base-type Pair pair?)
(define-base-type Symbol symbol?)
(define-base-type String string?)
(define-base-type Void void?)


;Alters enviorment variables on define so all type created predicates and type names are formated correctly 
(define-for-syntax (alter name case form)
  (datum->syntax
     name
       (string->symbol
        (case
         (format form
                 (syntax->datum
                   name))))))

;This is how users can create their own types.
;It defines both the identifier macro and the predicate thats satisfies all the sub-predicates provided by the user
;in a way which both the type name and the predicate are named due to the convention of
;Predicates lowercase?, and Type-names Titlecase
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

;Used in conjuntion with define-type, this creates a predicate that will be supplied to define type which acts as the sum-type from type theory
;Can be used outside of define-type for flexibility's sake
(define-syntax (sum-type stx)
  (syntax-parse stx
    [(_ pred ...)
     #'(lambda (x) (or (pred x) ...))]))

;This creates a predicate that makes sure that every element of a list obey's the predicates supplied by the user
(define-syntax (list-type stx)
  (syntax-parse stx
    [(_ pred ...)
     #'(lambda (lst) (apply and (map (lambda (x) (and (pred x) ...)) lst )))]))

;Similar to list-type but with pairs. more of a quality of life macro then anything. You could use product type but this is faster.
(define-syntax (pair-type stx)
  (syntax-parse stx
    [(_ (pred1 ...) (pred2 ...))
     #'(lambda (x) (and (pred1 (car x)) ... (pred2 (cdr x)) ...))]))

;Same as sum-type only for product-types
(define-syntax (product-type stx)
  (syntax-parse stx
    [(_ pred ...)
     (with-syntax
        ([pred-list #'(list pred ...)])
       #'(lambda (x) (apply and
                            (map (lambda (y) (eval y (make-base-namespace)))
                                 (for/list ([f pred-list] [v x]) (f v))))))]))
;This is just a 'I do not care what type this is please just return something or nothing' type. Should really only be used for debugging.
(define-type any ((lambda (x) #t)))