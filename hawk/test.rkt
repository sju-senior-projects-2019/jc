#lang hawk


(define (f g x)
  (-> Real (-> Real Real) Real)
  (g x))

(define x Real 10)



(define (win? p board)
  (-> symbol? list? boolean?)
  (elem? #t (map (lambda (b) (eqlist? p b)) (all-wins board))))

(define (eqlist? a l)
  (-> any/c list? boolean?)
  (cond ((null? l) #t)
        ((not (eq? a (car l))) #f)
        (else (eqlist? a (cdr l)))))

(define (elem? a l)
  (-> any/c list? boolean?)
   (cond ((null? l) #f)
          ((eq? (car l) a) #t)
          (else (elem? a (cdr l)))))

(define (all-wins board)
  (-> list? list?)
  (cons (diagnalD board) (cons (diagnalU board) (append board (vertical board)))))

(define (main-diagnal board); creates a single list with the main diagonal of board
  (-> list? list?)
  (if (null? board) '() 
        (cons (car (car board)) (main-diagnal (map cdr (cdr board))))))

(define (diagnalD board)
  (-> list? list?)
  (main-diagnal board))

(define (diagnalU board)
  (-> list? list?)
  (main-diagnal (reverse board)))

(define (vertical board)
  (-> list? list?)
  (apply map list board))


(define (add2 x)
  (-> Real Real)
  (+ x 2))

(define g Real 10)