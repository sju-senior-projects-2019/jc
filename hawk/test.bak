#lang hawk

(define (elem? a l); a -> [a] -> Bool
    (cond ((null? l) #f)
          ((eq? (car l) a) #t)
          (else (elem? a (cdr l)))))

(define (main-diagnal board); creates a single list with the main diagonal of board
    (if (null? board) '() 
        (cons (car (car board)) (main-diagnal (map cdr (cdr board))))))

(define (diagnalD board) (main-diagnal board))
(define (diagnalU board) (main-diagnal (reverse board)))
(define (vertical board) (apply map list board));Does the same as horizontal except it first transforms board in to its transpose

(define (all-wins board)
  (cons (diagnalD board) (cons (diagnalU board) (append board (vertical board)))))

(define (fill? l)
  (not (elem? '- (apply append l))))

(define (eqlist? a l); [Atom] -> Bool
  (cond ((null? l) #t)
        ((not (eq? a (car l))) #f)
        (else (eqlist? a (cdr l)))))

;Checks if someone won
(define (win? p board); :: [[Atom]] -> Bool
  (elem? #t (map (lambda (b) (eqlist? p b)) (all-wins board))))

(win? 'x '((x - -) (- x -) (- - x))) ; #t
(win? 'x '((- - x) (- x -) (x - -))) ; #t
(win? 'x '((- x -) (- x -) (- - -))) ; #f
(fill? '((- - -) (- - -) (- - -)))   ; #f
(fill? '((x o x) (o o x)  (x o o)))  ; #t
(fill? '((x o x) (o - x)  (x o o)))  ; #f