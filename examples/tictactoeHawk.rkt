#lang hawk

(define-type piece ((lambda (x) (or (eq? x 'x) (eq? x 'o) (eq? x '-)))))
(define-type row ((list-type piece?)))
(define-type rowlist ((list-type row?)))
(define-type board ((lambda (lst) (and (apply and (map (lambda (x) (row? x)) lst )) (= (length lst) 3)))))
(define-type herpair ((pair-type [(lambda (x) (or (board? x) (null? x)))] [int?])))
(define-type movelist ((list-type board?)))



;-----This is how you play the game------------------------------------------------------------------------------------------------------------------------
(define (start)
  (-> Void)
  (define (play depth turn board)
    (-> Int Int Board Void) 
    (define (ai depth board)
      (-> Int Board Board)
      (next-move depth board))
    (define (player board)
      (-> Board Board)
      (let ((r (begin (print "please input row!") (string->number (read-line))))
            (c (begin (print "please input col!") (string->number (read-line)))))
        (if (or (> r 2) (> c 2)
                (< r 0) (< c 0)
                (not (eq? (index r c board) '-)))
            (begin (print "Invalid row or col! \n") (player board))
            (place r c 'o board))))    
    (begin (print-board board)
            (cond ((win? 'x board) (print "X wins!"))
                  ((win? 'o board) (print "O wins!"))
                  ((fill? board) (print "It's a tie!"))
                  (else (play depth (add1 turn) (if (even? turn)
                                                    (ai depth board)
                                                    (player board)))))))
  (play 3 0 '((- - -) (- - -) (- - -))))


(define max-num (Int) 100)
(define min-num (Int) (- 100))

;-----Minimax algorithm--------------------------------------------------------------------------------------------------------------------------------------
(define (next-move depth board)
  (-> Int Board Board)
  (let ((moves (generate-moves 'x board)))
    (car (foldr max-pair  ;find the best pair where the car is the closted move and cdr is the deep evaluation
                (cons '() min-num) 
                (map cons moves
                          (map (lambda (l) (evaluate 'x depth l))
                          moves))))))
  (define (evaluate p depth board)
    (-> Piece Int Board Int)
    (let ((best (if (x? p) min max))) ;for switching between min and max player in procedure calls 
      (cond ((win? 'x board) (+ max-num depth)) 
            ((win? 'o board) (- min-num depth))
            ((fill? board) 0)
            ((zero? depth) (heuristic board))
            (else (apply best (map (lambda (l) (evaluate (switch-player p) (sub1 depth) l))
                              (generate-moves (switch-player p) board)))))))  



;-----Utility functions---------------------------------------------------------------------------------------------------------------
(define (print-board l)
  (-> Board Void)
  (map (lambda (x) (display x) (newline)) l) (newline))

(define (elem? a l)
  (-> Any List Bool)
  (cond ((null? l) #f)
        ((eq? (car l) a) #t)
        (else (elem? a (cdr l)))))


(define (max-pair x y)
  (-> Herpair Herpair Herpair)
  (if (> (cdr x) (cdr y)) x y))

(define (x? p)
  (-> Piece Bool)
  (if (eq? p 'x) #t #f))

(define (switch-player p)
  (-> Piece Piece)
  (if (eq? p 'x) 'o 'x))

(define (main-diagnal board)
  (-> Board Row)
  (if (null? board) '() 
      (cons (car (car board)) (main-diagnal (map cdr (cdr board))))))

(define (diagnalD board) (-> Board Row)(main-diagnal board))
(define (diagnalU board) (-> Board Row)(main-diagnal (reverse board)))
(define (vertical board) (-> Board Board)(apply map list board))
(define (all-wins board)
  (-> Board Rowlist)
  (cons (diagnalD board) (cons (diagnalU board) (append board (vertical board)))))


(define (index x y board)
  (-> Int Int Board Piece)
  (list-ref (list-ref board x) y))

(define (fill? l)
  (-> Board Bool)
  (not (elem? '- (apply append l))))

;Returns true iff every elment in the list is equal and not an empty space
(define (eqlist? a l)
  (-> Piece Row Bool)
  (cond ((null? l) #t)
        ((not (eq? a (car l))) #f)
        (else (eqlist? a (cdr l)))))

;Checks if someone won
(define (win? p board)
  (-> Piece Board Bool)
  (elem? #t (map (lambda (b) (eqlist? p b)) (all-wins board))))

(define (place x y piece board)
  (-> Int Int Piece Board Board)
  (define (row-constructor y l)
    (-> Int Row Row)
    (cond ((null? l) '())
          ((= y 0) (cons piece (cdr l)))
          (else (cons (car l) (row-constructor (- y 1) (cdr l))))))
  (cond ((null? board) '())
        ((= x 0) (cons (row-constructor y (car board)) (cdr board)))
        (else (cons (car board) (place (- x 1) y piece (cdr board))))))

(define (win-num player board)
  (-> Piece Board Int)
  (apply + (map (lambda (l) (if (not (elem? (switch-player player) l)) 1 0))
                (all-wins board))))

(define (heuristic board)
  (-> Board Int)
  (- (win-num 'x board) (win-num 'o board)))


(define (generate-moves p board)
  (-> Piece Board Movelist)
  (let ((n (length board)))
    (do ((states '())
         (i 0 (add1 i)))
        ((= n i) states)
      (do ((j 0 (add1 j)))
          ((= n j) '())
        (cond ((eq? (index i j board) '-)
                (set! states (cons (place i j p board)
                                   states))))))))
