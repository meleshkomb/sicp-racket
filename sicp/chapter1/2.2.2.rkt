#lang sicp

"Exercise 2.28"
(define (fringe tree) 
  (cond ((null? tree) '()) 
        ((pair? tree) (append (fringe (car tree)) 
                      (fringe (cdr tree)))) 
        (else (list tree))))

(define x (list (list 10 2) (list 30 4)))
(fringe x)
;(1 2 3 4)
(fringe (list x x))
;(1 2 3 4 1 2 3 4)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

"Exercise 2.33"
(define (map p sequence) 
    (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append2 seq1 seq2) 
    (accumulate cons seq2 seq1))

(define (length sequence) 
    (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
