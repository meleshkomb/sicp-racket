#lang sicp

(define one-through-four (list 1 2 3 4))
one-through-four

(car one-through-four)
(cdr one-through-four)
(car (cdr one-through-four))
(cons 10 one-through-four)
(cons 5 one-through-four)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

"Exercise 2.17"
(define (last-pair list) 
  (if (null? (cdr list)) 
      list 
      (last-pair (cdr list))))
(last-pair (list 23 72 149 34))

"Exercise 2.18"
(define (reverse items) 
  (define (reverse-iter source result) 
    (if (null? source) 
        result 
        (reverse-iter (cdr source) (cons (car source) result)))) 
  (reverse-iter items (list)))
(reverse (list 1 4 9 16 25))

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5) 10)

"Exercise 2.21"
(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items))
            (square-list (cdr items)))))
(define (square-list-map items)
  (map (lambda (x) (* x x))
       items)
  )
(square-list (list 1 2 3 4))
(square-list-map (list 1 2 3 4))

"Exercise 2.23"
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))