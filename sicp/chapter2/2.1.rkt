#lang sicp

"exercise 2.1.1 "
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define x (cons 66 78))
(car x) (cdr x)

(define y (cons 30 40))
(define z (cons x y))
(car (car z))
(car (cdr z))

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

"exercise 2.1"
(define (sign x) 
  (cond ((> x 0) 1) 
        ((< x 0) -1) 
        (else 0)))
(define (make-rat2 n d) 
  (let ((g (* (gcd (abs n) (abs d)) (sign d)))) 
    (cons (/ n g) (/ d g))))

"exercise 2.2"

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (make-segment start-point end-point) 
  (print-point start-point)
  (print-point end-point)
  (cons start-point end-point)
  )

(define (average a b)
  (/ (+ a b) 2.0))

(define (midpoint-segment segment) 
  (make-point (average (x-point (start-segment segment)) 
                       (x-point (end-segment segment))) 
              (average (y-point (start-segment segment)) 
                       (y-point (end-segment segment)))
              ))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(print-point
 (midpoint-segment
  (make-segment (make-point 1 1) (make-point 10 10)))
 )

"exercise 2.3"
(define (make-rectangle topleft bottomright) 
  (cons topleft bottomright))

(define (rectangle-topleft rectangle) 
  (car rectangle))

(define (rectangle-bottomright rectangle) 
  (cdr rectangle))

(define (rectangle-width rectangle) 
  (abs (- (x-point (rectangle-topleft rectangle)) 
          (x-point (rectangle-bottomright rectangle)))))

(define (rectangle-height rectangle) 
  (abs (- (y-point (rectangle-topleft rectangle)) 
          (y-point (rectangle-bottomright rectangle)))))
(define (rectangle-perimeter rectangle) 
  (* 2 (+ (rectangle-width rectangle) 
          (rectangle-height rectangle))))

(define (rectangle-area rectangle) 
  (* (rectangle-width rectangle) 
     (rectangle-height rectangle)))

(define (make-rectangle2 topleft width height) 
  (cons topleft (cons width height)))

(define (rectangle-topleft2 rectangle) 
  (car rectangle))

(define (rectangle-width2 rectangle) 
  (car (cdr rectangle)))

(define (rectangle-height2 rectangle) 
  (cdr (cdr rectangle)))

