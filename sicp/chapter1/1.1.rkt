#lang sicp
(+ (* 3 5) (- 10 6))
(+ (* 3
    (+ (* 2 4)
       (+ 3 5)))
   (+ (- 10 7)
    6))
(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
(* (+ 2 (* 4 6))
   (+ 3 5 7))

(define (square x) (* x x))
(square (square 3))

(define (sum-of-squares x y)
  (+ (square x) (square y)))
(sum-of-squares 3 4)

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
(abs -7)

(/
 (+ 5
    4
    (- 2
       (- 3
          (+ 6
             (/ 4 5)))))
 (* 3
    (- 6 2)
    (- 2 7)))



(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))


(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(new-if (= 1 1) 0 5)


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(sqrt 9)

(define (cube x) (* x x x))
(define (cube-iter guess x)
  (if (good-enough2? guess x)
      guess
      (cube-iter (improve2 guess x)
                 x)))
(define (improve2 guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3 ))
(define (good-enough2? guess x)
  (< (abs (- (cube guess) x)) 0.001))
(define (cubet x)
  (cube-iter 1.0 x))


(cubet 8)