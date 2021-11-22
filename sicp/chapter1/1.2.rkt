#lang sicp


; 1.2

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 5)

"Excercise 1.9"

;(define (+ a b)
;  (if (= a 0)
;      b
;      (inc (+ (dec a) b))))

;(define (+ a b)
;  (if (= a 0)
;      b
;      (+ (dec a) (inc b))))

(+ 5 6)

"Excercise 1.10"
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

"Размен денег"
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  ;(printf "~a ~a \n" amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 100)

"Excercise 1.11"

(define (f n)
  (if (> n 3)
      n
      (+ (f (- n 1))
         (f (- n 2))
         (f (- n 3))))
  )

(f 272)

(define (f-iterative n)
  (define (f-i a b c count) 
     (if (= count 0)
         n 
         (f-i (+ a b c) a b (- count 1))))
   (f-i 2 1 0 n))
(f-iterative 272)

"Excercise 1.12"
(define (paskal row col)
  (if (or (= col 1) (= row col))
      1
      (+ (paskal (- row 1) col)
         (paskal (- row 1) (- col 1)))))

(paskal 5 3)


"Excercise 1.13"
;TODO:

"Excercise 1.14"
;TODO:

"Excercise 1.15"
(define (cube x)
  (* x x x))
(define (p x)
  (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.15)

"1.2.4"
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(expt 2 8)


(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(fast-expt 2 8)

"Excercise 1.16"
(define (fast-expt2 b n) 
  (fast-expt-iter 1 b n))
(define (fast-expt-iter a b n) 
  (if (= n 0) 
      a 
      (if (even? n) 
          (fast-expt-iter a
                          (square b) (/ n 2)) 
          (fast-expt-iter (* a b)
                          b (- n 1)))))
(fast-expt2 2 8)


"Excercise 1.17"
(define (fix-* a b)
  (define (double n)
    (+ n n))
  (define (halve n)
    (/ n 2))
  
  (cond ((= b 0) 0) 
        ((= b 1) a) 
        ((even? b) (double
                    (fix-* a
                            (halve b)))) 
        (else (+ a
                 (fix-* a
                         (- b 1)))))
  )
(fix-* 2 10)

"Excercise 1.18"
(define (fast-* a b) 
  (fast-*-iter a b 0))

(define (fast-*-iter a b c)
  (define (double n)
    (+ n n))
  (define (halve n)
    (/ n 2))
  
  (cond ((= b 0) c) 
        ((even? b) (fast-*-iter
                    (double a) (halve b) c)) 
        (else (fast-*-iter a
                           (- b 1) (+ c a))))
  )
(fast-* 30 30) 

"Excercise 1.19"

"Excercise 1.20"
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(gcd 206 40)

