#lang sicp

(define (square x)
  (* x x))

"1.2.6"

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        ;(else (find-divisor n (+ test-divisor 1)))))
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))


"Exercise 1.23"
(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2))
  )

(define (prime? n)
  (= n (smallest-divisor n)))

(display "13 is prime?")
(prime? 13)

(display "12 is prime?")
(prime? 12)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

"Exercise 1.21"
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

"Exercise 1.22"
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime))
  )

(define (start-prime-test n start-time)
  ;(if (prime? n)
  (if (fast-prime? n 3)
      (report-prime (- (runtime) start-time))
      #f)
  )

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start count)
  (cond ((= count 0) )
        ((timed-prime-test start) (search-for-primes (+ start 2) (dec count)))
        (else (search-for-primes (+ start 2) count))))

;(search-for-primes 1001 3)
;(search-for-primes 10001 3)
;(search-for-primes 100001 3)
;(search-for-primes 1000001 3)

"Exercise 1.24"
(search-for-primes 1001 3)
(search-for-primes 1000001 3)

"Exercise 1.25"

"Exercise 1.26"

"Exercise 1.27"

"Exercise 1.28"
(define (apply-trivial-check k m r) 
  (if (and (not (= k 1)) 
           (not (= k (- m 1))) 
           (= r 1)) 
      0 
      r))
(define (remainder-or-trivial k m) 
  (apply-trivial-check k m (remainder (square k) m)))
(define (modified-expmod base exp m) 
  (cond ((= exp 0) 1) 
        ((even? exp) 
         (remainder-or-trivial (modified-expmod base (/ exp 2) m) m)) 
        (else 
         (remainder (* base (modified-expmod base (- exp 1) m)) 
                    m))))
(define (miller-rabin-test n) 
  (define (miller-rabin-iteration a t n) 
    (define (try-it a) 
      (= (modified-expmod a (- n 1) n) 1)) 
    (if (= a n) 
        (> t (/ n 2)) 
        (if (try-it a) 
            (miller-rabin-iteration (+ a 1) (+ t 1) n) 
            (miller-rabin-iteration (+ a 1) t n)))) 
  (miller-rabin-iteration 1 0 n))
(miller-rabin-test 1003) 
(miller-rabin-test 1009)

(miller-rabin-test 2821)
(miller-rabin-test 6601) 


