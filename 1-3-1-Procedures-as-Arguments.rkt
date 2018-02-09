#lang racket
; 1.29
(display "Ex 1.29\n")
(define (cube x)
  (* x x x))
(define (simpson f a b n)
  (define inc (/ (- b a) n))
  (define (simp-next x)
    (+ x inc))
  (define (simp-term x current)
    (cond ((= current 0) (f x))
          ((= current n) (f x))
          (else
           (define r (remainder current 2))
           (cond ((= r 1) (* 4 (f x)))
                 ((= r 0) (* 2 (f x)))))))
  (define (sum x current)
    (if (> current n)
        0
        (+ (simp-term x current)
           (sum (simp-next x) (+ current 1)))))
  (/ (* inc (sum a 0)) 3))
  

(simpson cube 0.0 1.0 100)
(simpson cube 0.0 1.0 1000)

; Ex 1.30
(display "Ex 1.30\n")
(define (sum term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x)
              (+ (term x) result))))
  (iter a 0))

; Ex 1.31
(display "Ex 1.31-a\n")
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
(define (factorial n)
  (define (id x) x)
  (define (inc x) (+ x 1))
  (product id 1 inc n))
(factorial 5)
(define (pi-prod n)
  (define (pi-term x)
    (define quo2 (quotient (+ x 1) 2))
    (define numer (* 2 (+ (quotient x 2) 1)))
    (define denom (+ 1
                     (* 2 quo2)))
    (/ numer denom))
    
  (define (next x)
    (+ x 1))
  (* 4.0
     (product pi-term 1 next n)))
(pi-prod 1000)

(display "Ex 1.31-b\n")
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

; Ex 1.32-a
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))
; Ex 1.32-b
(define (accu-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

; Ex 1.33
(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a)
                    (filtered-accumulate combiner
                                         null-value
                                         term
                                         (next a)
                                         next
                                         b
                                         filter))
          (combiner null-value
                    (filtered-accumulate combiner
                                         null-value
                                         term
                                         (next a)
                                         next
                                         b
                                         filter)))))

; 1.33-a
(define (prime-sum a b)
  (filtered-accumulate + 0 square a inc b prime?))
(define (inc x) (+ x 1))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (square x)
  (* x x))
(display "Ex 1.33-a\n")
(prime-sum 2 5)

; Ex 1.33-b
(define (strange-product n)
  (define (filter x)
    (= (gcd n x) 1))
  (filtered-accumulate * 1 identity 1 inc n filter))


