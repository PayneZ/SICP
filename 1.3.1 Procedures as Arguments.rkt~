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

