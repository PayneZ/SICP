#lang racket
(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try (f next)))))
  (try first-guess))

; Ex 1.35
(display "Ex 1.35\n")
(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
(display golden-ratio)

; Ex 1.36
(display "Ex 1.36\n")
(define (fixed-point1 f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try (f next)))))
  (try first-guess))
(fixed-point1 (lambda (x) (/ (log 1000) (log x))) 10)

; Ex 1.37
(display "Ex 1.37\n")
(define (cont-frac n d k)
  (define (term-i i)
    (if (= i k)
        (/ (n k) (d k))
        (/ (n i) ( + (d i) (term-i (+ i 1))))))
  (term-i 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)

; 1.38
(display "Ex 1.38\n")
(define (euler-d n)
  (let ((r (remainder n 3)))
    (cond ((= r 0) 1)
          ((= r 2) (* 2 (+ (quotient n 3) 1)))
          ((= r 1) 1))))
; 1.39
(display "Ex 1.39\n")
(define (tan-cf x k)
  (define (lambert-n n)
    (if (= n 1)
        x
        (- 0 (* x x))))
  (define (lambert-d n)
    (- (* 2 n) 1))
  (cont-frac lambert-n lambert-d k))
(tan-cf 0.7853 100000)