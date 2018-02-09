#lang racket
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (average x y)
  (/ (+ x y) 2))
(define (fixed-point f first-guess)
  (define tolerance 0.0001)
  (define (close-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define (deriv g)
  (define dx 0.000001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

; Ex 1.40
(display "Ex 1.40\n")
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))
(newtons-method (cubic 0 0 -1) 1)

; Ex 1.41
(display "Ex 1.41\n")
(define (double f)
  (lambda(x) (f (f x))))
(define (inc x)
  (+ x 1))
(((double (double double)) inc ) 5)

; Ex 1.42
(display "Ex 1.42\n")
(define (compose f g)
  (lambda (x) (f (g x))))
(define (square x)
  (* x x))
((compose square inc) 6)

; Ex 1.43
(display "Ex 1.43\n")
(define (repeated f n)
  (define (iter-compose g m)
    (if (= m 1)
        g
        (iter-compose (compose f g) (- m 1))))
  (iter-compose f n))
((repeated square 2) 5)

; Ex 1.44
(display "Ex 1.44\n")
(define (smooth f)
  (define dx 0.00001)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))
(define (n-fold-smooth f n)
  ((repeated smooth n) f))

; Ex 1.45
(display "Ex 1.45\n")
