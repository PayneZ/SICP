#lang racket
; half-interval method
(define (search f neg-point pos-point)
  (let ((mid-point (/ (+ neg-point pos-point) 2)))
    (if (close-enough? neg-point pos-point)
        mid-point
        (let ((test-value (f mid-point)))
          (cond ((positive? test-value)
                 (search f neg-point mid-point))
                ((negative? test-value)
                 (search f mid-point pos-point))
                (else mid-point))))))
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))
; fixed point
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Ex 1.35
(display "Ex 1.35\n")
(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
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
          (try next))))
  (try first-guess))
(fixed-point1 (lambda (x) (/ (log 1000) (log x))) 4)

; Ex 1.37
(display "Ex 1.37\n")
(define (cont-frac n d k)
)