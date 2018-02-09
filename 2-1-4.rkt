#lang racket
(display "##############################################\n")
(define (test str)
 (display "###### Testing Ex ")
 (display str)
 (display " ######")
 (newline))

; Ex 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound interval)
 (car interval))
(define (lower-bound interval)
 (cdr interval))

;; Alyssa Hacker's program
(define (add-interval x y)
 (make-interval (+ (lower-bound x) (lower-bound y))
                (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
 (let ((p1 (* (lower-bound x) (lower-bound y)))
       (p2 (* (lower-bound x) (upper-bound y)))
       (p3 (* (upper-bound x) (lower-bound y)))
       (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))

; Modified in Ex 2.10
; (define (div-interval x y)
;  (mul-interval x
;                (make-interval (/ 1.0 (upper-bound y))
;                               (/ 1.0 (lower-bound y)))))

; Ex 2,8
(define (sub-interval x y)
 (add-interval x
               (make-interval -(upper-bound y) -(lower-bound y))))
; Ex 2.9
(define (width interval)
 (/ (- (upper-bound interval) (lower-bound interval)) 2))

; Ex 2.10
(define (div-interval x y)
 (if (or (= 0 (lower-bound y)) (= 0 (upper-bound y)))
    (error "Cannot divide by zero!")
    (make-interval x
                   (make-interval (/ 1.0 (upper-bound y))
                                  (/ 1.0 (lower-bound y))))))
; Ex 2.11

; Ex 2.12
(define (make-center-percent c p)
 (make-interval (* c (- 1 p))
                (* c (+ 1 p))))
(define (center i)
 (/ (+ (upper-bound i) (lower-bound i)) 2))
(define (percent i)
 (let ((u (upper-bound i))
       (l (lower-bound i)))
  (/ (- u l) (+ u l))))

; Ex 2.13
; p_1 + p_2

; Ex 2.14
;

