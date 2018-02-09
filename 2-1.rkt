#lang racket
(display "##############################################\n")
(define (test str)
 (display "###### Testing Ex ")
 (display str)
 (display " ######")
 (newline))

(define (cons a b)
  (define (dispatch n)
    (cond ((= n 0) a)
          ((= n 1) b)))
  dispatch)

(define (car c)
 (c 0))
(define (cdr c)
 (c 1))

; Ex 2.1
(test "2.1")
(define (make-rat n d)
  (let ((sign (sgn (* n d)))
        (nn (abs n))
        (dd (abs d)))
    (let ((g (gcd nn dd)))
       (cons (* sign (/ nn g)) (/ dd g)))))

(define (display-rat rat)
  (display "rational: ")
  (display (car rat))
  (display "/")
  (display (cdr rat))
  (newline))

(define a (make-rat -9 16))
(display-rat a)

; Ex 2.2
;; Segment constructor and selectors
(define (make-segment sp ep)
 (cons  sp ep))
(define (start-segment sg)
 (car sg))
(define (end-segment sg)
 (cdr sg))
(define (print-segment sg)
 (display "Start-point: ")
 (print-point (start-segment sg))
 (display "End-point: ")
 (print-point (end-segment sg)))
(define (midpoint-segment sg)
 (mid-point (start-segment sg)
            (end-segment sg)))
;; Point constructor and selectors
(define (make-point x y)
 (cons  x y))
(define (x-point p)
 (car p))
(define (y-point p)
 (cdr  p))
(define (print-point p)
 (display "(")
 (display (x-point p))
 (display ",")
 (display (y-point p))
 (display ")")
 (newline))
(define (mid-point p1 p2)
 (make-point (/ (+ (x-point p1) (x-point p2)) 2)
             (/ (+ (y-point p1) (y-point p2)) 2)))
;;  Test 2.2
(test "2.1")
(define p1 (make-point 0 0))
(define p2 (make-point 1 1))
(define sg (make-segment p1 p2))
(print-segment sg)
(print-point (midpoint-segment sg) )

; Ex 2.3

; Ex 2.4
(define (cons2 x y)
 (lambda (m) (m x y)))
(define (car2 z)
 (z (lambda (p q) p)))
(define (cdr2 z)
 (z (lambda (p q) q)))
;;  Test 2.4
(test 2.4)
(define pair (cons2 1 2))
(display (car2 pair))
(newline)
(display (cdr2 pair))

; Ex 2.5
(define (pow b i)
 (define (pow-iter n res)
  (if (= n i)
    res
    (pow-iter (+ n 1) (* res b))))
 (pow-iter 1 1))
(define (factor b N)
 (define (factor-iter n res)
  (if (not (= 0 (remainder n b)))
    res
    (factor-iter (/ n b) (+ res 1))))
 (factor-iter N 1))

(define (cons3 a b)
 (* (pow 2 a) (pow 3 b)))
(define (car3 z)
 (factor 2 z))
(define (cdr3 z)
 (factor 3 z))

;;  Test 2.5
(test 2.5)
(define aa (cons3 3 4))
(display aa)
(newline)
(display (car3 aa))
(newline)
(display (cdr3 aa))

; Ex 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
 (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add n1 n2)
 (lambda (f) (lambda (x) ((n1 f) ((n2 f) x)))))
;;  Test 2.6
(test 2.6)
(define one1 (add-1 zero))
(define two1 (add-1 one1))
(define four (add two two))
(define (f x)
 (+ x 1))
((zero f) 0)
((one f) 0)
((one1 f) 0)
((two f) 0)
((two1 f) 0)
((four f) 0)


