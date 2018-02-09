#lang racket
(display "##############################################\n")
(define (test str)
 (display "###### Testing Ex ")
 (display str)
 (display " ######")
 (newline))

; Code in the text
(define (append list1 list2)
 (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))
(define (length items)
 (define (length-iter item counter)
  (if (null? item)
      counter
      (length-iter (cdr item) (+ counter 1))))
 (length-iter items 0))

; Ex 2.17
(define (last-pair l)
 (if (null? (cdr l))
    l
    (last-pair (cdr l))))
(test 2.17)
(last-pair (list 23 72 149 34))

; Ex 2.18
(define (my-reverse l)
 (if (null? (cdr l))
      l
      (append (my-reverse (cdr l)) (list(car l)))))

;;  Test 2.18
(test 2.18)
(my-reverse (list 1 2))
(my-reverse (list 1 2 3))
(my-reverse (list 1 4 9 16 25))
(reverse (list 1 4 9 16 25))

; Ex 2.19
;; TODO

; Ex 2.20
(define (same-parity a . w)
 (define (same-parity-iter iter-list iter-result)
    (cond ((null? iter-list) iter-result)
          ((even? (- a (car iter-list)))
                (same-parity-iter (cdr iter-list)
                                  (append iter-result (list (car iter-list)))))
          (else (same-parity-iter (cdr iter-list) iter-result))))
 (same-parity-iter w (list a)))
;;  Test 2.20
(test 2.20)
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

;;;;;;;;;;;;;;;;;   Code In Text   ;;;;;;;;;;;;;;;;;;
(define (map proc items)
 (if (null? items)
     null
     (cons (proc (car items))
           (map proc (cdr items)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Ex 2.21
(define (square x)
 (* x x))
(define (square-list items)
 (if (null? items)
     null
     (cons (square (car items)) (square-list (cdr items)))))

(define (square-list2 items)
 (map square items))
;;  Test 2.21
(square-list (list 1 2 3 4))
(square-list2 (list 1 2 3 4))

; Ex 2.22
(define (square-list3 items)
 (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
 (iter items null))

(define (square-list4 items)
 (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (square (car things))))))
 (iter items null))


;;  Test 2.22
(square-list3 (list 1 2 3 4))
(square-list4 (list 1 2 3 4))
