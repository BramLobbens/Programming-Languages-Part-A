#lang racket

(provide (all-defined-out))

(define s "hello") ; this is a comment
(define x 3)
(define y (+ x 2)) ; + is a function, call it here

(define cube1
  (lambda (x)
    (* x (* x x))))
(define cube2
  (lambda (x)
    (* x x x))) ; no syntactic sugar for currying/tuples. Function * here takes 3 actual arguments
; but this is syntactic sugar for cube2
(define (cube3 x)
  (* x x x))

(define (pow1 x y)
  (if (= y 0)
     1
     (* x (pow1 x (- y 1)))))

;example of currying, although not common in racket since it support multiple arguments as is
(define pow2
  (lambda (x)
    (lambda (y)
      (pow1 x y))))

(define three-to-the (pow2 3))

;; parentheses matter
(define sixteen (pow1 4 2))
(define sixteen_ ((pow2 4) 2)) ;; calling pow2 with the curried function--there is no syntactic sugar