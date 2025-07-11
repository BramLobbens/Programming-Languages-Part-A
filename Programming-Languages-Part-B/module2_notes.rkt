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


;; lists

(null? null)
(null? '())

(define (sum xs)
  (if (null? xs)
     0
     (+ (car xs) (sum (cdr xs)))))

(define (my-append xs ys)
  (if (null? xs)
     ys
     (cons (car xs) (my-append (cdr xs) ys))))

(define (my-map f xs)
  (if (null? xs)
     null
     (cons (f (car xs))
          (my-map f (cdr xs)))))

(define foo (my-map (lambda (x) (+ x 1))
                   (cons 2 (cons 4 (cons 5 null)))))


;; dynamic typing

(define (sum1 xs)
  (if (null? xs)
     0
     (if (number? (car xs))
        (+ (car xs) (sum1 (cdr xs)))
        (+ (sum1 (car xs) (sum1 (cdr xs)))))))

(define (sum2 xs)
  (cond [(null? xs) 0]
       [(number? (car xs)) (+ (car xs) (sum2 (cdr xs)))]
       [#t (+ (sum2 (car xs) (sum2 (cdr xs))))]))

(if 34 14 15) ;; 34 is truthy, cause only #f is considered actual false, everything else is true

;; local bindings

;let
;let*
;letrec
;define (locally)

(define (max-of-list xs)
  (cond [(null? xs) (error "max-of-list given empty list")]
       [(null? (cdr xs)) (car xs)]
       [#t (let ([tlans (max-of-list (cdr xs))])
             (if (> tlans (car xs))
                tlans
                (car xs)))]))

;let
;- a let expression can bind any number of local variables
;evaluated in the environment from BEFORE the let-expression
(define (silly-double x)
  (let ([x (+ x 3)] ; the x in e2 is the function argument
        [y (+ x 2)]) ; also here, the x is the function argument
    (+ x y -5))) ; body of the let

;let*
; (same as ML let)
; the expressions are evaluated in the environment produced from
; the PREVIOUS BINDINGS
(define (silly-double2 x)
  (let* ([x (+ x 3)]
         [y (+ x 2)]) ;x refers the the previous x bound with let
    (+ x y -8)))

;letrec
;the expressions are evaluated in the environment that includes ALL THE BINDINGS
;generally only recommended if you have MUTUAL RECURSION
(define (silly-triple x)
  (letrec ([y (+ x 2)]
           [f (lambda (z) (+ z y w x))]; x is the parameter
           ; y is the previous binding
           ; z is the later binding! (that's why letrec is need)
           [w (+ x 7)])
    (f -9))) ;racket still evaluates in order, so need to be careful
;since f is only evaluated when it's called (it's a closure)
;so w is already evaluated in the environment

;local defines
;in certain positions, like beginning of function bodies you can put defines
;for defining local variables, same semantics as letrec
(define (silly-mod2 x)
  (define (even? x) (if (zero? x) #t (odd? (- x 1))))
  (define (odd? x) (if (zero? x) #f (even? (- x 1))))
  (if (even? x) 0 1))
;same as
(define (silly-mod3 x)
  (letrec
      ([even? (lambda (x) (if (zero? x) #t (odd? (- x 1))))]
       [odd? (lambda (x) (if (zero? x) #f (even? (- x 1))))])
    (if (even? x) 0 1)))


;; mutation

;set! (set-bang)
(define b 3)
(define f (lambda (x) (* 1 (+ x b))))
(define c (+ b 4)) ;7
(set! b 5)
(define z (f 4)) ;9 -current value in environment that b takes is 5
(define w c) ;7 -unchanged

;begin
(define some-x (begin
                 (+ 5 3)
                 (* 3 8)
                 (- 3 2) ; When using the begin special form, the result of the final expression is the returned value
                 ))

;mcons
(define mpr (mcons 10 (mcons 3 (mcons "yay" "test"))))
(set-mcar! mpr 30)
(set-mcdr! mpr (mcons 5 null))
;(length mpr) ; will not work - mcons does not produce a 'proper list'


;; Delayed Evaluation

;arguments of functions get evaluated before calling the function
;if/cond only evaluates the branch that it needs to

;if you would want to emulate if via a wrapper function:
;e2 and e3 should be zero argument functions (delays evaluation)
(define (my-if-strange-but-works e1 e2 e3)
  (if e1 (e2) (e3)))

(define (factorial-okay x)
  (my-if-strange-but-works
   (= x 0)
   (lambda () 1)
   (lambda () (* x (factorial-okay (- x 1))))))
;Wrapping the expression in a lambda function delays evaluation, which is what it means to thunk an expression.

(define (my-force p)
  (if (mcar p)
     (mcdr p)
     (begin (set-mcar! p #t)
           (set-mcdr! p ((mcdr p)))
           (mcdr p))))


;; Streams

; A stream is an infinite sequence of values
; key idea: use a thunk to delay creating most of the sequence

; let a stream be a thunk that when called returns a pair:
;-- '(next-anser . next-thunk)
; given a stream st, the client can get any number of elements
;-- first: (car (s))
;--second: (car ((cdr (s))))
;--third: (car ((cdr ((cdr (s))))))
;usually bind (cdr (s)) to a variable or pass to a recursive function
(define (number-until stream tester)
  (letrec ([f (lambda (stream ans)
                (let ([pr (stream)]) ; let pr be the pair of thunking the stream
                  (if (tester (car pr)) ; get the result from the car of the pair
                     ans
                     (f (cdr pr) (+ ans 1)))))]) ; call f with the stream of the pair and accumulate onto ans
    (f stream 1)))

;defining a stream of ones
(define ones (lambda () (cons 1 ones)))
;more verbose - unecessary function wrapping, but equivalent
(define ones2 (lambda () (cons 1 (lambda () (ones2)))))
; but careful, not returning a thunk will cause an infinite loop !
;;(define ones-bad (lambda () (cons 1 (ones-bad))))

;defining a stream of increasing natural numbers
;e.g. define a func that starts at x and returns a pair starting at x and the recursion on that function +1
(define (somefunc x) (cons x (lambda () (somefunc (+ x 1)))))
(define nats_v1 (lambda () (f 1))) ; natural numbers uses this then to start at 1

;better style as a helper function
(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))


;; Memoization

(define fibonacci3
  (letrec ([memo null] ; list of pairs (arg . result) - It is essential that memo is bound outside of the recursive function
           [f (lambda (x)
                (let ([ans (assoc x memo)])
                  (if ans
                     (cdr ans)
                     (let ([new-ans (if (or (= x 1) (= x 2))
                                       1
                                       (+ (f (- x 1))
                                         (f (- x 2))))])
                       (begin
                         (set! memo (cons (cons x new-ans) memo))
                         new-ans)))))])
    f))