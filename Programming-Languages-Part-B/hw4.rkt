
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

#|(1. Write a function sequence that takes 3 arguments low, high, and stride, all assumed to be numbers.
Further assume stride is positive. sequence produces a list of numbers from low to high (including
low and possibly high) separated by stride and in sorted order)|#
(define sequence
  (lambda (low high stride)
    (cond
      [(> low high) (quote ())]
      [else (cons low (sequence (+ low stride) high stride))])))

#|(2. Write a function string-append-map that takes a list of strings xs and a string suffix and returns a
list of strings. Each element of the output should be the corresponding element of the input appended
with suffix (with no extra space between the element and suffix). You must use Racket-library
functions map and string-append.)|#
(define (string-append-map l suffix)
  (map (lambda (list-item) (string-append list-item suffix)) l))

#|(3. Write a function list-nth-mod that takes a list xs and a number n. If the number is negative,
terminate the computation with (error "list-nth-mod: negative number"). Else if the list is
empty, terminate the computation with (error "list-nth-mod: empty list"). Else return the ith
element of the list where we count from zero and i is the remainder produced when dividing n by the
list’s length. Library functions length, remainder, car, and list-tail are all useful)|#
(define (list-nth-mod xs n)
  (cond
    [(< n 0) (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [else (let ([i (remainder n (length xs))])
            (car (list-tail xs i)))]))

#|(4. Write a function stream-for-n-steps that takes a stream s and a number n. It returns a list holding
the first n values produced by s in order. Assume n is non-negative. Sample solution: 5 lines. Note:
You can test your streams with this function instead of the graphics code.)|#
(define (stream-for-n-steps s n)
  (let ([pr (s)])
    (cond
      [(or (zero? n) (null? (car pr)))
       (quote ())]
      [else (cons (car pr) (stream-for-n-steps (cdr pr) (sub1 n)))])))

#|(5. Write a stream funny-number-stream that is like the stream of natural numbers (i.e., 1, 2, 3, ...)
except numbers divisble by 5 are negated (i.e., 1, 2, 3, 4, -5, 6, 7, 8, 9, -10, 11, ...). Remember a stream
is a thunk that when called produces a pair. Here the car of the pair will be a number and the cdr will
be another stream.)|#
(define funny-number-stream
  (letrec ([recurse
            (lambda (n)
              (let ([next (if (zero? (modulo n 5)) (- n) n)])
                (cons next (lambda () (recurse (+ n 1))))))])
    (lambda () (recurse 1))))

#|(6. Write a stream dan-then-dog, where the elements of the stream alternate between the strings "dan.jpg"
and "dog.jpg" (starting with "dan.jpg"). More specifically, dan-then-dog should be a thunk that
when called produces a pair of "dan.jpg" and a thunk that when called produces a pair of "dog.jpg"
and a thunk that when called... etc.)|#
; (define dan-then-dog
;   (letrec ([recurse
;             (lambda (n)
;               (let ([next (if (even? n) "dan" "dog")])
;                 (cons (string-append next ".jpg") (lambda () (recurse (+ n 1))))))])
;     (lambda () (recurse 0))))

(define dan-then-dog
  (letrec ([recurse
            (lambda (s1 s2)
              (cons (string-append s1 ".jpg") (lambda () (recurse s2 s1))))])
    (lambda () (recurse "dan" "dog"))))

#|(7. Write a function stream-add-zero that takes a stream s and returns another stream. If s would
produce v for its ith element, then (stream-add-zero s) would produce the pair (0 . v) for its
ith element. Sample solution: 4 lines. Hint: Use a thunk that when called uses s and recursion.
Note: One of the provided tests in the file using graphics uses (stream-add-zero dan-then-dog)
with place-repeatedly.)|#
(define (stream-add-zero s)
  (letrec ([recurse
            (lambda (s) (let ([pr (s)])
                          (cons (cons 0 (car pr)) (lambda () (recurse (cdr pr))))))])
    (lambda () (recurse s))))