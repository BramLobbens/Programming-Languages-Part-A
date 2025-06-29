
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
      [(or (zero? n) (null? pr)) (quote ())]
      [else (cons (car pr)
                 (stream-for-n-steps (cdr pr) (sub1 n)))])))

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

#|(8. Write a function cycle-lists that takes two lists xs and ys and returns a stream. The lists may or
may not be the same length, but assume they are both non-empty. The elements produced by the
stream are pairs where the first part is from xs and the second part is from ys. The stream cycles
forever through the lists.)|#
; (define (cycle-lists xs ys)
;   (letrec ([recurse
;             (lambda (xs ys)
;               (cond [(or (null? xs) (null? ys)) (quote ())]
;                    [else (cons (cons (car xs) (car ys)) (lambda () (recurse (cdr xs) (cdr ys))))])
;               )])
;     (lambda () (recurse xs ys))))

(define (cycle-lists xs ys)
  (define (recurse n)
    (let ([x (list-nth-mod xs n)]
          [y (list-nth-mod ys n)])
      (cons (cons x y)
           (lambda () (recurse (+ n 1))))))
  (lambda () (recurse 0)))

#|(9. Write a function vector-assoc that takes a value v and a vector vec. It should behave like Racket’s
assoc library function except (1) it processes a vector (Racket’s name for an array) instead of a list,
(2) it allows vector elements not to be pairs in which case it skips them, and (3) it always takes exactly
two arguments. Process the vector elements in order starting from 0. You must use library functions
vector-length, vector-ref, and equal?. Return #f if no vector element is a pair with a car field
equal to v, else return the first pair with an equal car field. Sample solution is 9 lines, using one local
recursive helper function.)|#
(define (vector-assoc v vec)
  (let ([vec-length (vector-length vec)])
    (if (zero? vec-length)
       #f
       (letrec ([recurse (lambda (i)
                           (if (>= i vec-length) #f
                              (let ([vec-ref (vector-ref vec i)])
                                (cond
                                  [(and (pair? vec-ref) (equal? (car vec-ref) v)) vec-ref]
                                  [else (recurse (+ i 1))]))))])
         (recurse 0)))))

#|(10. Write a function cached-assoc that takes a list xs and a number n and returns a function that takes
one argument v and returns the same thing that (assoc v xs) would return. However, you should
use an n-element cache of recent results to possibly make this function faster than just calling assoc
(if xs is long and a few elements are returned often). The cache must be a Racket vector of length n
that is created by the call to cached-assoc (use Racket library function vector or make-vector) and
used-and-possibly-mutated each time the function returned by cached-assoc is called. Assume n is
positive.)|#
(define (cached-assoc xs n)
  (letrec ([vector-cache (make-vector n #f)]
           [cache-i 0])
    (lambda (v)
      (let ([res (vector-assoc v vector-cache)])
        (if res
           res ;found in cache
           (let ([new-res (assoc v xs)])
             (if new-res
                (begin
                  (vector-set! vector-cache cache-i new-res)
                  (set! cache-i (add1 cache-i))
                  new-res)
                #f
                )))))))