
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