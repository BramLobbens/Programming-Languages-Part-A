#lang racket

;; Module 3 notes

;Datatype-programming with structs

;;-struct
; Defines a new kind of thing and introduces several new functions:
;/1 (foo e1 e2 e3) returns a "foo" with bar, baz, quux fields
;holding results of evaluating e1, e2 and e3
;/2 (foo? e) evaluates e and returns #t if and only if the result is
;something that was made with the foo function
;/3 (foo-bar e), (foo-baz e), (foo-quux e) evaluate to e if the
;result was made with the foo function, return the contents
;of the bar or baz or quux fields, else an error
(struct foo (bar baz quux) #:transparent)