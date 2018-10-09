#lang racket

; How much of a language can we build using only lambda functions and substitution?

; Church numerals
(define (inc x)
  (+ 1 x))

(define zero
  (lambda (f)
    (lambda (x) x)))

(define (succ n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define one (succ zero))
(define two (succ one))
(define three (succ two))
(define four (succ three))
(define five (succ four))

(define (test n)
  ((n inc) 0))

(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

(define (mult a b)
  (lambda (f)
    (lambda (x)
      ((b (a f)) x))))

(define (expt a b)

(provide succ zero one two three four five inc add test mult)

(define (IF x y z)
  (x y z))

(define TRUE (lambda (y z)
               y))

(define FALSE (lambda (y z)
                z))

(provide IF TRUE FALSE)
