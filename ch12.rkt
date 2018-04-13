#lang racket

;; 1.9
; apparently these are no longer part of the stdlib for racket
(define (inc x)
  (+ x 1))
(define (dec x)
  (- x 1))

#| (define (plus a b) ; recursive process |#
#|   (if (= a 0) |#
#|     b |#
#|     (inc (plus (dec a) b)))) |#

#| > (trace plus) |#
#| > (plus 3 6) |#
#| >(plus 3 6) |#
#| > (plus 2 6) |#
#| > >(plus 1 6) |#
#| > > (plus 0 6) |#
#| < < 6 |#
#| < <7 |#
#| < 8 |#
#| <9 |#
#| 9 |#

#| (define (plus a b) ; iterative |#
#|   (if (= a 0) |#
#|     b |#
#|     (plus (dec a) (inc b)))) |#

#| > (trace plus) |#
#| > > (plus 3 6) |#
#| > > (plus 3 6) |#
#| > > (plus 2 7) |#
#| > > (plus 1 8) |#
#| > > (plus 0 9) |#
#| > > <9 |#
#| > > 9 |#

;; 1.10
; Ackermann's function
(define (A x y)
  (cond [(= y 0) 0] ; doesn't happen too often; base case
        [(= x 0) (* 2 y)] ; double y
        [(= y 1) 2] ; more often
        [else (A (- x 1) ; one less x

(A x (- y 1)))])) ; to A with y 1 less
(provide A)

(A 1 10)
; (A 0 (A 1 9))
; (* 2 (A 1 9))
; (* 2 (* 2 (A 1 8)))
; ...
; (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 1))))))))))
; or 2^10
; 1024
; so (A 1 n) -> 2^n
(A 2 4)
; (A 1 (A 2 3))
; or 2^(A 2 3)
; 2^(A 1 (A 2 2))
; or 2^2^(A 2 2)
; ...
; 2^2^2^2
; 65536
; so (A 2 n) -> 2↑n
; or 2^2^2^2^2....^2 n times
(A 3 3)
; (A 2 (A 3 2))
; (A 2 (A 2 (A 3 1)))
; (A 2 (A 2 2))
; (A 2 (2^2))
; (A 2 4)
; 65536
; so (A 3 n) -> 2↑2↑2↑2↑2↑2↑...↑2 n times
(define (f n) (A 0 n)) ; 2n
(define (g n) (A 1 n)) ; 2^n
(define (h n) (A 2 n)) ; 2↑n, or 2^2^2...^2 n times
(define (k n) (* 5 n n)) ; 5n^2
(provide f g h k)

;; 1.11

; iterative fibonacci
(define (fib n)
  (define (fib-helper a b count)
    (if (= count 0) b
      (fib-helper (+ a b)
                  a
                  (- count 1))))
  (fib-helper 1 0 n))

; f-rec is the recursive version of the function
(define (f-rec n)
  (cond [(< n 3) n]
        [else (+ (f-rec (- n 1))
                 (* 2 (f-rec (- n 2)))
                 (* 3 (f-rec (- n 3))))]))

(define (f-iter n)
  n)

(provide fib f-rec f-iter)
