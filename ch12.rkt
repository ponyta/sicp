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
; iterative fibonacci (just for example)
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
  (define (f-helper a b c count)
    (if (= count 0) c
      (f-helper (+ a
                   (* 2 b)
                   (* 3 c))
                a b (- count 1))))
  (f-helper 2 1 0 n))

(provide fib f-rec f-iter)

;; 1.12
(define (pascal row col)
  (if (or (= row col)
          (= 1 col)) 1
    (+ (pascal (- row 1) (- col 1))
       (pascal (- row 1) col))))

(provide pascal)

;; 1.13
; Done on paper

;; 1.14
(define (count-change amount)
  (define (first-denomination kinds-of-coins)
    (cond [(= kinds-of-coins 1) 1]
          [(= kinds-of-coins 2) 5]
          [(= kinds-of-coins 3) 10]
          [(= kinds-of-coins 4) 25]
          [(= kinds-of-coins 5) 50]))
  (define (cc amount kinds-of-coins)
    (cond [(= amount 0) 1] ; one way to make change for 0
          [(or (< amount 0)
               (= kinds-of-coins 0))
           0] ; if negative amount or no types of coins left, there are no ways to make change
          [else (+ (cc amount (- kinds-of-coins 1)) ; making change without using the first type of coin
                   (cc (- amount (first-denomination kinds-of-coins)) ; making change for amount-(first type of coin) with all coins
                       kinds-of-coins))]))
  (cc amount 5))

; Steps: for a constant increase in amount, given the increase is at least the
; size of some coin denomination, we double the number of steps needed since
; there needs to be an extra branch to count how many ways we can count change
; with that extra coin.
;
; The maximum depth of the tree (space complexity) is proportional to the amount.
; For this case of denominations, it is 5 (the number of denominations) plus
; the amount divided by the smallest denomination. Since this relation is not
; dependent on the actual denominations itself, this holds true regardless of
; what kind of coins your system has. Thus count-change has a space complexity
; that is linear in nature.

(provide count-change)

;; 1.15
(define (cube x)
  (* x x x))

(define (p x)
  (- (* 3 x)
     (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(provide sine)

; (sine 12.15)
; p is applied n-1 times, where n is such that (12.15/3^n) < 0.1, or 6 times.
; It also takes the same amount of space complexity
;
; Solving for n, we get:
; 121.5 < 3^n
; log(121.5)/log(3) < n
; Thus n is proportional to log(angle), and has logarithmic complexity.

;; 1.16
(define (fast-expt b n) ; calculates b^n in an iterative, faster way
  (define (square x)
    (* x x))
  (define (expt-iter b n acc)
    (cond [(= n 0) acc]
          [(even? n) (expt-iter
                       (square b)
                       (/ n 2)
                       acc)]
          [(odd? n) (expt-iter
                      (square b)
                      (/ (- n 1) 2)
                      (* acc b))]))
  (expt-iter b n 1))

(provide fast-expt)

;; 1.17
(define (mult a b)
  (define (double x)
    (+ x x))
  (define (halve x)
    (/ x 2))
  (
