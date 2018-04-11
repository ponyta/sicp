#lang racket

;; 1.1
10 ; 10
(+ 5 3 4) ; 12
(- 9 1) ; 8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6)) ; 6
(define a 3) ; a -> 3
(define b (+ a 1)) ; b -> 4
(+ a b (* a b)) ; 19
(= a b) ; #f
(if (and (> b a) (< b (* a b)))
    b
    a) ; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; 16
(+ 2 (if (> b a) b a)) ; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ; 16


;; 1.2
(/ (+ 5 4
      (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))


;; 1.3
(define (sq-sum x y)
  (+ (* x x) (* y y)))

(define (sum-large-sq x y z)
  (cond [(and (< x y) (< x z))
         (sq-sum y z)]
        [(and (< y x) (< y z))
         (sq-sum x z)]
        [else
         (sq-sum x y)]))
(provide sum-large-sq)


;; 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b)) ; if b > 0, use the + operator, otherwise use the 0 op
;; hence this function returns a + |b|
(provide a-plus-abs-b)


;; 1.5
(define (p) (p)) ;; perverse
(define (test a b)
  (if (= 0 a) 0
    (b)))
(test 0 p) ;; should return 0 under applicative order evaluation
;; under normal order evaluation, we get:
; (if (= 0 0) 0 (p))
; (if (= 0 0) 0 (p))
; (if (= 0 0) 0 (p))
; ...
; e.t.c forever (program never halts)


;; 1.6
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average a b)
  (/ (+ a b) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (sqrt x) ; start from a guess of 1
  (sqrt-iter 1.0 x))

(provide sqrt)

(define (new-if predicate then-clause else-clause)
  (cond [predicate then-clause]
        [else else-clause]))
;; under new-if program never halts because it evaluates both branches.

;; sub model
; (sqrt-iter 1.0 2)
; (new-if (good-enough? 1.0 2)
;         1.0
;         (sqrt-iter (improve 1.0 2) 2))
; (cond [(good-enough? 1.0 2) 1.0]
;       [else (sqrt-iter (improve 1.0 2) 2)])
; (cond [false 1.0]
;       [else (sqrt-iter (improve 1.0 2) 2)])
; (cond [else (sqrt-iter (improve 1.0 2) 2)])
; (sqrt-iter (improve 1.0 2) 2)
; (new-if (good-enough? (improve 1.0 2) 2)
;         (improve 1.0 2)
;         (sqrt-iter (improve (improve 1.0 2) 2) 2))
; ...
; (sqrt-iter (improve (improve 1.0 2) 2) 2)


;; 1.7
; examples of bad sqrts
(square 0.00004)
(sqrt (square 0.00004))

(square 2345823495723) ; big numbers aren't very affected actually because I guess my laptop is relatively fast.
; I guess the algorithm converges fast enough
(sqrt (square 2345823495723))

(define (better-sqrt x)
  (better-sqrt-iter 1.0 2.0 x))

(define (better-good-enough? guess prev-guess x)
  (< (abs (/ (- guess prev-guess) guess)) 0.01)) ; less than 1% change

(define (better-sqrt-iter guess prev-guess x)
  (if (better-good-enough? guess prev-guess x)
    guess
    (better-sqrt-iter (improve guess x) guess x)))

(provide better-sqrt square)


;; 1.8
(define (cube x)
  (* x x x))

(define (cubert x)
  (cubert-iter 1.0 x))

(define (good-enough-cube? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cubert-iter guess x)
  (if (good-enough-cube? guess x) guess
    (cubert-iter (improve-cube guess x) x)))

(provide cube cubert)

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

