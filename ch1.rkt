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
  (new-if (good-enough? guess x)
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
