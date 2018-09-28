#lang racket

#| (define (sum term a next b) |#
#|   (if (> a b) |#
#|     0 |#
#|     (+ (term a) |#
#|        (sum term (next a) next b)))) |#

; 1.29 Simpson's rule
; Increasing n increases the accuracy of the integral.
; It is the number of "buckets"

(define (inc num)
  (+ num 1))

(define (cube num)
  (* num num num))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (cond
      [(or (= k n) (= k 0)) (f (+ a (* k h)))]
      [(odd? k) (* 4 (f (+ a (* k h))))]
      [else (* 2 (f (+ a (* k h))))]))
  (* (/ h 3)
     (sum y 0 inc n)))

(provide simpson-integral cube)

;; 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

(provide sum inc)

;; 1.31
(define (product term a next b)
  (if (> a b) 1
    (* (term a)
       (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

; estimates pi/4
; increase n to increase accuracy
(define (pi-over-four n)
  (define (get-numerator i)
    (cond [(odd? i) (+ i 1)]
          [else i]))
  (define (get-denominator i)
    (cond [(even? i) (+ i 1)]
          [else i]))
  (/ (product get-numerator 2 inc (+ 2 n))
     (product get-denominator 2 inc (+ 2 n))))

(define (product-iter term a next b)
  (define (iter a acc)
    (if (> a b) acc
      (iter (next a) (* acc (term a)))))
  (iter a 1))

(provide factorial pi-over-four product-iter)

;; 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value
    (combiner (term a)
              (accumulate combiner null-value term (next a) next b))))

#| (define (new-sum term a next b) |#
#|   (accumulate + 0 term a next b)) |#

#| (define (new-product term a next b) |#
#|   (accumulate * 1 term a next b)) |#

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b) result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (new-sum term a next b)
  (accumulate-iter + 0 term a next b))

(define (new-product term a next b)
  (accumulate-iter * 1 term a next b))

(provide new-sum new-product accumulate accumulate-iter)

;; 1.33
(define (filtered-accumulate combiner filt null-value term a next b)
  (define (new-filter i)
    (if (filt i) (term i)
      null-value))
  (accumulate combiner
              null-value
              new-filter
              a
              next
              b))

(require math)

(define (square x) (* x x))
(define (sum-of-prime-squares a b)
  (filtered-accumulate + prime? 0 square a inc b))

;; product of all positive integers relatively prime to n under n
(define (product-of-relatively-prime n)
  (define (rel-prime i)
    (= (gcd i n) 1 ))
  (filtered-accumulate * rel-prime 1 identity 1 inc n))

(provide sum-of-prime-squares product-of-relatively-prime square)

;; 1.34
(define (f g) (g 2))
(provide f)
; (f f)
; => (f 2)
; => (2 2)
; => ERROR 2 is not a function

;; 1.35
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    #| (display guess) |#
    #| (newline) |#
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

;; phi is the fixed point of the function:
;; x -> 1 + 1/x
#| (define phi (fixed-point |#
#|               (lambda (x) (+ 1 (/ 1 x))) |#
#|               2)) |#
#| (provide phi) |#

;; 1.36
;; x -> log(1000)/log(x)
#| (fixed-point |#
#|   (lambda (x) |#
#|     (/ (log 1000) (log x))) |#
#|   4) |#

;; 1.37
; calculates a k-term finite continued fraction
; recursive
#| (define (cont-frac n d k) |#
#|   (define (cont-frac-helper i) |#
#|     (if (= i k) (/ (n i) (d i)) |#
#|       (/ (n i) (+ (d i) (cont-frac-helper (+ i 1)))))) |#
#|   (cont-frac-helper 1)) |#

; iterative
(define (cont-frac n d k)
  (define (cont-frac-helper i result)
    (if (= i 0) result
      (cont-frac-helper (- i 1)
                        (/ (n i) (+ (d i) result)))))
  (cont-frac-helper k 0))

;; aproximates 1/phi
;  pretty fast!
#| (cont-frac (lambda (i) 1.0) |#
#|            (lambda (i) 1.0) |#
#|            10) |#
(define (one i) 1.0)
(provide cont-frac one)

;; 1.38
(define (euler-expansion-denom i)
  (if (= (modulo i 3) 2) (* 2 (/ (+ i 1) 3))
    1))

(provide euler-expansion-denom)

;; 1.39
(define (tan-cf x k)
  (cont-frac
    (lambda (i)
      (if (= i 1) x
        (- (square x))))
    (lambda (i)
      (- (* i 2) 1))
    k))

(provide tan-cf)

;; 1.40
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))
(define root-example (newtons-method (cubic 0 0 0) 1)) ; should be 0
(provide root-example cubic)

;; 1.41
(define (double proc)
  (lambda (arg)
    (proc (proc arg))))
(provide double)

; (((double (double double)) inc) 5)
; => (((double (lambda (arg) (double (double arg)))) inc) 5) #| apply double twice
; => .... (((lambda (arg) (double (double (double (double arg))))) inc) 5) #| apply double 4 times
; => (double (double (double (inc (inc 5))))) #| apply inc twice (one double applied)
; => (double (double (inc (inc (inc (inc 5)))))) #| apply inc 4 times (two doubles applied)
; => (double (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))) #| apply inc 8 times (three doubles applied)
; => .... (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5)))))))))))))))) #| apply inc 16 times
; so it returns 5+16 or 21

;; 1.42
; returns a procedure implementing the composition f after g
(define (compose f g)
  (lambda (x)
    (f (g x))))

(provide compose)

;; 1.43
; returns a procedure implementing the repeated application of f n times
(define (repeated f n)
  (if (= n 1)
    (lambda (x) (f x))
    (compose f (repeated f (- n 1)))))

(provide repeated)

;; 1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f x)
          (f (+ x dx))
          (f (- x dx)))
       3)))

; apply smooth to f n times which is f smoothed n times (confusing I know)
(define (n-smooth f n)
  ((repeated smooth n) f))

(provide smooth n-smooth)

;; 1.45
; return x^(1/n) or the n'th root of x.

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

; sqrt example
(define (sqrt x)
  (fixed-point
    (average-damp
      (pow-fixed-func 2 x))
    1.0))

; returns x^y
(define (power x y)
  (if (= y 1) x
    (* x (power x (- y 1)))))

; returns a fixed func that should converge to the nth root of its parameter x
; if average dampened enough.
(define (pow-fixed-func n x)
  (lambda (y)
    (/ x (power y (- n 1)))))

; returns a function that average damps f n times
(define (n-average-damp n f)
  ((repeated average-damp n) f))

; for a given power, try average damping i times and try to find a fixed point function
; WARNING: will infinite loop given too few dampenings.
; n is the power of the root.
; i is the number of times you wish to dampen the fixed point function.
; x is the value you would like to try and calculate the n'th root of.
(define (try-converge n i x)
  (fixed-point
    (n-average-damp i
                    (pow-fixed-func n x))
    1)) ; guess 1 initially

;; after testing, i believe the pattern is that we must dampen log_2(n) times, where n
;; is the n'th root we are trying to calculate.

(define (nth-root n x)
  (define dampenings (floor (/ (log n)
                               (log 2))))
  (fixed-point
    ((repeated average-damp dampenings) (pow-fixed-func n x))
    1.0))

(provide average-damp nth-root fixed-point sqrt pow-fixed-func n-average-damp try-converge nth-root)

;; 1.46

(define (iterative-improve good-enough? improve)
  (define (helper guess)
    (if (good-enough? guess (improve guess))
      (improve guess)
      (helper (improve guess))))
  helper)

(define (new-fixed-point f initial-guess)
  ((iterative-improve
    (lambda (x y)
      (if (< (abs (- x y)) 0.0001) #t
        #f))
    (lambda (guess)
      (f guess)))  initial-guess))

(define (new-sqrt x)
  ((iterative-improve
    (lambda (x y)
      (if (< (abs (- x y)) 0.0001) #t
        #f))
    (lambda (guess)
      (average guess (/ x guess))))  1.0))

(provide iterative-improve new-fixed-point new-sqrt)
