#lang racket

(define (square x)
  (* x x))

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
(define (double x)
  (+ x x))
(define (halve x)
  (/ x 2))
(define (mult a b)
  (cond [(= b 0) 0]
        [(even? b)
         (mult (double a) (halve b))]
        [(odd? b)
         (+ (mult (double a) (halve (- b 1))) a)]))

;; 1.18
(define (mult-better a b)
  (define (mult-iter a b acc)
    (cond [(= b 0) acc]
          [(even? b) (mult-iter (double a)
                                (halve b)
                                acc)]
          [(odd? b) (mult-iter (double a)
                               (halve (- b 1))
                               (+ acc a))]))
  (mult-iter a b 0))

(provide mult mult-better)

;; 1.19
(define (fib-better n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond [(= count 0) b]
        [(even? count) (fib-iter a
                                 b
                                 (+ (* p p)
                                    (* q q))
                                 (+ (* q q)
                                    (* 2 p q))
                                 (/ count 2))]
        [(odd? count) (fib-iter (+ (* b q)
                                   (* a q)
                                   (* a p))
                                (+ (* b p)
                                   (* a q))
                                p
                                q
                                (- count 1))]))

(provide fib-better)

;; 1.21
;; slow prime test
(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next num)
  (if (= num 2) (+ 1 num)
    (+ 2 num)))

(define (find-divisor n test-divisor)
  (define (square x)
    (* x x))
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
                n
                (next test-divisor)))))

(define (prime? n)
  (= n (smallest-divisor n)))

;; faster probabilistic prime test
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
           (square (expmod base (/ exp 2) m))
           m))
        (else
          (remainder
            (* base (expmod base (- exp 1) m))
            m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else false)))

(provide smallest-divisor prime? fast-prime?)

; 199 is prime
; 1999 is prime
; 19999 has 7 as the smallest divisor

;; 1.22
(define (timed-prime-test n)
  (start-prime-test n (current-milliseconds)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
    (report-prime n start-time)
    #f))

(define (report-prime prime start-time)
  (display prime)
  (display " *** ")
  (display (- (current-milliseconds) start-time))
  (newline))

; searches for primes in [start, end]
(define (search-for-primes start end)
  (cond [(> start end) (display "DONE")]
        [else (timed-prime-test start) (search-for-primes (+ start 1) end)]))

(provide timed-prime-test search-for-primes)

;; 1.23 done above
;; 1.24 done above no difference (modern computers are too fast for these exercises..)
;; 1.25 no, calculating the remainder grows exponentially as base^exp grows
;;      whereas our expmod keeps the value logarithmically low
;; 1.26 He has made two recursive calls; whereas before the interpreter would have
;;      calculated the value once and squared it, now it is calculating it twice and
;;      multiplying the answers exponentially increasing the amount of work done.
;
;; 1.27
; Does the fermat test for ever a < n, a > 2 (a = 1 is obvious)
(define (fermat-prime? n)
  (define (test a)
    (= (expmod a n n) a))
  (define (test-fermat-prime a)
    (cond [(>= a n) #t]
          [(not (test a)) #f]
          [else (test-fermat-prime (+ 1 a))]))
  (test-fermat-prime 2))

(provide fermat-prime?)

; fails for carmichael numbers; try (fermat-prime? 41041); returns #t even though divisible by 7
;
; 1.28
; miller-rabin test on n to see if n is prime or not.
(define (miller-rabin n)
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder
             (square (expmod base (/ exp 2) m))
             m))
          (else
            (remainder
              (* base (expmod base (- exp 1) m))
              m))))

)

(provide miller-rabin)
