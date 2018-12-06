#lang racket

; Streams
(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (force delayed-object)
  (delayed-object))

(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (lambda ()
       expr))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

(define the-empty-stream '())

(define (stream-null? stream)
  (eq? stream the-empty-stream))

(define (stream-ref stream n)
  (if (= n 0) (stream-car stream)
    (stream-ref (stream-cdr stream)
                (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-line x)
  (display x)
  (newline))

(define (print-stream s)
  (stream-for-each
    display-line
    s))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream low
                 (stream-enumerate-interval (+ low 1)
                                            high))))

(define (stream-filter pred stream)
  (cond [(stream-null? stream) the-empty-stream]
        [(pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream)))]
        [else
          (stream-filter pred (stream-cdr stream))]))

; 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc
                   (map stream-cdr
                        argstreams))))))

; for example
; (print-stream (stream-map +
;                           (stream-enumerate-interval 0 10)
;                           (stream-enumerate-interval 20 30)))
; => 20 22 24 26 .... 40

; 3.51
; (define (show x)
;   (display-line x)
;   x)
; (define x
;   (stream-map
;     show
;     (stream-enumerate-interval 0 10)))
; prints 0 -> 10 b/c it evaluates each element in the stream

; (stream-ref x 5)
; prints 5
; (stream-ref x 7)
; prints 6

; 3.52
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq
  (stream-map
    accum
    (stream-enumerate-interval 1 20)))
; sum => 0 + 1 + 2 + ... + 20 = 210
; seq => 1 3 6 10 15 21 ... 210

(define y (stream-filter even? seq))

(define z
  (stream-filter
    (lambda (x)
      (= (remainder x 5) 0)) seq))

; (stream-ref y 7)
; y => (6 10 28 36 66 78 120 136 190 210)
; returns 136
; (print-stream z)
; (10 15 45 55 105 120 190 210)
; it does not matter that the delay procedure is memoized.

(define (integers-starting-from n)
  (cons-stream
    n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (divisible n divisor)
  (= (remainder n divisor) 0))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream s factor)
  (stream-map
    (lambda (x) (* factor x))
    s))

; 3.53
(define s (cons-stream 1 (add-streams s s)))
; should be 1 2 4 8 16 32 ...

(provide print-stream s)

; 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))

(define (print-first-n stream n)
  (if (= n 0) 'done
    (begin (display-line (stream-car stream))
           (print-first-n (stream-cdr stream) (- n 1)))))

(provide factorials stream-car stream-cdr cons-stream print-first-n)

; 3.55
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))
(provide integers partial-sums)

; 3.56
(define (merge s1 s2)
  (cond [(stream-null? s1) s2]
        [(stream-null? s2) s1]
        [else
          (let [(s1car (stream-car s1))
                (s2car (stream-car s2))]
            (cond [(< s1car s2car)
                   (cons-stream s1car
                                (merge (stream-cdr s1)
                                       s2))]
                  [(< s2car s1car)
                   (cons-stream s2car
                                (merge s1
                                       (stream-cdr s2)))]
                  [else
                    (cons-stream s1car
                                 (merge (stream-cdr s1)
                                        (stream-cdr s2)))]))]))

(define hamming-stream (cons-stream 1 (merge (scale-stream hamming-stream 2)
                                             (merge (scale-stream hamming-stream 3)
                                                    (scale-stream hamming-stream 5)))))
(provide hamming-stream)

; 3.57
; (define fibs
;   (cons-stream
;     0 (cons-stream
;         1 (add-streams
;             (stream-cdr fibs) fibs))))
; We call n additions for generating the nth fibinonacci number. If the delay procedure was not
; memoized, it would be much worse (exponential run time), and would have to be called fib(n)
; times.

; 3.58
(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den)
            den
            radix)))
; it expands the decimal representation of num/den

; 3.59
; part 1
(define (inverse s)
  (stream-map (lambda (x) (/ 1 x)) s))

(define (integrate-series s)
  (mul-streams (inverse integers)
               s))

; part 2
(define (negate s)
  (stream-map (lambda (x) (- x)) s))

(define exp-series
  (cons-stream
   1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (negate sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(provide exp-series cosine-series sine-series)

; 3.60
(define add-series add-streams)

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2)
                                          (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

(define trig-identity (add-series (mul-series sine-series
                                              sine-series)
                                  (mul-series cosine-series
                                              cosine-series)))

(provide mul-series trig-identity)

; 3.61
(define (invert-unit-series s)
  (cons-stream 1 (negate (mul-series (stream-cdr s)
                                     (invert-unit-series s)))))

(provide invert-unit-series)

; 3.62
(define (div-series a b)
  (scale-stream (mul-series a
                            (invert-unit-series
                              (scale-stream b
                                (/ 1 (stream-car b)))))
                (stream-car b)))

(define tangent (div-series sine-series cosine-series))
(provide tangent)

; 3.63
;
; prev definition:
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
      1.0 (stream-map
            (lambda (guess)
              (sqrt-improve guess x))
            guesses)))
  guesses)
;
; (define (sqrt-stream x)
;   (cons-stream
;    1.0
;    (stream-map (lambda (guess)
;                  (sqrt-improve guess x))
;                (sqrt-stream x))))
;
; the second sqrt-streams calculates an entire sqrt-stream for each value of x.
; If not memoized, they both have the same run time
;
; 3.64
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (stream-limit s tolerance)
  (cond [(< (abs (- (stream-car s)
                   (stream-car (stream-cdr s))))
           tolerance)
         (stream-car (stream-cdr s))]
        [else (stream-limit (stream-cdr s) tolerance)]))

(provide sqrt stream-limit)

; 3.65
(define (square x)
  (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream
      (- s2 (/ (square (- s2 s1))
               (+ s0 (* -2 s1) s2)))
      (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream
    s
    (make-tableau
      transform
      (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define e-summands
  (stream-map (lambda (n)
                (cond [(= (modulo n 2) 1) (/ 1.0 n)]
                      [else (- (/ 1.0 n))]))
              integers))

(define e-stream
  (partial-sums e-summands))

(provide e-summands e-stream accelerated-sequence euler-transform)
; they converge pretty slowly.

; 3.66
; ((1 1) # car car # 1
;  (1 2) # first interleave
;  (2 2) # car car # 2
;  (1 3) # first interleave
;  (2 3) # second interleave
;  (1 4) # first interleave
;  (3 3) # car car # 4
;  (1 5) # first interleave
;  (2 4) # second interleave
;  (1 6) # first interleave
;  (3 4) # third interleave
;  (1 7) # first interleave
;  (2 5) # second interleave
;  (1 8) # first interleave
;  (4 4) # car car # 8
;  ....
;
;  for (1 100) it will be about 100*2 pairs preceeding it since the (1 n) numbers occur
;  every other number
;
;  for (99 100) it takes 2^99 pairs to get to (99 99). (99 100) occurs another 2^99/2 or 2^98
;  pairs after so about 2^99 + 2^98 pairs
;
;  the pair (100 100) should take about 2^100 pairs to get to.
;  3.67
;  I'm not sure this is right
(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x)
                    (list (stream-car s) x))
                  (stream-cdr t))
      (interleave
        (stream-map (lambda (x)
                      (list x (stream-car s)))
                    (stream-cdr t))
        (pairs (stream-cdr s) (stream-cdr t))))))

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream
      (stream-car s1)
      (interleave s2 (stream-cdr s1)))))

(define (stream-append s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream
      (stream-car s1)
      (stream-append (stream-cdr s1) s2))))

(provide pairs interleave stream-append)
