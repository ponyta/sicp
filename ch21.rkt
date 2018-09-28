#lang racket

;; 2.1
;; Rational number data structure
(define (make-rat n d)
  ; sequential let bindings, allowing isPositive to be available in later bindings
  (let* ((g (gcd n d))
        (isPositive (> (* n d) 0))
        (corrected-n (if isPositive (abs (/ n g))
                       (- (abs (/ n g)))))
        (corrected-d (abs (/ d g))))
    (cons corrected-n corrected-d)))

#| (define (make-rat n d) |#
#|   (let ((g (gcd n d))) |#
#|     (cons (/ n g) |#
#|           (/ d g)))) |#

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(provide make-rat numer denom add-rat sub-rat mul-rat div-rat equal-rat?)

;; 2.2

(define (average x y)
  (/ (+ x y) 2))
(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment seg)
  (make-point (average (x-point (start-segment seg))
                       (x-point (end-segment seg)))
              (average (y-point (start-segment seg))
                       (y-point (end-segment seg)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(provide make-segment start-segment end-segment make-point x-point y-point midpoint-segment print-point)

;; 2.3
; defines a rectangle with the top left most point coords, and it's width and height.
(define (make-rect top-left width height)
  (cons top-left (cons width height)))

(define (width-rect r)
  (car (cdr r)))
(define (height-rect r)
  (cdr (cdr r)))
(define (top-left-rect r)
  (car r))

(define (perimeter rect)
  (+ (* 2 (width-rect rect))
     (* 2 (height-rect rect))))

(define (area rect)
  (* (width-rect rect) (height-rect rect)))

(provide make-rect perimeter area)

;; 2.4
#| (define (cons x y) |#
#|   (lambda (m) (m x y))) |#

#| (define (car z) |#
#|   (z (lambda (p q) p))) |#

#| (define (cdr z) |#
#|   (z (lambda (p q) q))) |#

#| (provide cons car cdr) |#

;; 2.5
(define (power x y)
  (if (= y 0) 1
    (* x (power x (- y 1)))))

(define (biggest-pow-divisible base num)
  (define (helper i)
    (if (= (modulo num (power base i)) 0) (helper (+ 1 i))
      (- i 1)))
  (helper 1))

(define (cons x y)
  (* (power 2 x) (power 3 y)))

(define (car z)
  (biggest-pow-divisible 2 z))

(define (cdr z)
  (biggest-pow-divisible 3 z))

(provide cons car cdr)


;; 2.6
;; Church numerals

(define (inc x)
  (+ 1 x))

(define zero
  (lambda (f)
    (lambda (x)
      x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

#| ;; let's try evaluating |#
#| (add-1 zero) |#
#| => (lambda (f) |#
#|       (lambda (x) |#
#|         (f ((zero f) x)))) |#

#| ; first let's evaluate |#
#| (zero f) |#
#| => ((lambda (f) |#
#|       (lambda (x) |#
#|         x)) f) |#
#| => (lambda (x) x) |#

#| ; (note that (lambda (x) x) is just the identity function) |#
#| ; so the next evaluation from above is |#
#| => (lambda (f) |#
#|       (lambda (x) |#
#|         (f ((lambda (x) x) x)))) |#
#| => (lambda (f) |#
#|       (lambda (x) |#
#|         (f x))) |#

;; let's try evaluating two now.
#| (define one (add-1 zero)) |#
#| (add-1 one) |#
#| => (lambda (f) |#
#|      (lambda (x) |#
#|        (f ((one f) x)))) |#
#| ... |#
#| (one f) |#
#| => ((lambda (f) |#
#|      (lambda (x) |#
#|        (f x))) f) |#
#| => (lambda (x) (f x)) |#
#| ... |#
#| ((lambda (x) (f x)) x) |#
#| => (f x) |#
#| ... |#
#| => (lambda (f) |#
#|      (lambda (x) |#
#|        (f (f x)))) |#

; Hence add-1 is "unwrapping" the number by applying it to f and then x, then applies f to it and
; rewraps it. Every iteration of add-1 adds another application of f internally.
;
; One really cool thing is that for some church numeral n, n is actually a procedure that returns a
; procedure that takes in an argument x. That procedure applies f n times to x. Thus, you can print
; the numeric value of a church numeral n as follows:
;
; ((n inc) 0)
; => n
;
; For example
; ((one inc) 0)
; => 1
; Since we are applying inc to 0 "one time"

(define one (lambda (f)
              (lambda (x)
                (f x))))
(define two (lambda (f)
              (lambda (x)
                (f (f x)))))
(define three (add-1 two))
(define four (add-1 three))
(define five (add-1 four))

(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

(provide add-1 zero one two three four five inc add)
