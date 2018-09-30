#lang racket

;; 2.7
(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

;; 2.11
; fuck this question
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; 2.10
(define (div-interval x y)
  (if (or (= (upper-bound y) 0)
          (= (lower-bound y) 0))
    (error "Cannot divide by an interval with a bound at zero.")
    (mul-interval x
                  (make-interval
                    (/ 1.0 (upper-bound y))
                    (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))

(define (upper-bound int)
  (cdr int))

(define (lower-bound int)
  (car int))

;; 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

;; 2.9
(define (width x)
  (/ (- (upper-bound x)
        (lower-bound x))
     2))

(provide make-interval upper-bound lower-bound add-interval mul-interval sub-interval div-interval width)

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

;; 2.12
(define (make-center-percent center perc)
  (make-center-width center (* (/ perc 100) center)))

(define (percent i)
  (* (/ (width i) (center i)) 100))

(provide make-center-percent make-center-width center percent)

;; 2.13
; Given two intervals:
; (c-p%, c+p%)
; and (d-q%, d+q%)
;
; their product is:
; (c-p%)*(c-q%)
; => ((cd - cq% - dp% + p%q%), (cd + dp% + cq% + p%q%))
; assuming p%q% is insignificant, we can simplify this to
; (cd - (cq% + dp%), cd + (cq% + dp%))
; Thus we have an interval with center cd and tolerance cq%+dp%.

; 2.14 2.15 2.16 SKIP
(define (par1 r1 r2)
  (div-interval
    (mul-interval r1 r2)
    (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
      one
      (add-interval
        (div-interval one r1)
        (div-interval one r2)))))
