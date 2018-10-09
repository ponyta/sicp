#lang racket

; Section 2.3.2 - symbolic differentiator
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
           (exponent exp)
           (make-product
             (make-exponentiation
               (base exp)
               (make-sum -1 (exponent exp)))
             (deriv (base exp) var))))
        (else (error "unknown expression
                      type: DERIV" exp))))

; parses an exp and generates an unambigious product/sum
; e.g. (x + y * y) => (x + (y * y))
(define (simplify exp)
  (define (simp-subexp exp)
    (list (simplify (car exp))
          (cadr exp)
          (simplify (cddr exp)))))
  (if (> (length exp) 3)
    (

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s)
  (foldl make-sum 0 (cddr s)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p)
  (foldl make-product 1 (cddr p)))

(define (make-exponentiation base exponent)
  (cond [(and (number? base) (number? exponent))
        (expt base exponent)]
        [(=number? exponent 0) 1]
        [(=number? exponent 1) base]
        [(=number? base 0) 0]
        [(=number? base 1) 1]
        [else (list base '** exponent)]))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (cadr exp) '**)))

(define (base exp)
  (car exp))

(define (exponent exp)
  (caddr exp))

(provide deriv)
