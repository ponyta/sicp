#lang racket

; 3.1
(define (make-accumulator initial)
  (lambda (x)
    (begin (set! initial (+ x initial))
           initial)))

(provide make-accumulator)

; 3.2
(define (make-monitored f)
  (let [(calls 0)]
    (lambda (x)
       (cond [(eq? x 'how-many-calls?) calls]
             [(eq? x 'reset-count) (set! calls 0)]
             [else (begin (set! calls (+ calls 1))
                          (f x))]))))
(provide make-monitored)

; 3.3, 3.4
; Modified later for 3.7
(define (make-account balance password)
  (define incorrect-guesses 0)
  (define (call-the-cops)
    (error "WEE WOO WEE WOO IT'S THE POLICE HANDS IN THE AIR"))
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance
               (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password amount)
    (begin (set! incorrect-guesses (+ incorrect-guesses 1))
           (if (>= incorrect-guesses 7) (call-the-cops)
             "Incorrect password")))
  (define (check-pass try-pw)
    (if (eq? password try-pw)
      (begin (set! incorrect-guesses 0)
             #t)
      (begin (incorrect-password 0)
             #f)))
  (define (dispatch try-pw m)
    (cond [(eq? m 'check-pass) (check-pass try-pw)]
          [(eq? try-pw password)
           (begin
             (set! incorrect-guesses 0)
             (cond ((eq? m 'withdraw) withdraw)
                   ((eq? m 'deposit) deposit)
                   (else (error "Unknown request:
                                MAKE-ACCOUNT" m))))]
          [else incorrect-password]))
                                dispatch)

(provide make-account)

; 3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define area (* (abs (- x2 x1))
                  (abs (- y2 y1))))
  (define (trial n success)
    (if (= n 0) success
      (if (P (random-in-range x1 x2)
             (random-in-range y1 y2))
        (trial (- n 1)
               (+ success 1))
        (trial (- n 1)
               success))))
  (* (/ (trial trials 0) trials)
     area))

(define (circle-pred x y)
  (<= (+ (* x x)
         (* y y))
      1))

(provide circle-pred estimate-integral)

; 3.6

(define (random-init x)
  (lambda () x))

; PNG using LCG: see wikipedia
(define (random-update state)
  (lambda ()
    (modulo-32 (+ (* 1664525 (state))
                  1013904223))))

(define (modulo-32 x)
  (if (> (integer-length x) 32)
    (arithmetic-shift x (- 32 (integer-length x)))
    x))

(define rand
  (let [(state (random-init (random 2147483648)))]
    (define (dispatch m)
      (cond [(eq? m 'generate)
             (begin (set! state (random-update state))
                    (state))]
            [(eq? m 'reset)
             (lambda (new-val)
               (set! state (random-init new-val)))]
            [else (error "No message matched.")]))
    dispatch))

(provide rand random-init random-update)

; 3.7
(define (make-joint acc pw joint-pw)
  (define (dispatch try-pw m)
    (cond [(eq? try-pw joint-pw) (acc pw m)]
          [else (error "Incorrect password!")]))
  (cond [(acc pw 'check-pass) dispatch]
        [else (error "Incorrect password!")]))

(provide make-joint)

; 3.8
(define var 0)
(define (f x)
  (let [(oldvar var)]
    (begin (set! var (+ var x))
            oldvar)))
(provide f)
