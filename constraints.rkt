#lang racket

; adder constraint
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond [(and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me)]
          [(and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me)]
          [(and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me)]))

  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))

  (define (me request)
    (cond [(eq? request 'I-have-a-value)
           (process-new-value)]
          [(eq? request 'I-lost-my-value)
           (process-forget-value)]
          [else
            (error "Unknown request -- ADDER" request)]))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

; multiplier constraint
(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond [(or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me)]
          [(and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me)]
          [(and (has-value? m1) (has-value? product))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me)]
          [(and (has-value? m2) (has-value? product))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me)]))

  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))

  (define (me request)
    (cond [(eq? request 'I-have-a-value)
           (process-new-value)]
          [(eq? request 'I-lost-my-value)
           (process-forget-value)]
          [else
            (error "Unknown request -- MULTIPLIER" request)]))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

; helper functions
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

; probe
(define (probe name connector)
  (define (print-probe value)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value)
    (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond [(eq? request 'I-have-a-value)
           (process-new-value)]
          [(eq? request 'I-lost-my-value)
           (process-forget-value)]
          [else
            (error "Unknown request -- PROBE" request)]))
  (connect connector me)
  me)

; connector is like wire
(define (for-each-except exception proc list)
  (define (loop items)
    (cond [(null? items) 'done]
          [(eq? (car items) exception) (loop (cdr items))]
          [else (proc (car items))
                (loop (cdr items))]))
  (loop list))

(define (make-connector)
  (let ((value false)
        (informant false)
        (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except
               setter
               inform-about-value
               constraints))
            ((not (= value newval))
             (error "Contradiction"
                    (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
        (begin (set! informant false)
               (for-each-except
                 retractor
                 inform-about-no-value
                 constraints))
        'ignored))
    (define (connect new-constraint)
      (when (not (memq new-constraint
                     constraints))
        (set! constraints
          (cons new-constraint
                constraints)))
      (when (has-value? me)
        (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!)
             set-my-value)
            ((eq? request 'forget)
             forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation:
                         CONNECTOR"
                         request))))
    me))

; connector helpers
(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector
                    new-value
                    informant)
  ((connector 'set-value!)
   new-value
   informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

; build C-F converter with constraints
; (define (celsius-fahrenheit-converter c f)
;   (let [(u (make-connector))
;         (v (make-connector))
;         (w (make-connector))
;         (x (make-connector))
;         (y (make-connector))]
;     (multiplier c w u)
;     (multiplier v x u)
;     (adder v y f)
;     (constant 9 w)
;     (constant 5 x)
;     (constant 32 y)
;     'ok))

(define (celsius-fahrenheit-converter x)
    (c+ (c* (c/ (cv 9) (cv 5))
                      x)
              (cv 32)))

; 3.37
(define (c+ x y)
  (let ((z (make-connector)))
      (adder x y z)
          z))

(define (c- x y)
  (let [(z (make-connector))]
    (adder z y x)
    z))

(define (c* x y)
  (let [(z (make-connector))]
    (multiplier x y z)
    z))

(define (c/ x y)
  (let [(z (make-connector))]
    (multiplier z y x)
    z))

(define (cv x)
  (let [(y (make-connector))]
    (constant x y)
    y))


; 3.33
(define (averager a b avg)
  (define (process-new-value)
    (cond [(and (has-value? a) (has-value? b))
           (set-value! avg
                       (/ (+ (get-value a) (get-value b)) 2)
                       me)]
          [(and (has-value? a) (has-value? avg))
           (set-value! b
                       (- (* 2 (get-value avg))
                          a)
                       me)]
          [(and (has-value? b) (has-value? avg))
           (set-value! a
                       (- (* 2 (get-value avg))
                          b)
                       me)]))

  (define (process-forget-value)
    (forget-value! avg me)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))

  (define (me request)
    (cond [(eq? request 'I-have-a-value)
           (process-new-value)]
          [(eq? request 'I-lost-my-value)
           (process-forget-value)]
          [else
            (error "Unknown request -- AVERAGER" request)]))
  (connect a me)
  (connect b me)
  (connect avg me)
  me)

(define C (make-connector))
(define A (make-connector))
(define B (make-connector))
(define D (make-connector))
(define F (celsius-fahrenheit-converter C))
(probe "A probe" A)
(probe "B probe" B)
(probe "C probe" C)
(probe "D probe" D)
(probe "F probe" F)

(provide A B D C F probe set-value! get-value has-value? forget-value! connect adder multiplier constant averager make-connector)

; 3.34
; When you try to set the value of B, A is not updated properly since m2 is not set
; (i.e. none of the conditions in multiplier's process-new-value are fullfilled)
;
; 3.35
(define (squarer a b)
  (define (process-new-value)
    (when (has-value? b)
      (if (< (get-value b) 0)
        (error "square less than 0:
               SQUARER"
               (get-value b))
        (set-value! a (sqrt (get-value b)) me)))
    (when (has-value? a)
      (set-value! b (* (get-value a)
                       (get-value a)) me)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me))

  (define (me request)
    (cond [(eq? request 'I-have-a-value)
           (process-new-value)]
          [(eq? request 'I-lost-my-value)
           (process-forget-value)]
          [else
            (error "Unknown request -- SQUARER" request)]))
  (connect a me)
  (connect b me)
  me)

(provide squarer)
