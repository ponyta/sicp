#lang racket

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(define (half-adder a b s c)
  (let [(d (make-wire)) (e (make-wire))]
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let [(c1 (make-wire))
        (c2 (make-wire))
        (s (make-wire))]
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let [(new-value (logical-not (get-signal input)))]
      (after-delay
        inverter-delay
        (lambda ()
          (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond [(= s 0) 1]
        [(= s 1) 0]
        [else (error "Invalid signal" s)]))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let [(new-value (logical-and (get-signal a1)
                                  (get-signal a2)))]
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond [(and (= s1 1) (= s2 1)) 1]
        [(or (= s1 0) (= s2 0)) 0]
        [else (error "Invalid signal" s)]))

; 3.28
(define (or-gate o1 o2 output)
  (define (or-gate-procedure)
    (let [(new-value (logical-or (get-signal o1)
                                 (get-signal o2)))]
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! o1 or-gate-procedure)
  (add-action! o2 or-gate-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond [(and (= s1 0) (s2 0)) 0]
        [(or (= s1 1) (= s2 1)) 1]
        [else (error "Invalid signal" s)]))

; 3.29
; or-gate built from ANDs and inverters
(define (or-gate-2 a b output)
  (let [(a-inv (make-wire))
        (b-inv (make-wire))
        (c (make-wire))]
    (inverter a a-inv)
    (inverter b b-inv)
    (and-gate a-inv b-inv c)
    (inverter c output)
    'ok))

; the delay is 2 inverter delays and an and-gate delay (since the first two inverters work
; in parallel)
; If I is the inverter delay, A is the and delay and O is the or gate delay, then this OR gate
; has delay: 2I + A
;
; A half-adder takes delay:
; O + 2A + I for the sum
; A for the carry
;
; A full-adder takes delay:
; 2 * (O + 2A + I) for the sum
; = 2O + 4A + 2I for the sum
; (O + 2A + I) + A + O
; = 2O + 3A + I for the carry

; 3.30
; takes in three lists: input wires A and B, and output wires S and the single carryover
; output c
; (full-adder a b c-in sum c-out)
(define (ripple-carry-adder A B S c)
  (define (helper A B S cout)
    (if (null? A)
      ; do something...
      (let [(c (make-wire))]
        (full-adder (car A)
                    (car B)
                    (helper (cdr A)
                            (cdr B)
                            (cdr S)
                    (car S)
                    c

(full-adder A1 B1 c1

; wires
(define (make-wire)
  (let [(signal-value 0)
        (action-procedures '())]
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
        (begin (set! signal-value new-value)
               (call-each action-procedure))
        'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond [(eq? m 'get-signal) signal-value]
            [(eq? m 'set-signal!) set-my-signal!]
            [(eq? m 'add-action!) accept-action-procedure!]
            [else (error "Unknown operation -- WIRE" m)]))

    dispatch))

(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin
      ((car procedures))
      (call-each (cdr procedures)))))

;; helper functions
(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire proc)
  ((wire 'add-action!) proc))

; agenda
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let [(first-item (first-agenda-item the-agenda))]
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire))
                 (newline))))

(define the-agenda (make-agenda)) ; global agenda used
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments-agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))

  (define (make-new-time-segment time action)
    (let [(q (make-queue))]
      (insert-queue! q action)
      (make-time-segment time q)))

  ; searches through segments for proper time, otherwise insert new time segment
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
      (insert-queue! (segment-queue (car segments))
                     action)
      (let [(rest (cdr segments))]
        (if (belongs-before? rest)
          (set-cdr!
            segments
            (cons (make-new-time-segment time action)
                  (cdr segments)))
          (add-to-segments! rest)))))

  (let [(segments (segments agenda))]
    (if (belongs-before? segments)
      (set-segments!
        agenda
        (cons (make-new-time-segment time action)
              segments))
      (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let [(q (segment-queue (first-segment agenda)))]
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda))))) ; otherwise do nothing (one-armed if)

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty -- FIRST-AGENDA-ITEM")
    (let [(first-seg (first-segment agenda))]
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

(provide the-agenda propagate probe make-wire set-signal! and-gate inverter or-gate)

;; queue data structure from 3.3.2
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let [(new-pair (cons item '()))]
    (cond [(empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue]
          [else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue])))

(define (delete-queue! queue)
  (cond [(empty-queue? queue)
         (error "DELETE! called with an empty queue" queue)]
        [else
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue]))

(define (print-queue queue)
  (front-ptr queue))
