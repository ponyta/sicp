#lang racket

; wires
(define (make-wire)
  (let [(signal-value 0)
        (action-procedures '())]
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
        (begin (set! signal-value new-value)
               (call-each action-procedures))
        'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (mcons proc action-procedures))
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
      ((mcar procedures))
      (call-each (mcdr procedures)))))

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

(define (make-agenda) (mcons 0 '()))
(define the-agenda (make-agenda)) ; global agenda used
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (make-time-segment time queue)
  (mcons time queue))

(define (segment-time s) (mcar s))
(define (segment-queue s) (mcdr s))

(define (current-time agenda) (mcar agenda))
(define (set-current-time! agenda time)
  (set-mcar! agenda time))

(define (segments agenda) (mcdr agenda))
(define (set-segments! agenda segments)
  (set-mcdr! agenda segments))

(define (first-segment agenda)
  (mcar (segments agenda)))
(define (rest-segments agenda)
  (mcdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (mcar segments)))))

  (define (make-new-time-segment time action)
    (let [(q (make-queue))]
      (insert-queue! q action)
      (make-time-segment time q)))

  ; searches through segments for proper time, otherwise insert new time segment
  (define (add-to-segments! segments)
    (if (= (segment-time (mcar segments)) time)
      (insert-queue! (segment-queue (mcar segments))
                     action)
      (let [(rest (mcdr segments))]
        (if (belongs-before? rest)
          (set-mcdr!
            segments
            (mcons (make-new-time-segment time action)
                  (mcdr segments)))
          (add-to-segments! rest)))))

  (let [(segments (segments agenda))]
    (if (belongs-before? segments)
      (set-segments!
        agenda
        (mcons (make-new-time-segment time action)
              segments))
      (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let [(q (segment-queue (first-segment agenda)))]
    (delete-queue! q)
    (when (empty-queue? q)
      (set-segments! agenda (rest-segments agenda))))) ; otherwise do nothing (one-armed if)

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty -- FIRST-AGENDA-ITEM")
    (let [(first-seg (first-segment agenda))]
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

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
        [else (error "Invalid signal" s1 s2)]))

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
  (cond [(and (= s1 0) (= s2 0)) 0]
        [(or (= s1 1) (= s2 1)) 1]
        [else (error "Invalid signal" s1 s2)]))

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
; A for the mcarry
;
; A full-adder takes delay:
; 2 * (O + 2A + I) for the sum
; = 2O + 4A + 2I for the sum
; (O + 2A + I) + A + O
; = 2O + 3A + I for the mcarry

; 3.30
; takes in three lists: input wires A and B, and output wires S and the single mcarryover
; output c
; (full-adder a b c-in sum c-out)
(define (ripple-carry-adder A B S c)
  ; returns cin
  (define (helper A B cout S)
    (if (null? A) (make-wire)
      (let [(cin (helper (mcdr A)
                         (mcdr B)
                         (make-wire)
                         (mcdr S)))]
        (full-adder (mcar A)
                    (mcar B)
                    cin
                    (mcar S)
                    cout)
        cin)))
  (helper A B c S))

;; helper functions
(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire proc)
  ((wire 'add-action!) proc))

;; queue data structure from 3.3.2
(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr! queue item) (set-mcdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (mcons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let [(new-pair (mcons item '()))]
    (cond [(empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue]
          [else
            (set-mcdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue])))

(define (delete-queue! queue)
  (cond [(empty-queue? queue)
         (error "DELETE! called with an empty queue" queue)]
        [else
          (set-front-ptr! queue (mcdr (front-ptr queue)))
          queue]))

(define (print-queue queue)
  (front-ptr queue))

(define (make-wire-lst lst)
  (if (null? lst) '()
    (let [(w (make-wire))]
      (set-signal! w (car lst))
      (mcons w (make-wire-lst (cdr lst))))))

(define A (make-wire-lst '(1 0 0 0 0 0 0 1)))
(define B (make-wire-lst '(0 1 1 0 0 1 1 0)))
(define S (make-wire-lst '(0 0 0 0 0 0 0 0)))
(define (set-probes wires)
  (if (null? wires) 'ok
    (begin (probe 'probe (mcar wires))
           (set-probes (mcdr wires)))))
(set-probes S)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define carry (make-wire))

(probe 'carry carry)

(define (print-bits wires)
  (if (null? wires) (newline)
    (begin (display (get-signal (mcar wires)))
           (display " ")
           (print-bits (mcdr wires)))))

(provide print-bits A B S ripple-carry-adder probe make-wire set-signal! get-signal half-adder or-gate and-gate input-1 input-2 carry propagate)
