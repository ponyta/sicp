#lang sicp

; 3.12
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair p)
  (if (null? (cdr p)) p
      (last-pair (cdr p))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

; (cdr x)
; should be '(b)
; (define w (append! x y))
; w
; (a b c d)
; (cdr x)
; should be '(b c d) b/c we have modified the environment in x

; 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define c (make-cycle '(a b c)))
; box and pointer looks something like this (see paper):
; a -> b -> c -> (back to beginning) a
; it will infinite loop.

; 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let [(temp (cdr x))]
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v '(a b c d))
(define w (mystery v))

; mystery reverses a list.
; At the end, w points to the beginning of the list. v points to the original element, so the
; last element.
; v -> '(a)
; w -> '(d c b a)

; 3.15 on paper
;
; 3.16 see paper
; (define (count-pairs x)
;   (if (not (pair? x))
;     0
;     (+ (count-pairs (car x))
;        (count-pairs (cdr x))
;        1)))

(define p1 (cons (cons 'x '())
                 (cons 'y '())))
; (count-pairs p1)

(define a (cons 'x '()))
(define p2 (cons (cons a '()) a))
; (count-pairs p2)

(define b (cons a a))
(define p3 (cons b b))
; (count-pairs p3)

(define d (cons 3 '()))
(define p4 (cons 1 (cons 2 d)))
(set-cdr! d p4)
; (count-pairs p4) runs forever!

; 3.17
(define (count-pairs x)
  (define seen '())
  (define (helper p)
    (cond [(memq p seen) 0]
          [(not (pair? p)) 0]
          [else (set! seen (cons p seen))
                (+ (helper (car p))
                   (helper (cdr p))
                   1)]))
  (helper x))

; (count-pairs p1)
; (count-pairs p2)
; (count-pairs p3)
; (count-pairs p4)

; 3.18
; (define (is-cycle lst)
;   (define (helper pair seen)
;     (cond [(null? pair) #f]
;           [(memq pair seen) #t]
;           [else (helper (cdr pair)
;                         (cons pair seen))]))
;   (helper lst '()))

; (is-cycle c)
; (is-cycle '(1 2 3))

; 3.19
; tortise and the hare algorithm
(define (is-cycle lst)
  (define (helper ptr1 ptr2)
    (cond [(null? ptr2) #f] ; not a cycle
          [else (let
                    [(next-ptr1 (cdr ptr1))
                     (next-ptr2
                      (if (null? (cdr ptr2))
                          '()
                          (cdr (cdr ptr2))))]
                  (if (eq? next-ptr1 next-ptr2) #t
                      (helper next-ptr1 next-ptr2)))]))
  (helper lst lst))
; (is-cycle '(1 2 3))
; (is-cycle c)

; 3.20
; see paper

; 3.21
; (define (front-ptr queue) (car queue))
; (define (rear-ptr queue) (cdr queue))
; (define (set-front-ptr! queue item) (set-car! queue item))
; (define (set-rear-ptr! queue item) (set-cdr! queue item))
; (define (empty-queue? queue) (null? (front-ptr queue)))
; (define (make-queue) (cons '() '()))

; (define (front-queue queue)
;   (if (empty-queue? queue)
;     (error "FRONT called with an empty queue" queue)
;     (car (front-ptr queue))))

; (define (insert-queue! queue item)
;   (let [(new-pair (cons item '()))]
;     (cond [(empty-queue? queue)
;            (set-front-ptr! queue new-pair)
;            (set-rear-ptr! queue new-pair)
;            queue]
;           [else
;             (set-cdr! (rear-ptr queue) new-pair)
;             (set-rear-ptr! queue new-pair)
;             queue])))

; (define (delete-queue! queue)
;   (cond [(empty-queue? queue)
;          (error "DELETE! called with an empty queue" queue)]
;         [else
;           (set-front-ptr! queue (cdr (front-ptr queue)))
;           queue]))

; Ben is looking at the rear pointer, which is still pointing to the old cons value with 'b.
; However, this cons is no longer in the list pointed to be front-ptr as front-ptr has
; set-cdr! to a different value. print-queue should print the list pointed to be the front-ptr
; like so:

; (define (print-queue queue)
;   (front-ptr queue))

; (define q1 (make-queue))
; (begin
;   (insert-queue! q1 'a)
;   (insert-queue! q1 'b)
;   (delete-queue! q1)
;   (delete-queue! q1)
;   (print-queue q1))

; 3.22
(define (make-queue)
  (let [(front-ptr '())
        (rear-ptr '())]
    (define (front-queue)
      (if (null? front-ptr)
          (error "Front called with nothing in queue!")
          (car front-ptr)))
    (define (empty-queue?)
      (null? front-ptr))
    (define (insert-queue! item)
      (let [(new-pair (cons item '()))]
        (if (empty-queue?)
            (begin (set! front-ptr new-pair)
                   (set! rear-ptr new-pair)
                   dispatch)
            (begin (set-cdr! rear-ptr new-pair)
                   (set! rear-ptr new-pair)
                   dispatch))))
    (define (print-queue)
      front-ptr)
    (define (delete-queue!)
      (if (empty-queue?)
          (error "Delete called with nothing in queue!")
          (begin (set! front-ptr (cdr front-ptr))
                 dispatch)))
    (define (dispatch m)
      (cond [(eq? m 'front) (front-queue)]
            [(eq? m 'empty?) (empty-queue?)]
            [(eq? m 'insert) insert-queue!]
            [(eq? m 'delete) (delete-queue!)]
            [(eq? m 'print) (print-queue)]
            [else (error "No function to dispatch" m)]))
    dispatch))

; 3.23
; deques
; deques need nodes with 2 pointers: one to the prev node and one to the next
(define (make-deque)
  (cons '() '()))
(define (front-ptr deq)
  (car deq))
(define (set-front-ptr! deq node)
  (set-car! deq node))
(define (rear-ptr deq)
  (cdr deq))
(define (set-rear-ptr! deq node)
  (set-cdr! deq node))
(define (make-node val prev next)
  (list val prev next))
(define (val node)
  (car node))
(define (prev node)
  (cadr node))
(define (next node)
  (caddr node))
(define (set-prev! node p)
  (set-car! (cdr node) p))
(define (set-next! node p)
  (set-car! (cdr (cdr node)) p))

(define (empty-deque? deq)
  (null? (front-ptr deq)))

(define (front-deque deq)
  (if (empty-deque? deq)
      (error "FRONT called with nothing in deque")
      (val (front-ptr deq))))

(define (rear-deque deq)
  (if (empty-deque? deq)
      (error "REAR called with nothing in deque")
      (val (rear-ptr))))

(define (print-deque deq)
  (define (print-nodes node)
    (if (null? node)
        (newline)
        (begin
          (display (val node))
          (display " ")
          (print-nodes (next node)))))
  (print-nodes (front-ptr deq)))

(define (front-insert-deque! deq item)
  (let [(new-node (make-node item '() (front-ptr deq)))]
    (cond [(empty-deque? deq)
           (begin (set-front-ptr! deq new-node)
                  (set-rear-ptr! deq new-node)
                  (print-deque deq))]
          [else
           (begin (set-prev! (front-ptr deq) new-node)
                  (set-front-ptr! deq new-node)
                  (print-deque deq))])))

(define (rear-insert-deque! deq item)
  (let [(new-node (make-node item (rear-ptr deq) '()))]
    (cond [(empty-deque? deq)
           (begin (set-front-ptr! deq new-node)
                  (set-rear-ptr! deq new-node)
                  (print-deque deq))]
          [else
           (begin (set-next! (rear-ptr deq) new-node)
                  (set-rear-ptr! deq new-node)
                  (print-deque deq))])))

(define (front-delete-deq! deq)
  (cond [(empty-deque? deq)
         (error "FRONT-DELETE called on empty deque")]
        [else
         (let [(el (val (front-ptr deq)))]
           (if (eq? (front-ptr deq)
                    (rear-ptr deq))
               (begin (set-front-ptr! deq '())
                      (set-rear-ptr! deq '())
                      el)
               (begin (set-front-ptr! deq (next (front-ptr deq)))
                      el)))]))

(define (rear-delete-deq! deq)
  (cond [(empty-deque? deq)
         (error "REAR-DELETE called on empty deque")]
        [else
         (let [(el (val (rear-ptr deq)))]
           (if (eq? (front-ptr deq)
                    (rear-ptr deq))
               (begin (set-front-ptr! deq '())
                      (set-rear-ptr! deq '())
                      el)
               (begin (set-rear-ptr! deq (prev (rear-ptr deq)))
                      (set-next! (rear-ptr deq) '())
                      el)))]))

;; TABLES
(define (lookup key table)
  (let [(record (assoc key (cdr table)))]
    (if record
        (cdr record)
        #f)))

(define (assoc key records)
  (cond [(null? records) #f]
        [(equal? key (caar records)) (car records)]
        [else (assoc key (cdr records))]))

(define (insert! key value table)
  (let [(record (assoc key (cdr table)))]
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

; 3.24 -> 3.27 SKIP FOR NOW