#lang racket

; List time
; 2.17
(define (last-pair lst)
  (if (null? (cdr lst)) (car lst)
    (last-pair (cdr lst))))

; 2.18
(define (reverse lst)
  (define (reverse-helper to-reverse reversed)
    (if (null? to-reverse) reversed
      (reverse-helper (cdr to-reverse)
                      (cons (car to-reverse) reversed))))
  (reverse-helper lst empty))

(provide last-pair reverse)

; 2.19
(define us-coins
  (list 50 25 10 5 1))

(define uk-coins
  (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (define first-denomination car)
  (define except-first-denomination cdr)
  (define no-more? null?)
  (cond ((= amount 0)
         1)
        ((or (< amount 0)
             (no-more? coin-values))
         0)
        (else
          (+ (cc
               amount
               (except-first-denomination
                 coin-values))
             (cc
               (- amount
                  (first-denomination
                    coin-values))
               coin-values)))))

(provide cc us-coins uk-coins)

; the order of the coins does not matter - the recursive definition of counting change does not
; specify that you must choose the largest or smallest denomination, it just says you must choose
; a denomination of coin.
;
; One demonstration of this is:
; (cc 100 us-coins)
; => 292
; (define us-coins (list 1 5 10 25 50))
; (cc 100 us-coins)
; => 292

; 2.20
(define (same-parity num . lst)
  (define (filter-parity l)
    (cond [(null? l) empty]
          [(= (modulo num 2)
              (modulo (car l) 2))
           (cons (car l) (filter-parity (cdr l)))]
          [else (filter-parity (cdr l))]))
  (cons num (filter-parity lst)))

(provide same-parity)

; 2.21
(define (square x)
  (* x x))

#| (define (square-list items) |#
#|   (if (null? items) |#
#|     empty |#
#|     (cons (square (car items)) |#
#|           (square-list (cdr items))))) |#

(define (square-list items)
    (map square items))

(provide square-list)

; 2.22
#| (define (square-list items) |#
#|   (define (iter things answer) |#
#|     (if (null? things) |#
#|       answer |#
#|       (iter (cdr things) |#
#|             (cons (square (car things)) |#
#|                   answer)))) |#
#|   (iter items empty)) |#

; Louis Reasoner is an idiot
; this is how I implemented reverse...
#| (define (square-list items) |#
#|   (define (iter things answer) |#
#|     (if (null? things) |#
#|       answer |#
#|       (iter (cdr things) |#
#|             (cons answer |#
#|                   (square |#
#|                     (car things)))))) |#
#|   (iter items empty)) |#

#| (provide square-list) |#

; Louis Reasoner is still an idiot
; (cons answer ...) means add a list to the beginning of a list
; To create a proper list, the second element must be a list
; This results in a weird structure like
; '((((() . 1) . 4) . 9) . 16)

; 2.23
(define (for-each proc lst)
  (if (null? lst) #t
    (begin (proc (car lst))
           (for-each proc (cdr lst)))))

(provide for-each)

; 2.24
; (list 1 (list 2 (list 3 4)))
; => (1 (2 (3 4)))
; AKA
#| (cons 1 |#
#|       (cons (cons 2 |#
#|                   (cons (cons 3 |#
#|                               (cons 4 |#
#|                                     empty)) |#
#|                         empty)) |#
#|             empty)) |#

; 2.25
(define a '(1 3 (5 7) 9))
(define b '((7)))
(define c '(1 (2 (3 (4 (5 (6 7)))))))

; (car (cdr (car (cdr (cdr a)))))
; (car (car b))
; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))

; 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

; (append x y)
; => '(1 2 3 4 5 6)
; (cons x y)
; => '((1 2 3) 4 5 6)
; (list x y)
; => '((1 2 3) (4 5 6))

; 2.27
(define (deep-reverse lst)
  (define (reverse-helper to-reverse reversed)
    (cond [(null? to-reverse) reversed]
          [(pair? (car to-reverse))
           (reverse-helper (cdr to-reverse)
                           (cons (deep-reverse (car to-reverse))
                                 reversed))]
          [else (reverse-helper (cdr to-reverse)
                      (cons (car to-reverse) reversed))]))
  (reverse-helper lst empty))

(provide deep-reverse)

; 2.28
(define (fringe tree)
  (cond [(null? tree) empty]
        [(pair? (car tree)) (append (fringe (car tree))
                                    (fringe (cdr tree)))]
        [else (cons (car tree)
                    (fringe (cdr tree)))]))

(provide fringe)

; 2.29
; part 1
; mobiles have a left and right branch
(define (make-mobile left right)
  (list left right))

; has a length and either a number (weight) or a mobile
(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

; part 2
(define (total-weight mobile)
  (define (weight branch)
    (if [pair? (branch-structure branch)]
      (total-weight (branch-structure branch))
      (branch-structure branch)))
  (+ (weight (left-branch mobile))
     (weight (right-branch mobile))))

(define m1
  (make-mobile (make-branch 4 1)
               (make-branch 1 (make-mobile (make-branch 2 3)
                                           (make-branch 6 1)))))

; part 3
(define (balanced? mobile)
  (define (balanced-branch? b)
    (if [pair? (branch-structure b)]
      (balanced? (branch-structure b))
      #t))
  (define (torque b)
    (if [pair? (branch-structure b)]
      (* (branch-length b)
         (total-weight (branch-structure b)))
      (* (branch-length b)
         (branch-structure b))))
  (and (balanced-branch? (left-branch mobile))
       (balanced-branch? (right-branch mobile))
       (= (torque (left-branch mobile))
          (torque (right-branch mobile)))))

; part 4
; There is not much need to change representations. We can simply redefine
; (define left-branch car)
; (define right-branch cdr)
; (define branch-length car)
; (define (branch-structure cdr))
;
; This is because of the additional layer of abstraction we have built on top of our
; representation of our data.

(provide total-weight m1 balanced? left-branch right-branch branch-length branch-structure)

; 2.30
(define (square-tree tree)
  (map (lambda (x)
         (if (pair? x) (square-tree x)
           (square x))) tree))

(define t1 (list 1
                 (list 2 (list 3 4) 5)
                 (list 6 7)))

(provide square-tree t1)

; 2.31
(define (tree-map proc tree)
  (map (lambda (x)
         (if (pair? x) (tree-map proc x)
           (proc x))) tree))

(provide tree-map)

; 2.32
; The set of subsets of s is:
; all the subsets not containing the first element s AND
; all of the subsets not containing s plus s
(define (subsets s)
  (if (null? s)
    (list empty)
    (let [(rest (subsets (cdr s)))]
      (append rest (map
                     (lambda (x)
                       (cons (car s) x))
                     rest)))))

(provide subsets)

; 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op
                    initial
                    (cdr sequence)))))

; (define (map p sequence)
;   (accumulate (lambda (x y)
;                 (cons (p x) y))
;               empty sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (inc x)
  (+ x 1))

(define (length sequence)
  (accumulate (lambda (x y)
                (inc y)) 0 sequence))

(provide map append length inc square)

; 2.34
(define
  (horner-eval x coefficient-sequence)
  (accumulate
    (lambda (this-coeff higher-terms)
      (+ this-coeff (* x higher-terms)))
    0
    coefficient-sequence))

; (horner-eval 2 (list 1 3 0 5 0 1)) => 1 + 6 + 40 + 32 => 79

(provide horner-eval)

; 2.35
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) (if (pair? x) (count-leaves x) 1))
                   t)))

(provide count-leaves)

; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    empty
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(provide accumulate-n)

; 2.37
(define mat '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define id '((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1)))
(define v '(1 1 1 1))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons empty mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (matrix-*-vector cols row)) m)))

(provide mat v id dot-product matrix-*-vector transpose matrix-*-matrix)

; 2.38
(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; (fold-right / 1 (list 1 2 3))
; => 1/(2/(3/1))
; => 1/(2/3)
; => 3/2
; (fold-left  / 1 (list 1 2 3))
; => (((1/2)/3)/1)
; => (1/6)/1
; => 1/6
; (fold-right list empty (list 1 2 3))
; => (list 1 (list 2 (list 3 empty)))
; => '(1 (2 (3 ())))
; (fold-left list empty (list 1 2 3))
; => (list (list (list 1 2) 3) empty)
; => '(((1 2) 3) ())
; if fold-right and fold-left are to the same, op must be commutative
; i.e. (op a b) == (op b a)

(provide fold-right fold-left)

; 2.39
(define (push el lst)
  (if (null? lst) (cons el empty)
    (cons (car lst)
          (push el (cdr lst)))))

(define (fold-r-reverse sequence)
  (fold-right
   (lambda (x y) (push x y)) empty sequence))

(define (fold-l-reverse sequence)
  (fold-left
   (lambda (x y) (cons y x)) empty sequence))

(provide fold-r-reverse fold-l-reverse)

; 2.40

(define (enumerate-interval a b)
  (if (> a b) empty
    (cons a (enumerate-interval (+ a 1) b))))

(define (flatmap proc seq)
  (accumulate append empty (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(require math/number-theory)

(define (prime-sum? pair)
  (prime? (+ (car pair)
             (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter
         prime-sum?
         (unique-pairs n))))

(provide enumerate-interval flatmap unique-pairs prime-sum-pairs)

; 2.41
(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (triplets-under-n-sum n s)
  (filter (lambda (triplet)
            (= (accumulate + 0 triplet)
               s)) (unique-triples n)))

(provide unique-triples triplets-under-n-sum)

; 2.42
; eight-queens problem
(define (queens board-size)
  ; returns #t if el is in lst, #f otherwise
  (define (exist? el lst)
    (if (empty? lst) #f
      (or (= el (car lst))
          (exist? el (cdr lst)))))

  (define (diagonal? pos positions)
    (define (helper distance lst)
      (cond
        [(empty? lst) #f]
        [(= (abs (- pos (car lst)))
            distance) ; then they are diagonal
         #t]
        [else (helper (+ distance 1)
                      (cdr lst))]))
    (helper 1 positions))

  (define (safe? positions)
    (and (not (exist? (car positions) (cdr positions)))
         (not (diagonal? (car positions) (cdr positions)))))

  (define (adjoin-position new-row rest-of-queens)
    (cons new-row rest-of-queens))

  (define empty-board '())

  ; returns all the ways to place queens on the first k columns
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      ; else
      (filter
        (lambda (positions)
          (safe? positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                     new-row
                     rest-of-queens))
                 (enumerate-interval
                   1
                   board-size)))
          (queen-cols (- k 1))))))

  (queen-cols board-size))

(provide queens)

; 2.43
; Because Louis Reasoner swapped the mappings, he has made the recursive call inside the inner map.
; Thus we must generate a solution for every possible row adjoin.
; Since there are n*n slots on the board, this increases the time n*n time.
; If it takes T time to solve a board of n size, then now we must solve it n*n*T
