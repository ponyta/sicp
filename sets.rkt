#lang racket

; sets as unordered elements
; 2.59
; (define (element-of-set? x set)
;   (cond [(null? set) #f]
;         [(equal? x (car set)) #t]
;         [else (element-of-set? x (cdr set))]))

; (define (adjoin-set x set)
;   (if (element-of-set? x set)
;     set
;     (cons x set)))

; (define (intersection-set set1 set2) ; n^2 time
;   (cond [(or (null? set1) (null? set2)) '()]
;         [(element-of-set? (car set1) set2)
;          (cons (car set1)
;                (intersection-set (cdr set1) set2))]
;         [else (intersection-set (cdr set1) set2)]))

; (define (union-set set1 set2) ; n^2 time
;   (cond [(null? set1) set2]
;         [(null? set2) set1]
;         [(element-of-set? (car set1) set2)
;          (union-set (cdr set1) set2)]
;         [else (union-set (cdr set1)
;                          (cons (car set1) set2))]))
(define x (list 2 3 5))
(define y (list 1 2 3 6 7 8 9))

; 2.60
; element-of-set? is the same ; n time
; But now n potentially worse as it is number of elements in list
; (define (adjoin-set x set) ; constant time
;   (cons x set))

; (define union-set append) ; n time

; intersection-set is the same (n^2 time) but also potentially worse
; I would use this implementation if union and adjoin time were important
; Otherwise, it is potentially a strictly worse implementation as you increase
; future element-of-set? and intersection-set lookups a lot by adjoining duplicates
; in.

; sets as ordered elements
; (define (element-of-set? x set)
;   (cond [(null? set) #f]
;         [(= x (car set)) #t]
;         [(< x (car set)) #f]
;         [else (element-of-set? x (cdr set))]))

(define (intersection-set-list set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let [(x1 (car set1))
          (x2 (car set2))]
      (cond [(= x1 x2)
             (cons x1 (intersection-set-list (cdr set1)
                                         (cdr set2)))]
            [(< x1 x2)
             (intersection-set-list (cdr set1) set2)]
            [(< x2 x1)
             (intersection-set-list set1 (cdr set2))]))))

; 2.61
; (define (adjoin-set x set)
;   (cond [(null? set) (list x)]
;         [(= x (car set)) set]
;         [(< x (car set)) (cons x set)]
;         [else (cons (car set) (adjoin-set x (cdr set)))]))

; 2.62
(define (union-set-list set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [(= (car set1) (car set2))
         (cons (car set1) (union-set-list (cdr set1) (cdr set2)))]
        [(< (car set1) (car set2))
         (cons (car set1) (union-set-list (cdr set1) set2))]
        [(< (car set2) (car set1))
         (cons (car set2) (union-set-list set1 (cdr set2)))]))

; sets as binary trees
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond [(null? set) #f]
        [(= x (entry set)) #t]
        [(< x (entry set))
         (element-of-set? x (left-branch set))]
        [(> x (entry set))
         (element-of-set? x (right-branch set))]))

(define (adjoin-set x set)
  (cond [(null? set) (make-tree x '() '())]
        [(= x (entry set)) set]
        [(< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set))]
        [(> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set)))]))

; 2.63
(define (tree->list tree)
  (if (null? tree)
    '()
    (append (tree->list (left-branch tree))
            (cons (entry tree)
                  (tree->list (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

(define example-tree
  (make-tree 8
             (make-tree 4
                        (make-tree 1 '() '())
                        (make-tree 6 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))
(define example-tree2
  (make-tree 3
             (make-tree 1 '() '())
             (make-tree 7
                        (make-tree 5 '() '())
                        (make-tree 9 '() (make-tree 11 '() '())))))

(define example-tree3
  (make-tree 5
             (make-tree 3
                        (make-tree 1 '() '())
                        '())
             (make-tree 9
                        (make-tree 7 '() '())
                        (make-tree 11 '() '()))))
; a)
; Yes they do produce the same result. For the example trees they all produce '(1 3 5 7 9 11)
;
; b)
; The first has time O(nlogn), the second has O(n) (use the master method)

(provide element-of-set? adjoin-set x y example-tree example-tree2 example-tree3 tree->list tree->list-2)

; 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

; takes in a list of elements to turn into a tree and the number of elements to turn into a tree
; returns a pair; the car is the tree and the cdr is the list of elements not in the tree yet.
(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let* [(left-size (quotient (- n 1) 2))
           (left-result (partial-tree elts left-size)) ; transform the left elements
           (left-tree (car left-result))
           (non-left-elts (cdr left-result))
           (right-size (- n (+ left-size 1)))
           (this-entry (car non-left-elts))
           (right-result (partial-tree (cdr non-left-elts)
                                       right-size))
           (right-tree (car right-result))
           (remaining-elts (cdr right-result))]
      (cons (make-tree this-entry left-tree right-tree)
            remaining-elts))))

; a)
; partial tree first takes the first half of the elts list and recursively calls itself to
; generate a tree for that half of the elements. Then, it takes the right half minus the middle
; element, and also generates that half. It then constructs the tree and returns it with any
; left over elements
; b)
; This takes O(n) time (use master method)
(provide list->tree)

; 2.65
(define (intersection-set x y)
  (list->tree (intersection-set-list (tree->list x)
                                     (tree->list y))))

(define (union-set x y)
  (list->tree (union-set-list (tree->list x)
                              (tree->list y))))

(provide union-set intersection-set union-set-list intersection-set-list)

; 2. 66
; assume it's the same as our normal tree representation
; yea there's no "entry" but I'm too lazy to build a whole data structure for one question
(define key entry)

(define (lookup given-key set-of-records)
  (cond [(null? set-of-records) #f]
        [(= given-key (car set-of-records)) #t]
        [(< given-key (car set-of-records))
         (lookup given-key (left-branch set-of-records))]
        [(> given-key (car set-of-records))
         (lookup given-key (right-branch set-of-records))]))

(provide lookup)
