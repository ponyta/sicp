#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

; make a huffman encoding tree
; a tree node is a list with:
; the left branch
; the right branch
; the set of symbols
; and the weight
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '() ; just return empty if we didn't find anything
      (let [(next-branch
              (choose-branch (car bits) current-branch))]
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree)) ; cons the symbol on and start over again with tree
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond [(= bit 0) (left-branch branch)]
        [(= bit 1) (right-branch branch)]
        [else (error "Bad bit -- CHOOSE-BRANCH" bit)]))

; special adjoin set for set of tree nodes for generating huffman tree
(define (adjoin-set x set)
  (cond [(null? set) (list x)]
        [(< (weight x) (weight (car set))) ; add it in!
         (cons x set)]
        [else (cons (car set) ; keep searching
                    (adjoin-set x (cdr set)))]))

; takes in pairs of the form (symbol, weight). For example,
; ((A 4) (B 2) (C 1) (D 1))
(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let [(pair (car pairs))]
      (adjoin-set (make-leaf (car pair) ; symbol
                             (cadr pair)) ; weight
                  (make-leaf-set (cdr pairs))))))

; sample encoding tree
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; 2.67
; (decode sample-message sample-tree)
; should give ADABBCA

; 2.68
(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  ; (cond [(null? tree) #f]
  ;       [(leaf? tree)
  ;        (if (eq? (symbol-leaf tree) symbol) '()
  ;          #f)]
  ;       [else
  ;         (let [(left-encoding (encode-symbol symbol (left-branch tree)))
  ;               (right-encoding (encode-symbol symbol (right-branch tree)))]
  ;           (cond [(list? left-encoding) (cons 0 left-encoding)]
  ;                 [(list? right-encoding) (cons 1 right-encoding)]
  ;                 [else #f]))]))
  (define (encode-helper encoding branch)
    (cond [(and (leaf? branch)
                (eq? (symbol-leaf branch) symbol))
           encoding]
          [(list? (memq symbol (symbols (left-branch branch))))
           (encode-helper (append encoding '(0)) (left-branch branch))]
          [(list? (memq symbol (symbols (right-branch branch))))
           (encode-helper (append encoding '(1)) (right-branch branch))]
          [else (display "symbol was not found")]))
  (encode-helper '() tree))

; 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (cond [(= (length leaf-set) 1) (car leaf-set)]
        [else
          (successive-merge
            (adjoin-set
              (make-code-tree
                (car leaf-set)
                (cadr leaf-set))
              (cddr leaf-set)))]))

(provide encode-symbol encode decode make-leaf-set sample-tree sample-message generate-huffman-tree)

; 2.70
(define songs (generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1))))
(define beatles-song '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

; it takes 84 bits to encode with huffman
; using a standard fixed-length code it would take 3 bits to encode each letter. With 36 words in a song, this is 108 bits

(provide songs beatles-song)

; 2.71
; the huffman tree is similar for to a linked list. It takes 1 bit to encode the most frequent symbol, and n-1 for the least frequent.
; For example, assume we have the frequencies ((A 1) (B 2) (C 4) (D 8) (E 16))
; Then, assuming we have 16 Es, 8 Ds, etc, with our huffman encoding it takes:
; 16*1 + 8*2 + 4*3 + 2*4 + 1*4
; = 16 + 16 + 12 + 8 + 4
; = 32 + 20 + 4
; = 56 bits
; Given a 3 bit fixed encoding, it would take:
; (16 + 8 + 4 + 2 + 1) * 3
; = 31 * 3
; = 93 bits
; which is significantly more.

; 2.72
; In general, it takes n time to encode the more frequent symbol, and n^2 time to encode the
; least frequent symbol.
