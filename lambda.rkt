#lang racket

; How much of a language can we build using only lambda functions and substitution?
; The rules of lambda calculus:
;
; An expression is:
; <expression> := <name> | <function> | <application>
; <function> := lambda <name> . <expression>
; <application> := <expression> <expression>
;
; A name is any variable. An application is a substitution of a variable in the first expression
; with the second expression.
;
; In Racket:
;
; A variable/name is any symbol i.e. x, n, num
; A function might look like: (lambda (<name>) (<expression>))
; An application might look like: (<function> <expression>).
;
; If you are more familar with a language like Javascript, you can also implement them like so:
; A variable is any symbol like x
; A function might look like: function(<name>) { <expression> }
; An application might look like: (<function>)(<expression>);
;
; Note that there are NO named functions! ALL functions are anonymous in lambda calculus: to
; call a function, we write out the body and call it's parameters. However, for the sake of
; convience we will allow naming our functions when it is convenient (but not to cheat our way
; through things) by using Racket's define keyword to give things names.
;
; Note also that although multi-variable functions are easy to have via currying, I have decided
; to forgo a formal proof for now. Just know that the following is equivilent:
;
; (lambda (x y z) <body>) <=> (lambda (x) (lambda (y) (lambda (z) <body>)))
;
; I will use curried and non-curried functions based on what is convenient and more clear.

; Church numerals
; numerals themselves
(define zero
  (lambda (f)
    (lambda (x)
      x)))

(define suc
  (lambda (n)
    (lambda (f)
      (lambda (x)
        (f ((n f) x))))))

(define one (suc zero))
(define two (suc one))
(define three (suc two))
(define four (suc three))
(define five (suc four))

; note that a church numeral a is the application of f a times. Hence, addition of a and b is
; applying suc a times to b
(define add
  (lambda (a b)
    ((a suc) b)))

(define mult
  (lambda (a b)
    (lambda (f)
      (a (b f)))))

; testing helpers
(define (inc x)
  (+ 1 x))

(define (test n)
  ((n inc) 0))

(provide suc zero one two three four five inc add test mult)
; (test (add four three))
; (test (mult four three))

; Conditionals
;
; True returns the first argument, false returns the second.
(define TRUE
  (lambda (x)
    (lambda (y)
      x)))

(define FALSE
  (lambda (x)
    (lambda (y)
      y)))

; if x is TRUE, it will return y (returning TRUE if y is also TRUE, and FALSE if y is FALSE).
; Otherwise, return FALSE
(define AND
  (lambda (x y)
    ((x y) FALSE)))

(define OR
  (lambda (x y)
    ((x TRUE) y)))

(define NOT
  (lambda (x)
    ((x FALSE) TRUE)))

(define IF
  (lambda (pred truth otherwise)
    ((pred truth) otherwise)))

(define test-bool
  (lambda (b)
    (IF b "TRUE" "FALSE")))

(provide TRUE FALSE AND OR NOT IF test-bool)

; (IF (OR FALSE
;         (AND TRUE
;              (NOT FALSE)))
;     "recurse"
;     "center")

; Okay but maybe some actually useful things...
(define isZero?
  (lambda (n)
    (((n FALSE) NOT) FALSE)))

(provide isZero?)

; pairs can be accessed via TRUE and FALSE
(define pair
  (lambda (a b)
    (lambda (z)
      ((z a) b))))

(define first
  (lambda (p)
    (p TRUE)))
(define rest
  (lambda (p)
    (p FALSE)))

(provide pair first rest)

; increments a pair of number (n, n-1) to (n+1, n)
(define inc-pair
  (lambda (p)
    (pair (suc (first p))
          (first p))))

; predecessor function
(define pre
  (lambda (n)
    (rest ((n inc-pair) (pair zero zero)))))

; now we can have comparison operators!
; greater than or equal
(define GTE
  (lambda (x y)
    (isZero? ((x pre) y))))

(define LTE
  (lambda (x y)
    (isZero? ((y pre) x))))

(define EQ
  (lambda (x y)
    (AND (GT x y) (LT x y))))

(define GT
  (lambda (x y)
    (AND (GTE x y) (NOT (EQ x y)))))

(define LT
  (lambda (x y)
    (AND (LTE x y) (NOT (EQ x y)))))

(define NEQ
  (lambda (x y)
    (NOT (EQ x y))))

(provide inc-pair pre GT LT GTE LTE NEQ EQ)

; one important thing we can't do yet is recursion. Remember that all functions are
; anonymous; we cannot give them names. I have only allowed definitions in here for the
; sake of convinence but a function is not allowed to refer to itself in it's own body.
;
; Recursion however is still possible! Let us consider the following function:
;
; (define Y
;   (lambda (y)
;     ((lambda (x)
;        (y (x x)))
;      (lambda (x)
;        (y (x x))))))

; Let's just say I apply some function R to Y. What happens?
; (Y R)
; => ((lambda (x) (R (x x)) (lambda (x) (R (x x))
; substituting the second (lambda (x) (R x x)) as x in the first half, we get...
; => (R ((lambda (x) (R (x x)) (lambda (x) (R (x x))))
; which is (R (Y R))
; and so on.
;
; Now let's actually utilizing this Y function. I will write a function that sums all the
; numbers from n to 0. For reference, this recursive relationship can be written as:
;
; (define (sum-example n)
;   (cond [(= n 0) 0]
;         [else (+ (sum-example (- n 1))
;                  n)]))
;
; I will define the function R as:
; R = (lambda (r) (lambda (n) (IF (isZero? n) zero (add n (r (pre n))))))
; Then let us consider ((Y R) three)
; From above, we know that (Y R) <=> (R (Y R))
; Hence ((Y R) three)
; => ((R (Y R)) three) ; substituting (Y R) into the first argument of R
; => ((lambda (n) (IF (isZero? n) zero (add n ((Y R) (pre n))))) three) ; substituting three in
; => (IF (isZero? three) zero (add three ((Y R) (pre three))))
; and thus, evaluating the IF statement and (pre three) we get:
; => (add three ((Y R) two))
; => (add three ((R (Y R)) two))
; ... and the whole cycle begins again until we end up with
; => (add three (add two (add one zero)))
; => six
;
; Thanks for our helper function Y, we have recursion! In fact, Y is so famous it has its own
; name: it is called the Y-combinator, and was found by Haskell Curry.
;
; Unfortunately, there are a few small caveats:
;
; 1. Racket is not a lazy language, and will try to evaluate (Y R) first. This will result in
; (R (Y R)) => (R (R (Y R))) => (R (R (R (Y R)))) ... forever instead
;
; 2. Also because of our implementation of IF/booleans, Racket will fully evaluate both
; branches. This means that even if we get around the above, we have to use Racket's built-in
; if statement to only evaluate the branch we want (otherwise we will again infinitely recurse
; since Racket will evaluate the non-base case branch even when it's not needed).
;
; There is one last interesting thing about the Y-combinator. I will define a fixed-point of a
; function g as some x such that g(x) = x. The Y-combinator is also known as a fixed-point
; combinator: if you give it some g, it will find g's fixed point. That is:
;
; (Y g) = x = (g x) = x
; hence
; (Y g) = (g (Y g))
;
; Which we have already shown above. In fact this is how Y implements recursion - by finding
; the fixed-point of its argument (you can imagine the base case of our function R being its
; fixed-point).
;
; To get around this, we are going to tweak our definition of the Y-combinator to work with
; strict-order evaluation.
(define Y
  (lambda (t)
    ((lambda (y)
       (t (lambda (x)
                   ((y y) x))))
     (lambda (y)
       (t (lambda (x)
                   ((y y) x)))))))

; this works similarly, as so:
;
; Y => (lambda (t)
;        ((lambda (y)
;           (t (lambda (x)
;                ((y y) x))))
;         (lambda (y)
;           (t (lambda (x)
;                ((y y) x))))))
; (Y R)
; => ((lambda (y)
;       (R (lambda (x)
;            ((y y) x))))
;     (lambda (y)
;       (R (lambda (x)
;            ((y y) x)))))
; => (R (lambda (x)
;         (((lambda (y)
;             (R (lambda (x)
;                  ((y y) x))))
;           (lambda (y)
;             (R (lambda (x)
;                  ((y y) x))))) x)))
; => (R (lambda (x)
;         ((R (lambda (x)
;               (((lambda (y)
;                   (R (lambda (x)
;                        ((y y) x))))
;                 (lambda (y)
;                   (R (lambda (x)
;                        ((y y) x)))))
;                x))) x)))
; => (R (lambda (x)
;        ((R (lambda (x)
;              ((R (lambda (x)
;                    (((lambda (y)
;                       (R (lambda (x)
;                            ((y y x)))))
;                     (lambda (y)
;                       (R (lambda (x)
;                            ((y y x)))))) x)))
;               x)))
;         x)))
; ...
; => (R (lambda (x)
;         ((R (lambda (x)
;               ((R (lambda (x)
;                     ...
;                     ((R (lambda (x)
;                           (((lambda (y)
;                               (R (lambda (x)
;                                    ((y y) x))))
;                             (lambda (y)
;                               (R (lambda (x)
;                                    ((y y) x))))) x)))
;                      ...
;                      x)))
;                x)))
;          x)))

(define Z
  (lambda (n)
    (((isZero? n) #t) #f)))

; However, let us write this in lambda calculus. We need a two argument (curried) function:
(define R-SUM
  (lambda (r)
    (lambda (n)
      (if (Z n) zero
      ; we have to use if instead of our IF here because our IF evaluates both branches, leading
      ; to an infinite loop :(
        (add n (r (pre n)))))))

(define R-FACT
  (lambda (fact)
    (lambda (n)
      (if (Z n) one
        (mult n (fact (pre n)))))))

(define fact
  (lambda (n)
    ((Y R-FACT) n)))

(provide Y R-SUM fact)
