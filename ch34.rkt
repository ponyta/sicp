#lang sicp

; 3.38
; There are 6 possibilities:
; Peter, Paul, Mary: $45
; Peter, Mary, Paul: $35
; Paul, Peter, Mary: $45
; Paul, Mary, Peter: $50
; Mary, Paul, Peter: $40
; Mary, Peter, Paul: $40
; Overall, there are 4 possibilities.
; If they can be interleaved, there are much, much more possibilities. One example is that it is
; now possible for the balance to just be $110. Let's say Peter starts his transaction first. He
; reads in the balance as $100 and adds $10 to get $110. Then, Paul and Mary both fully complete
; their transactions, and Peter finished his. Peter then is writing $110 to the balance, completely
; overwriting Paul and Mary's changes.
;
; 3.39
; (define x 10)
; (define s (make-serializer))
; (parallel-execute
;   (lambda ()
;       (set! x ((s (lambda () (* x x))))))
;         (s (lambda () (set! x (+ x 1)))))
;
; Let us label the following events:
; A -> set x
; B -> run (lambda () (* x x))
; C -> run (lambda () (set! x (+ x 1)))
; A must follow B, so we are limited to 3 orders:
;
; BAC => B becomes 100, A sets x to 100, C sets x to 101
; BCA => B becomes 100, C sets x to 11, A sets x to 100
; CBA => C sets x to 11, B becomes 121, A sets x to 121
;
; EDIT:
; However, C can also be interrupted, and finish last, giving us 11. This is because A is not
; serialized, and hence can be interleaved between C as it is not a member of serialized
; processes. Thus, B calculates (* x x) = 100, C begines running, calculating (+ x 1) = 11, A runs
; setting x to 100, and then C finishes, setting x to 11.
;
; 1. 100
; 2. 121
; 3. 101
; 4. 11
;
; 3.40
; (define x 10)
; (parallel-execute (lambda () (set! x (* x x)))
;                   (lambda () (set! x (* x x x))))
; 1. 100 -> Let P1 calculate (* x x) to be 100, then P2 run, then P1 sets x to 100
; 2. 1000 -> Let P2 calculate (* x x x) to be 1000, then P1 runs, then P2 sets x to 1000
; 3. 10000 -> Let P1 first get x as 10, then P2 runs setting x to 1000, then P1 finishes, multiplying 10 by 1000 and setting 10000
; 4. 100000 -> Let P2 first get x as 10, then P1 runs setting x to 100. Then P2 finishes, multiplying 10*100*100 setting x to 100000
; 5. 1000000 -> Let P1 and P2 run serially, getting 1000000
;
; (define x 10)
; (define s (make-serializer))
; (parallel-execute (s lambda () (set! x (* x x)))
;                   (s (lambda () (set! x (* x x x)))))
;
; Given serialized procedures, either order gives us x as 1000000
;
; 3.41
; No, reading the balance does not matter; except that the value read may be incorrect.
;
; 3.42
; Yes, this is a safe change to make; two calls to the same serialized procedure should not be
; allowed to interleave with itself.
;
; 3.43
; If processes are run sequentially, then the balances should stay the same. After any sequential
; run of serialized-exchange, let A1 be the balance of account1 and A2 be the balance of account2.
; Then, since exchange is sequential, at the end:
; difference = A1-A2
; A1 = A1 - (A1-A2)
;    = A1 - A1 + A2
;    = A2
; A2 = A2 + (A1-A2)
;    = A2 + A1 - A2
;    = A1
; Thus exchange literally swaps the values in the two accounts, and so the accounts should have 10,
; 20 and 30 in some order. However, if the exchange program is not serialized, then the following
; can occur:
;
; (define (exchange account1 account2)
;   (let ((difference (- (account1 'balance)
;                        (account2 'balance))))
;     ((account1 'withdraw) difference)
;     ((account2 'deposit) difference)))
;
; Let A1 have a balance of $10, A2 have a balance of $20 and A3 have a balance of $30. Then, let us
; run:
; (parallel-execute (lambda () (exchange A1 A2)) <- call this P1
;                   (lambda () (exchange A1 A3))) <- call this P2
; Let P1 calculate difference as -10, and P2 calculate difference as -20. Then, P1 withdraws -10 from
; A1, giving it 20 and deposits -10 from A2 giving it 10. Then, P2 withdraws -20 from A1 giving it
; 40 and deposits -20 from A3 giving it 10. Thus A1 = 40, A2 = 10 and A3 = 10.
;
; 3.44
; (define
;   (transfer from-account to-account amount)
;   ((from-account 'withdraw) amount)
;   ((to-account 'deposit) amount))
; There is no concurrency problem here. The amount transfered is not dependant on some state that
; can be changed from outside the procedure, hence there is no issue with parallel execution, so
; long as from-account has sufficient funds.
;
; If from-account does not have sufficient funds, a problem may arise as a transfer procedure
; could initialize a withdraw, and not be able to complete it.
;
; 3.45
; If we called serialized-exchange on A1 and A2, then we will first try to run serialized-exchange,
; locking on balance-serializer. Next we will try to run withdraw from A1, but the serializer has
; already been locked. Hence we will end up in deadlock.
;
; 3.46
; This is a classic concurrency problem. Test-and-set! can fail in the following fashion: Let P1 and
; P2 be two concurrently executing processes both trying to acquire the mutex m. If P1 attempts to
; acquire m by running test-and-set!, but P2 also fetches the value of the mutex between P1 fetching
; the value of m and setting it, then both P1 and P2 will assume the mutex is unacquired and both
; will acquire it, breaking our serialization.
;
; 3.47
; Using a mutex
(define (make-semaphore n)
  (let [(mutex (make-mutex))
        (count 0)]
    (define (semaphore command)
      (mutex 'acquire)
      (cond [(eq? command 'acquire)
             (if (< count n)
               (begin (set! n (+ n 1))
                      (mutex 'release))
               (begin (mutex 'release)
                      (semaphore 'acquire)))]
            [(eq? command 'release)
             (begin (set! n (- n 1))
                    (mutex 'release))]
            [else (begin (error "Unknown command -- SEMAPHORE" command)
                         (mutex 'release))])))
  semaphore)

; Using test-and-set!
(define (make-semaphore n)
  (define (clear! cell)
    (set-car! cell false))

  (let [(count 0)
        (used (list false))]
    (define (semaphore command)
      (if (test-and-set! used) ; if the semaphore is being used
        (semaphore command) ; try again
        (cond [(eq? command 'acquire) ; otherwise...
               (if (< count n)
                 (begin (set! n (+ n 1))
                        (clear! used))
                 (begin (clear! used)
                        (semaphore 'acquire)))]
              [(eq? command 'release)
               (begin (set! n (- n 1))
                      (clear! used))]
              [else (begin (error "Unknown command -- SEMAPHORE" command)
                           (clear! used))]))))

  semaphore)

; 3.48
; Ordering resources in an DAG prevents deadlock, since if there are two competing processes looking
; to acquire R, one of them will acquire it first and "win" the race.
;
; 3.49
; If a process needs to acquire a resource later in the DAG before knowing to try and acquire a
; resource before in the ordering, then this can cause a problem. One possible solution is to
; force all processes to release all resources, and reacquire them in the correct order should they
; run into this situation. However, this is usually inefficient enough to be a bad solution.
