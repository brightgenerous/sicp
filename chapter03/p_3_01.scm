(print "----------------")

((lambda ()

  (define balance 100)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

  (print "--")
  (display "(withdraw 25) =>")
  (print (withdraw 25))
  (display "(withdraw 25) =>")
  (print (withdraw 25))
  (display "(withdraw 60) =>")
  (print (withdraw 60))
  (display "(withdraw 15) =>")
  (print (withdraw 15))

))

((lambda ()

  (define new-withdraw
    (let ((balance 100))
      (lambda (amount)
        (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

  (define withdraw new-withdraw)

  (print "--")
  (display "(withdraw 25) =>")
  (print (withdraw 25))
  (display "(withdraw 25) =>")
  (print (withdraw 25))
  (display "(withdraw 60) =>")
  (print (withdraw 60))
  (display "(withdraw 15) =>")
  (print (withdraw 15))

))

((lambda ()

  (define (make-withdraw balance)
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

  (define withdraw (make-withdraw 100))
  (define withdraw2 (make-withdraw 100))

  (print "--")
  (display "(withdraw 25) =>")
  (print (withdraw 25))
  (display "(withdraw 25) =>")
  (print (withdraw 25))
  (display "(withdraw 60) =>")
  (print (withdraw 60))
  (display "(withdraw 15) =>")
  (print (withdraw 15))

  (display "(withdraw2 25) =>")
  (print (withdraw2 25))
  (display "(withdraw2 25) =>")
  (print (withdraw2 25))
  (display "(withdraw2 60) =>")
  (print (withdraw2 60))
  (display "(withdraw2 15) =>")
  (print (withdraw2 15))

))

((lambda ()

   (define (make-account balance)
     (define (withdraw amount)
       (if (>= balance amount)
         (begin (set! balance (- balance amount))
                balance)
         "Insufficient funds"))
     (define (deposit amount)
       (set! balance (+ balance amount))
       balance)
     (define (dispatch m)
       (cond ((eq? m 'withdraw) withdraw)
             ((eq? m 'deposit) deposit)
             (else (error "Unknown request - MAKE ACCOUNT" m))))
     dispatch)

  (define account (make-account 100))

  (print "--")
  (display "((account 'withdraw) 50) =>")
  (print ((account 'withdraw) 50))
  (display "((account 'withdraw) 60) =>")
  (print ((account 'withdraw) 60))
  (display "((account 'deposit) 30) =>")
  (print ((account 'deposit) 30))
  (display "((account 'withdraw) 60) =>")
  (print ((account 'withdraw) 60))

))

;; -----

(print "--")
(print "問題3.01")

((lambda arg

  (define (make-accumulator current)
    (lambda (inc)
      (begin (set! current (+ current inc))
             current)))

  (define A (make-accumulator 5))
  (display "(A 10) => ")
  (print (A 10))
  (display "(A 10) => ")
  (print (A 10))

))

;; -----

(print "--")
(print "問題3.02")

((lambda arg

  (define (make-monitored f)
    (let ((count 0))
      (define (called) (set! count (+ count 1)))
      (define (dispatch arg)
        (cond ((eq? arg 'how-many-calls?) count)
              (else
                (called)
                (f arg))))
      dispatch))

  (define a (make-monitored sqrt))
  (display "(a 100) => ")
  (print (a 100))
  (display "(a 'how-many-calls?) => ")
  (print (a 'how-many-calls?))
  (display "(a 'how-many-calls?) => ")
  (print (a 'how-many-calls?))
  (display "(a 100) => ")
  (print (a 100))
  (display "(a 'how-many-calls?) => ")
  (print (a 'how-many-calls?))

))

;; -----

(print "--")
(print "問題3.03")

((lambda arg

  (define (make-account password balance)
    (define (withdraw amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
           "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request - MAKE ACCOUNT" m))))

    (define (with-check-password pass f)
      (if (eq? pass password) (f) (lambda arg "Incorrect password")))
    (define (dispatch-with-pass pass m)
      (with-check-password pass (lambda () (dispatch m))))

    dispatch-with-pass)

  (define acc (make-account 'secret-password 100))
  (display "((acc 'secret-password 'withdraw) 40) => ")
  (print ((acc 'secret-password 'withdraw) 40))
  (display "((acc 'some-other-password 'deposit) 50) => ")
  (print ((acc 'some-other-password 'deposit) 50))

))

;; -----

(print "--")
(print "問題3.04")

((lambda arg

  (define (make-account password balance)
    (define (withdraw amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
           "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request - MAKE ACCOUNT" m))))

    (let ((count 0) (lock #f))
      (define (call-the-cops)
        ;(display "[-----hogehoge!-----]")
        (set! lock #t)
        (lambda arg "Warning!"))
      (define (with-check-password pass f)
        (cond (lock (lambda arg "Account is Locked"))
              ((eq? pass password)
                (set! count 0)
                (f))
              (else
                (set! count (+ count 1))
                (if (>= count 7) (call-the-cops)
                  (lambda arg "Incorrect password")))))
      (define (dispatch-with-pass pass m)
        (with-check-password pass (lambda () (dispatch m))))

      dispatch-with-pass)
  )

  (define acc (make-account 'secret-password 100))

  (define (itr c f)
    (if (> c 0) (begin (f) (itr (- c 1) f)) 'done))

  (define (correct)
    (display "((acc 'secret-password 'deposit) 50) => ")
    (print ((acc 'secret-password 'deposit) 50)))
  (define (incorrect)
    (display "((acc 'some-other-password 'deposit) 50) => ")
    (print ((acc 'some-other-password 'deposit) 50)))

  (print "-- (itr 5 incorrect) --")
  (itr 5 incorrect)
  (print "-- (itr 1 correct) --")
  (itr 1 correct)
  (print "-- (itr 8 incorrect) --")
  (itr 8 incorrect)
  (print "-- (itr 1 correct) --")
  (itr 1 correct)
  (print "-- (itr 1 incorrect) --")
  (itr 1 incorrect)

))

