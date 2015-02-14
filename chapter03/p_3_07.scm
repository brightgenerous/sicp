(print "----------------")

;; -----

(print "--")
(print "問題3.07")

((lambda arg

  (define (make-account pass bal)

    (let ((balance bal))
      (define (copy-account password)

        ; <-- as it is
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

                ; --> add <--
                ((eq? m 'copy) copy-account)

                (else (error "Unknown request - MAKE ACCOUNT" m))))

        (define (with-check-password pass f)
          (if (eq? pass password) (f) (lambda arg "Incorrect password")))
        (define (dispatch-with-pass pass m)
          (with-check-password pass (lambda () (dispatch m))))

        dispatch-with-pass)
        ; as it is -->

      (copy-account pass)))

  (define (make-joint account pass new-pass)
    ((account pass 'copy) new-pass))

  (define acc1 (make-account 'pass-1 100))
  (define acc2 (make-joint acc1 'pass-1 'pass-2))
  (display "((acc1 'pass-1) 'deposit 10) => ")
  (print ((acc1 'pass-1 'deposit) 10))
  (display "((acc1 'pass-2) 'deposit 10) => ")
  (print ((acc1 'pass-2 'deposit) 10))
  (display "((acc1 'pass-1) 'deposit 20) => ")
  (print ((acc1 'pass-1 'deposit) 20))
  (display "((acc2 'pass-1) 'deposit 20) => ")
  (print ((acc2 'pass-1 'deposit) 20))
  (display "((acc2 'pass-2) 'deposit 20) => ")
  (print ((acc2 'pass-2 'deposit) 20))
  (display "((acc1 'pass-1) 'deposit 10) => ")
  (print ((acc1 'pass-1 'deposit) 10))

))

;; -----

(print "--")
(print "問題3.08")

((lambda arg

  (define f
    (let ((val -1))
      (lambda (x)
        (if (= val -1)
          (begin
            (set! val x)
            x)
          val))))

  (print (+ (f 0) (f 1)))
))

;; -----

(print "--")
(print "問題3.09")

; A.
(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

; B.
(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))

;
; A.
; 大域環境
; +-----------------------------------------------------+
; | 他の変数                                            |
; | factorial: +                                        |
; +------------+----------------------------------------+
;              |   ^             ^                  ^
;              v   |             |                  |
;          +---+---+-+       +-------+          +-------+
;          |   |   | |       |       |          |       |
;          | ○ | ○ + |  E6 ->| n : 6 | ... E1 ->| n : 1 |
;          | | |     |       |       |          |       |
;          +-+-------+       +-------+          +-------+
;            |               (* n (factorial (- n 1)))
;            v
;       パラメタ: n
;       本体: (* n (factorial (- n 1)))
;
; B.
; 大域環境
; +----------------------------------------------------------------------------------------------------------+
; | 他の変数                                                                                                 |
; | factorial: -------------+                                                                                |
; | fact-iter: +            |                                                                                |
; +------------+------------+--------------------------------------------------------------------------------+
;              |   ^        |   ^             ^                 ^                  ^                   ^
;          E2  v   |    E1  v   |             |                 |                  |                   |
;          +---+---+-+  +---+---+-+       +-------+         +-------+         +---------+         +----------+
;          |   |   | |  |   |   | |       |       |         | p : 1 |         | p : 840 |         | p : 5040 |
;          | ○ | ○ + |  | ○ | ○ + |  E1 ->| n : 6 |  E2-1 ->| c : 1 |  E2-6 ->| c :   6 |  E2-7 ->| c :    7 |
;          | | |     |  | | |     |       |       |         | m : 6 |         | m :   6 |         | m :    6 |
;          +-+-------+  +-+-------+       +-------+         +-------+         +---------+         +----------+
;            |            |               (factorial        (fact-iter        (fact-iter          (fact-iter
;            v            v
;          パラメタ:    パラメタ:
;            product      n
;            counter    本体:
;            max-count    (fact-iter 1 1 n)
;          本体:
;            (if (> counter max-count)
;              product
;              (fact-iter (* counter product)
;                         (+ counter 1)
;                         max-count))
;

