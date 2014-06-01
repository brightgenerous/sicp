(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
  )
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next)
      )
    )
  )
  (try first-guess)
)

(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)
  )
)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))
  )
)

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess)
)

(define (inc x) (+ x 1))

; import-----

(print "問題1.40")
(print "--")

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* x x a) (* b x) c)
  )
)
(display "(netwons-method (cubic 1 2 3) 1)) => ")
(print (newtons-method (cubic 1 2 3) 1))
(display "(netwons-method (cubic 3 2 1) 1)) => ")
(print (newtons-method (cubic 3 2 1) 1))

(print "----------------")

(print "問題1.41")
(print "--")

(define (double f)
  (lambda (x) (f (f x)))
)

(display "(((double (double double)) inc) 5) => ")
(print (((double (double double)) inc) 5))

(display "((double (double (double inc))) 5) => ")
(print ((double (double (double inc))) 5))

(print "----------------")

(print "問題1.42")
(print "--")

(define (square x) (* x x))

; import-----

(define (compose f g)
  (lambda (x) (f (g x)))
)

(display "((compose square inc) 6) => ")
(print ((compose square inc) 6))

(print "----------------")

(print "問題1.43")
(print "--")

(define (repeated f count)
  (define (repeated-iter cnt res)
    (if (= cnt 0)
      res
      (repeated-iter (- cnt 1) (f res))
    )
  )
  (lambda (x) (repeated-iter count x))
)

(display "((repeated square 0) 5) => ")
(print ((repeated square 0) 5))
(display "((repeated square 4) 5) => ")
(print ((repeated square 4) 5))

(print "----------------")

(print "問題1.44")
(print "--")

(define dx 0.00001)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))
)

(display "((smooth (lambda (x) (* x x x))) 10)")
(print ((smooth (lambda (x) (* x x x))) 10))

(print "--")

(define (smooth-fold f count)
  ((repeated smooth count) f)
)

(display "((smooth-fold (lambda (x) (* x x)) 1) 10)")
(print ((smooth-fold (lambda (x) (* x x)) 1) 10))

(display "((smooth-fold (lambda (x) (* x x)) 2) 10)")
(print ((smooth-fold (lambda (x) (* x x)) 2) 10))
