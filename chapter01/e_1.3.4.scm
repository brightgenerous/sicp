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

(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))

; import-----

(print "例題1.3.4")
(print "--")

(define (average-damp f)
  (lambda (x) (average x (f x)))
)

(display "((average-damp square) 10) => ")
(print ((average-damp square) 10))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0)
)

(display "(sqrt 2) => ")
(print (sqrt 2))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0)
)

(display "(cube-root 3) => ")
(print (cube-root 3))
(print "----------------")

(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)
  )
)

(define (cube x) (* x x x))
(display "((deriv cube) 5) => ")
(print ((deriv cube) 5))
(print "----------------")

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))
  )
)
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess)
)

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0)
)

(print "[newtons-method]")
(display "(sqrt 2) => ")
(print (sqrt 2))
(print "----------------")

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess)
)

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0)
)
(print "[fixed-point] [average-damp] [lambda (y) (/ x y)]")
(display "(sqrt 2) => ")
(print (sqrt 2))
(print "----------------")

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y)) newton-transform 1.0)
)
;(display "[fixed-point] [newton-transform] [lambda (y) (/ x y)] (sqrt 2) => ")
;(print (sqrt 2))
;(print "----------------")

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x)) newton-transform 100.0)
)
(print "[fixed-point] [newton-fransform] [lambda (y) (- (square y) x)]")
(display "(sqrt 2) => ")
(print (sqrt 2))
(print "----------------")

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x)) average-damp 3.0)
)
(display "[fixed-point] [average-damp] [lambda (y) (- (square y) x)] (sqrt 2) => ")
(print (sqrt 2))
(print "----------------")
