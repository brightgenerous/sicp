
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (square x)
  (* x x))


;; -----------------------------------------------

(print (sqrt 40000000000009000000000000000000000))
(print (sqrt 4000000000001))
(print (sqrt 4000000000000))
(print (sqrt 2))
(print (sqrt 0.01))
(print (sqrt 0.00000000001))


;; -----------------------------------------------

(define (good-enough? guess x)
  (< (abs (- guess (improve guess x))) 0.000001))


;; -----------------------------------------------

(print (sqrt 40000000000009000000000000000000000))
(print (sqrt 4000000000001))
(print (sqrt 4000000000000))
(print (sqrt 2))
(print (sqrt 0.01))
(print (sqrt 0.00000000001))

