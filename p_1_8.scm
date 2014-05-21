
(define (rippo-iter guess x)
  (if (good-enough? guess x)
    guess
    (rippo-iter (improve guess x)
               x)))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (sanjo guess) x)) 0.001))

(define (sanjo x)
  (* x x x))

(define (rippo x)
  (rippo-iter 1.0 x))


;;---------------------------------


;;--------------------------------

(print (rippo (* 10 10 10)))
(print (rippo (* 300 300 300)))
(print (rippo 4000000000000))
(print (rippo 2))
(print (rippo 0.001))
(print (rippo 0.00000000001))

