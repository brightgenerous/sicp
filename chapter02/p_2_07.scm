(print "----------------")

(print "問題2.7")
(print "--")

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))
  )
)

(define (mul-interval x y)
  (let (
         (p1 (* (lower-bound x) (lower-bound y)))
         (p2 (* (lower-bound x) (upper-bound y)))
         (p3 (* (upper-bound x) (lower-bound y)))
         (p4 (* (upper-bound x) (upper-bound y)))
       )
       (make-interval (min p1 p2 p3 p4)
                      (max p1 p2 p3 p4)
       )
  )
)

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))
                )
  )
)

(define (make-interval a b) (cons a b))

; --

(define (lower-bound interval)
  (let (
         (x (car interval))
         (y (cdr interval))
       )
       (if (< x y) x y)
  )
)

(define (upper-bound interval)
  (let (
         (x (car interval))
         (y (cdr interval))
       )
       (if (> x y) x y)
  )
)

(define (disp-interval desc interval)
  (print desc)
  (print interval)
  (display "  lower-bound => ")
  (print (lower-bound interval))
  (display "  upper-bound => ")
  (print (upper-bound interval))
)

; --

(define interval-1 (make-interval 100 200))
(disp-interval "[interval-1] => " interval-1)

(define interval-2 (make-interval 300 50))
(disp-interval "[interval-2] => " interval-2)

(disp-interval "(add-interval interval-1 interval-2) => " (add-interval interval-1 interval-2))
(disp-interval "(mul-interval interval-1 interval-2) => " (mul-interval interval-1 interval-2))

(print "----------------")

(print "問題2.8")
(print "--")

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))
  )
)

(disp-interval "(sub-interval interval-1 interval-2) => " (sub-interval interval-1 interval-2))
