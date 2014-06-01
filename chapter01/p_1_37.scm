(print "問題1.37")
(print "--")

(define tolerance 0.0001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
  )
  (define (try guess)
    (let
      ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next)
      )
    )
  )
  (try first-guess)
)

(define (cont-frac n d k s)
  (define (cont-frac-itr cnt res)
    (if (= cnt 0)
      res
      (let
        ((ni (n cnt)) (di (d cnt)))
        (cont-frac-itr (- cnt 1) (/ ni (+ di res)))
      )
    )
  )
  (cont-frac-itr k s)
)

(define (golden-ratio num)
  (/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) num 0.0))
)

(print (golden-ratio 1000))

(print "----------------")

(print "問題1.38")
(print "--")

(define (euler num)
  (define (cont-frac-tri cnt res)
    (define (n i) 1.0)
    (define (d i) (if (odd? i) 1.0 (* cnt 2.0)))
    (cont-frac n d 3 res)
  )
  (define (cont-frac-tri-itr cnt res)
    (if (= cnt 0)
      res
      (let
        ((nxt (cont-frac-tri cnt res)))
        (cont-frac-tri-itr (- cnt 1) nxt)
      )
    )
  )
  (cont-frac-tri-itr num 0.0)
)

(print (euler 1000))

(print "----------------")

(print "問題1.39")
(print "--")

(define (tan-cf x k)
  (/ (cont-frac (lambda (i) (- (* x x))) (lambda (i) (- (* i 2) 1)) k 0.0) (- x))
)

(define pi 3.14159265359)
(print (tan-cf pi       1000))
(print (tan-cf (/ pi 2) 1000))
(print (tan (/ pi 2)))
(print (tan-cf (/ pi 4) 1000))
(print (tan (/ pi 4)))
(print (tan-cf (/ pi 6) 1000))
(print (tan-cf (- (/ pi 4)) 1000))

