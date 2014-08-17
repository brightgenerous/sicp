(print "----------------")

(print "問題2.4")
(print "--")

(define (cons x y)
  (lambda (m) (m x y))
)

(define (car z)
  (z (lambda (p q) p))
)

(define (cdr z)
  (z (lambda (p q) q))
)

(define (sum z)
  (z (lambda (p q) (+ p q)))
)

(define (max z)
  (z (lambda (p q) (if (> p q) p q)))
)
; --

(define xy (cons 100 200))
(display "car(100,200) => ")
(print (car xy))
(display "cdr(100,200) => ")
(print (cdr xy))
(display "sum(100,200) => ")
(print (sum xy))
(display "max(100,200) => ")
(print (max xy))
(display "avg(100,200) => ")
(print (xy (lambda (p q) (/ (+ p q) 2))))
