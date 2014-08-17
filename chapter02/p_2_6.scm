(print "----------------")

(print "問題2.6")
(print "--")

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x))))
)

(define (to-int church)
  ((church (lambda (x) (+ x 1))) 0)
)

(display "(to-int zero) => ")
(print (to-int zero))
(display "(to-int (add-1 zero)) => ")
(print (to-int (add-1 zero)))

; (define inc (lambda (x) (+ x 1)))
;
; ((zero inc) 0)
; ((lambda (x) x) 0)
; 0
;
; (((add-1 zero) inc) 0)
; (((lambda (f) (lambda (x) (f   ((zero f  ) x)))) inc) 0)
; (             (lambda (x) (inc ((zero inc) x)))       0)
;                           (inc ((zero inc) 0))
;                           (inc ((zero inc) 0))
;                           (inc 0             )
;                           1

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(display "(to-int one) => ")
(print (to-int one))
(display "(to-int two) => ")
(print (to-int two))

(define (plus a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x))))
)

(print "--")

(display "(to-int (plus two two)) => ")
(print (to-int (plus two two)))
(display "(to-int (plus one two)) => ")
(print (to-int (plus one two)))
(display "(to-int (plus zero two)) => ")
(print (to-int (plus zero two)))
(display "(to-int (plus (add-1 zero) (add-1 (add-1 two)))) => ")
(print (to-int (plus (add-1 zero) (add-1 (add-1 two)))))
