(print "----------------")

(print "問題2.2")
(print "--")

(define (make-segment start end)
  (cons start end)
)

(define (make-point x y)
  (cons x y)
)

(define (x-point p)
  (car p)
)

(define (y-point p)
  (cdr p)
)

(define (midpoint-segment segment)
  (define (average a b) (/ (+ a b) 2))
  (make-point (average (car (car segment)) (car (cdr segment)))
              (average (cdr (car segment)) (cdr (cdr segment)))
  )
)

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline)
)

; --

(define start-point (make-point 1 2))
(define end-point (make-point 3 4))
(define segment (make-segment start-point end-point))
(define mid-point (midpoint-segment segment))

(print-point start-point)
(print-point end-point)
(print-point mid-point)
(print "--")

(define start-point (make-point -100 50))
(define end-point (make-point 20 30))
(define segment (make-segment start-point end-point))
(define mid-point (midpoint-segment segment))

(print-point start-point)
(print-point end-point)
(print-point mid-point)

(print "----------------")

; -- define

(define (start-point rectangle)
  (car rectangle)
)

(define (end-point rectangle)
  (cdr rectangle)
)

; -----

(print "問題2.3")
(print "--")

(define (make-rectangle start end)
  (make-segment start end)
)

(define (length a b) 
  (abs (- a b))
)

(define (width rectangle)
  (length (x-point (start-point rectangle)) 
          (x-point (end-point rectangle))
  )
)

(define (height rectangle)
  (length (y-point (start-point rectangle))
          (y-point (end-point rectangle))
  )
)

(define (perimeter rectangle)
  (define (around w h) (+ (* w 2) (* h 2)))
  (around (width rectangle) (height rectangle))
)

(define (area rectangle)
  (* (width rectangle) (height rectangle))
)

; --

(define start (make-point -100 50))
(define end (make-point 20 30))
(define rectangle (make-rectangle start end))

(display "start => ")
(print-point start)
(display "end   => ")
(print-point end)
(display "perimeter => ")
(print (perimeter rectangle))
(display "area      => ")
(print (area rectangle))
