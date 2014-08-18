(print "----------------")

(print "問題2.5")
(print "--")

(define (make-ab a b)
  (define (pow x y)
    (define (pow-itr res count)
      (if (= count 0)
        res
        (pow-itr (* res x) (- count 1))
      )
    )
    (pow-itr 1 y)
  )
  (* (pow 2 a) (pow 3 b))
)

(define (count-divisor num divisor)
  (define (count-divisor-itr num count)
    (if (= (remainder num divisor) 0)
      (count-divisor-itr (/ num divisor) (+ count 1))
      count
    )
  )
  (count-divisor-itr num 0)
)

(define (get-a ab)
  (count-divisor ab 2)
)

(define (get-b ab)
  (count-divisor ab 3)
)

(define cons make-ab)
(define car get-a)
(define cdr get-b)

; --

(define ab (cons 3 4))
(display "ab: (cons 3 4) => ")
(print ab)
(display "(car ab) => ")
(print (car ab))
(display "(cdr ab) => ")
(print (cdr ab))
