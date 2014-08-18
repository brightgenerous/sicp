(print "----------------")

(print "問題2.20")
(print "--")

(define (same-parity a . ary)
  (define (parity? b) (even? (+ a b)))
  (define (same-parity-itr head tail)
    (let (
           (next-tail (if (null? tail)
                        tail
                        (same-parity-itr (car tail) (cdr tail))
                      )
           )
         )
         (if (parity? head)
           (cons head next-tail)
           next-tail
         )
    )
  )
  (same-parity-itr a ary)
)

(print (same-parity 1 2 3 4 5 6))
(print (same-parity 2 2 3 4 5 6))
(print (same-parity 3 2 3 4 5 6))
(print (same-parity 3))
(print (same-parity 3 2 4 6 8))
