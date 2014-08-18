(print "----------------")

(print "問題2.17")
(print "--")

(define (last-pair ary)
  (define (last-pair-itr head tail)
    (if (null? tail)
      head
      (last-pair-itr (car tail) (cdr tail))
    )
  )
  (if (null? ary)
    ary
    (list (last-pair-itr (car ary) (cdr ary)))
  )
)

(print (last-pair (list 1 2 3 4)))
(print (last-pair (list 1 2 3 4 (list 5 6))))
(print (last-pair (list)))

(print "----------------")

(print "問題2.18")
(print "--")

(define (reverse ary)
  (define (reverse-itr ary res)
    (if (null? ary)
      res
      (reverse-itr (cdr ary) (cons (car ary) res))
    )
  )
  (reverse-itr ary (list))
)

(print (reverse (list)))
(print (reverse (list 1 2 3 4)))
