(print "----------------")

(print "問題2.28")
(print "--")

(define (fringe items)
  (define (flat-itr head tail)
    (let
      (
        (new-head (if (pair? head) (flat-itr (car head) (cdr head)) (list head)))
        (new-tail (if (pair? tail) (flat-itr (car tail) (cdr tail)) tail))
                                              ; tail => list であって pair でない
      )
      (append new-head new-tail)
    )
  )
  (flat-itr (car items) (cdr items))
)

(define x (list (list 1 2) (list 3 4)))
(display "x => ")
(print x)
(display "(fringe x) => ")
(print (fringe x))
(display "(fringe (list x x)) => ")
(print (fringe (list x x)))

