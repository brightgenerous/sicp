(print "----------------")

(print "問題2.24")
(print "--")

(print (list 1 (list 2 (list 3 4))))
; (1 (2 (3 4)))
;
;    /\
;   1 /\
;    2 /\
;     3  4

(print "----------------")

(print "問題2.25")
(print "--")

(define items (list 1 3 (list 5 7) 9))
(print items)
(display "(car (cdr (car (cdr (cdr items))))) => ")
(print (car (cdr (car (cdr (cdr items))))))
(newline)
(define items (list (list 7)))
(print items)
(display "(car (car items)) => ")
(print (car (car items)))
(newline)
(define items (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 (list 7))))))))
(print items)
(display "(car (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr items))))))))))))) => ")
(print (car (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr items))))))))))))))
(print (car (cadadr (cadadr (cadadr items)))))

(print "----------------")

(print "問題2.26")
(print "--")

(define x (list 1 2 3))
(define y (list 4 5 6))

(print (append x y)) ; (1 2 3 4 5 6)
(print (cons x y))   ; ((1 2 3) 4 5 6)
(print (list x y))   ; ((1 2 3) (4 5 6))

(print "----------------")

(print "問題2.27")
(print "--")

(define (deep-reverse items)
  (define (reverse-if-pair item)
    (if (pair? item)
      (map reverse-if-pair (reverse item))
      item
    )
  )
  (reverse-if-pair items)
)

(define x (list (list 1 2) (list 3 4)))

(display "x => ")
(print x)
(display "(reverse x) => ")
(print (reverse x))
(display "(deep-reverse x) => ")
(print (deep-reverse x))
