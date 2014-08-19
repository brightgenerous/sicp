(print "----------------")

(print "問題2.32")
(print "--")

; (1 2 3)
;
;          | rest              | (map (lambda (x) (cons (car s) x)) rest) | s       |
;
; => (()),                                                                | ()      | (1 2 3)
; => (append (()),               ((3))),                                  | (3)     | (1 2)
; => (append (() (3)),           ((2) (2 3))),                            | (2 3)   | (1)
; => (append (() (3) (2) (2 3)), ((1) (1 3) (1 2) (1 2 3)))               | (1 2 3) |
; => (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

(define (subsets s)
  (if (null? s)
    (list (list))
    (let (
            (rest (subsets (cdr s)))
         )
         (append rest (map (lambda (x) (cons (car s) x)) rest))
    )
  )
)

(print (subsets (list 1 2 3)))
