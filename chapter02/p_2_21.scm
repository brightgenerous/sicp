(print "----------------")

(print "問題2.21")
(print "--")

(define (square-list items)
  (if (null? items)
    items
    (let
      (
        (head (car items))
        (tail (cdr items))
      )
      (cons (* head head) (square-list tail))
    )
  )
)

(display "(square-list (list 1 2 3)) => ")
(print (square-list (list 1 2 3)))

(print "--")

(define (square-list items)
  (map (lambda (x) (* x x)) items)
)

(display "(square-list (list 1 2 3)) => ")
(print (square-list (list 1 2 3)))

(print "----------------")

(print "問題2.22")
(print "--")

; itr (1 2 3 4)
;   => itr (2 3 4) (cons (* 1 1) nil)
;   => itr (3 4) (cons 2 (cons 1 nil))
;   => itr (4) (cons 3 (cons 2 (cons 1 nil)))
;   => (cons 4 (cons 3 (cons 2 (cons 1 nil))))
;
; itr (1 2 3 4)
;   => itr (cons nil 1) (2 3 4)
;   => itr (cons (cons nil 1) 2) (3 4)
;   => itr (cons (cons (cons nil 1) 2) 3) (4)
;   => (cons (cons (cons (cons nil 1) 2) 3) 4)

(print "----------------")

(print "問題2.23")
(print "--")

(define (for-each proc items)
  (define (itr head tail)
      (proc head)
      (if (null? tail)
        undefined
        (itr (car tail) (cdr tail))
      )
  )
  (if (null? items)
    undefined
    (itr (car items) (cdr items))
  )
)

(for-each (lambda (x) (newline) (display x)) (list 1 2 3 4))

