(print "----------------")

(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(display "(memq 'apple '(pear banana prune)) => ")
(print (memq 'apple '(pear banana prune)))
(display "(memq 'apple '(x (apple sauce) y apple pear)) => ")
(print (memq 'apple '(z (apple sauce) y apple pear)))


(print "----------------")

(print "問題2.53")
(print "--")

(display "(list 'a 'b 'c) => ")
(print    (list 'a 'b 'c)     )
; => (a b c)

(display "(list (list 'george)) => ")
(print    (list (list 'george))     )
; => ((george))

(display "(cdr '((x1 x2) (y1 y2))) => ")
(print    (cdr '((x1 x2) (y1 y2)))     )
; => ((y1 y2))
;(car '((x1 x2) (y1 y2)))
; => (x1 x2)

(display "(cadr '((x1 x2) (y1 y2))) => ")
(print    (cadr '((x1 x2) (y1 y2)))     )
; => (y1 y2)

(display "(pair? (car '(a short list))) => ")
(print    (pair? (car '(a short list)))     )
; => #f

(display "(memq 'red '((red shoes) (blue socks))) => ")
(print    (memq 'red '((red shoes) (blue socks)))     )
; => #f

(display "(memq 'red '(red shoes blue socks)) => ")
(print    (memq 'red '(red shoes blue socks))     )
; => (red shoes blue socks)


(print "----------------")

(print "問題2.54")
(print "--")

(define (equal? a b)
  (if (pair? a)
    (if (pair? b)
      (if (equal? (car a) (car b))
        (equal? (cdr a) (cdr b))
        #f
      )
      #f
    )
    (if (pair? b) #f (eq? a b))
  )
)

(display "(equal? 'a 'a) => ")
(print (equal? 'a 'a))
(display "(equal? 'a 'b) => ")
(print (equal? 'a 'b))
(display "(equal? '(a b) '(a b)) => ")
(print (equal? '(a b) '(a b)))
(display "(equal? '(a b) '(a (b))) => ")
(print (equal? '(a b) '(a (b))))


(print "----------------")

(print "問題2.55")
(print "--")

(print (car ''abracadabra))
(display "(equal? ''ab '(quote ab)) => ")
(print (equal? ''ab '(quote ab)))
(display "(equal? ''ab (list 'quote 'ab)) => ")
(print (equal? ''ab (list 'quote 'ab)))
; ''ab => '(quote ab)

