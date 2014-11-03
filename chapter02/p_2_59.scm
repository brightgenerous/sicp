(print "----------------")

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((of (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


(print "----------------")

(print "問題2.59")
(print "--")

(define (union-set set1 set2)
  (define (itr set res)
    (if (null? set)
      res
      (if (element-of-set? (car set) res)
        (itr (cdr set) res)
        (itr (cdr set) (cons (car set) res))
      )
    )
  )
  ;(itr set1 (itr set2 '()))
      ;; => 元のデータが重複ありの場合
  (itr set1 set2)
      ;; => 元のデータが重複なしの場合
)

(print "--")

(display "(union-set (list 1 2 3 4) (list 3 4 5)) => ")
(print    (union-set (list 1 2 3 4) (list 3 4 5))     )


(print "----------------")

(print "問題2.60")
(print "--")

(define (element-of-set-dup? x set)
  (element-of-set? x set))

(define (adjoin-set-dup x set)
  (cons x set))
    ;; => no check!!

(define (union-set-dup set1 set2)
  (append set1 set2))
    ;; => no check!!

(define (intersection-set-dup set1 set2)
  (define (itr set res)
    (if (null? set)
      res
      (if (element-of-set? (car set) set2)
        (itr (cdr set) (cons (car set) res))
        (itr (cdr set) res)
      )
    )
  )
  (itr set1 '())
)

(print "--")

(display "(adjoin-set-dup 3 (list 3 3 4 5)) => ")
(print    (adjoin-set-dup 3 (list 3 3 4 5))     )
(display "(adjoin-set-dup 3 (list 1 2 4 5)) => ")
(print    (adjoin-set-dup 3 (list 1 2 4 5))     )

(display "(union-set-dup (list 1 2 3) (list 3 3 4 5)) => ")
(print    (union-set-dup (list 1 2 3) (list 3 3 4 5))     )

(display "(intersection-set-dup (list 1 2 3 4) (list 3 3 4 5)) => ")
(print    (intersection-set-dup (list 1 2 3 4) (list 3 3 4 5))     )
(display "(intersection-set-dup (list 1 2 3 4 3) (list 3 4 5)) => ")
(print    (intersection-set-dup (list 1 2 3 4 3) (list 3 4 5))     )


(print "----------------")

(define (element-of-set-sorted? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set-sorted? x (cdr set)))))

(define (intersection-set-sorted set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1 (intersection-set-sorted (cdr set1) (cdr set2))))
            ((< x1 x2)
             (intersection-set-sorted (cdr set1) set2))
            ((< x2 x1)
             (intersection-set-sorted set1 (cdr set2)))))))


(print "問題2.61")
(print "--")

;;
;; θ(n): n / 2
;;
(define (adjoin-set-sorted x set)
  (if (null? set)
    (list x)
    (let ((head (car set)))
      (cond ((= head x) set)
            ((< head x) (cons head (adjoin-set-sorted x (cdr set))))
            (else (cons x set))
      )
    )
  )
)

;;
;; matsubisaiki
;;
(define (adjoin-set-sorted-2 x set)
  (define (rev-append s1 s2)
      ;; => s2 の逆順を s1 の先頭にくっつける
    (if (null? s2)
      s1
      (rev-append (cons (car s2) s1) (cdr s2))))
  (define (itr lasts rev-heads)
      ;; lasts     => 元のリストの後ろ部分
      ;; rev-heads => 元のリストの前部分の（逆順）
    (if (null? lasts)
      (reverse (cons x rev-heads))
          ;; => (append set (list x)) でもいいかも
      (let ((head (car lasts)))
        (cond ((= head x) set)
                            ;; => when contains
              ((< head x) (itr (cdr lasts) (cons head rev-heads)))
                            ;; => next loop
              (else (rev-append lasts (cons x rev-heads)))
                            ;; => return
        )
      )
    )
  )
  (itr set '())
)

(print "--")

(display "(adjoin-set-sorted 4 (list 2 3)) => ")
(print    (adjoin-set-sorted 4 (list 2 3))     )
(display "(adjoin-set-sorted 4 (list 5 6)) => ")
(print    (adjoin-set-sorted 4 (list 5 6))     )
(display "(adjoin-set-sorted 4 (list 1 2 3 4 5)) => ")
(print    (adjoin-set-sorted 4 (list 1 2 3 4 5))     )
(display "(adjoin-set-sorted 4 (list 1 2 3 5)) => ")
(print    (adjoin-set-sorted 4 (list 1 2 3 5))     )

(display "(adjoin-set-sorted-2 4 (list 2 3)) => ")
(print    (adjoin-set-sorted-2 4 (list 2 3))     )
(display "(adjoin-set-sorted-2 4 (list 5 6)) => ")
(print    (adjoin-set-sorted-2 4 (list 5 6))     )
(display "(adjoin-set-sorted-2 4 (list 1 2 3 4 5)) => ")
(print    (adjoin-set-sorted-2 4 (list 1 2 3 4 5))     )
(display "(adjoin-set-sorted-2 4 (list 1 2 3 5)) => ")
(print    (adjoin-set-sorted-2 4 (list 1 2 3 5))     )

(print "----------------")

(print "問題2.62")
(print "--")

;;
;; θ(n)
;;
(define (union-set-sorted set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((head1 (car set1)) (head2 (car set2)))
            (cond ((= head1 head2)
                   (cons head1 (union-set-sorted (cdr set1) (cdr set2))))
                  ((< head1 head2)
                   (cons head1 (union-set-sorted (cdr set1) set2)))
                  (else
                   (cons head2 (union-set-sorted set1 (cdr set2))))
            )
          )
        )
  )
)

;;
;; matsubisaiki
;;
(define (union-set-sorted-2 set1 set2)
  (define (rev-append s1 s2)
      ;; => s2 の逆順を s1 の先頭にくっつける
    (if (null? s2)
      s1
      (rev-append (cons (car s2) s1) (cdr s2))))
  (define (itr set1 set2 rev-heads)
    (cond ((null? set1) (rev-append set2 rev-heads))
          ((null? set2) (rev-append set1 rev-heads))
          (else
            (let ((head1 (car set1)) (head2 (car set2)))
              (cond ((= head1 head2)
                     (itr (cdr set1) (cdr set2) (cons head1 rev-heads)))
                    ((< head1 head2)
                     (itr (cdr set1) set2 (cons head1 rev-heads)))
                    (else
                     (itr set1 (cdr set2) (cons head2 rev-heads)))
              )
            )
          )
    )
  )
  (itr set1 set2 '())
)

(display "(union-set-sorted (list 1 2 3) (list 4 5)) => ")
(print    (union-set-sorted (list 1 2 3) (list 4 5))     )
(display "(union-set-sorted (list 1 2 3 10) (list 3 4 5)) => ")
(print    (union-set-sorted (list 1 2 3 10) (list 3 4 5))     )
(display "(union-set-sorted-2 (list 1 2 3) (list 4 5)) => ")
(print    (union-set-sorted-2 (list 1 2 3) (list 4 5))     )
(display "(union-set-sorted-2 (list 1 2 3 10) (list 3 4 5)) => ")
(print    (union-set-sorted-2 (list 1 2 3 10) (list 3 4 5))     )

