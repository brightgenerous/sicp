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


(print "----------------")

(print "問題2.29")
(print "--")

; mobile
;   left  branch
;   right branch
; branch
;   length    - Num
;   structure - Num or mobile

;
; - a -
;
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

; --

(print "-- a --")

(define branch-1 (make-branch 10 20))
(define branch-2 (make-branch 100 200))
(define mobile-1 (make-mobile branch-1 branch-2))
(print (branch-length branch-1))
(print (branch-structure branch-1))
(print (left-branch mobile-1))
(print (right-branch mobile-1))

(print "-------")
(print "")


; -------


;
; - b -
;
(define (total-weight mobile)
  (define (is-mobile structure)
    (list? structure)
  )
  (define (weight-structure structure)
    (if (is-mobile structure)
      (total-weight structure)
      structure
    )
  )
  (define (weight-itr branch)
    (*
      (branch-length branch)
      (weight-structure (branch-structure branch))
    )
  )
  (+
    (weight-itr (left-branch mobile))
    (weight-itr (right-branch mobile))
  )
)

; --

(print "-- b --")

(define branch-1 (make-branch 10 20))
(define branch-2 (make-branch 100 200))
(define mobile-1 (make-mobile branch-1 branch-2))
;
;    /          \
; (10, 20)   (100, 200)
;
(display "mobile-1 => ")
(print (total-weight mobile-1))

(define branch-3 (make-branch 2 mobile-1))
(define mobile-2 (make-mobile branch-1 branch-3))
;
;    /          \
; (10, 20)   (2, |)
;               /\
;       (10, 20) (100, 200)
;
(display "mobile-2 => ")
(print (total-weight mobile-2))

(print "-------")
(print "")


; -------


;
; - c -
;
(define (total-weight mobile)
  (define (is-mobile structure)
    (list? structure)
  )
  (define (weight-structure structure)
    (if (is-mobile structure)
      (total-weight structure)
      structure
    )
  )
  (define (weight-itr branch)
    (*
      (branch-length branch)
      (weight-structure (branch-structure branch))
    )
  )
  (let
    (
      (left-weight (weight-itr (left-branch mobile)))
      (right-weight (weight-itr (right-branch mobile)))
    )
    (if (= left-weight right-weight)
      (+
        (weight-itr (left-branch mobile))
        (weight-itr (right-branch mobile))
      )
      (error "not equal.")
    )
  )
)

; --

(print "-- c --")

(define branch-1 (make-branch 10 40))
(define branch-2 (make-branch 10 40))
(define mobile-1 (make-mobile branch-1 branch-2))
(define branch-3 (make-branch 10 20))
(define branch-4 (make-branch 10 20))
(define mobile-2 (make-mobile branch-3 branch-4))
(define branch-3 (make-branch 10 mobile-1))
(define branch-4 (make-branch 20 mobile-2))
(define mobile-3 (make-mobile branch-3 branch-4))
(display "mobile-3 => ")
(print (total-weight mobile-3))

(print "-------")
(print "")


; -------


;
; - d -
;

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

; --

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

; --

(print "-- d --")

(define branch-1 (make-branch 10 20))
(define branch-2 (make-branch 100 200))
(define mobile-1 (make-mobile branch-1 branch-2))
(print (branch-length branch-1))
(print (branch-structure branch-1))
(print (left-branch mobile-1))
(print (right-branch mobile-1))

(print "-------")

