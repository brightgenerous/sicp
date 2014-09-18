(print "----------------")

(print "問題2.33")
(print "--")


;
; op の適用は(initial と)最後の要素から
;
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) (accumulate op initial (cdr sequence)))
  )
)

; --


; あとで使う
(define map_org map)

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) (list) sequence))

(display "(map (lambda (x) (* x x)) (list 1 2 3)) => ")
(print (map (lambda (x) (* x x)) (list 1 2 3)))



(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(display "(append (list 0 1 2 3) (list 4 5 6 7)) => ")
(print (append (list 0 1 2 3) (list 4 5 6 7)))



(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(display "(length (list 1 2 3 4 5 6 7)) => ")
(print (length (list 1 2 3 4 5 6 7)))


(print "----------------")

(print "問題2.34")
(print "--")


(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* x higher-terms) this-coeff)
              )
              0
              coefficient-sequence))

; --

(display "(horner-eval 2 (list 1 3 0 5 0 1)) => ")
(print (horner-eval 2 (list 1 3 0 5 0 1)))


(print "----------------")

(print "問題2.35")
(print "--")

;
; count-leaves 再起する length
;
(define (count-leaves t)
  (accumulate
    (lambda (x y)
      (if (list? x)
        (+ (count-leaves x) y)
        (+ 1 y)
      )
    )
    0
    t
  )
)

; --

(display "(count-leaves (list 1 2 3 (list 4 5 (list 6)))) => ")
(print (count-leaves (list 1 2 3 (list 4 5 (list 6)))))
(display "(count-leaves (list 1 2 3 (list 4 5 (list 6) 7))) => ")
(print (count-leaves (list 1 2 3 (list 4 5 (list 6) 7))))

; --

(define (count-leaves-2 t)
  (accumulate
    (lambda (x y) (+ x y))
    0
    (map
      (lambda (x) (if (list? x) (count-leaves-2 x) 1))
      t
    )
  )
)

; --

(display "(count-leaves-2 (list 1 2 3 (list 4 5 (list 6)))) => ")
(print (count-leaves-2 (list 1 2 3 (list 4 5 (list 6)))))
(display "(count-leaves-2 (list 1 2 3 (list 4 5 (list 6) 7))) => ")
(print (count-leaves-2 (list 1 2 3 (list 4 5 (list 6) 7))))


(print "----------------")

(print "問題2.36")
(print "--")
;
; (cons (accumulate op init (??)) (accumulate-n op init (??)))
;           A                             B
; A : head : 先頭の要素の`合計`
; B : tail : 先頭以外の要素の`合計`のリスト
;
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    (list)
    (cons (accumulate op init (map (lambda (x) (car x)) seqs))
          (accumulate-n op init (map (lambda (x) (cdr x)) seqs))
    )
  )
)

; --

(display "(accumulate-n + 0 (list (list 1 2) (list 3 4))) => ")
(print (accumulate-n + 0 (list (list 1 2) (list 3 4))))


(print "----------------")

(print "問題2.37")
(print "--")

; map を戻す
(define map map_org)

;
; 内積
;
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(print "-- dot-procudt --")
(define vector-1 (list 1 2 3))
(define vector-2 (list 4 5 6))
(display "vector-1 => ")
(print vector-1)
(display "vector-2 => ")
(print vector-2)
(print (dot-product vector-1 vector-2))
(newline)

; --

;
;       | a f |
;  x =  | b g |              y = (x y)
;       | c h |
;
; ((a f) (b g) (c h)         (x y)
;
; => ((a * x + f * y) (b * x + g * y) (c * x + h * y))
;
(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

(print "-- matrix-*-vector --")
(define matrix (list (list 1 2) (list 3 4) (list 5 6)))
(define vector (list 7 8))
(display "matrix => ")
(print matrix)
(display "vector => ")
(print vector)
(print (matrix-*-vector matrix vector))
(newline)

; accumulate-n そのもの
(define (transpose mat)
  (accumulate-n cons (list) mat))

(print "-- transpose --")
(define matrix (list (list 1 2) (list 3 4) (list  5 6) (list 7 8)))
(display "matrix => ")
(print matrix)
(print (transpose matrix))
(newline)

;
;       | a |
;       | b |
;  x =  | c |              y = | v w x y z |
;       | d |
;       | e |
;
; ((a) (b) (c) (d) (e))    ((v w x y z))
;
;
;       | av aw ax ay az |
;       | bv bw bx by bz |
;  xy = | cv cw cx cy cz |
;       | dv dw dx dy dz |
;       | ev ew ex ey ez |
;
;
; -----
;
;                          (transpose y)
;
;                               | v |
;                               | w |
;                          y' = | x |
;                               | y |
;                               | z |
;                          ((v) (w) (x) (y) (z))
;
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

(print "-- matrix-*-matrix --")
(define matrix-1 (list (list 1) (list 2) (list 3)))
(define matrix-2 (list (list 1 2 3)))
(display "matrix-1 => ")
(print matrix-1)
(display "matrix-2 => ")
(print matrix-2)
(print (matrix-*-matrix matrix-1 matrix-2))
(newline)


(print "----------------")

(print "問題2.38")
(print "--")

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))
    )
  )
  (iter initial sequence)
)

; --

(define f-r-1 (fold-right / 1 (list 1 2 3)))
(display "f-r-1 => ")
(print f-r-1)


(define f-l-1 (fold-left / 1 (list 1 2 3)))
(display "f-l-1 => ")
(print f-l-1)


(define f-r-2 (fold-right list '() (list 1 2 3)))
(display "f-r-2 => ")
(print f-r-2)


(define f-l-2 (fold-left list '() (list 1 2 3)))
(display "f-l-2 => ")
(print f-l-2)

; --
;
;  関数 f が任意の x, y に対して (= (f x y) (f y x)) を満たす
;  場合に
;    (fold-left f initial sequence)
;    (fold-right f initial sequence)
;  が同じになる
;
; --


(print "----------------")

(print "問題2.39")
(print "--")

(define (reverse-r sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))


(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

;
; --
;

(display "(reverse-r (list 1 2 3)) => ")
(print (reverse-r (list 1 2 3)))

(display "(reverse-l (list 1 2 3)) => ")
(print (reverse-l (list 1 2 3)))

