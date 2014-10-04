(print "----------------")

;
; flip-vert  : 上下逆転
; flip-horiz : 左右逆転
; beside     : 横に連結
; below      : 縦に連結
;
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define wave4 (flipped-pairs wave))


(print "--")

(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(print "----------------")

(print "問題2.44")
(print "--")

;
;                    |
; top-left           | corner
;   = (beside up up) |   = (corner-split painer (- n 1))
;                    |
; ------------------------------------------------------
;                    |
; painter            | bottom-right
;                    |   = (below right right)
;                    |
;
; ------------------------------------------------------
;   (up (up-split paintr (- n 1)))
;   (right (right-split painter (- n 1)))
;

;
; right-split を元にすれば簡単
;
(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

(print "----------------")


(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))


(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))


(print "----------------")

(print "問題2.45")
(print "--")

(define right-split (split beside below))
(define up-split (split below beside))

;
;(define (right-split painter n)
;  (if (= n 0)
;    painter
;    (let ((smaller (right-split painter (- n 1))))
;      (beside painter (below smaller smaller)))))
;
;(define (up-split painter n)
;  (if (= n 0)
;    painter
;    (let ((smaller (up-split painter (- n 1))))
;      (below painter (beside smaller smaller)))))
;
(define (split dir1 dir2)
  (define (split-inner painter n)
    (if (= n 0)
      painter
      (let ((smaller (split-inner painter (- n 1))))
        (dir1 painter (dir2 smaller smaller)))))
  (lambda (painter n)
    (split-inner painter n))
)

