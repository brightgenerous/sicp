(print "----------------")

;
; origin: vector
; edge1 : vector
; edge2 : vector
;
; origin + x * edge1 + y * edge2
;
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))

;
; A(0,1)             | B(1,1)
;   vector-y のみmax |   vector-x, vector-y ともに max
; -------------------------------------------------
; C(0,0)             | D(1,0)
;                    |   vector-x のみ max
;
; a-frame の A の座標が欲しい => ((frame-coord-map a-frame) (make-vect 0 1))
;                                  => origin + edge2
; a-frame の B の座標が欲しい => ((frame-coord-map a-frame) (make-vect 1 1))
;                                  => origin + edge1 + edge2
; a-frame の C の座標が欲しい => ((frame-coord-map a-frame) (make-vect 0 0))
;                                  => origin
; a-frame の D の座標が欲しい => ((frame-coord-map a-frame) (make-vect 1 0))
;                                  => origin + edge1
;


(print "----------------")

(print "問題2.46")
(print "--")

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))

(define (scala-vect scale v1)
  (make-vect (* scale (xcor-vect v1)) (* scale (ycor-vect v1))))


(print "----------------")

(print "問題2.47")
(print "--")

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cddr frame))

(print "----------------")

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))


(print "----------------")

(print "問題2.48")
(print "--")

;
; segment - 線分
;
(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))


(print "----------------")

(print "問題2.49")
(print "--")

; 外形
(define painter-a
  (segments->painter (list
                      (cons (make-vect 0.0 0.0) (make-vect 0.0 1.0))
                      (cons (make-vect 0.0 1.0) (make-vect 1.0 1.0))
                      (cons (make-vect 1.0 1.0) (make-vect 1.0 0.0))
                      (cons (make-vect 1.0 0.0) (make-vect 0.0 0.0))
                    )))

; x
(define painter-b
  (segments->painter (list
                      (cons (make-vect 0.0 0.0) (make-vect 1.0 1.0))
                      (cons (make-vect 0.0 1.0) (make-vect 1.0 0.0))
                    )))

; +
(define painter-c
  (segments->painter (list
                      (cons (make-vect 0.0 0.5) (make-vect 0.5 1.0))
                      (cons (make-vect 0.5 1.0) (make-vect 1.0 0.5))
                      (cons (make-vect 1.0 0.5) (make-vect 0.5 0.0))
                      (cons (make-vect 0.5 0.0) (make-vect 0.0 0.5))
                    )))

; wave
(define painter-d
  (segments->painter (list
                      ;; TODO
                    )))


(print "----------------")

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
          (make-frame new-origin
                      (sub-vect (m corner1) new-origin)
                      (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-paint (make-vect 0.5 0.0)))
    (let ((paint-left
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               split-paint
                               (make-vect 0.0 1.0)))
          (paint-right
            (transform-painter painter2
                               split-paint
                               (make-vect 1.0 0.0)
                               (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))


(print "----------------")

(print "問題2.50")
(print "--")

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))


(print "----------------")

(print "問題2.51")
(print "--")

(define (below-1 painter1 painter2)
  (let ((split-paint (make-vect 0.0 0.5)))
    (let ((paint-bottom
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-paint))
          (paint-top
            (transform-painter painter2
                               split-paint
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))

(define (below-2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))



(print "----------------")

(print "問題2.52")
(print "--")

; a
;; TODO

; b
;; TODO
(define (corner-split-52-b painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))

(define (square-limit-52-b painter n)
  (let ((quarter (corner-split-52-b painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))


; c
(define (corner-split-52-c painter n)
  (if (= n 0)
    (flip-horiz painter)                     ;; ここだけ変更
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split-52-c painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))

(define (square-limit-52-c painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split-52-c painter n))))

