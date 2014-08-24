(print "----------------")

(print "問題2.7")
(print "--")

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))
  )
)

(define (mul-interval x y)
  (let (
         (p1 (* (lower-bound x) (lower-bound y)))
         (p2 (* (lower-bound x) (upper-bound y)))
         (p3 (* (upper-bound x) (lower-bound y)))
         (p4 (* (upper-bound x) (upper-bound y)))
       )
       (make-interval (min p1 p2 p3 p4)
                      (max p1 p2 p3 p4)
       )
  )
)

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))
                )
  )
)

(define (make-interval a b) (cons a b))

; --

(define (lower-bound interval)
  (let (
         (x (car interval))
         (y (cdr interval))
       )
       (if (< x y) x y)
  )
)

(define (upper-bound interval)
  (let (
         (x (car interval))
         (y (cdr interval))
       )
       (if (> x y) x y)
  )
)

(define (disp-interval desc interval)
  (display desc)
  (print interval)
  (display "  lower-bound => ")
  (print (lower-bound interval))
  (display "  upper-bound => ")
  (print (upper-bound interval))
)

; --

(define interval-1 (make-interval 100 200))
(disp-interval "[interval-1] => " interval-1)

(define interval-2 (make-interval 300 50))
(disp-interval "[interval-2] => " interval-2)

(disp-interval "(add-interval interval-1 interval-2) => " (add-interval interval-1 interval-2))
(disp-interval "(mul-interval interval-1 interval-2) => " (mul-interval interval-1 interval-2))

(print "----------------")

(print "問題2.8")
(print "--")

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))
  )
)

(disp-interval "(sub-interval interval-1 interval-2) => " (sub-interval interval-1 interval-2))

(print "----------------")

(print "問題2.9")
(print "--")
;
; ２つの区間から作った結果
;   => (define added-interval (add-interval interval-1 interval-2))
;   => (define subed-interval (sub-interval interval-1 interval-2))
;   => (define muled-interval (mul-interval interval-1 interval-2))
;   => (define dived-interval (div-interval interval-1 interval-2))
;
; ２つの区間から作った結果の幅
;   => (width added-interval)
;   => (width subed-interval)
;   => (width muled-interval)
;   => (width dived-interval)
;
; 引数の幅だけの関数
;   => (define width-1 (width interval-1))
;   => (define width-2 (width interval-2))
;   ex. (define (add-width width-1 width-2) (+ width-1 width-2))
;       (define (sub-width width-1 width-2) (abs (- width-1 width-2)))
;       (define (mul-width width-1 width-2) (* width-1 width-2))
;       (define (sub-width width-1 width-2) (/ width-1 width-2))
;
;   add, sub => 「二つの区間から作った結果の幅は、引数の幅だけの関数」
;   mul, div => 「二つの区間から作った結果の幅は、引数の幅だけの関数にならない」
;

(define (width interval)
  (/
    (-
      (upper-bound interval)
      (lower-bound interval)
    )
    2
  )
)

(define interval-1 (make-interval 100 200))
(disp-interval "[interval-1] => " interval-1)

(define interval-2 (make-interval 300 50))
(disp-interval "[interval-2] => " interval-2)

(print "--")

(display "(width interval-1) => ")
(print (width interval-1))

(display "(width interval-2) => ")
(print (width interval-2))

(print "-- plus width --")
(display "(+ (width interval-1) (width interval-2))    => ")
(print (+ (width interval-1) (width interval-2)))

(display "(width (add-interval interval-1 interval-2)) => ")
(print (width (add-interval interval-1 interval-2)))

(print "-- sub width --")
(display "(abs (- (width interval-1) (width interval-2))) => ")
(print (abs (- (width interval-1) (width interval-2))))

(display "(width (sub-interval interval-1 interval-2))    => ")
(print (width (sub-interval interval-1 interval-2)))

(print "-- mul width --")
(display "(* (width interval-1) (width interval-2))    => ")
(print (* (width interval-1) (width interval-2)))

(display "(width (mul-interval interval-1 interval-2)) => ")
(print (width (mul-interval interval-1 interval-2)))

(print "-- div width --")
(display "(/ (width interval-1) (width interval-2))    => ")
(print (/ (width interval-1) (width interval-2)))

(display "(width (div-interval interval-1 interval-2)) => ")
(print (width (div-interval interval-1 interval-2)))

(print "----------------")

(print "問題2.10")
(print "--")

(define (disp-interval-with-div interval-1 interval-2)
  (display "[interval-1] => ")
  (display interval-1)
  (newline)
  (display "  upper => ")
  (display (upper-bound interval-1))
  (display " lower => ")
  (display (lower-bound interval-1))
  (newline)
  (display "[interval-2] => ")
  (display interval-2)
  (newline)
  (display "  upper => ")
  (display (upper-bound interval-2))
  (display " lower => ")
  (display (lower-bound interval-2))
  (newline)
  (display "  (/ (lower-bound interval-1) (lower-bound interval-2)) => ")
  (print (/ (lower-bound interval-1) (lower-bound interval-2)))
  (display "  (/ (lower-bound interval-1) (upper-bound interval-2)) => ")
  (print (/ (lower-bound interval-1) (upper-bound interval-2)))
  (display "  (/ (upper-bound interval-1) (lower-bound interval-2)) => ")
  (print (/ (upper-bound interval-1) (lower-bound interval-2)))
  (display "  (/ (upper-bound interval-1) (upper-bound interval-2)) => ")
  (print (/ (upper-bound interval-1) (upper-bound interval-2)))
  (disp-interval "(div-interval interval-1 interval-2) => " (div-interval interval-1 interval-2))
)

(define (disp-interval-with-mul interval-1 interval-2)
  (display "[interval-1] => ")
  (display interval-1)
  (newline)
  (display "  upper => ")
  (display (upper-bound interval-1))
  (display " lower => ")
  (display (lower-bound interval-1))
  (newline)
  (display "[interval-2] => ")
  (display interval-2)
  (newline)
  (display "  upper => ")
  (display (upper-bound interval-2))
  (display " lower => ")
  (display (lower-bound interval-2))
  (newline)
  (display "  (/ (lower-bound interval-1) (lower-bound interval-2)) => ")
  (print (/ (lower-bound interval-1) (lower-bound interval-2)))
  (display "  (/ (lower-bound interval-1) (upper-bound interval-2)) => ")
  (print (/ (lower-bound interval-1) (upper-bound interval-2)))
  (display "  (/ (upper-bound interval-1) (lower-bound interval-2)) => ")
  (print (/ (upper-bound interval-1) (lower-bound interval-2)))
  (display "  (/ (upper-bound interval-1) (upper-bound interval-2)) => ")
  (print (/ (upper-bound interval-1) (upper-bound interval-2)))
  (disp-interval "(mul-interval interval-1 interval-2) => " (mul-interval interval-1 interval-2))
)

(for-each
  (lambda (xy) (disp-interval-with-div (car xy) (cdr xy)) (newline))
  (list
    (cons (make-interval 6 12) (make-interval 2 3))
    (cons (make-interval 6 12) (make-interval 2 -3))
    (cons (make-interval 6 12) (make-interval -2 3))
    (cons (make-interval 6 12) (make-interval -2 -3))
  )
)

; --

(print "--")
(newline)

(define (div-interval x y)
  (let
    (
     (lx (lower-bound x))
     (ux (upper-bound x))
     (ly (lower-bound y))
     (uy (upper-bound y))
    )
    (if (< (* ly uy) 0)
      (error "error")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))
                    )
      )
    )
  )
)

(for-each
  (lambda (xy) (disp-interval-with-div (car xy) (cdr xy)) (newline))
  (list
    (cons (make-interval 6 12) (make-interval 2 3))
  ; (cons (make-interval 6 12) (make-interval 2 -3))
  ; (cons (make-interval 6 12) (make-interval -2 3))
    (cons (make-interval 6 12) (make-interval -2 -3))
  )
)

(print "----------------")

(print "問題2.11")
(print "--")

(for-each
  (lambda (xy) (disp-interval-with-mul (car xy) (cdr xy)) (newline))
  (list
    (cons (make-interval 6 12) (make-interval 2 3))
    (cons (make-interval 6 12) (make-interval -2 3))
    (cons (make-interval 6 12) (make-interval -2 -3))
    (cons (make-interval -6 12) (make-interval 2 3))
    (cons (make-interval -6 12) (make-interval -2 3))
    (cons (make-interval -6 12) (make-interval -2 -3))
    (cons (make-interval -6 -12) (make-interval 2 3))
    (cons (make-interval -6 -12) (make-interval -2 3))
    (cons (make-interval -6 -12) (make-interval -2 -3))
  )
)


(print "--")
(newline)

(define (mul-interval x y)
  (let
    (
     (lx (lower-bound x))
     (ux (upper-bound x))
     (ly (lower-bound y))
     (uy (upper-bound y))
    )
    (cond
      ((and (> lx 0) (> ux 0))
        (cond
          ((and (> ly 0) (> uy 0))
            (make-interval (* lx ly) (* ux uy))
              ; => (< (* lx ly) (* ux uy))
          )
          ((and (< ly 0) (> uy 0))
            (make-interval (* ux ly) (* ux uy))
              ; => (< (* ux ly) (* ux uy))
          )
          ((and (< ly 0) (< uy 0))
            (make-interval (* ux ly) (* lx uy))
              ; => (< (* ux ly) (* lx uy))
          )
        )
      )
      ((and (< lx 0) (> ux 0))
        (cond
          ((and (> ly 0) (> uy 0))
            (make-interval (* lx uy) (* ux uy))
              ; => (< (* lx uy) (* ux uy))
          )
          ((and (< ly 0) (> uy 0))
            (make-interval (min (* ux ly) (* lx uy))
                             ; 負になる組み合わせのどちらか
                           (max (* lx ly) (* ux uy))
                             ; 正になる組み合わせのどちらか
            )
          )
          ((and (< ly 0) (< uy 0))
            (make-interval (* ux ly) (* lx ly))
              ; => (< (* ux uy) (* lx ly))
          )
        )
      )
      ((and (< lx 0) (< ux 0))
        (cond
          ((and (> ly 0) (> uy 0))
            (make-interval (* lx uy) (* ux ly))
              ; => (< (* lx uy) (* ux ly))
          )
          ((and (< ly 0) (> uy 0))
            (make-interval (* lx uy) (* lx ly))
              ; => (< (* lx uy) (* lx ly))
          )
          ((and (< ly 0) (< uy 0))
            (make-interval (* ux uy) (* lx ly))
              ; => (< (* ux uy) (* lx ly))
          )
        )
      )
    )
  )
)

(for-each
  (lambda (xy) (disp-interval-with-mul (car xy) (cdr xy)) (newline))
  (list
    (cons (make-interval 6 12) (make-interval 2 3))
    (cons (make-interval 6 12) (make-interval -2 3))
    (cons (make-interval 6 12) (make-interval -2 -3))
    (cons (make-interval -6 12) (make-interval 2 3))
    (cons (make-interval -6 12) (make-interval -2 3))
    (cons (make-interval -6 12) (make-interval -2 -3))
    (cons (make-interval -6 -12) (make-interval 2 3))
    (cons (make-interval -6 -12) (make-interval -2 3))
    (cons (make-interval -6 -12) (make-interval -2 -3))
  )
)
