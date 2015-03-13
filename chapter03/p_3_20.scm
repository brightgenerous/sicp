(print "----------------")

;; -----

(print "--")
(print "問題3.20")

((lambda arg

   (define (cons x y)
     (define (set-x! v) (set! x v))
     (define (set-y! v) (set! y v))
     (define (dispatch m)
       (cond ((eq? m 'car) x)
             ((eq? m 'cdr) y)
             ((eq? m 'set-car!) set-x!)
             ((eq? m 'set-cdr!) set-y!)
             (else (error "Undefined operation -- CONS" m))))
     dispatch)

   (define (car z) (z 'car))

   (define (cdr z) (z 'cdr))

   (define (set-car! z new-value)
     ((z 'set-car!) new-value)
     z)

   (define x (cons 1 2))
   (define z (cons x x))
   (set-car! (cdr z) 17)

   (print (format "(car x) => ~a" (car x)))
))
;
; 大域環境
; +--------------------------------------------------+
; | cons:                                            |
; | car:                                             |
; | cdr:                                             |
; | set-car!                                         |
; | x:                                               |
; | z:                                               |
; +--------------------------------------------------+
;             ^           ^        ^           ^
;             |           |        |           |
;        E1:cons       E2:car   E3:cdr   E4:set-car!
;        +----------+  +-----+  +-----+  +-----------+
;        | x        |  | z   |  | z   |  | z         |
;        | y        |  +-----+  +-----+  | new-value |
;        | set-x!   |                    +-----------+
;        | set-y!   |
;        | dispatch |
;        +----------+
;               ^
;               |
;             +-+-----------+-------------+
;             |             |             |
;        E1-1:set-x!   E1-2:set-y!  E1-3:dispatch
;        +----------+  +---------+  +-----------+
;        | v        |  | v       |  | m         |
;        +----------+  +---------+  +-----------+
;
;
;
;
;
;
;
;
