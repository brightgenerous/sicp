(print "----------------")

;; -----

(print "--")
(print "問題3.15")

((lambda arg

  (define x (list 'a 'b))
  (define z1 (cons x x))
  (define z2 (cons (list 'a 'b) (list 'a 'b)))
  (define (set-to-wow! x)
    (set-car! (car x) 'wow))

  (print (format "z1 => ~a" z1))
  (print (format "z2 => ~a" z2))
  (set-to-wow! z1)
  (set-to-wow! z2)
  (print (format "z1 => ~a" z1))
  (print (format "z2 => ~a" z2))

))

;
;        +-------+
;  z1 -> | ○ | ○ |
;        +-+---+-+
;          |   |
;          v   v
;        +-------+     +-------+
;   x -> | ◎ | ○ +---> | ○ | / |
;        +-+-----+     +-+-----+
;          |             |
;          v             v
;       +-----+        +---+
;       | wow |        | b |
;       +-----+        +---+
;
;
;        +-------+     +-------+     +-------+
;  z2 -> | ○ | ○ +---> | ○ | ○ +---> | ○ | / |
;        +-+-----+     +-+-----+     +-+-----+
;          |             |             |
;          |             v             v
;          |           +---+         +---+
;          |           | a |         | b |
;          |           +---+         +---+
;          |                           ^
;          |                           |
;          |           +-------+     +-+-----+
;          +---------> | ◎ | ○ +---> | ○ | / |
;                      +-+-----+     +-------+
;                        |
;                        v
;                     +-----+
;                     | wow |
;                     +-----+
;

;; -----

(print "--")
(print "問題3.16")

((lambda arg
  (define (count-pairs x)
    (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;
; 一つのPairを複数回countさせることができる
;
  ((lambda arg
    ; count => 3
    (define x (cons 'a 'b))
    (define y (cons 'c x))
    (define z (cons 'd y))
    (print (format "(count-pairs z) => ~a" (count-pairs z)))
    (print (format "z => ~a" z))
  ))
  ((lambda arg
    ; count => 4
    (define x (cons 'a 'b))
    (define y (cons 'c x))
    (define z (cons x y))
    (print (format "(count-pairs z) => ~a" (count-pairs z)))
    (print (format "z => ~a" z))
  ))
  ((lambda arg
    ; count => 7
    (define x (cons 'a 'b))
    (define y (cons x x))
    (define z (cons y y))
    (print (format "(count-pairs z) => ~a" (count-pairs z)))
    (print (format "z => ~a" z))
  ))
  ((lambda arg
    ; count => ∞
    (define x (cons 'a 'b))
    (define y (cons 'c x))
    (define z (cons 'd y))
    (set-car! x x)
    ;(print (format "(count-pairs z) => ~a" (count-pairs z)))
    (print (format "z => ~a" z))
  ))
))

;; -----

(print "--")
(print "問題3.17")

((lambda arg

  (define (count-pairs x)

    (let ((rec '()))

      (define (recorded? x)
        (define (itr rec x)
          (cond
            ((null? rec) #F)
            ((eq? (car rec) x) #T)
            (else (itr (cdr rec) x))))
        (itr rec x))

      (define (record! x)
        (set! rec (cons x rec)))

      (define (itr x)
        (cond
          ((not (pair? x)) 0)
          ((recorded? x) 0) ;; 追加
          (else
            (begin
              (record! x)   ;; 追加
              (+ (itr (car x)) (itr (cdr x)) 1)))))
      (itr x)))

  ((lambda arg
    ; count /=> 3
    ; count => 3
    (define x (cons 'a 'b))
    (define y (cons 'c x))
    (define z (cons 'd y))
    (print (format "(count-pairs z) => ~a" (count-pairs z)))
    (print (format "z => ~a" z))
  ))
  ((lambda arg
    ; count /=> 4
    ; count => 3
    (define x (cons 'a 'b))
    (define y (cons 'c x))
    (define z (cons x y))
    (print (format "(count-pairs z) => ~a" (count-pairs z)))
    (print (format "z => ~a" z))
  ))
  ((lambda arg
    ; count /=> 7
    ; count => 3
    (define x (cons 'a 'b))
    (define y (cons x x))
    (define z (cons y y))
    (print (format "(count-pairs z) => ~a" (count-pairs z)))
    (print (format "z => ~a" z))
  ))
  ((lambda arg
    ; count /=> ∞
    ; count => 3
    (define x (cons 'a 'b))
    (define y (cons 'c x))
    (define z (cons 'd y))
    (set-car! x x)
    (print (format "(count-pairs z) => ~a" (count-pairs z)))
    (print (format "z => ~a" z))
  ))
))

;; -----

(print "--")
(print "問題3.18")

((lambda arg

  (define (loop? x)

    (let ((rec '()))

      (define (recorded? x)
        (define (itr rec x)
          (cond
            ((null? rec) #F)
            ((eq? (car rec) x) #T)
            (else (itr (cdr rec) x))))
        (itr rec x))

      (define (record! x)
        (set! rec (cons x rec)))

      (define (itr x)
        (cond
          ((null? x) #F)
          ((recorded? x) #T)
          (else
            (begin
              (record! x)
              (itr (cdr x))))))
      (itr x)))

  ((lambda arg
    (define x '(a b))
    (define y (cons 'c x))
    (define z (cons 'd y))
    (print (format "(loop? z) => ~a" (loop? z)))
    (print (format "z => ~a" z))
  ))
  ((lambda arg
    (define x '(a b))
    (define y (cons 'c x))
    (define z (cons 'd y))
    (set-cdr! x x)
    (print (format "(loop? z) => ~a" (loop? z)))
    (print (format "z => ~a" z))
  ))
))

;; -----

(print "--")
(print "問題3.19")

((lambda arg

  (define (loop? x)

    (let ((mark 'mark))

      (define (itr x)
        (if (null? x)
          #F
          (let ((head (car x)) (tail (cdr x)))
            (if (eq? tail mark)
              #T
              (begin
                (set-cdr! x mark)
                (itr tail))))))
      (itr x)))

  ((lambda arg
    (define x '(a b))
    (define y (cons 'c x))
    (define z (cons 'd y))
    (print (format "(loop? z) => ~a" (loop? z)))
    (print (format "z => ~a" z))
  ))
  ((lambda arg
    (define x '(a b))
    (define y (cons 'c x))
    (define z (cons 'd y))
    (set-cdr! x x)
    (print (format "(loop? z) => ~a" (loop? z)))
    (print (format "z => ~a" z))
  ))
))

