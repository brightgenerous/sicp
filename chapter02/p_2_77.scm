(print "----------------")

;; -----

(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

;; --

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types -- APPLY GENERIC"
          (list op type-tags))))))

;; --

;; rectangular
(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))

  'done)
(install-rectangular-package)

;; polar
(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))

  'done)
(install-polar-package)

;; --

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; --

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;; scheme-number
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))

  'done)
(install-scheme-number-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; rational
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  'done)
(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;; complex
(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-rag r a)
    ((get 'make-from-mag-rag 'polar) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  'done)
(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; -----

(print "問題2.77")

((lambda ()
  (define complex (make-complex-from-real-imag 1 1))

  ;; ('complex ('rectangular (1 . 1)))
  ;; => apply-generic 'magnitude '(complex)
  ;;   ('rectangular (1 . 1))
  ;;   => apply-generic 'magnitude '(rectangular)
  (display "(apply-generic 'real-part complex) => ")
  (print (apply-generic 'real-part complex))

  (display "(apply-generic 'imag-part complex) => ")
  (print (apply-generic 'imag-part complex))

  (display "(apply-generic 'magnitude complex) => ")
  (print (apply-generic 'magnitude complex))

  (display "(apply-generic 'angle complex) => ")
  (print (apply-generic 'angle complex))
))

;; -----

(print "--")
(print "問題2.78")

(define (attach-tag type-tag contents)
  (if (or (number? contents) (symbol? contents)) contents
    (cons type-tag contents)))

(define (type-tag datum)
  (if (or (number? datum) (symbol? datum)) 'scheme-number
    (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (if (or (number? datum) (symbol? datum)) datum
    (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum))))

;; --

((lambda ()
   (define s-num1 (make-scheme-number 100))
   (define s-num2 (make-scheme-number 200))
   (define num1 10)
   (define num2 '20)
   (display "(apply-generic 'add s-num1 s-num2) => ")
   (print (apply-generic 'add s-num1 s-num2))
   (display "(apply-generic 'add num1 num2) => ")
   (print (apply-generic 'add num1 num2))
   (display "(add s-num1 num2) => ")
   (print (add s-num1 num2))
))


;; -----

(print "--")
(print "問題2.79")

((lambda()
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
))

(define (install-rational-extend-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (put 'equ? '(rational rational)
       (lambda (x y)
         (and (= (numer x) (numer y))
              (= (denom x) (denom y)))))
)(install-rational-extend-package)

((lambda()
  (put 'equ? '(complex complex)
       (lambda (x y)
         (and (= (magnitude x) (magnitude y))
              (= (angle x) (angle y)))))
))

;; --

((lambda()
   (define s1 (make-scheme-number 2))
   (define s2 (make-scheme-number 2))
   (define s21 (make-scheme-number 2))
   (define s22 (make-scheme-number 22))
   (display "(apply-generic 'equ? s1 s2) => ")
   (print (apply-generic 'equ? s1 s2))
   (display "(apply-generic 'equ? s21 s22) => ")
   (print (apply-generic 'equ? s21 s22))
))

((lambda()
   (define r1 (make-rational 2 3))
   (define r2 (make-rational 2 3))
   (display "(apply-generic 'equ? r1 r2) => ")
   (print (apply-generic 'equ? r1 r2))
))

((lambda()
   (define c1 (make-complex-from-real-imag 2 3))
   (define c2 (make-complex-from-real-imag 2 3))
   (display "(apply-generic 'equ? c1 c2) => ")
   (print (apply-generic 'equ? c1 c2))
))


;; -----

(print "--")
(print "問題2.80")

((lambda()
  (put 'zero? '(scheme-number)
       (lambda (x) (= x 0)))
))

(define (install-rational-extend2-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (put 'zero? '(rational)
       (lambda (x) (= (numer x) 0)))
)(install-rational-extend2-package)

((lambda()
  (put 'zero? '(complex)
       (lambda (x) (and (= (real-part x) 0) (= (imag-part x) 0))))
))

;; --

((lambda()
   (define s1 (make-scheme-number 2))
   (define s2 (make-scheme-number 0))
   (display "(apply-generic 'zero? s1) => ")
   (print (apply-generic 'zero? s1))
   (display "(apply-generic 'zero? s2) => ")
   (print (apply-generic 'zero? s2))
))

((lambda()
   (define r1 (make-rational 2 3))
   (define r2 (make-rational 0 3))
   (display "(apply-generic 'zero? r1) => ")
   (print (apply-generic 'zero? r1))
   (display "(apply-generic 'zero? r2) => ")
   (print (apply-generic 'zero? r2))
))

((lambda()
   (define c1 (make-complex-from-real-imag 2 3))
   (define c2 (make-complex-from-real-imag 0 0))
   (display "(apply-generic 'zero? c1) => ")
   (print (apply-generic 'zero? c1))
   (display "(apply-generic 'zero? c2) => ")
   (print (apply-generic 'zero? c2))
))


;; -----

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
            (let ((type1 (car type-tags))
                  (type2 (cadr type-tags))
                  (a1 (car args))
                  (a2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2
                      (apply-generic op (t1->t2 a1) a2))
                    (t2->t1
                      (apply-generic op a1 (t2->t1 a2)))
                    (else
                      (error "No method for these types"
                             (list op type-tags))))))
          (error "No method for these types"
                 (list op type-tags)))))))

(define (symbol-append . symbols)
  (string->symbol (apply string-append (map symbol->string symbols))))

(define (get-coercion type1 type2)
  (get
    (symbol-append type1 '-> type2)
    (list type1 type2)))

(define (put-coercion type1 type2 proc)
  (put
    (symbol-append type1 '-> type2)
    (list type1 type2)
    proc))


(put-coercion 'scheme-number 'complex
              (lambda (n) (make-complex-from-real-imag n 0)))

;; -----

(print "--")
(print "問題2.81")

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

((lambda ()
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y))))
))
(define (exp x y) (apply-generic 'exp x y))

;; a

(print (exp (make-scheme-number 3) (make-scheme-number 2)))
;;(print (exp (make-complex-from-real-imag 1 1) (make-complex-from-real-imag 2 2)))
;;  => (apply-generic 'exp ('complex (1 . 1)) ('complex (2 . 2)))
;;    を繰り返す
;;

;; b
;;
;; 変換可能な異なる型が見つからなかった場合にループを抜ける必要がある
;; どちらか一方をもう一方の型に合わせるので、変換後も同じ型ならばループする
;;
;;

;; -----

(print "--")
(print "問題2.82")

(define (apply-generic op . args)
  ;; data : ('rational (1 . 1))
  ;; tag  : 'complex
  ;; =>   : ('complex (1 . 0))
  (define (data-to-type data tag)
    (let ((data-type (type-tag data)))
      (if (eq? data-type tag)
        data
        (let
          ((d->t (get-coercion data-type tag)))
          (if d->t (d->t data) #f)
        )
      )
    )
  )
  ;; datas : (('rational (1 . 1)) ('complex (2 . 0)))
  ;; tag   : 'complex
  ;; =>    : (('complex (1 . 0)) ('complex (2 . 0)))
  (define (datas-to-type datas tag)
    (define (inner datas)
      (if (null? datas)
        '()
        (let
          ((head (data-to-type (car datas) tag)))
          (if head
            (let
              ((tail (inner (cdr datas))))
              (if tail (cons head tail) #f)
            )
            #f
          )
        )
      )
    )
    (inner datas)
  )
  ;; datas : (('rational (1 . 1)) ('complex (2 . 0)))
  ;; tags  : ('rational 'complex)
  ;; =>    : (('complex (1 . 0)) ('complex (2 . 0)))
  (define (datas-to-any datas tags)
    (define (inner tags)
      (if (null? tags)
        #f
        (let
          ((ds-t-ty (datas-to-type datas (car tags))))
          (if ds-t-ty ds-t-ty (inner (cdr tags)))
        )
      )
    )
    (if (null? datas) '() (inner tags))
  )

  (let ((type-tags (map type-tag args)))
    (let
      ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (let
          ((datas (datas-to-any args type-tags)))
          (if datas
              (apply apply-generic (cons op datas))
              (error "No method for these types" (list op type-tags))
          )
        )
      )
    )
  )
)

;; --
(print (add (make-scheme-number 2) (make-scheme-number 3)))
(print (add (make-complex-from-real-imag 2 2) (make-complex-from-real-imag 3 3)))
(print (add (make-scheme-number 4) (make-complex-from-real-imag 5 5)))
(print (sub (make-complex-from-real-imag 4 4) (make-scheme-number 5)))
;; --
;;(print (sub (make-rational 4 4) (make-scheme-number 5)))
;; ERROR: No method for these types (sub (rational scheme-number))
;;

;; -----

(print "--")
(print "問題2.83")
((lambda ()
  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (put 'raise '(scheme-number)
     (lambda (n) (make-complex-from-real-imag n 0)))

  (put 'raise '(rational)
     (lambda (n) (make-scheme-number (/ (numer n) (denom n)))))
))
(define (raise x) (apply-generic 'raise x))


(display "(raise (make-scheme-number 3)) => ")
(print (raise (make-scheme-number 3)))
(display "(raise (make-rational 2 3)) => ")
(print (raise (make-rational 2 3)))
(display "(raise (apply-generic 'raise (make-rational 2 3))) => ")
(print (raise (apply-generic 'raise (make-rational 2 3))))

