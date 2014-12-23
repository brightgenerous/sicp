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
))


;; -----

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
   (display "(apply-generic 'equ? s1 s2) => ")
   (print (apply-generic 'equ? s1 s2))
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
       (lambda (x) (and (= (magnitude x) 0) (= (angle x) 0))))
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

