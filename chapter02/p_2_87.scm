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
  (if (number? contents) contents
    (cons type-tag contents)))

(define (type-tag datum)
  (if (number? datum) 'scheme-number
    (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (if (number? datum) datum
    (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum))))

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

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

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

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

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

  (put 'equ? '(complex complex)
       (lambda (x y)
         (and (= (real-part x) (real-part y))
              (= (imag-part x) (imag-part y)))))
  (put '=zero? '(complex)
       (lambda (x) (= (magnitude x) 0)))

  'done)
(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; --

;; scheme-number
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))

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

  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'raise '(scheme-number)
     (lambda (n) (make-complex-from-real-imag n 0)))

  'done)
(install-scheme-number-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; --

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

  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)

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

  (put 'equ? '(rational rational)
       (lambda (x y)
           (if (and (= (numer x) (numer y)) (= (denom x) (denom y)))
             #t
             #f)))
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (put 'raise '(rational)
     (lambda (n) (make-scheme-number (/ (numer n) (denom n)))))

  'done)
(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;; --

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (numer z) (apply-generic 'numer z))
(define (denom z) (apply-generic 'denom z))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))

;; -----

(define (apply-generic op . args)

  ;; data : ('rational (1 . 1))
  ;; tag  : 'complex
  ;; =>   : ('complex (1 . 0))
  (define (data-to-type data tag)
    (define (raise-itr data)
      (let ((t-tag (type-tag data)))
        (cond ((eq? t-tag tag) data)
              ((eq? t-tag 'complex) #f) ;; safely!!
              (else (raise-itr (raise data)))
        )
      )
    )
    (raise-itr data)
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

;; -----

(define (install-polynomial-sparse-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (variable? x) (symbol? x))
  (define (same-variable? x y)
    (and (variable? x) (variable? y) (eq? x y)))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1) (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))

  (define (tag p) (attach-tag 'polynomial-sparse p))

  (put 'make 'polynomial-sparse
       (lambda (var terms) (tag (make-poly var terms))))

  (put 'add '(polynomial-sparse polynomial-sparse)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial-sparse polynomial-sparse)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))

  ;; 2-87
  (put '=zero? '(polynomial-sparse)
       (lambda (p) (every =zero? (map coeff (term-list p)))))

  ;; 2-88
  (put 'sub '(polynomial-sparse polynomial-sparse)
       (lambda (p1 p2)
         (add-poly p1
                   (mul-poly p2 (make-poly (variable p2) '((0 -1)))))))

  ;; 2-90
  (put 'variable '(polynomial-sparse)
       (lambda (p1) (variable p1)))
  (put 'term-list '(polynomial-sparse)
       (lambda (p1) (term-list p1)))

  'done)
(install-polynomial-sparse-package)

(define (make-polynomial-sparse var terms)
  ((get 'make 'polynomial-sparse) var terms))

;; 2-89
(define (install-polynomial-dense-package)
  (define (make-poly variable coeff-list)
    (cons variable coeff-list))
  (define (variable p) (car p))
  (define (coeff-list p) (cdr p))

  (define (variable? x) (symbol? x))
  (define (same-variable? x y)
    (and (variable? x) (variable? y) (eq? x y)))

  (define (adjoin-coeff coeff coeff-list)
    (cons coeff coeff-list))
  (define (the-empty-coefflist) '())
  (define (first-coeff coeff-list) (car coeff-list))
  (define (rest-coeffs coeff-list) (cdr coeff-list))
  (define (empty-coefflist? coeff-list) (null? coeff-list))

  (define (add-coeffs L1 L2)
    (cond ((empty-coefflist? L1) L2)
          ((empty-coefflist? L2) L1)
          (else
            (adjoin-coeff
              (add (first-coeff L1) (first-coeff L2))
              (add-coeffs (rest-coeffs L1) (rest-coeffs L2))))))
  (define (mul-coeffs L1 L2)
    (define (inner l1 order)
      (if (empty-coefflist? l1)
        (the-empty-coefflist)
        (add-coeffs (mul-coeff-by-all-coeffs (first-coeff l1) L2 order)
                   (inner (rest-coeffs l1) (+ order 1)))))
    (inner L1 0))
  (define (mul-coeff-by-all-coeffs c1 L order)
    (define (inner l)
      (if (empty-coefflist? l)
        (the-empty-coefflist)
        (let ((c2 (first-coeff l)))
          (adjoin-coeff
            (mul c1 c2)
            (inner (rest-coeffs l))))))
    (padding-left (inner L) order 0))
  (define (padding-left l count v)
    (if (< count 1) l
      (padding-left (cons v l) (- count 1) v)))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-coeffs (coeff-list p1)
                            (coeff-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-coeffs (coeff-list p1)
                            (coeff-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))

  (define (tag p) (attach-tag 'polynomial-dense p))

  (put 'make 'polynomial-dense
       (lambda (var coeffs) (tag (make-poly var coeffs))))

  (put 'add '(polynomial-dense polynomial-dense)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial-dense polynomial-dense)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (put '=zero? '(polynomial-dense)
       (lambda (p) (every =zero? (coeff-list p))))

  (put 'sub '(polynomial-dense polynomial-dense)
       (lambda (p1 p2)
         (add-poly p1
                   (mul-poly p2 (make-poly (variable p2) '(-1))))))

  ;; 2-90
  (put 'variable '(polynomial-dense)
       (lambda (p1) (variable p1)))
  (put 'term-list '(polynomial-dense)
       (lambda (p1)
         (define (inner res cs count)
           (if (empty-coefflist? cs)
             res
             (let ((c (first-coeff cs)))
               (let ((r (if (=zero? c) res (cons (list count c) res))))
                 (inner r (rest-coeffs cs) (+ count 1))
               )
             )
           )
         )
         (inner '() (coeff-list p1) 0)))

  'done)
(install-polynomial-dense-package)

(define (make-polynomial-dense var coeffs)
  ((get 'make 'polynomial-dense) var coeffs))

;; 2-90
(define (variable p) (apply-generic 'variable p))
(define (term-list p) (apply-generic 'term-list p))

;; 2-90
(define (install-polynomial-package)

  (define (to-sparse p)
    (let ((v (variable p)) (tl (term-list p)))
      (make-polynomial-sparse v tl)))

  (define (tag p) (attach-tag 'polynomial p))

  (put 'make-from-sparse 'polynomial
       (lambda (var terms) (tag (make-polynomial-sparse var terms))))
  (put 'make-from-dense 'polynomial
       (lambda (var coeffs) (tag (make-polynomial-dense var coeffs))))

  (put 'add '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (add (to-sparse p1) (to-sparse p2)))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (mul (to-sparse p1) (to-sparse p2)))))

  (put '=zero? '(polynomial)
       (lambda (p) (=zero? p)))

  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (sub (to-sparse p1) (to-sparse p2)))))

  'done)
(install-polynomial-package)

(define (make-polynomial-from-sparse var terms)
  ((get 'make-from-sparse 'polynomial) var terms))
(define (make-polynomial-from-dense var coeffs)
  ((get 'make-from-dense 'polynomial) var coeffs))

;; -----

(print "--")
(print "問題2.87")

(define poly-s-1 (make-polynomial-sparse 'x '((3 3) (2 2) (1 1))))
(define poly-s-2 (make-polynomial-sparse 'x '((5 1) (4 2) (1 3) (0 4))))
(define poly-s-3 (make-polynomial-sparse 'x '((3 0) (2 0) (1 0))))
(define poly-s-4 (make-polynomial-sparse 'x '()))
(display "poly-s-1 => ")
(print poly-s-1)
(display "poly-s-2 => ")
(print poly-s-2)
(display "poly-s-3 => ")
(print poly-s-3)
(display "poly-s-4 => ")
(print poly-s-4)
(display "(add poly-s-1 poly-s-2) => ")
(print (add poly-s-1 poly-s-2))
(display "(add poly-s-1 poly-s-3) => ")
(print (add poly-s-1 poly-s-3))
(display "(mul poly-s-1 poly-s-2) => ")
(print (mul poly-s-1 poly-s-2))
(display "(=zero? poly-s-1) => ")
(print (=zero? poly-s-1))
(display "(=zero? poly-s-2) => ")
(print (=zero? poly-s-2))
(display "(=zero? poly-s-3) => ")
(print (=zero? poly-s-3))
(display "(=zero? poly-s-4) => ")
(print (=zero? poly-s-4))

;; -----

(print "--")
(print "問題2.88")

(display "(sub poly-s-1 poly-s-2) => ")
(print (sub poly-s-1 poly-s-2))

;; -----

(print "--")
(print "問題2.89")

(define poly-d-1 (make-polynomial-dense 'x '(0 1 2 3)))
(define poly-d-2 (make-polynomial-dense 'x '(4 3 0 0 2 1)))
(define poly-d-3 (make-polynomial-dense 'x '(0 0 0 0)))
(define poly-d-4 (make-polynomial-dense 'x '()))
(display "poly-d-1 => ")
(print poly-d-1)
(display "poly-d-2 => ")
(print poly-d-2)
(display "poly-d-3 => ")
(print poly-d-3)
(display "poly-d-4 => ")
(print poly-d-4)
(display "(add poly-d-1 poly-d-2) => ")
(print (add poly-d-1 poly-d-2))
(display "(add poly-d-1 poly-d-3) => ")
(print (add poly-d-1 poly-d-3))
(display "(mul poly-d-1 poly-d-2) => ")
(print (mul poly-d-1 poly-d-2))
(display "(=zero? poly-d-1) => ")
(print (=zero? poly-d-1))
(display "(=zero? poly-d-2) => ")
(print (=zero? poly-d-2))
(display "(=zero? poly-d-3) => ")
(print (=zero? poly-d-3))
(display "(=zero? poly-d-4) => ")
(print (=zero? poly-d-4))

(display "(sub poly-d-1 poly-d-2) => ")
(print (sub poly-d-1 poly-d-2))

;; -----

(print "--")
(print "問題2.90")

(define poly-1 (make-polynomial-from-sparse 'x '((3 3) (2 2) (1 1))))
(define poly-2 (make-polynomial-from-dense 'x '(4 3 0 0 2 1)))
(display "poly-1 => ")
(print poly-1)
(display "poly-2 => ")
(print poly-2)
(display "(add poly-1 poly-2) => ")
(print (add poly-1 poly-2))
(display "(mul poly-1 poly-2) => ")
(print (mul poly-1 poly-2))

;; -----

(print "--")
(print "問題2.91")

