(print "--")

;(variable? e)
;(same-variable? v1 v2)
;(sum? e)
;(addend e)
;(augend e)
;(make-sum a1 a2)
;(product? e)
;(multiplier e)
;(multiplicand e)
;(make-product m1 m2)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((sub? exp)
         (make-sub (deriv (sub-l exp) var)
                   (deriv (sub-r exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
          (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  ;(and (variable? v1) (variable? v2) (eq? v1 v2)))
  (and (variable? v1) (equal? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (sub? x) (and (pair? x) (eq? (car x) '-)))
(define (sub-l s) (cadr s))
(define (sub-r s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(display "(deriv '(+ x 3) 'x) => ")
(print (deriv '(+ x 3) 'x))
(display "(deriv '(* x y) 'x) => ")
(print (deriv '(* x y) 'x))
(display "(deriv '(* (* x y) (+ x 3)) 'x) => ")
(print (deriv '(* (* x y) (+ x 3)) 'x))


(print "--")

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (number? a2) (< a2 0)) (make-sub a1 (abs a2)))
        (else (list '+ a1 a2))))

(define (make-sub a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (- a1 a2))
        ((and (number? a2) (< a2 0)) (make-sum a1 (abs a2)))
        (else (list '- a1 a2))))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(display "(deriv '(+ x 3) 'x) => ")
(print (deriv '(+ x 3) 'x))
(display "(deriv '(* x y) 'x) => ")
(print (deriv '(* x y) 'x))
(display "(deriv '(* (* x y) (+ x 3)) 'x) => ")
(print (deriv '(* (* x y) (+ x 3)) 'x))

(print "----------------")

(print "問題2.56")
(print "--")

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base s) (cadr s))
(define (exponent s) (caddr s))
(define (make-exponentiation b e)
  (define (pow x y)
    (if (< y 2)
        x
        (* x (pow x (- y 1)))))
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e))
         (if (> e 0)
             (pow b e)
             (/ 1 (pow b (abs e))))) ; if (= b 0) error?
        (else (list '** b e))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
           (make-product
             (exponent exp)
             (make-exponentiation
               (base exp) (make-sum (exponent exp) -1)))
           (deriv (base exp) var)))
        (else
          (error "unknown expression type -- DERIV" exp))))


(display "(deriv '(* 2 3) 'x) => ")
(print (deriv '(* 2 3) 'x))
(display "(deriv '(** 2 3) 'x) => ")
(print (deriv '(** 2 3) 'x))
(display "(deriv '(** x 3) 'x) => ")
(print (deriv '(** x 3) 'x))
(display "(deriv '(** x y) 'x) => ")
(print (deriv '(** x y) 'x))
(display "(deriv '(** (** x y) (+ x 3)) 'x) => ")
(print (deriv '(** (** x y) (+ x 3)) 'x))
(display "(deriv '(** (** x 2) 2) 'x) => ")
(print (deriv '(** (** x 2) 2) 'x))






(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))
(define (make-sum a1 a2) (list a1 '+ a2))
(define (make-product m1 m2) (list m1 '* m2))
(print (deriv '(x * 2) 'x))

