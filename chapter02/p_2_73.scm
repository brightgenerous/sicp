(print "----------------")

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (let ((number-1? (number? a1))
        (number-2? (number? a2)))
       (cond ((and number-1? (= a1 0)) a2)
             ((and number-2? (= a2 0)) a1)
             ((and number-1? number-2?) (+ a1 a2))
             (else (list '+ a1 a2)))))

(define (make-product m1 m2)
  (let ((number-1? (number? m1))
        (number-2? (number? m2)))
       (cond ((and number-1? (= m1 0)) 0)
             ((and number-1? (= m1 1)) m2)
             ((and number-2? (= m2 0)) 0)
             ((and number-2? (= m2 1)) m1)
             ((and number-1? number-2?) (* m1 m2))
             (else (list '* m1 m2)))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

;; --

(define operation-table (make-hash-table))

(define (put op type item)
  (if (not (hash-table-exists? operation-table op))
      (hash-table-put! operation-table op (make-hash-table)))
  (let ((type-table (hash-table-get operation-table op)))
       (hash-table-put! type-table type item)))

(define (get op type)
  (if (hash-table-exists? operation-table op)
      (let ((type-table (hash-table-get operation-table op)))
           (hash-table-get type-table type))
      (error "Not exists" op type)))

;; --

(print "問題2.73")

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ;;
        (else (error "unknown expression type -- DEVIV" exp))))

;; override deriv
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;;
;; a
;;


;;
;; b
;;
(print "-- b --")

(define (deriv-sum operands var)
  (let ((addend (car operands))
        (augend (cadr operands)))
       (make-sum (deriv addend var)
                 (deriv augend var))))

(define (deriv-product operands var)
  (let ((multiplicand (car operands))
        (multiplier (cadr operands)))
       (make-sum
         (make-product multiplier
                       (deriv multiplicand var))
         (make-product (deriv multiplier var)
                       multiplicand))))

(put 'deriv '+ deriv-sum)
(put 'deriv '* deriv-product)

;; --

(display "(deriv '(* x y) 'x) => ")
(print (deriv '(* x y) 'x))

(display "(deriv '(+ x x) 'x) => ")
(print (deriv '(+ x x) 'x))

;;
;; c
;;
(print "-- c --")

(define (make-exponentiation base exponent)
  (let ((number-base? (number? base))
        (number-exponent? (number? exponent)))
       (cond ((and number-base? (= base 1)) 1)
             ((and number-exponent? (= exponent 0)) 1)
             ((and number-exponent? (= exponent 1)) base)
             (else (list '** base exponent)))))

(define (deriv-exponentiation operands var)
  (let ((base (car operands))
        (exponent (cadr operands)))
       (make-product
         (make-product
           exponent
           (make-exponentiation base (- exponent 1)))
         (deriv base var))))

(put 'deriv '** deriv-exponentiation)

;; --

(display "(deriv '(** x 3) 'x) => ")
(print (deriv '(** x 3) 'x))

(display "(deriv '(** x 3) 'y) => ")
(print (deriv '(** x 3) 'y))


;;
;; d
;;
(print "-- d --")

;; override deriv
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get (operator exp) 'deriv) (operands exp) var))))


(put '+ 'deriv deriv-sum)
(put '* 'deriv deriv-product)
(put '** 'deriv deriv-exponentiation)

;; --

(display "(deriv '(* x y) 'x) => ")
(print (deriv '(* x y) 'x))

(display "(deriv '(+ x x) 'x) => ")
(print (deriv '(+ x x) 'x))
(display "(deriv '(** x 3) 'x) => ")
(print (deriv '(** x 3) 'x))

(display "(deriv '(** x 3) 'y) => ")
(print (deriv '(** x 3) 'y))


(print "----------------")
(print "問題2.74")

;; override
(define repository (make-hash-table))

(define (put office name data)
  (if (not (hash-table-exists? repository office))
      (hash-table-put! repository office (make-hash-table)))
  (let ((name-table (hash-table-get repository office)))
       (hash-table-put! name-table name data)))

(define (get office name)
  (if (hash-table-exists? repository office)
      (let ((name-table (hash-table-get repository office)))
           (if (hash-table-exists? name-table name)
               (hash-table-get name-table name)
               #f))
      #f))
      ;(error "Not exists" office name)))

;;
;; common
;;   employee: (office data)
;;

;; office-a
;;   data: (name address salary)
(define (install-office-a-package)
  (define office 'office-a)

  (define (make-employee name address salary)
    ;; (name address salary)
    (list name address salary))
  (define (get-record name)
    (let ((data (get office name)))
         (if data
             (get office name))
             data))
  (define (put-record name address salary)
    (put office name (make-employee name address salary)))

  (define (name employee)
    (car employee))
  (define (address employee)
    (cadr employee))
  (define (salary employee)
    (caddr employee))

  (put 'name       office name)
  (put 'address    office address)
  (put 'salary     office salary)
  (put 'get-record office get-record)
  (put 'put-record office put-record)

  'done)
(install-office-a-package)

;; office-b
;;   data: (name salary address)
(define (install-office-b-package)
  (define office 'office-b)

  (define (make-employee salary address)
    ;; (salary address)
    (list salary address))
  (define (get-record name)
    (let ((data (get office name)))
         (if data
             (cons name (get office name)))
             data))
  (define (put-record name address salary)
    (put office name (make-employee salary address)))

  (define (name employee)
    (error "unsupport!!"))
    ;(car employee))
  (define (address employee)
    (cadr employee))
  (define (salary employee)
    (car employee))

  (put 'name       office name)
  (put 'address    office address)
  (put 'salary     office salary)
  (put 'get-record office get-record)
  (put 'put-record office put-record)

  'done)
(install-office-b-package)

;;
;; a
;;
(print "-- a --")

(define (attach-tag tag x) (cons tag x))

(define (put-record office name address salary)
  ((get 'put-record office) name address salary))

(define (get-record office name)
  (let ((data ((get 'get-record office) name)))
       (if data
           (attach-tag office data)
           #f)))

;; --

(put-record 'office-a 'kato 'asakusa 10)
(define kato (get-record 'office-a 'kato))
(display "(get-record 'office-a 'kato) => ")
(print kato)

(put-record 'office-b 'akihiro 'asakusa 20)
(define akihiro (get-record 'office-b 'akihiro))
(display "(get-record 'office-b 'akihiro) => ")
(print akihiro)

;;
;; b
;;
(print "-- b --")

(define (get-salary employee)
  ((get 'salary (car employee)) (cdr employee)))

;; --

(display "(get-salary kato) => ")
(print (get-salary kato))
(display "(get-salary akihiro) => ")
(print (get-salary akihiro))

;;
;; c
;;
(print "-- c --")

(define (find-employee-record name offices)
  (define (itr offices)
    (if (null? offices)
      #f
      (let ((employee (get-record (car offices) name)))
        (if employee
          employee
          (itr (cdr offices))))))
  (itr offices))

;; --

(display "(find-employee-record 'kato '(office-a office-b)) => ")
(print (find-employee-record 'kato '(office-a office-b)))
(display "(find-employee-record 'akihiro '(office-a office-b)) => ")
(print (find-employee-record 'akihiro '(office-a office-b)))
(display "(find-employee-record 'akihiro '(office-a)) => ")
(print (find-employee-record 'akihiro '(office-a)))

;;
;; d
;;
(print "-- d --")

(define (install-office-c-package)
  (define office 'office-c) ;; <- edit

  (define (make-employee name address salary)
    ;; (name address salary)
    (list name address salary))
  (define (get-record name)
    (let ((data (get office name)))
         (if data
             (get office name))
             data))
  (define (put-record name address salary)
    (put office name (make-employee name address salary)))

  (define (name employee)
    (car employee))
  (define (address employee)
    (cadr employee))
  (define (salary employee)
    (caddr employee))

  (put 'name       office name)
  (put 'address    office address)
  (put 'salary     office salary)
  (put 'get-record office get-record)
  (put 'put-record office put-record)

  'done)
(install-office-c-package)

;; --

(put-record 'office-c 'kato 'kyoto 0)
(define kato-c (get-record 'office-c 'kato))
(display "(get-record 'office-c 'kato) => ")
(print kato-c)

