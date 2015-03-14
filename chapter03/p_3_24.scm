(print "----------------")

((lambda arg

  (define (lookup key table)
    (let ((record (assoc key (cdr table))))
      (if record
        (cdr record)
        #f)))

  (define (assoc key records)
    (cond ((null? records) #f)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

  (define (insert! key value table)
    (let ((record (assoc key (cdr table))))
      (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
    'ok)

  (define (make-table)
    (list '*table*))
))

((lambda arg

   (define (lookup key-1 key-2 table)
     (let ((subtable (assoc key-1 (cdr table))))
       (if subtable
         (let ((record (assoc key-2 (cdr subtable))))
           (if record
             (cdr record)
             #f))
         #f)))

   (define (insert! key-1 key-2 value table)
     (let ((subtable (assoc key-1 (cdr table))))
       (if subtable
         (let ((record (assoc key-2 (cdr subtable))))
           (if record
             (set-cdr! record value)
             (set-cdr! subtable
                       (cons (cons key-2 value)
                             (cdr subtable)))))
         (set-cdr! table
                   (cons (list key-1 (cons key-2 value))
                         (cdr table)))))
     'ok)
))

((lambda arg

   (define (make-table)
     (let ((local-table (list '*table*)))
       (define (lookup key-1 key-2)
         (let ((subtable (assoc key-1 (cdr local-table))))
           (if subtable
             (let ((record (assoc key-2 (cdr subtable))))
               (if record
                 (cdr record)
                 #f))
             #f)))
       (define (insert! key-1 key-2 value)
         (let ((subtable (assoc key-1 (cdr local-table))))
           (if subtable
             (let ((record (assoc key-2 (cdr subtable))))
               (if record
                 (set-cdr! record value)
                 (set-cdr! subtable
                           (cons (cons key-2 value)
                                 (cdr subtable)))))
             (set-cdr! local-table
                       (cons (list key-1
                                   (cons key-2 value))
                             (cdr local-table)))))
         'ok)

       (define (dispatch m)
         (cond ((eq? m 'lookup-proc) lookup)
               ((eq? m 'insert-proc!) insert!)
               (else (error "Unknown operation -- TABLE" m))))

       dispatch))
))

;; -----

(print "--")
(print "問題3.24")

((lambda arg

   (define (make-table same-key?)

     (define (assoc-same-key? key datas)
       (if (null? datas)
         #f
         (let ((head (car datas)))
           (if (same-key? (car head) key)
             head
             (assoc-same-key? key (cdr datas))))))

     (let ((local-table (list '*table*)))

       (define (lookup key-1 key-2)
         (let ((subtable (assoc-same-key? key-1 (cdr local-table))))
           (if subtable
             (let ((record (assoc-same-key? key-2 (cdr subtable))))
               (if record
                 (cdr record)
                 #f))
             #f)))

       (define (insert! key-1 key-2 value)
         (let ((subtable (assoc-same-key? key-1 (cdr local-table))))
           (if subtable
             (let ((record (assoc-same-key? key-2 (cdr subtable))))
               (if record
                 (set-cdr! record value)
                 (set-cdr! subtable
                           (cons (cons key-2 value)
                                 (cdr subtable)))))
             (set-cdr! local-table
                       (cons (list key-1
                                   (cons key-2 value))
                             (cdr local-table)))))
         'ok)

       (define (dispatch m)
         (cond ((eq? m 'lookup-proc) lookup)
               ((eq? m 'insert-proc!) insert!)
               (else (error "Unknown operation -- TABLE" m))))

       dispatch))

   (define table (make-table (^(k1 k2) (= 10 (+ k1 k2)))))
   ((table 'insert-proc!) 3 4 'hoge)
   (print (format "((table 'lookup-proc) 7 6) => ~a" ((table 'lookup-proc) 7 6)))
   (print (format "((table 'lookup-proc) 3 4) => ~a" ((table 'lookup-proc) 3 4)))
   (print (format "((table 'lookup-proc) 7 4) => ~a" ((table 'lookup-proc) 7 4)))
))

;; -----

(print "--")
(print "問題3.25")

((lambda arg

   (define (make-table same-key?)

     (define (assoc-same-key? key datas)
       (if (null? datas)
         #f
         (let ((head (car datas)))
           (if (same-key? (car head) key)
             head
             (assoc-same-key? key (cdr datas))))))

     (define (new-datas) '())

     (let ((local-table (list '*table*)))

       (define (lookup keys)
         (define (itr table head-key tail-keys)
           (let ((datas (cdr table)))
               ; here. datas must be checked?
             (let ((record (assoc-same-key? head-key datas)))
               (if record
                 (if (null? tail-keys)
                   (cdr record)
                   (itr record (car tail-keys) (cdr tail-keys)))
                 #f))))
         (if (null? keys)
           (error "keys is empty!")
           (itr local-table (car keys) (cdr keys))))

       (define (insert! keys value)
         (define (itr table head-key tail-keys)
           (let ((datas (cdr table)))
               ; here. datas must be checked?
             (let ((record (assoc-same-key? head-key datas)))
               (if (null? tail-keys)
                 (begin
                   (if record
                     (set-cdr! record value)
                     (set-cdr! table (cons (cons head-key value) datas)))
                   'ok)
                 (let ((next-head-key (car tail-keys)) (next-tail-keys (cdr tail-keys)))
                   (if record
                     (itr record next-head-key next-tail-keys)
                     (let ((new-record (cons head-key (new-datas))))
                       (set-cdr! table (cons new-record datas))
                       (itr new-record next-head-key next-tail-keys))))))))
         (if (null? keys)
           (error "keys is empty!")
           (itr local-table (car keys) (cdr keys))))

       (define (dispatch m)
         (cond ((eq? m 'lookup-proc) lookup)
               ((eq? m 'insert-proc!) insert!)
               (else (error "Unknown operation -- TABLE" m))))

       dispatch))

   (define table (make-table equal?))
   ((table 'insert-proc!) '(3 4) 'hoge)
   (print (format "((table 'lookup-proc) '(7 6)) => ~a" ((table 'lookup-proc) '(7 6))))
   (print (format "((table 'lookup-proc) '(3 4)) => ~a" ((table 'lookup-proc) '(3 4))))
   (print (format "((table 'lookup-proc) '(7 4)) => ~a" ((table 'lookup-proc) '(7 4))))
   (print (format "((table 'lookup-proc) '(3)) => ~a" ((table 'lookup-proc) '(3))))
))

