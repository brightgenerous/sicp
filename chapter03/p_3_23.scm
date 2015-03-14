(print "----------------")

;; -----

(print "--")
(print "問題3.23")

((lambda arg

  ;
  ; element ((prev .  next) . (element . [next cons])
  ;
  (define (make-dequeue)
    (let
      ((front-ptr '()) (rear-ptr '()))

      (define (clear)
        (set! front-ptr '())
        (set! rear-ptr '()))

      (define (init e)
        (let ((new-datas (cons e '())))
          (let ((new-node (create-node '() '() new-datas)))
            (set! front-ptr new-node)
            (set! rear-ptr new-node))))

      (define (create-node prev next datas)
        (cons (cons prev next) datas))

      (define (get-datas ptr)
        (cdr ptr))

      (define (get-list)
        (if (empty-queue?)
          '()
          (get-datas front-ptr)))

      (define (get-element ptr)
        (car (get-datas ptr)))

      (define (get-prev node)
        (caar node))

      (define (get-next node)
        (cdar node))

      (define (set-prev! node prev)
        (set-car! (car node) prev))

      (define (set-next! node next)
        (set-cdr! (car node) next))

      (define (empty-queue?)
        (null? front-ptr))

      (define (with-empty-check f)
        (if (empty-queue?) (error "queue must not be empty!") (f)))

      (define (front-queue) (with-empty-check (^()
        (get-element front-ptr))))

      (define (rear-queue) (with-empty-check (^()
        (get-element rear-ptr))))

      (define (front-insert-queue! e)
        (if (empty-queue?)
          (init e)
          (let ((old-front-ptr front-ptr))
            (let ((new-datas (cons e (get-datas old-front-ptr))))
              (let ((new-node (create-node '() old-front-ptr new-datas)))
                (set-prev! old-front-ptr new-node)
                (set! front-ptr new-node)))))
        dispatch)

      (define (rear-insert-queue! e)
        (if (empty-queue?)
          (init e)
          (let ((old-rear-ptr rear-ptr))
            (let ((new-data (cons e '())))
              (let ((new-node (create-node old-rear-ptr '() new-data)))
                (set-cdr! (get-datas old-rear-ptr) new-data)
                (set-next! old-rear-ptr new-node)
                (set! rear-ptr new-node)))))
        dispatch)

      (define (front-delete-queue!) (with-empty-check (^()
        (let ((ret (get-element front-ptr)))
          (if (eq? front-ptr rear-ptr)
            (clear)
            (let ((new-front-ptr (get-next front-ptr)))
              (set-prev! new-front-ptr '())
              (set! front-ptr new-front-ptr)))
          ret))))

      (define (rear-delete-queue!) (with-empty-check (^()
        (let ((ret (get-element rear-ptr)))
          (if (eq? front-ptr rear-ptr)
            (clear)
            (let ((new-rear-ptr (get-prev rear-ptr)))
              (set-cdr! (get-datas new-rear-ptr) '())
              (set-next! new-rear-ptr '())
              (set! rear-ptr new-rear-ptr)))
          ret))))

      (define (dispatch m)
        (cond
          ((eq? m 'empty-queue?) (empty-queue?))
          ((eq? m 'front-queue) (front-queue))
          ((eq? m 'rear-queue) (rear-queue))
          ((eq? m 'front-insert-queue!) front-insert-queue!)
          ((eq? m 'rear-insert-queue!) rear-insert-queue!)
          ((eq? m 'front-delete-queue!) (front-delete-queue!))
          ((eq? m 'rear-delete-queue!) (rear-delete-queue!))
          ((eq? m 'datas) (get-list))
          (else (error "Undefined operation -- dequeue" m))))
      dispatch))

  (define deq (make-dequeue))

  (print (format "(deq 'empty-queue?) => ~a" (deq 'empty-queue?)))
  (print (format "((deq 'front-insert-queue!) 'f1) => ~a" ((deq 'front-insert-queue!) 'f1)))
  (print (format "(deq 'datas) => ~a" (deq 'datas)))
  (print (format "((deq 'front-insert-queue!) 'f2) => ~a" ((deq 'front-insert-queue!) 'f2)))
  (print (format "(deq 'datas) => ~a" (deq 'datas)))
  (print (format "((deq 'rear-insert-queue!) 'r1) => ~a" ((deq 'rear-insert-queue!) 'r1)))
  (print (format "(deq 'datas) => ~a" (deq 'datas)))
  (print (format "((deq 'rear-insert-queue!) 'r2) => ~a" ((deq 'rear-insert-queue!) 'r2)))
  (print (format "(deq 'datas) => ~a" (deq 'datas)))

  (print (format "(deq 'front-delete-queue!) => ~a" (deq 'front-delete-queue!)))
  (print (format "(deq 'datas) => ~a" (deq 'datas)))
  (print (format "(deq 'rear-delete-queue!) => ~a" (deq 'rear-delete-queue!)))
  (print (format "(deq 'datas) => ~a" (deq 'datas)))

  (print (format "(deq 'empty-queue?) => ~a" (deq 'empty-queue?)))

  (print (format "(deq 'rear-delete-queue!) => ~a" (deq 'rear-delete-queue!)))
  (print (format "(deq 'datas) => ~a" (deq 'datas)))
  (print (format "(deq 'rear-delete-queue!) => ~a" (deq 'rear-delete-queue!)))
  (print (format "(deq 'datas) => ~a" (deq 'datas)))

  (print (format "(deq 'empty-queue?) => ~a" (deq 'empty-queue?)))
))

