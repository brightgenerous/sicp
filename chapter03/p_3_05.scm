(print "----------------")

(use gauche.generator)
(use data.random)

(define rand-update (begin
  (define generater (integers$ 999999999 2))
  (lambda x (car (generator->list generater 1)))))
(define random-init (rand-update))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

; --

(map
  (lambda (x)
      (display (string-append "(estimate-pi " (number->string x) ") => "))
      (print (estimate-pi x)))
  '(3 30 300 3000 30000))

; --

(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1) (+ trials-passed 1) x2))
              (else
                (iter (- trials-remaining 1) trials-passed x2))))))
  (iter trials 0 initial-x))

; --

(map
  (lambda (x)
      (display (string-append "(estimate-pi " (number->string x) ") => "))
      (print (estimate-pi x)))
  '(3 30 300 3000 30000))

;; -----

(print "--")
(print "問題3.05")

(define (estimate-integral trials)
  (define (random x)
    (car (generator->list (integers$ x) 1)))
  (define (random-in-range low heigh)
    (let ((range (- heigh low)))
      (+ low (random range))))

  (define x 5)
  (define y 7)
  (define r 3)

  (define (randX)
    (random-in-range (- x r) (+ x r)))
  (define (randY)
    (random-in-range (- y r) (+ y r)))
  (define cesaro-test
    (lambda ()
      (<= (+ (expt (- (randX) x) 2) (expt (- (randY) y) 2)) (expt r 2))))

  (* (expt (* r 2) 2) (monte-carlo trials cesaro-test)))

; --

(map
  (lambda (x)
    (display (string-append "(estimate-integral " (number->string x) ") => "))
    (print (+ (estimate-integral x) 0.0)))
  '(3 30 300 3000 30000))

;; -----

(print "--")
(print "問題3.06")

(define rand (begin
  (define val 1)
  (define (simple-rand)
    (set! val (+ val 1))
    val)
  (define (reset-val x)
    (set! val x))
  (define (dispatch-rand arg)
    (cond ((eq? arg 'generate) (simple-rand))
          ((eq? arg 'reset) reset-val)
          (else (error "Unknown request - " m))))
  dispatch-rand))

; --

(map
  (lambda (x)
    (define (once x)
      (display (string-append "(rand " (symbol->string x) ") => "))
      (print (rand x)))
    (define (twice x v)
      (display (string-append "((rand " (symbol->string x) ") " (number->string v) ") => "))
      (print ((rand x) v)))
    (if (= (length x) 1)
      (once (car x))
      (twice (car x) (cadr x))))
  '((generate) (generate) (reset 30) (generate)))

