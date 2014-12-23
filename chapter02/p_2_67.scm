(print "----------------")

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
;
; make-code-tree
;
; 図2.18
;
;          (left right (A B C D E F G H) 17)
;           /    \
;   ('leaf A 8)   \
;                  \
;              (left right (B C D E F G H) 9)
;               /                \
;         (left right (B C D) 5)  \
;          /    \                  \
;  ('leaf B 3)   \             (left right (E F G H) 4)
;                 \                |                |
;             (left right (C D) 2) |                |
;              /    \              |                |
;      (`leaf C 1)   \        (left right (E F) 2)  |
;                     \        |    \               |
;               (`leaf D 1)    |     \              |
;                              |      \             |
;                       ('leaf E 1)    \            |
;                                       \           |
;                                 ('leaf F 1)       |
;                                                   |
;                                              (left right (G H) 2)
;                                               /    \
;                                       ('leaf G 1)   \
;                                                      \
;                                                ('leaf H 1)
;

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))
;
; adjoin-set
;
;   element-of-set? のチェックは行わない
;   weightによる昇順
;   要素は tree or leaf どちらでも可
;
;  (('leaf C 1) ('leaf D 1) (left right (A B) 4))
;

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)
                             (cadr pair))
                  (make-leaf-set (cdr pairs))))))
;
; make-leaf-set
;
;  ((D 1) (B 2) (A 4) (C 1))
;  => (('leaf A 4) ('leaf B 2) ('leaf C 1) ('leaf D 1))
;

(print "問題2.67")

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(print "--")

(display "(decode sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0)) => ")
(print    (decode sample-message sample-tree))
; (0):A (1 1 0):D (0):A (1 0):B (1 0):B (1 1 1):C (0):A
; (A D A B B C A)


(print "----------------")

(print "問題2.68")

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

; --

;
; A => (0)
; B => (1 0)
; D => (1 1 0)
; C => (1 1 1)
;

(print "--")

(define (encode-symbol symbol tree)
  (define (itr tree)
    (if (leaf? tree)
      (if (equal? (symbol-leaf tree) symbol)
        '()
        #f)
      (let
        ((left-res (itr (left-branch tree))))
        (if left-res
          (cons 0 left-res)
          (let
            ((right-res (itr (right-branch tree))))
            (if right-res
              (cons 1 right-res)
              #f))))))
  (if (leaf? tree)
    (error "tree must not be leaf")
    (let
      ((res (itr tree)))
      (if res
        res
        (error "symbol not found")))))


(display "(encode-symbol 'A sample-tree) => ")
(print    (encode-symbol 'A sample-tree))
(display "(encode-symbol 'B sample-tree) => ")
(print    (encode-symbol 'B sample-tree))
(display "(encode-symbol 'C sample-tree) => ")
(print    (encode-symbol 'C sample-tree))
(display "(encode-symbol 'D sample-tree) => ")
(print    (encode-symbol 'D sample-tree))
;(display "(encode-symbol 'E sample-tree) => ")
;(print    (encode-symbol 'E sample-tree))

; --

; (A D A B B C A)
; => (0 1 1 0 0 1 0 1 0 1 1 1 0))
(display "(encode '(A D A B B C A) sample-tree) => ")
(print    (encode '(A D A B B C A) sample-tree))


(print "----------------")

(print "問題2.69")

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(print "--")

(define (successive-merge leaf-set)
  (define (merge-heads leaf-set)
    (adjoin-set
      (make-code-tree (car leaf-set) (cadr leaf-set))
      (cddr leaf-set)))
  (define (itr leaf-set)
    (if (null? (cdr leaf-set))
      (car leaf-set)
      (itr (merge-heads leaf-set))))
  (itr leaf-set))

;(print (make-leaf-set '((D 1) (B 2) (A 4) (C 1))))
(print (successive-merge (make-leaf-set '((D 1) (B 2) (A 4) (C 1)))))


(print "----------------")

(print "問題2.70")
(print "--")

(define tree-1950 (generate-huffman-tree '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))
(define song '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
(define encoded (encode song tree-1950))

(display "tree => ")
(print tree-1950)
(display "encoded => ")
(print encoded)
(display "size => ")
(print (length encoded))
(display "decode => ")
(print (decode encoded tree-1950))

(display "8 = 2^3 => ")
(print (* (length song) 3))


(print "----------------")

(print "問題2.71")
(print "--")

;
; tree-n5
;
;                      (left right (A B C D E) 31)
;                       /    \
;                      /   ('leaf E 16)
;                     /
;                (left right (A B C D) 15)
;                 /    \
;                /   ('leaf D 8)
;               /
;          (left right (A B C) 7)
;           /    \
;          /   (`leaf C 4)
;         /
;    (left right (A B) 3)
;     /    \
;    /   (`leaf B 2)
;   /
; (`leaf A 1)
;
;
; tree-n10
;
;                      (left right (A B C D E F G H I J) 1023)
;                       /    \
;                      /   ('leaf J 512)
;                     /
;                (left right (A B C D E F G H I) 511)
;                 /    \
;                /   ('leaf I 256)
;               /
;          (left right (A B C D E F G H) 255)
;           /    \
;          /   (`leaf H 128)
;         /
;    (left right (A B C D E F G) 127)
;     /    \
;    /   (`leaf G 64)
;   /
; (left right (A B C D E F) 63)
;  |    \
;  |  ('leaf F 32)
;  |
;  tree-n5
;

(define tree-n5 (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16))))
(define tree-n10 (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512))))

(display "tree-n5 => ")
(print tree-n5)
(display "tree-n10 => ")
(print tree-n10)

(display "(length (encode '(E) tree-n5)) => ")
(print (length (encode '(E) tree-n5)))
; => 1
(display "(encode '(A) tree-n5) => ")
(print (length (encode '(A) tree-n5)))
; => 5 - 1

(display "(length (encode '(J) tree-n10)) => ")
(print (length (encode '(J) tree-n10)))
; => 1
(display "(length (encode '(A) tree-n10)) => ")
(print (length (encode '(A) tree-n10)))
; => 10 - 1


(print "----------------")

(print "問題2.72")
(print "--")

