(print "----------------")

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (elemnt-of-set? x (left-branch set)))
        ((> x (entry set))
         (elemnt-of-set? x (right-branch set)))
  )
)

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(print "問題2.63")
(print "--")

(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

;; --

(define tree-1-l-l (make-tree  1 '() '()))
(define tree-1-l-r (make-tree  5 '() '()))
(define tree-1-l   (make-tree  3 tree-1-l-l tree-1-l-r))
(define tree-1-r-r (make-tree 11 '() '()))
(define tree-1-r   (make-tree  9 '() tree-1-r-r))
(define tree-1     (make-tree  7 tree-1-l tree-1-r))

(define tree-2
  (list 3
        (list 1 '() '())
        (list 7
              (list 5 '() '())
              (list 9
                    '()
                    (list 11 '() '())))))
(define tree-3
  (list 5
        (list 3
              (list 1 '() '())
              '())
        (list 9
              (list 7 '() '())
              (list 11 '() '()))))

(display "(tree->list-1 tree-1) => ")
(print    (tree->list-1 tree-1)     )
;; => (1 3 5 7 9 11)
(display "(tree->list-2 tree-1) => ")
(print    (tree->list-2 tree-1)     )
;; => (1 3 5 7 9 11)

(display "(tree->list-1 tree-2) => ")
(print    (tree->list-1 tree-2)     )
;; => (1 3 5 7 9 11)
(display "(tree->list-2 tree-2) => ")
(print    (tree->list-2 tree-2)     )
;; => (1 3 5 7 9 11)

(display "(tree->list-1 tree-3) => ")
(print    (tree->list-1 tree-3)     )
;; => (1 3 5 7 9 11)
(display "(tree->list-2 tree-3) => ")
(print    (tree->list-2 tree-3)     )
;; => (1 3 5 7 9 11)

;;
;; a. -> 結果に違いが出るようにみえない('A`)
;;
;; b. ->
;;   tree->list-1 =>
;;     left-branch, right-branch に対して 1回ずつ tree->list-1 を再帰的に呼ぶ
;;     append がつらい？
;;     処理の過程で無駄なデータが作られる
;;   tree->list-2 =>
;;     left-branch, right-branch に対して 1回ずつ tree->list-2 を再帰的に呼ぶ
;;     最初に作られる '() に entry を cons する
;;     それがどう移動していくかみるとわかる
;;     処理の過程で無駄なデータが作られない
;;


(print "----------------")
(print "問題2.64")
(print "--")

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))



;; --

(display "(partial-tree (list 1 2 3 4 5 6 7 8 9 10) 5) => ")
(print    (partial-tree (list 1 2 3 4 5 6 7 8 9 10) 5)     )
(display "(list->tree (list 1 2 3 4 5)) => ")
(print    (list->tree (list 1 2 3 4 5))     )

;;
;; a.
;;   partial-tree set n
;;     set のうち、先頭から n 個を tree に変換して置き換える
;;     (partial-tree (1 2 3 4 5 6 7 8 9) 4)
;;       => ((1 ~ 4 の tree) 5 6 7 8 9)
;;       => 手順 - ループ 1回目
;;         -> (1 2 3 4) が
;;            tree へ変換する対象であり、呼び出し元にとっての left-tree になる
;;         -> 5 が entry
;;         -> 残った (6 7 8 9) がまだ tree になっていない部分
;;            remaining-elts であり、呼び出し元にとっての right-tree になる

(print "--")

(display "(list->tree (list 1 3 5 7 9 11)) => ")
(print    (list->tree (list 1 3 5 7 9 11))     )

;;
;; b.
;;   すべての要素が1回ずつ entry としてチェックされる
;;   θ(n)
;;


(print "----------------")
(print "問題2.65")
(print "--")

