(print "----------------")

(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2)
  )
  (define (find-divisor n test-divisor)
    (cond
      ((> (square test-divisor) n) n)
      ((divides? test-divisor n) test-divisor)
      (else (find-divisor n (+ test-divisor 1)))
    )
  )
  (define (divides? a b)
    (zero? (modulo b a))
  )
  (= n (smallest-divisor n))
)

(define accumulate fold-left)

(define (enumerate-interval from to)
  (define (itr f t res)
    (if (> f t)
      res
      (itr f (- t 1) (cons t res))
    )
  )
  (itr from to '())
)

(define (flatmap proc seq)
  (accumulate append '() (map proc seq))
)

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair)))
)

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair)))
)

(define (prime-sum-pairs n)
  (map make-pair-sum
     (filter prime-sum?
       (flatmap
         (lambda (i)
           (map (lambda (j) (list i j))
             (enumerate-interval 1 (- i 1))
           )
         )
         (enumerate-interval 1 n)
       )
     )
  )
)

(define (remove item sequence)
  (filter (lambda (x) (not (= x item))) sequence)
)

(define (permutations s)
  (if (null? s)
    (list '())
    (flatmap
      (lambda (x)
        (map
          (lambda (p) (cons x p))
          (permutations (remove x s))
        )
      )
      s
    )
  )
)

; (print (prime-sum-pairs 13))
(print (permutations (list 1 2 3 4)))


(print "----------------")

(print "問題2.40")
(print "--")


(define (unique-pairs n)
  (flatmap
    (lambda (x)
      (map
        (lambda (y) (cons x y))
        (enumerate-interval 1 (- x 1))
      )
    )
    (enumerate-interval 1 n)
  )
)


(print (unique-pairs 3))


(print "----------------")

(print "問題2.41")
(print "--")

(define (sum-eql n s)
  (filter
    (lambda (ns) (= s (+ (car ns) (cadr ns) (caddr ns))))
    (flatmap
      (lambda (a)
        (flatmap
          (lambda (b)
            (map
              (lambda (c) (list a b c))
              (remove a (remove b (enumerate-interval 1 n)))
            )
          )
          (remove a (enumerate-interval 1 n))
        )
      )
      (enumerate-interval 1 n)
    )
  )
)

(print (sum-eql 10 20))


(print "----------------")

(print "問題2.42")


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map
              (lambda (new-row)
                (adjoin-position new-row k rest-of-queens)
              )
              (enumerate-interval 1 board-size)
            )
          )
          (queen-cols (- k 1))
        )
      )
    )
  )
  (queen-cols board-size)
)

;
; 1. 再帰で呼ばれている関数はどれか
;    その関数からデータの形式を読み取る
;
;   => (queen-cols k)
;     -> flatmap
;       (queen-cols (- k 1))
;       -> flatmap
;         (queen-cols (- k 2))
;         -> flatmap
;           (lambda (rest-of-queens) (...))
;          ...
;           -> flatmap
;             (lambda (rest-of-queens) (...))
;             (queen-cols 0) 
;
;   => flatmap を繰り返し適用するパターン
;     ex) board-size => 3
;       (queen-cols 0) :
;         (初期データ)
;       (queen-cols 1) :
;         (flatmap (lambda ...) (queen-cols 0)) => １行目が完成
;       (queen-cols 2) :
;         (flatmap (lambda ...) (queen-cols 1)) => ２行目が完成
;       (queen-cols 3) :
;         (flatmap (lambda ...) (queen-cols 2)) => ３行目が完成
;       => 終了
;
;   => 最終的なサイズ 4 x 4 の場合で
;      (flatmap (lambda ...) (queen-cols 1)) を考える
;        * (queen-cols 1) を (queen-cols 2) にする flatmap
;      (queen-cols 1) は１列だけの場合(4 x 1)におけるパターン
;      この場合の (queen-cols 1) のデータは
;        (list A B C D)
;        A : o x x x
;        B : x o x x
;        C : x x o x
;        D : x x x o
;      と、なっているはず(横向き、縦向きはいったん無視)
;
;      次に、flatmap での一回ごとの処理を見る
;      (map (lambda ...) (enumerate-interval 1 board-size))
;      となっていることから、データの要素数が board-size 倍になる
;        (list A B C D)
;        => (list AA AB AC AD BA BB BC BD CA CB CC CD DA DB DC DD)
;      そして、 filter をかけてダメなやつを取り除く
;        たとえば、上記で AB と続くと、斜めに存在するのでダメ
;
;      データが見えてきた
;        (list A B C D)
;        => (list AA AB AC AD BA BB BC BD CA CB CC CD DA DB DC DD)
;        => (list AAA AAB AAC ....)
;      ここで
;        A, B, C, D, AA, AB, ... AAA, AAB はすべて同じデータ構造になる
;        リストで表現できる
;          A  : (list A)
;          B  : (list B)
;          AB : (list A B)
;          AAB: (list A A B)
;        つまり
;          (list (list A A A) (list A A B) ... (list D D D))
;
;     (queen-cols 0)
;          (list (list))
;     (queen-cols 1)
;          (list (list A) (list B) (list C) (list D))
;
;     ここで、 "A" をどう表現するか決めていない
;     1. (col, row)を両方持たせると楽
;       => (cons col row)
;     2. row だけ持たせて、
;         col カウントダウン(listでの順序)で順番を取ることもできる
;       => row: Int
;         これはめんどくさいかも
;         これでやってみる(ﾟ∀ﾟ)
;

(define empty-board '())

(define (adjoin-position row k queens)
;        最終的なサイズ 4 x 4
;        (list A B C D)
;          => (list AA AB AC AD BA BB BC BD CA CB CC CD DA DB DC DD)
;        のケースで
;
;        k 2(固定)
;        queens A, B, C, D ; outer-loop
;          row 1, 2, 3, 4  ; inner-loop
;
;        それぞれの列に必ず１つのqueenが置かれるので
;        (= k (length (cons row queens))) となる
  (cons row queens)
)

(define (safe? k positions)
;        上記の adjoin-position の結果を検証するので
;        最後に追加したやつがそれ以前のものと共存できればよい
;        最後の追加したやつは positions の先頭にしている
;
;        (list A B C D)
;          => (list AA AB AC AD BA BB BC BD CA CB CC CD DA DB DC DD)
;          !!! ここのフィルタでの処理 !!!
;          => (list AC AD BD CA DA DB)
;
  (define (check-dup-itr queen col targets)
    (cond
      ; ((null? targets) #t)
      ((= col 0) #t)
      ((dup? queen col (car targets))
        (check-dup-itr queen (- col 1) (cdr targets)))
      (else #f)))

  (define (dup? queen col target)
    (cond
      ((or (= queen target)
           (= (abs (- queen target)) (- k col)))
        #f)
      (else #t)))

  (check-dup-itr (car positions) (- k 1) (cdr positions))
)


(print (queens 4))


(print "----------------")

(print "問題2.43")
(print "--")

;
; 元のやつ
;        ループ回数 * 要素数(filter無視)
;        (list `())
;            1 * 1
;        (list A B C D)
;          + 1 * 4
;        => (list AA AB ...)
;          + 1 * 16
;        => (list AAA AAB ...)
;          + 1 * 64
;        => (list AAAA AAAB ...)
;          + 1 * 256
;
; Reasonerのやつ
;
(define (queens-pain board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (new-row)
            (map
              (lambda (rest-of-queens)
                (adjoin-position new-row k rest-of-queens)
              )
              (queen-cols (- k 1))
            )
          )
          (enumerate-interval 1 board-size)
        )
      )
    )
  )
  (queen-cols board-size)
)
;
;        要素数の掛け算(filter無視)
;          上方が内側のループ
;        (list `())
;          * 1
;        (list A B C D)
;          * 4
;        => (list AA AB ...)
;          * 16
;        => (list AAA AAB ...)
;          * 64
;        => (list AAAA AAAB ...)
;          * 256
;
;      (filterを無視して)
;      ((A B C D) (B B C D))
;      とあった場合、
;      前者のやり方では
;        (cons A (B C D)) の (B C D) は使いまわされていた
;        branch の root から leaf へ
;      後者のやり方では
;        (cons A (B C D)) の (B C D) は都度、再帰で生成されている
;        使い回しが一切ない
;        branch の leaf から root へ
;
;

(print "(queens 6)")
(time (queens 6))
(print "(queens-pain 6)")
(time (queens-pain 6))

(define (safe? k positions)
  #t
)
(print "after-----")

(print "(queens 6)")
(time (queens 6))
(print "(queens-pain 6)")
(time (queens-pain 6))
(print (queens-pain 6))

;; (queens 5)
;; ;(time (queens 5))
;; ; real   0.000
;; ; user   0.000
;; ; sys    0.000
;; (queens-pain 5)
;; ;(time (queens-pain 5))
;; ; real   0.005
;; ; user   0.010
;; ; sys    0.000
;; (queens 6)
;; ;(time (queens 6))
;; ; real   0.001
;; ; user   0.000
;; ; sys    0.000
;; (queens-pain 6)
;; ;(time (queens-pain 6))
;; ; real   0.093
;; ; user   0.110
;; ; sys    0.000
;; (queens 7)
;; ;(time (queens 7))
;; ; real   0.004
;; ; user   0.000
;; ; sys    0.000
;; (queens-pain 7)
;; ;(time (queens-pain 7))
;; ; real   1.590
;; ; user   2.090
;; ; sys    0.060
;; (queens 8)
;; ;(time (queens 8))
;; ; real   0.073
;; ; user   0.120
;; ; sys    0.000
;; (queens-pain 8)
;; ;(time (queens-pain 8))
;; ; real  33.630
;; ; user  44.880
;; ; sys    1.160
;; (queens 9)
;; ;(time (queens 9))
;; ; real   1.081
;; ; user   1.890
;; ; sys    0.090
;; (queens-pain 9)
;; ;(time (queens-pain 9))
;; ; real 840.745
;; ; user 1154.480
;; ; sys   17.150
;;
