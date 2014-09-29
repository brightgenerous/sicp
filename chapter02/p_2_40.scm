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
; 元のやつ
;          ↓  T4_0-1
;             (何も無しから `() を生成)
;        (list `())
;          ↓  T4_1-2
;             ('() から (list A B C D) を生成)
;        (list A B C D)
;          ↓  T4_2-3
;             ((list A B C D) から (list AA AB AC ... DD) を生成)
;        => (list AA AB ... DD)
;          ↓  T4_3-4
;             ((list AA AB AC ... DD) から (list AAA AAB AAC ... DDD) を生成)
;        => (list AAA AAB ... DDD)
;          ↓  T4_4-5
;             ((list AAA AAB AAC ... DDD) から (list AAAA AAAB AAAC ... DDDD) を生成)
;        => (list AAAA AAAB ... DDDD)
;
;   => T4_0-1 + T4_1-2 + T4_2-3 + T4_3-4 + T4_4-5
;
; Reasonerのやつ
;
;        (list AAAA AAAB ... DDDD)
;          ↑  T4_4-5
;        (list AAA AAB ... DDD)
;          ↑  T4_3-4 * 4
;        (list AA AB ... DD)
;          ↑  T4_2-3 * 4 * 4
;        (list A B C D)
;          ↑  T4_1-2 * 4 * 4 * 4
;        (list `())
;          ↑  T4_0-1 * 4 * 4 * 4 * 4
;
;   => T4_0-1 * 4^4 + T4_1-2 * 4^3 + T4_2-3 * 4^2 + T4_3-4 * 4^1 + T4_4-5
;

(for-each
  (lambda (x)
    (display "(queens ")(display x)(display ")")(newline)
    (time (queens x))
    (display "(queens-pain ")(display x)(display ")")(newline)
    (time (queens-pain x))
  )
  (enumerate-interval 4 7)
)

;; (queens 4)
;; ;(time (queens x))
;; ; real   0.000
;; (queens-pain 4)
;; ;(time (queens-pain x))
;; ; real   0.001
;; (queens 5)
;; ;(time (queens x))
;; ; real   0.000
;; (queens-pain 5)
;; ;(time (queens-pain x))
;; ; real   0.008
;; (queens 6)
;; ;(time (queens x))
;; ; real   0.001
;; (queens-pain 6)
;; ;(time (queens-pain x))
;; ; real   0.120
;; (queens 7)
;; ;(time (queens x))
;; ; real   0.006
;; (queens-pain 7)
;; ;(time (queens-pain x))
;; ; real   2.222
;; (queens 8)
;; ;(time (queens x))
;; ; real   0.095
;; (queens-pain 8)
;; ;(time (queens-pain x))
;; ; real  47.504


; --------------------------
; d | queens | queens-pain
; --------------------------
; 4 |  0.000 |  0.001
; 5 |  0.000 |  0.008
; 6 |  0.001 |  0.120       | x 120 ? | 6^3 (216) ?
; 7 |  0.006 |  2.222       | x 350 ? | 7^3 (343) ?
; 8 |  0.095 | 47.504       | x 500 ? | 8^3 (512) ?
;
;   8のとき
;   => ... + T8_5-6       + T8_6-7       + T8_7-8       + T8_8-9
;   => ... + T8_5-6 * 8^3 + T8_6-7 * 8^2 + T8_7-8 * 8^1 + T8_8-9


;   9のときの予想
; --------------------------
; d | queens | queens-pain
; --------------------------
; 9 |  1.000 | 729          | x 729 ? | 9^3 (729) ?

;; (queens 9)
;; ;(time (queens x))
;; ; real   1.560
;; (queens-pain 9)
;; ;(time (queens-pain x))
;; ; real 868.281

(print "--")

; 処理時間が何故が逆になる
;(define (safe? k positions) #t)

;(print "chaged safe?")
;(print "(queens 6)")
;(time (queens 6))
;(print "(queens-pain 6)")
;(time (queens-pain 6))
;(print (queens-pain 6))

