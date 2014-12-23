(print "----------------")

(print "問題2.75")

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
            (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;; x = 1, y = 1
(define ra-5-1 (make-from-mag-ang 1.414 (/ 3.14 4)))
(display "(ra-5-1 'real-part) => ")
(print (ra-5-1 'real-part))
(display "(ra-5-1 'imag-part) => ")
(print (ra-5-1 'imag-part))
(display "(ra-5-1 'magnitude) => ")
(print (ra-5-1 'magnitude))
(display "(ra-5-1 'angle) => ")
(print (ra-5-1 'angle))


(print "----------------")

(print "問題2.76")

;;
;; データ x 種類
;; 追加する演算 y 種類
;;
;; 明確な振り分けの汎用演算
;;   演算の種類の数だけ分岐を修正する必要がある
;;     修正箇所
;;       修正する関数 : y
;;   データと演算が別々に存在する
;;     動的に演算を追加できる
;;     動的に型を追加できない
;;
;; データ主導
;;   `install-package` をする必要がある
;;   ただし、修正は不要で、追加するだけでよい
;;     修正箇所
;;       追加 install : x
;;   データと演算が別々に存在する
;;     動的に演算を追加できる？
;;     動的に型を追加できる
;;
;; メッセージパッシング
;;   データの種類の数だけ分岐を修正する必要がある
;;     修正箇所
;;       修正する関数 : x
;;   データと演算がくっついている（この中ではオブジェクト指向に近い）
;;     動的に演算を追加できない？
;;     動的に型を追加できる
;;
