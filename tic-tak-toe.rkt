#lang racket
;;-*- mode: scheme -*-
;;
;; コメント熟読せよ。
;;
;; * GUI からプログラムしてみる
;; * 判定ルーチンを独立させる
;;

(require racket/gui/base)

;; ゲームの縦横
(define rows 3)
(define cols 3)

;; リストの n 番目の要素を返す。基本関数。
(define nth
  (λ (obj n)
    (cond
      ((null? obj) '())
      ((zero? n) (first obj))
      (else (nth (rest obj) (- n 1))))))

;; (map-index '(a b c d)) は ((0 a) (1 b) (2 c) (3 d)) を返す。
;; 味方の石が置かれた位置を知る時に使う。
;; a c に味方の石があるとき (0 2) が返るような関数をどこかに定義する。
(define map-index
  (λ (lst)
    (map list (range (length lst)) lst)))


;; 打ったマスを覚えておくのに current を導入。
(define current #f)

(define frame (new frame% [label "tic-tac-toe"]))
(send frame show #t)

;; ダイアログを作って msg を表示する。
;; クローズボタンが押されるまでゲームを止める。
(define message
  (λ (msg)
    (message-box "tic-tac-toe" msg)))

;; ボタンの背景は "" かどうか？
;; "" ならば #f、それ以外なら #t
(define marked?
  (λ (btn)
    (not (equal? (send btn get-label) ""))))

;; ボタン btn の背景を mark とする。
;; すでにマークされていたらエラー。
(define mark
  (λ (btn mark)
    (if (marked? btn)
        (begin
          (message "already marked.")
          #f)
        (begin
          (send btn set-label mark)
          #t))))

;; リスト buttons-index 中のマーク m がついたボタンのインデックスを返す。
;; car を忘れずに。
(define select-index
  (λ (m)
    (map car
         (filter (λ (ib) (equal? m (send (second ib) get-label)))
                 buttons-index))))

;; FIXME, インデックスから縦横斜めに揃っているかどうかを判定。
;; 揃っていたら #t, そうでなければ #f を返す。
;; インチキして長さで判定。これでは正しく判定できないよ。
;; 宮崎の出番だ。
(define wins?
  (lambda (marks)
    (> (length marks) 3)))

;; 勝敗判定
;; select-index で味方の石のある場所のリスト、
;; wins? で石が並んでるかどうかを判定、メッセージをだす。
;; 勝敗がつかない時は #f で抜ける。
;; FIXME, 勝敗ついた時はゲームを止めなくちゃな。
(define judge
  (lambda ()
    (cond
     ((wins? (select-index "o")) (message "o wins"))
     ((wins? (select-index "x")) (message "x wins"))
     (else #f))))

;; 相手に手を渡す。
;; 空いている目を調べ、ランダムにその一つにマーク "x" を入れる。
;; ルールは知っているが、戦略は持たないバカな相手。
(define opposite
  (λ ()
    (let* ((empties)
           (filter (λ (b) (equal? (send b get-label) "")) buttons)
           (my (first empties)))
      ;; FIXME, 空いている最初のマスを選択する。
      (mark my "x")
      (set! current my)
      (judge))))

;; 盤面の作成。
;; 同時にボタンのコールバック関数を定義。
;; この関数はボタンがすでに押されていたらエラーメッセージを出し、
;; そうでなければ "o" を表示し、相手に手を渡す関数を呼ぶ。
;; 2重ループが list の list を返すので、
;; map 関数が楽になるよう flatten で平たくしておく。
;; panel の代わりに軽い pane を使う。あんまり変わらん。
(define buttons
  (flatten
   (for/list ([row (range rows)])
             (let ((pane (new horizontal-pane% [parent frame])))
               (for/list ([col (range cols)])
                         (new button% [parent pane]
                              [label ""]
                              [callback (λ (b e)
                                          (when (mark b "o")
                                            (set! current (+ (* row rows) col))
                                            (judge)
                                            (opposite)))]))))))

;; ボタンのリストにインデックスをつけたリストをあらかじめ作っておく。
;; (b0 b1 b2) => ((0 b0) (1 b1) (2 b2)) みたいに。
(define buttons-index (map-index buttons))
