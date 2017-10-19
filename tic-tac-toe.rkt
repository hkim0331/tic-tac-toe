#lang racket
;;-*- mode: scheme -*-
;;
;; コメント熟読せよ。
;;
;; * GUI からプログラムしてみる。
;; * 判定ルーチンを独立させる。
;;

(require racket/gui/base)

;; ゲームの縦横
(define rows 5)
(define cols 5)

;; 打ったマスを覚えておくのに current を導入。
(define current #f)

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

(define frame (new frame% [label "tic-tac-toe"]))
(send frame show #t)

;; ダイアログを作って msg を表示する。
;; クローズボタンが押されるまでゲームを止める。
(define message
  (λ (msg)
    (message-box "tic-tac-toe" msg)))

;; ボタンの背景は "" かどうか？
;; 引数はボタンオブジェクト、
;; 戻り値は背景が "" ならば #f、それ以外なら #t.
(define marked?
  (λ (btn)
    (not (equal? (send btn get-label) ""))))

;; ボタン btn の背景を mark とする。
;; すでにマークされていたらダイアログを出す。戻り値は #f,
;; マークできたら戻り値は #t.
(define mark
  (λ (btn mark)
    (if (marked? btn)
        (begin
          (message "already marked.")
          #f)
        (begin
          (send btn set-label mark)
          #t))))

;; リスト buttons-index 中のマーク m がついたボタンのリストを返す。
(define select-marked
  (λ (m)
    (filter (λ (ib) (equal? m (send (second ib) get-label)))
            buttons-index)))

;; アトム a はリスト xs の要素か？
(define exists?
  (λ (a xs)
    (cond
     ((null? xs) #f)
     ((= (car xs) a) #t)
     (else (exists? a (cdr xs))))))

;; FIXME, 打った石の横縦斜めに自分の石があるかどうかを判定する。
;; 一般化できたと思うが、長すぎ。
;; もっとコンサイスに書けないの？
;; BUGFIX cols=5 の時、(3 4 5) を #t にしてはいけない。
(define horizontal?
  (λ (c marks)
    (let ((m (modulo c cols)))
      (or
       (and (< m (- cols 2)) (exists? (+ c 1) marks) (exists? (+ c 2) marks)))
       (and (> m 2) (exists? (- c 1) marks) (exists? (- c 2) marks))
       (and (exists? (+ c 1) marks) (exists? (- c 1) marks)))))

(define vertical?
  (λ (c marks)
    (or
      (and (exists? (+ c cols) marks) (exists? (+ c (* 2 cols)) marks))
      (and (exists? (+ c cols) marks) (exists? (- c cols) marks))
      (and (exists? (- c cols) marks) (exists? (- c (* 2 cols)) marks)))))

(define diagonal?
  (λ (c marks)
    (or
     (and (exists? (+ c (+ cols 1)) marks)
          (exists? (+ c (* 2 (+ cols 1))) marks))
     (and (exists? (+ c (+ cols 1)) marks)
          (exists? (- c (+ cols 1)) marks))
     (and (exists? (- c (+ cols 1)) marks)
          (exists? (- c (* 2 (+ cols 1))) marks))
     (and (exists? (+ c (- cols 1)) marks)
          (exists? (+ c (* 2 (- cols 1))) marks))
     (and (exists? (+ c (- cols 1)) marks)
          (exists? (- c (- cols 1)) marks))
     (and (exists? (- c (- cols 1)) marks)
          (exists? (- c (* 2 (- cols 1))) marks)))))

(define win?
  (λ (objs)
    (let ((marks (map car objs)))
      (or
       (horizontal? current marks)
       (vertical? current marks)
       (diagonal? current marks)))))

;; 勝敗判定
;; select-marked で味方の石のある場所のリスト、
;; win? で石が並んでるかどうかを判定、メッセージをだす。
;; 勝敗がつかない時は #f で抜ける。
;;
;; FIXME, 勝敗ついた時はゲームを止めなくちゃな。
;; exit はあんまりだ。
(define judge
  (λ (mark)
    (if (win? (select-marked mark))
        (begin
          (message (string-append mark " wins"))
          (exit))
        #f)))

;; 相手に手を渡す。
;; 空いている目を調べ、ランダムにその一つにマーク "x" を入れる。
;; ルールは知っているが、戦略は持たない人工無脳。
;; 打った手を current に記録する。
(define opposite
  (λ ()
    (let* ((empties (select-marked ""))
           (my (nth empties (random (length empties)))))
      (mark (second my) "x")
      (set! current (first my))
      (judge "x"))))

;; 盤面の作成。
;; 同時にボタンのコールバック関数を定義。
;; この関数はそのマスにすでに石が入っていたらエラー、
;; そうでなければ "o" を表示し、相手に手を渡す関数を呼ぶ。
;; 二重ループが list の list を返すので、
;; map 関数が楽になるよう flatten で平たくしておく。
;; panel の代わりに軽い pane を使う。あんまり変わらん。
;; クリックされたボタンの場所を current にセーブする。
;; 場所は一次元リスト中のオフセット。
;; クリックされた場所を起点に縦横斜めの石の配置を調べるため。
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
                                            (judge "o")
                                            (opposite)))]))))))

;; ボタンのリストにインデックスをつけたリストをあらかじめ作っておく。
;; buttons がリスト (ba bb bc) であれば
;; buttonx-index は ((0 ba) (1 bb) (2 bc)) となる。
(define buttons-index (map-index buttons))
