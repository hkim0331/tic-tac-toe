#lang racket

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
(define mark
  (λ (btn mark)
    (if (marked? btn)
        (begin
          (message "already marked.")
          #f)
        (begin
          (send btn set-label mark)
          #t))))


;; 相手に手を渡す。
;; 空いている目を調べ、ランダムにその一つにマーク "x" を入れる。
;; ルールは知っているが、戦略は持たないバカな相手。
(define opposite
  (λ ()
    (let ((empties
           (filter (λ (b) (equal? (send b get-label) "")) buttons)))
      ;; FIXME, version 1. 空いている最初のマスを選択する。
      (mark (first empties) "x"))))

;; 盤面の作成。
;; 同時にボタンのコールバック関数を定義。
;; この関数はボタンがすでに押されていたらエラーメッセージを出し、
;; そうでなければ "o" を表示し、相手に手を渡す関数を呼ぶ。
;; 2重ループが list の list を返すので、
;; map 関数が楽になるよう flatten で平たくしておく。
(define buttons
  (flatten (for/list ([row (range rows)])
                     (let ((pane (new horizontal-pane% [parent frame])))
                       (for/list ([col (range cols)])
                                 (new button% [parent pane]
                                      [label ""]
                                      [callback (λ (b e)
                                                  (when (mark b "o")
                                                    (opposite)))]))))))
