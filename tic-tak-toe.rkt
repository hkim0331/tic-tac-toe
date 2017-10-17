#lang racket

(require racket/gui/base)

(define rows 3)
(define cols 3)

(define frame (new frame% [label "tic-tac-toe"]))
(send frame show #t)

(define message
  (λ (msg)
    (message-box "tic-tac-toe" msg)))

(define marked?
  (λ (obj)
    (not (equal? (send obj get-label) ""))))
  
(define mark
  (λ (obj mark)
    (if (marked? obj)
        (begin
          (message "already marked.")
          #f)
        (begin
          (send obj set-label mark)
          ;; jobs.
          #t))))

(define opposite
  (λ ()
    (message "opposite turn")))

(define buttons
  (flatten (for/list ([row (range rows)])
                     (let ((pane (new horizontal-pane% [parent frame])))
                       (for/list ([col (range cols)])
                                 (new button% [parent pane]
                                      [label ""]
                                      [callback (λ (b e)
                                                  (when (mark b "o")
                                                    (opposite)))]))))))

(define nth
  (λ (obj n)
    (cond
      ((null? obj) '())
      ((zero? n) (first obj))
      (else (nth (rest obj) (- n 1))))))