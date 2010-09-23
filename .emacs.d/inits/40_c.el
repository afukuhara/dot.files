;;; =============================================== ;;;
;;;                    C-mode                       ;;;
;;; =============================================== ;;;
(add-hook 'c-mode-hook
          '(lambda ()
             (c-set-style "stroustrup")
             (setq c-basic-offset 4)
             (setq c-tab-width 4 )
             ; (setq c-argdecl-indent 0)       ; 関数の引数行のインデント
             ; 但し引数行で明示的にタブを押さない
             ; 場合は、インデントしない
             ;   (setq c-auto-newline t)               ; 自動改行
             (setq c-continued-statement-offset 4) ; { を書く時のインデント
             (setq c-indent-level 4)               ; { を書いた後のインデント
             (setq c-label-offset -4)              ; ラベルの深さ
             (setq c-tab-always-indent t)          ; タブ記号を押した時にユーザーが
                                                   ; 任意にタブ記号を入れることは不可
             (setq tab-width 4)         ; タブ記号のインデント深さ
             (setq c-brace-imaginary-offset 0)     ; 不明
             (setq c-brace-offset 0)               ; 不明
             (define-key cperl-mode-map "\C-c\C-d" 'gdb) ; GDBの起動
             ))

(add-hook 'c-mode-common-hook
          '(lambda()
             (gtags-mode 1)
             (gtags-make-complete-list)))

