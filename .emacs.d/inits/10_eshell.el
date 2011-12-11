;; ------------------------------------------------------------
;;  From: eshellをはじめてみた - とりあえず暇だったし何となく始めたブログ
;;         http://d.hatena.ne.jp/khiker/20060919/1158686507
;; ------------------------------------------------------------
;; 補完時に大文字小文字を区別しない
(setq eshell-cmpl-ignore-case t)
;; 確認なしでヒストリ保存
(setq eshell-ask-to-save-history (quote always))
;; 補完時にサイクルする
(setq eshell-cmpl-cycle-completions t)
;;補完候補がこの数値以下だとサイクルせずに候補表示
(setq eshell-cmpl-cycle-cutoff-length 5)
;; 履歴で重複を無視する
(setq eshell-hist-ignoredups t)

;; prompt文字列の変更
(setq eshell-prompt-function
      (lambda ()
        (concat "["
                (eshell/whoami)
                "@"
                (system-name)
                " "
                (eshell/pwd)
                (if (= (user-uid) 0) "]\n# " "]\n$ ")
                )))

;; 変更したprompt文字列に合う形でpromptの初まりを指定
;; (C-aで"$ "の次にカーソルがくるようにする)
;; これの設定を上手くしとかないとタブ補完も効かなくなるっぽい
(setq eshell-prompt-regexp "^[^#$]*[$#] ")

;; キーバインドの変更
(add-hook
 'eshell-mode-hook
 '(lambda ()
    (progn
      (define-key eshell-mode-map "\C-a" 'eshell-bol)
      (define-key eshell-mode-map "\C-p" 'eshell-previous-matching-input-from-input)
      (define-key eshell-mode-map "\C-n" 'eshell-next-matching-input-from-input)

      ;; ------------------------------------------------------------
      ;;  エイリアスの設定
      ;;    From: Eshell(Emacs Shell) で alias を定義する
      ;;           - ひげぽん OSとか作っちゃうかMona-
      ;;            http://d.hatena.ne.jp/higepon/20070303/1172922429
      ;; ------------------------------------------------------------
      (add-to-list 'eshell-command-aliases-list (list "ll" "ls -l"))
      (add-to-list 'eshell-command-aliases-list (list "la" "ls -la"))
      )
    ))


;; ------------------------------------------------------------
;; カレントバッファのディレクトリに移動しつつeshellを起動する
;;   From: Eshellを使いこなす - Meadow memo
;;           http://www.bookshelf.jp/pukiwiki/pukiwiki.php?Eshell%A4%F2%BB%C8%A4%A4%A4%B3%A4%CA%A4%B9#content_1_34
;; ------------------------------------------------------------
(defun eshell-cd-default-directory ()
  (interactive)
  (let ((dir default-directory))
    (eshell)
    (cd dir)
    (eshell-interactive-print (concat "cd " dir "\n"))
    (eshell-emit-prompt)))

;; ------------------------------------------------------------
;; shell でのコマンドの引数を補完入力することのできるようにする
;;   From: Meadow/Emacs memo: shell を便利に使おう
;；        http://www.bookshelf.jp/soft/meadow_45.html#SEC678
;; ------------------------------------------------------------
(add-hook 'shell-mode-hook 'pcomplete-shell-setup)

