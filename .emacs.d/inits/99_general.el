;;; 日本語環境設定
(set-language-environment "Japanese")

;;; font-lockの設定
(global-font-lock-mode t)

;; font-lock extension
(require 'font-lock+)

;;; タブのデフォルト設定
(setq-default tab-width 4)    ; 表示幅
(setq-default indent-tabs-mode nil)    ; タブ->スペース
(setq indent-line-function 'indent-relative-maybe)


;; リージョンをハイライト
(setq-default transient-mark-mode t)

;; windmove
;; ウィンドウを分割した時， Shift+ カーソルキーだけで，
;; 分割したバッファへカーソルを移動できる．
(windmove-default-keybindings)

(require 'auto-save-buffers)
(run-with-idle-timer 2 t 'auto-save-buffers)

;; 最近開いたファイル
(recentf-mode)


;; Develock
(load "develock")

;; 対応する括弧を表示
(show-paren-mode t)

;; wdired 一括リネーム
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)


;; 縦分割時に文字を折り返す
(setq truncate-partial-width-windows nil)

;; 列数表示
(column-number-mode t)

;; BS で選択範囲を消す
(delete-selection-mode t)

;; hide tool-bar
(tool-bar-mode 0)

;; dired を拡張する
(load "dired-x")


(load "sorter")

;; バッファの読み直し
(global-set-key "\C-x\C-r" 'revert-buffer)

;; デフォルトの文字コード
(setq default-buffer-file-coding-system 'utf-8)

;; スタートアップメッセージの禁止
(setq inhibit-startup-message t)

;; reload a file when it was changed by another process
(global-auto-revert-mode t)

(set-scroll-bar-mode 'right)

;; --------------------------------------------------

;; moccur-grep-find
(require 'color-moccur)
(load "moccur-edit")
(global-set-key "\C-cM" 'moccur-grep-find)
(setq dmoccur-exclusion-mask
      (append '("\\.svn\.\*" "\\.git\.\*" "\\.log\$")
              dmoccur-exclusion-mask))


;; filecache
(file-cache-add-directory-list
   (list "~"))
(add-hook 'minibuffer-setup-hook
          '(lambda ()
             (local-set-key "\C-c\C-i" 'file-cache-minibuffer-complete)))

;; 全角空白の表示
(require 'jaspace)
(setq jaspace-highlight-tabs t)  ; highlight tabs
(add-hook 'text-mode-hook
          '(lambda () (jaspace-mode-on)))


;; キルリングの一覧を表示し選択ヤンクできるようにする
(autoload 'kill-summary "kill-summary" nil t)
(define-key global-map "\ey" 'kill-summary)


(require 'company-mode)
(require 'company-bundled-completions)


;; フォルダを開く時, 新しいバッファを作成しない
(defvar my-dired-before-buffer nil)
(defadvice dired-advertised-find-file
  (before kill-dired-buffer activate)
  (setq my-dired-before-buffer (current-buffer)))

(defadvice dired-advertised-find-file
  (after kill-dired-buffer-after activate)
  (if (eq major-mode 'dired-mode)
      (kill-buffer my-dired-before-buffer)))

(defadvice dired-up-directory
  (before kill-up-dired-buffer activate)
  (setq my-dired-before-buffer (current-buffer)))

(defadvice dired-up-directory
  (after kill-up-dired-buffer-after activate)
  (if (eq major-mode 'dired-mode)
      (kill-buffer my-dired-before-buffer)))



;; App::Ack (for source code searching)
(add-hook 'dired-load-hook
          '(lambda ()
             (load-library "ls-lisp")
             (setq ls-lisp-dirs-first t)
             (setq dired-listing-switches "-AFl")
             (setq find-ls-option '("-exec ls -AFGl {} \;" . "-AFGl"))
             (setq grep-find-command "ack --nocolor --nogroup ")
             (require 'wdired)))



;; 同一バッファ名にディレクトリ名を追加する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; 補完候補を自動的に表示
;;; http://dev.ariel-networks.com/Members/matsuyama/auto-complete/
(require 'auto-complete)
(global-auto-complete-mode t)

(require 'anything-config)
(require 'recentf)
(recentf-mode 1)


;;----------------------------------------------------------------------
;; yasnippet - YASnippet is a template system for emacs.
;;  - http://code.google.com/p/yasnippet/
;;----------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/elisp/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/elisp/yasnippet/snippets")



;;; ================================================================ ;;;
;;;               Smartchr Settings                                  ;;;
;;; ================================================================ ;;;

(require 'smartchr)
;; (global-set-key (kbd "=") (smartchr '(" = " " == "  "=")))
(global-set-key (kbd "{") (smartchr '("{ `!!' }" "{")))
(global-set-key (kbd "(") (smartchr '("(`!!')" "((`!!'))" "(")))

;;; ================================================================ ;;;
;;;  日付入力
;;; ================================================================ ;;;
(defun my-get-date-gen (form) (insert (format-time-string form)))
(defun my-get-date () (interactive) (my-get-date-gen "%Y年%m月%d日"))
(defun my-get-time () (interactive) (my-get-date-gen "%H時%M分"))
(defun my-get-dtime () (interactive) (my-get-date-gen "%Y年%m月%d日 %H時%M分"))
(global-set-key "\C-c\C-d" 'my-get-date)
(global-set-key "\C-c\C-t" 'my-get-time)
(global-set-key "\C-c\ed" 'my-get-dtime)



;; -------------------------------------------------------
;; 前回の編集地点にカーソルを移動する
;; Eclipse に合わせて、ALT + 矢印 で移動できるように設定
;; -------------------------------------------------------
(require 'goto-chg)
(global-set-key [M-left] 'goto-last-change)
(global-set-key [M-right] 'goto-last-change-reverse)


;; -------------------------------------------------------

(defun today ()
  (interactive)
  (insert (format-time-string "%y%m%d")))


;; -------------------------------------------------------
;;; 今日の日付のブログテキストを開く
(defun todays-blog ()
  (interactive)
  (find-file (format-time-string "~/blog/%Y/%m%d.txt")))

(global-set-key "\M-rb" 'todays-blog)


;;; ================================================================ ;;;
;;;  zencoding-mode Settings                                         ;;;
;;;    - http://www.emacswiki.org/emacs/ZenCoding                    ;;;
;;; ================================================================ ;;;
(require 'zencoding-mode)
(add-hook 'html-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes


;;; =======================================================================
;;;  今日のアジェンダを開く
;;;  初めてアジェンダファイルを開く場合は、
;;;  直近 (10日前まで) のアジェンダファイルの内容をデフォルト表示する
;;; =======================================================================
(defun todays-agenda ()

  ;; from "doukaku"
  ;; http://ja.doukaku.org/comment/1291/
  (defun n-days-before (n)
    `(lambda (time) (- time (* ,n 3600 24))))

  (defun seconds-to-filename (sec)
    (format-time-string "~/blog/%Y/%m%d.txt"
                        (seconds-to-time sec)))

  (let* ((now (float-time))
         (new-filename (seconds-to-filename now))
         (old-filename (seconds-to-filename now))
         (count 1))
    (find-file new-filename)
    (if (not (file-readable-p old-filename))
        (progn (while (and (not (file-readable-p old-filename))
                           (< count 10))
                 (setf now (funcall (n-days-before 1) now))
                 (setf old-filename (seconds-to-filename now))
                 (setf count (1+ count)))
               (if (and (file-readable-p old-filename)
                        (< count 10))
                   (insert-file-contents  old-filename)
                 (insert "# -*- coding: utf-8; -*-\n#+STARTUP: showall\n"))))))

(defun command-todays-agenda ()
  (interactive)
  (todays-agenda))

(global-set-key "\M-ra" 'command-todays-agenda)



;; -------------------------------------------------------
;;  auto-install.el
;; -------------------------------------------------------
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/elisp/")
(add-to-list 'load-path auto-install-directory)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


;; -------------------------------------------------------
;;  auto-async-byte-compile.el
;; -------------------------------------------------------
(require 'auto-async-byte-compile)
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)


;; emacs 終了時に確認
(setq confirm-kill-emacs 'yes-or-no-p)


;; -------------------------------------------------------
;;  略語展開や補完を行うコマンドをまとめる
;;  From: http://dev.ariel-networks.com/Members/matsuyama/emacs-abbrev
;; -------------------------------------------------------
(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-complete-file-name
        try-expand-dabbrev))


;; -------------------------------------------------------
;;  カーソルの行全体を削除する
;; -------------------------------------------------------
(defun kill-whole-line (&optional numlines)
  "One line is deleted wherever there is a cursor."
  (interactive "p")
  (setq pos (current-column))
  (beginning-of-line)
  (kill-line numlines)
  (move-to-column pos))

(define-key esc-map "k" 'kill-whole-line)


;; -------------------------------------------------------
;;  使い捨てのファイルを開く
;; -------------------------------------------------------
(require 'open-junk-file)
(setq open-junk-file-format "~/junk/%Y-%m-%d-%H%M%S.")


;; -------------------------------------------------------
;; インデントの深さを基準に折り畳み表示する
;;   From: http://d.hatena.ne.jp/kitokitoki/20091220/p1
;; -------------------------------------------------------
(require 'cn-outline)
 ;; どのファイルでもデフォルトで on にする場合
(setq-default cn-outline-mode t)
(global-set-key (kbd "C-c C-c C-c") 'cn-outline-mode)


;; バックアップファイルを一箇所にまとめる
;;  http://blawat2015.no-ip.com/~mieki256/diary/200609073.html
(setq backup-directory-alist
  (cons (cons "\\.*$" (expand-file-name "~/backup"))
    backup-directory-alist))

