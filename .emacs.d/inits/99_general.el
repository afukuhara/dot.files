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

;; ----------------------------------------
;;  auto-save-buffers.el
;;    バッファの自動保存の設定
;; ----------------------------------------
(require 'auto-save-buffers)
(run-with-idle-timer 3 t 'auto-save-buffers)

(define-key ctl-x-map "as" 'auto-save-buffers-toggle)

;; C-x a w で自動保存時の末尾空白の有無を切り替え
(define-key ctl-x-map "aw" 'auto-save-buffers-trailing-whitespace-toggle)



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

;;====================================
;;; 折り返し表示ON/OFF
;;====================================
(defun toggle-truncate-lines ()
  "折り返し表示をトグル動作します."
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))

(global-set-key "\C-cl" 'toggle-truncate-lines) ; 折り返し表示ON/OFF


;; 列数表示
(column-number-mode t)

;; BS で選択範囲を消す
(delete-selection-mode t)

;; hide tool-bar
(tool-bar-mode 0)

;; dired を拡張する
(load "dired-x")


(load "sorter")

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
(setq open-junk-file-format "~/junk/%Y/%m-%d-%H%M%S.")


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

;; -------------------------------------------------------
;; 矩形選択
;;   From: http://tech.kayac.com/archive/emacs-rectangle.html
;; -------------------------------------------------------
(require 'sense-region)
(sense-region-on)

(setq cua-enable-cua-keys nil) ; そのままだと C-x が切り取りになってしまったりするので無効化
(cua-mode t
)

; カーソル位置の単語をバッファ内から探す設定例
(global-set-key "\M-o" (lambda () (interactive)
                         (if (thing-at-point 'symbol)
                             (occur (thing-at-point 'symbol))
                           (call-interactively 'occur))))


;; Emacs の compilation-mode で ansi color が化けてしまうことへの対処
;; from: http://www.moreslowly.jp/mw/index.php?title=Emacs_%E3%81%AE_compilation-mode_%E3%81%A7_ansi_color_%E3%81%8C%E5%8C%96%E3%81%91%E3%81%A6%E3%81%97%E3%81%BE%E3%81%86%E3%81%93%E3%81%A8%E3%81%B8%E3%81%AE%E5%AF%BE%E5%87%A6
;; (add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)
;; (add-hook 'compilation-filter-hook
;;           '(lambda ()
;;              (let ((start-marker (make-marker))
;;                    (end-marker (process-mark (get-buffer-process (current-buffer)))))
;;                (set-marker start-marker (point-min))
;;                (ansi-color-apply-on-region start-marker end-marker))))


;;
;; C-a と C-e を拡張する
;; From: 空気のようなEmacs Lisp -2010 冬- - Emacs/Lisp/Drill - Emacsグループ
;;       <http://emacs.g.hatena.ne.jp/k1LoW/20101211/1292046538>
;;
(require 'sequential-command)

(define-sequential-command seq-home
  back-to-indentation  beginning-of-line beginning-of-buffer seq-return)
(global-set-key "\C-a" 'seq-home)

(define-sequential-command seq-end
  end-of-line end-of-buffer seq-return)
(global-set-key "\C-e" 'seq-end)


;;
;; popwin.el
;; From: ヘルプバッファや補完バッファをポップアップで表示してくれるpopwin.el
;;        <http://d.hatena.ne.jp/m2ym/20110120/1295524932>
;;
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(push '("*auto-async-byte-compile*" :height 20) popwin:special-display-config)
(push '(dired-mode :position top) popwin:special-display-config)

;; カーソル付近のファイル/URL を開く
(ffap-bindings)


;; ===== undo 関連 [start] =====

;;
;; バッファを閉じてもアンドゥ情報を保持
;;
(require 'undohist)
(undohist-initialize)

;;
;; undo の履歴を木構造としてもって、それを辿る事ができる elisp
;; From: undo-tree.el の導入 - とりあえず暇だったし何となく始めたブログ
;;       <http://d.hatena.ne.jp/khiker/20100123/undo_tree>
;;
(require 'undo-tree)
(global-undo-tree-mode)

;; =============================



