;;;; -*- mode: emacs-lisp; coding: iso-2022-7bit -*-

;; add load-path
(setq elisp-dir "~/.emacs.d/elisp/")
(setq load-path
      (append
       (list (expand-file-name elisp-dir)) load-path))

;;; 日本語環境設定
(set-language-environment "Japanese")


;; ---------------------------------
;; ------ Setting for each OS ------
;; ---------------------------------
(setq load-path (cons (concat elisp-dir "init") load-path))
(cond ((featurep 'meadow)                ; for Win32 (Meadow)
       (load "init/meadow"))
      ((string= system-type "gnu/linux") ; for Linux (GNU Emacs)
       (load "init/gnu-emacs"))
      ((string= system-type "darwin") ; for Mac
       (load "init/carbon-emacs")))

;; --- Setting for each OS [END] ---


;;; font-lockの設定
(global-font-lock-mode t)

;;; タブのデフォルト設定
(setq-default tab-width 4)    ; 表示幅
(setq-default indent-tabs-mode nil)    ; タブ->スペース
(setq indent-line-function 'indent-relative-maybe)

;; use \C-h as backspace
(keyboard-translate ?\C-h ?\C-?)

;; use \C-? as help
(global-set-key [\C-?] 'help)

;; font-lock extension
(require 'font-lock+)

(global-set-key [?\C--] (lambda () (interactive) (insert "->"))) ; powered by hiboma
(global-set-key [?\M--] (lambda () (interactive) (insert "=>")))

;; リージョンをハイライト
(setq-default transient-mark-mode t)


;;; ================================================================ ;;;
;;;                   Perl Programming Settings                      ;;;
;;;  From: http://www.hasta-pronto.org/archives/2007/05/30-2252.php  ;;;
;;; ================================================================ ;;;

(autoload 'cperl-mode "cperl-mode"
  "alternate mode for editing Perl programs" t)
(defalias 'perl-mode 'cperl-mode)
(setq cperl-indent-parens-as-block t)
(setq cperl-close-paren-offset -4)
(setq cperl-indent-level 4)
(setq cperl-label-offset -4)
(setq cperl-continued-statement-offset 4)
(setq cperl-highlight-variables-indiscriminately t)
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(setq auto-mode-alist (cons '("\\.\\(p[lm]\\|f?cgi\\|t\\)$" . cperl-mode) auto-mode-alist))
(add-hook
 'cperl-mode-hook
 (lambda ()
   (require 'perlplus)
   (local-set-key "\C-cp"     'cperl-check-syntax) ; Syntax Check
   (local-set-key "\C-ct"     'perltidy-region)    ; perltidy
   (local-set-key "\C-c\C-t"  'perltidy-defun)
   (local-set-key "\C-c\C-ct" 'perltidy-buffer)
   (local-set-key "\M-p"      'cperl-perldoc) ; perldoc
   (local-set-key "\C-c\C-m"  'cperl-view-module-src-at-point) ; perldoc -m
   (local-set-key "\C-c\C-cc" 'perl-eval)   ; リージョンを実行
   (local-set-key "\C-c\C-cp" 'cperl-prove) ; テストを実行
   (local-set-key "\C-c\C-cs" 'cperl-start)
   (local-set-key "\C-c\C-l"  'cperl-local-newline)
   (local-set-key "\C-c\C-cd" 'cperl-use-data-dumper)
   (local-set-key "\C-c\C-cy" 'cperl-use-yaml-dump)
   (local-set-key "\C-c\C-w"  'cperl-warn-dumper)
   (local-set-key "\C-c\C-y"  'cperl-warn-yaml)
   (local-set-key "\C-c\C-ch" 'cperl-pod4method)
   (local-set-key "\C-c\C-cf" 'cperl-use-feature)

   (define-key cperl-mode-map "\M-," 'perlplus-complete-symbol) ; 関数&シンボル補完
   (perlplus-setup)
   (require 'perl-debug)
   (perl-debug-set-coding-system)
   (define-key cperl-mode-map "\C-cd"    'perl-debug-lint)
   (define-key cperl-mode-map "\C-c\C-d" 'perl-debug) ; デバッガの起動
   ;; syntax color
   (set-face-bold-p   'cperl-array-face nil)
   (set-face-bold-p   'cperl-hash-face nil)
   (set-face-italic-p 'cperl-hash-face nil)

   ;; snipets
   (defun cperl-start ()
     (interactive)
     (insert "use strict\;\nuse warnings\;\n"))
   (defun cperl-local-newline ()
     (interactive)
     (insert "local $\\ \= \"\\n\";\n"))
   (defun cperl-use-data-dumper ()
     (interactive)
     (insert "use Data::Dumper;\n"))
   (defun cperl-use-yaml-dump ()
     (interactive)
     (insert "use YAML::Syck;\n"))
   (defun cperl-warn-dumper ()
     (interactive)
     (insert "warn Dumper "))
   (defun cperl-warn-yaml ()
     (interactive)
     (insert "warn Dump "))
   (defun cperl-pod4method ()
     (interactive)
     (insert "  Description\: \n  Args       \:\n  Return     \:\n"))
   (defun cperl-use-feature ()
     (interactive)
     (insert "use feature \":5.10\"\;"))
   ))

;; ;;; Perl Debug の設定
(autoload 'perl-debug "perl-debug" nil t)
(autoload 'perl-debug-lint "perl-debug" nil t)

;;; perltidy-region powered by typester
(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)))
(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
                  (perltidy-region)))
(defun perltidy-buffer ()
  "Run perltidy on the current buffer."
  (interactive)
  (save-excursion (mark-whole-buffer)
                  (perltidy-region)))

;;; perldoc -m
(defun cperl-view-module-src-at-point (module)
  "Visit perl module's source file"
  (interactive
   (list (let* ((default-entry (or (cperl-word-at-point) ""))
                (input (read-string
                        (format "View perl module's source%s: "
                                (if (string= default-entry "")""
                                  (format " (default %s)" default-entry))))))
           (if (string= input "")
               (if (string= default-entry "")
                   (error "No Perl module given")default-entry)input))))
  (let ((file (substring (shell-command-to-string(concat "perldoc -m " module))0 -1)))
    (if (string-match "No module found for" file)
        (error file)
      (view-file-other-window file))))

;;; テストを実行 by PBP
(defun cperl-prove ()
  "Run the current test."
  (interactive)
  (save-excursion
    (shell-command (concat "prove -vl "
                           (shell-quote-argument (buffer-file-name))))))

;;; リージョン内のコードを実行する by PBP
(defun perl-eval (beg end)
  "Run selected region as Perl code"
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "perl")))


;;; ================================================================ ;;;
;;;                   flymake for Perl Programming                   ;;;
;;; from: http://d.hatena.ne.jp/antipop/20080701/1214838633          ;;;
;;; ================================================================ ;;;

;; flymake (Emacs22から標準添付されている)
(require 'flymake)

;; set-perl5lib
;; 開いたスクリプトのパスに応じて、@INCにlibを追加してくれる
;; 以下からダウンロードする必要あり
;; http://svn.coderepos.org/share/lang/elisp/set-perl5lib/set-perl5lib.el
(require 'set-perl5lib)

;; エラー、ウォーニング時のフェイス
(set-face-background 'flymake-errline "pink")
(set-face-foreground 'flymake-errline "black")
(set-face-background 'flymake-warnline "yellow")
(set-face-foreground 'flymake-warnline "black")

;; エラーをミニバッファに表示
;; http://d.hatena.ne.jp/xcezx/20080314/1205475020
(defun flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info
                                      flymake-err-info line-no)))
         (count               (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file
                            (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file
                            (nth (1- count) line-err-info-list)))
               (text       (flymake-ler-text
                            (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line
                            (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)))
      (setq count (1- count)))))

;; Perl用設定
;; http://unknownplace.org/memo/2007/12/21#e001
(defvar flymake-perl-err-line-patterns
  '(("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)))

(defconst flymake-allowed-perl-file-name-masks
  '(("\\.pl$" flymake-perl-init)
    ("\\.pm$" flymake-perl-init)
    ("\\.t$" flymake-perl-init)))

(defun flymake-perl-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "perl" (list "-wc" local-file))))

(defun flymake-perl-load ()
  (interactive)
  (defadvice flymake-post-syntax-check
    (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (setq flymake-allowed-file-name-masks
        (append flymake-allowed-file-name-masks
                flymake-allowed-perl-file-name-masks))
  (setq flymake-err-line-patterns flymake-perl-err-line-patterns)
  (set-perl5lib)
  (flymake-mode t))

(add-hook 'cperl-mode-hook 'flymake-perl-load)

;;;                 flymake for Perl Programming [END]                ;;;

;;;                 Perl Programming Settings [END]                   ;;;


;; mode-compile
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)
(global-set-key "\C-cn" 'next-error) ;; エラー箇所に飛ぶ
;; 質問してこないように
(setq mode-compile-always-save-buffer-p t)
(setq mode-compile-never-edit-command-p t)
(setq mode-compile-expert-p t)
(setq mode-compile-reading-time 0)
;; コンパイルウィンドウサイズ
(setq compilation-window-height 15)


;;; ruby-mode
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append '(("Rakefile" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
                                     interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook
 'ruby-mode-hook
 '(lambda ()
    (inf-ruby-keys)
    ;; Rails.el
    (defun try-complete-abbrev (old)
      (if (expand-abbrev) t nil))
    (setq hippie-expand-try-functions-list
          '(try-complete-abbrev
            try-complete-file-name
            try-expand-dabbrev))
    (setq rails-use-mongrel t)
    (setq load-path (cons (concat elisp-dir "rails") load-path))
    (require 'rails)
    (define-key rails-minor-mode-map "\C-c\C-p" 'rails-lib:run-primary-switch)
    (define-key rails-minor-mode-map "\C-c\C-n" 'rails-lib:run-secondary-switch)

    (local-set-key "\C-c\C-e" 'ruby-insert-comment)
    (defun ruby-insert-comment ()
      (interactive)
      (ruby-indent-command)
      (insert "\#\n")
      (ruby-indent-command)
      (insert "\#\n")
      (ruby-indent-command)
      (insert "\#"))

    ;; ruby-electric.el --- electric editing commands for ruby files
    (require 'ruby-electric)
    (ruby-electric-mode t)
    (setq ruby-electric-expand-delimiters-list nil)
    ))

(autoload 'rubydb "rubydb3x"
  "run rubydb on program file in buffer *gud-file*.
the directory containing file becomes the initial working directory
and source-file directory for your debugger." t)


;; windmove
;; ウィンドウを分割した時， Shift+ カーソルキーだけで，
;; 分割したバッファへカーソルを移動できる．
(windmove-default-keybindings)

(require 'auto-save-buffers)
(run-with-idle-timer 1 t 'auto-save-buffers)

;; 最近開いたファイル
(recentf-mode)


;; css-mode
(autoload 'css-mode "css-mode")
(setq auto-mode-alist
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))
(setq cssm-indent-function #'cssm-c-style-indenter)

;; Develock
(load "develock")

;; YAML-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; 対応する括弧を表示
(show-paren-mode t)

;; wdired 一括リネーム
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; moccur-grep-find
(require 'color-moccur)
(load "moccur-edit")
(global-set-key "\C-cM" 'moccur-grep-find)
(setq dmoccur-exclusion-mask
      (append '("\\.svn\.\*" "\\.git\.\*" "\\.log\$")
              dmoccur-exclusion-mask))

;;; RD-mode
;(global-font-lock-mode 1 t)
;(autoload 'rd-mode "rd-mode" "major mode for ruby document formatter RD" t)
;(add-to-list 'auto-mode-alist '("\\.rd$" . rd-mode))


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

;; 縦分割時に文字を折り返す
(setq truncate-partial-width-windows nil)

;; 列数表示
(column-number-mode t)

;; BS で選択範囲を消す
(delete-selection-mode t)

;; キルリングの一覧を表示し選択ヤンクできるようにする
(autoload 'kill-summary "kill-summary" nil t)
(define-key global-map "\ey" 'kill-summary)



;; (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
;; ;;(autoload 'javascript-mode "javascript" nil t)
;; ;; (setq javascript-indent-level 4)
;; (autoload 'javascript-mode "javascript" "javascript-mode" t)
;; (add-hook 'javascript-mode-hook
;;           '(lambda ()
;;              (setq tab-width 4)
;;              (setq js-indent-level 4)
;;              (setq javascript-basic-offset tab-width)))

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq js2-basic-offset 4)
             (setq js2-cleanup-whitespace nil)
             (setq align-c++-modes (cons 'js2-mode align-c++-modes))
             (setq js2-mirror-mode nil)
             (setq js2-electric-keys '())))


;; hide tool-bar
(tool-bar-mode 0)

(global-set-key "\C-\\" 'redo)
(require 'redo)
(global-set-key "\C-_" 'undo)



;;-----------------------------------------------------------------
;; psvn.el
;;-----------------------------------------------------------------
(require 'psvn)

(define-key svn-status-mode-map "q" 'egg-self-insert-command)
(define-key svn-status-mode-map "Q" 'svn-status-bury-buffer)
(define-key svn-status-mode-map "p" 'svn-status-previous-line)
(define-key svn-status-mode-map "n" 'svn-status-next-line)
(define-key svn-status-mode-map "<" 'svn-status-examine-parent)

(add-hook 'dired-mode-hook
          '(lambda ()
             (require 'dired-x)
             ;;(define-key dired-mode-map "V" 'cvs-examine)
             (define-key dired-mode-map "V" 'svn-status)
             (turn-on-font-lock) ))

(setq svn-status-hide-unmodified t)

(setq process-coding-system-alist
      (cons '("svn" . euc-jp) process-coding-system-alist))

;; VC-SVN
(add-to-list 'vc-handled-backends 'SVN)


(require 'company-mode)
(require 'company-bundled-completions)


;; dired を拡張する
(load "dired-x")

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

(load "sorter")

;; バッファの読み直し
(global-set-key "\C-x\C-r" 'revert-buffer)

;; デフォルトの文字コード
(setq default-buffer-file-coding-system 'utf-8)

;; ESS (R-mode)
;; (setq load-path (cons "~/site-lisp/ess" load-path))
;; (require 'ess-site)
;; (add-hook 'ess-mode-hook
;;           '(lambda ()
;;              (jaspace-mode-on)
;;              (local-set-key "\C-cc" 'ess-eval-buffer-and-go)))

;; anything.el
(require 'anything-config)
(setq anything-sources
      (list anything-c-source-buffers
            anything-c-source-bookmarks
            anything-c-source-file-name-history
            anything-c-source-man-pages
            anything-c-source-info-pages
            anything-c-source-calculation-result
            anything-c-source-locate))
(define-key anything-map (kbd "C-v") 'anything-next-source)
(define-key anything-map (kbd "M-v") 'anything-previous-source)
(global-set-key "\C-x\C-b" 'anything)

;; C言語
(add-hook 'c-mode-hook
          '(lambda ()
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


;; スタートアップメッセージの禁止
(setq inhibit-startup-message t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNU GLOBAL(gtags)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbol)
         (local-set-key "\C-t" 'gtags-pop-stack)))


;; App::Ack (for source code searching)
(add-hook 'dired-load-hook
          '(lambda ()
             (load-library "ls-lisp")
             (setq ls-lisp-dirs-first t)
             (setq dired-listing-switches "-AFl")
             (setq find-ls-option '("-exec ls -AFGl {} \;" . "-AFGl"))
             (setq grep-find-command "ack --nocolor --nogroup ")
             (require 'wdired)))

;; reload a file when it was changed by another process
(global-auto-revert-mode t)

;; align
(global-set-key [?\C-;] 'align)

(set-scroll-bar-mode 'right)

;; git
(load "git.el" t)
(load "git-blame.el" t)
(load "vc-git.el" t)
(add-to-list 'vc-handled-backends 'GIT)


;; (require 'auto-complete)
;; (global-auto-complete-mode t)


;;----------------------------------------------------------------------
;; mmm-mode
;;  - http://sourceforge.net/projects/mmm-mode/
;;  - http://www.bookshelf.jp/soft/meadow_13.html#SEC100
;;----------------------------------------------------------------------
(setq load-path (cons (concat elisp-dir "mmm-mode-0.4.8") load-path))

(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
;; 色設定．これは，好みで．色をつけたくないなら nil にします．
(set-face-background 'mmm-default-submode-face "alice blue")


(mmm-add-group
 'html-js
 '((js-tag
    :submode js2-mode
    :face mmm-code-submode-face
    :delimiter-mode nil
    :front "<script\[^>\]*\\(language=\"javascript\\([0-9.]*\\)\"\\|type=\"text/javascript\"\\)\[^>\]*>"
    :back"</script>"
    :insert ((?j js-tag nil @ "<script language=\"JavaScript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-inline
    :submode js2-mode
    :face mmm-code-submode-face
    :delimiter-mode nil
    :front "on\\w+=\""
    :back "\"")))
(mmm-add-mode-ext-class nil "\\.html?\\'" 'html-js)

(mmm-add-classes
 '((embedded-css
    :submode css
    :face mmm-declaration-submode-face
    :delimiter-mode nil
    :front "<style[^>]*>"
    :back "</style>")))
