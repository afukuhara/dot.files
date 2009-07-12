
;; 先頭に '/usr/local/bin' を追加
(setq exec-path (append '("/usr/local/bin") exec-path))
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

;; initial value of frame
(setq default-frame-alist
      (append (list '(width . 100)
                    '(height . 88)
                    '(top . 25)
                    '(left . 10)
            '(background-color . "snow"))
              default-frame-alist))

(set-face-attribute 'default nil
                    :family "monaco"
                    :height 105)
(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  '("ヒラギノ丸ゴ pro w4*" . "jisx0208.*"))
(set-fontset-font "fontset-default"
                  'katakana-jisx0201
                  '("ヒラギノ丸ゴ pro w4*" . "jisx0201.*"))
(add-to-list 'face-font-rescale-alist
             `(,(encode-coding-string ".*ヒラギノ丸ゴ pro w4.*" 'emacs-mule) . 1.2))


(setq scheme-program-name "/usr/local/bin/scm")
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
(setq cmuscheme-load-hook
      '((lambda () (define-key inferior-scheme-mode-map "\C-c\C-t"
                     'favorite-cmd))))

;; scheme-mode(gauche)
(setq scheme-program-name "gosh")
(require 'cmuscheme)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(add-hook 'scheme-mode-hook
      (lambda ()
        (local-set-key "\C-cS" 'scheme-other-window)
        (local-set-key [?\C-\;] (lambda () (interactive)(insert ";=> ")))))

;; gca.el
;; http://practical-scheme.net/wiliki/wiliki.cgi?GaucheFest%3akoguro
(require 'gca)

;; C-c C-uで (use module) をインサートする
(define-key scheme-mode-map "\C-c\C-u" 'gca-insert-use)

(let ((m (make-sparse-keymap)))
  ;; C-c C-d h でドキュメントを検索する
  (define-key m "h" 'gca-show-info)
  ;; C-c C-d i でauto-info-modeの切り替えを行う。(ONの場合自動的にinfoを表示します)
  (define-key m "i" 'auto-info-mode)
  (define-key scheme-mode-map "\C-c\C-d" m))

;; C-c C-,でinfoの次のトピックを表示します(検索結果が複数あった場合)
(define-key scheme-mode-map [(control c) (control ,)] 'gca-info-next)

;; C-. でシンボルを補完する
(define-key scheme-mode-map [(control .)] 'gca-completion-current-word)

;; C-c C-. でコードのひな形をインサートする
(define-key scheme-mode-map [(control c) (control .)] 'gca-insert-template)
(define-key c-mode-map [(control c) (control .)] 'gca-insert-template)

;; C-c t でテストケースをインサートする (run-schemeでtgosh.scmを使う必要があります)
;; C-u で引数を与えると、その番号で実行された結果をもとにしてテストケースを作成します。
;; 省略時は直前の実行結果が使われます。
(define-key scheme-mode-map "\C-ct" 'gca-make-test)

;; C-c h で履歴をみる (run-schemeでtgosh.scmを使う必要があります)
(define-key scheme-mode-map "\C-ch" 'gca-show-history)

;; 単体テストケース作成の支援(gca-make-test, gca-show-history)を使うには
(setq scheme-program-name "gosh ~/.emacs.d/elisp/tgosh.scm")


;; common lisp
(setq inferior-lisp-program "clisp")


;; actionsctipt mode
(autoload 'actionscript-mode "actionscript-mode" "AScript" t)
(setq auto-mode-alist
      (cons
       '("\\.as$" . actionscript-mode) auto-mode-alist))

;;; tt
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist
      (cons
       '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons
       '("\\.tt2?$" . html-helper-mode) auto-mode-alist))
(require 'html-tt)
(add-hook 'html-helper-mode-hook 'html-tt-load-hook)

;; Setting values:
;; change sequence face
(make-face 'my-sequence-face)
(set-face-foreground 'my-sequence-face "blue")
(set-face-background 'my-sequence-face "bisque")
(setq html-tt-sequence-face 'my-sequence-face)

;; change sequence for insert
(setq html-tt-sequence-start "[% ")
(setq html-tt-sequence-end " %]")


;;; haskell-mode
(setq load-path (cons "~/.emacs.d/elisp/haskell" load-path))
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.[hg]s$"  . haskell-mode)
                ("\\.hi$"     . haskell-mode)
                ("\\.l[hg]s$" . literate-haskell-mode))))
(autoload 'haskell-mode "haskell-mode"
  "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
   "Major mode for editing literate Haskell scripts." t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)

(setq haskell-literate-default 'latex)
(setq haskell-doc-idle-delay 0)

;; ;;; erlang-mode
;; (setq load-path 
;;       (cons "/opt/local/lib/erlang/lib/tools-2.5.2/emacs"
;;             load-path))
;; (setq erlang-root-dir "/opt/local/lib/erlang/")
;; (setq exec-path (cons "/opt/local/lib/erlang/bin/" exec-path))
;; (require 'erlang-start)

;; rails-mode
;;;(defun try-complete-abbrev (old)
;;;  (if (expand-abbrev) t nil))

(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-complete-file-name
        try-expand-dabbrev))
(setq rails-use-mongrel t)
(require 'rails)


;; redefine set-mark
(global-unset-key [?\C- ])
(global-set-key [?\C- ] 'set-mark-command)
(global-unset-key [?\C-@])
(global-set-key [?\C-@] 'set-mark-command)
(global-set-key [?\C-:] 'set-mark-command)


(global-set-key "\C-c\C-cF" 'mac-toggle-max-window)
;; (setq default-frame-alist
;;       (append (list
;;                '(height . 63))))

;; ESS (R-mode)
(setq load-path (cons "~/.emacs.d/elisp/ess" load-path))
(require 'ess-site)
(add-hook 'ess-mode-hook
          '(lambda ()
             (defun ess-eval-buffer-only ()
               (ess-eval-buffer-and-go)
               (other-window 1))
             (defun ess-eval-function-or-paragraph-and-step-and-go (vis)
               "Send the current buffer to the inferior S and switch to the process buffer.
Arg has same meaning as for `ess-eval-region'."
               (interactive "P")
               (ess-eval-function-or-paragraph-and-step vis)
               (ess-switch-to-ESS t))
             (local-set-key "\C-cc"    'ess-eval-buffer-and-go) ; Eval and go buffer
             (local-set-key "\C-c\C-c" 'ess-eval-function-or-paragraph-and-step-and-go)
             ))


;; emacs installer
(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/elisp/")

;; assembler
(add-hook 'asm-mode-hook
          '(lambda ()
             (setq asm-comment-char ?\#)))

;; ;; widen-window
;; (require 'widen-window)
;; (global-widen-window-mode 'nil)

;(setq auto-mode-alist (cons '("\\.ya?ml$" . yaml-mode) auto-mode-alist))
