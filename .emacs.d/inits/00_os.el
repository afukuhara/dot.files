
;; ---------------------------------
;; ------ Setting for each OS ------
;; ---------------------------------
(cond ((featurep 'meadow)                ; for Win32 (Meadow)
       (load "init/meadow"))
      ((string= system-type "gnu/linux") ; for Linux (GNU Emacs)
       (load "init/gnu-emacs"))
      ((string= system-type "darwin") ; for Mac
       (load "darwin

;; 先頭に '/usr/local/bin' を追加
(setq exec-path (append '("/usr/local/bin") exec-path))
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

;;; コマンド、コントロール、オプションの各キーを
;;; meta, control, super
;;; の各キーとして設定
(setq mac-command-modifier 'meta)
(setq mac-control-modifier 'control)
(setq mac-option-modifier 'super)

;; option + yen (jis keyboard) => backslash
(setq ns-alternate-modifier 'option)

;; redefine set-mark
(global-unset-key [?\C- ])
(global-set-key [?\C- ] 'set-mark-command)
(global-unset-key [?\C-@])
(global-set-key [?\C-@] 'set-mark-command)


;; initial value of frame
(setq default-frame-alist
      (append (list '(width . 100)
                    '(height . 88)
                    '(top . 25)
                    '(left . 10)
            '(background-color . "snow"))
              default-frame-alist))


;; for carbon emacs
(when (< emacs-major-version 23)
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

  (create-fontset-from-ascii-font "Menlo-14:weight=normal:slant=normal" nil "menlokakugo")
  (set-fontset-font "fontset-menlokakugo"
                    'unicode
                    (font-spec :family "Hiragino Kaku Gothic ProN" :size 16)
                    nil
                    'append)
  (add-to-list 'default-frame-alist '(font . "fontset-menlokakugo")))


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



;; ;;; erlang-mode
;; (setq load-path
;;       (cons "/opt/local/lib/erlang/lib/tools-2.5.2/emacs"
;;             load-path))
;; (setq erlang-root-dir "/opt/local/lib/erlang/")
;; (setq exec-path (cons "/opt/local/lib/erlang/bin/" exec-path))
;; (require 'erlang-start)
;; 先頭に '/usr/local/bin' を追加
(setq exec-path (append '("/usr/local/bin") exec-path))
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

;;; コマンド、コントロール、オプションの各キーを
;;; meta, control, super
;;; の各キーとして設定
(setq mac-command-modifier 'meta)
(setq mac-control-modifier 'control)
(setq mac-option-modifier 'super)

;; option + yen (jis keyboard) => backslash
(setq ns-alternate-modifier 'option)

;; redefine set-mark
(global-unset-key [?\C- ])
(global-set-key [?\C- ] 'set-mark-command)
(global-unset-key [?\C-@])
(global-set-key [?\C-@] 'set-mark-command)


;; initial value of frame
(setq default-frame-alist
      (append (list '(width . 100)
                    '(height . 88)
                    '(top . 25)
                    '(left . 10)
            '(background-color . "snow"))
              default-frame-alist))


;; for carbon emacs
(when (< emacs-major-version 23)
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

  (create-fontset-from-ascii-font "Menlo-14:weight=normal:slant=normal" nil "menlokakugo")
  (set-fontset-font "fontset-menlokakugo"
                    'unicode
                    (font-spec :family "Hiragino Kaku Gothic ProN" :size 16)
                    nil
                    'append)
  (add-to-list 'default-frame-alist '(font . "fontset-menlokakugo")))


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



;; ;;; erlang-mode
;; (setq load-path
;;       (cons "/opt/local/lib/erlang/lib/tools-2.5.2/emacs"
;;             load-path))
;; (setq erlang-root-dir "/opt/local/lib/erlang/")
;; (setq exec-path (cons "/opt/local/lib/erlang/bin/" exec-path))
;; (require 'erlang-start)
")))

;; --- Setting for each OS [END] ---
