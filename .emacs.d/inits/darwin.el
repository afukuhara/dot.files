
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

