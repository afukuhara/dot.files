;;; ================================================================ ;;;
;;;                   Ruby Programming Settings                      ;;;
;;; ================================================================ ;;;

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

    ;; RSense setting
    ;; http://cx4a.org/software/rsense/index.ja.html
    (local-set-key (kbd "C-c .") 'ac-complete-rsense)
    (add-to-list 'ac-sources 'ac-source-rsense-method)
    (add-to-list 'ac-sources 'ac-source-rsense-constant)

    (add-hook 'before-save-hook 'ruby-insert-magic-comment-if-needed)
 ))

(autoload 'rubydb "rubydb3x"
  "run rubydb on program file in buffer *gud-file*.
the directory containing file becomes the initial working directory
and source-file directory for your debugger." t)

;; magic comment
;; Ruby1.9から、ファイルの文字コードを明記する必要がある
;; From: http://d.hatena.ne.jp/rubikitch/20080307/magiccomment
;;       http://coderepos.org/share/browser/dotfiles/emacs/kentaro/.emacs.d/conf/init-ruby.el
;; Changes:
;;   2009-11-12: 既に値が設定済みの場合はマジックコメントの再挿入を行わないように修正
;;               (再挿入を行うと、Undo/Redo 履歴がおかしくなる)
(defun ruby-insert-magic-comment-if-needed ()
  "バッファのcoding-systemをもとにmagic commentをつける。"
  (when (and (eq major-mode 'ruby-mode)
             (find-multibyte-characters (point-min) (point-max) 1))
    (save-excursion
      (goto-char 1)
      (when (looking-at "^#!")
        (forward-line 1))
      (let* ((coding-system (symbol-name buffer-file-coding-system))
             (encoding (cond ((string-match "japanese-iso-8bit\\|euc-j" coding-system)
                              "euc-jp")
                             ((string-match "shift.jis\\|sjis\\|cp932" coding-system)
                              "shift_jis")
                             ((string-match "utf-8" coding-system)
                              "utf-8")))
             (magic-comment (format "# -*- coding: %s -*-\n" encoding)))
        (unless (search-forward magic-comment (1+ (point-at-eol)) t)
          (progn
            (if (re-search-forward "^#.+coding" (point-at-eol) t)
                ; 別の文字コードがマジックコメントに設定されている
                (delete-region (point-at-bol) (point-at-eol)))
            (insert magic-comment)))))))



