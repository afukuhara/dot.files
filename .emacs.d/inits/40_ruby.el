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
 ))

(autoload 'rubydb "rubydb3x"
  "run rubydb on program file in buffer *gud-file*.
the directory containing file becomes the initial working directory
and source-file directory for your debugger." t)

