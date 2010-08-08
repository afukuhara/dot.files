
;;; ================================================================ ;;;
;;;               ESS (R-mode) Programming Settings                    ;;;
;;; ================================================================ ;;;

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
