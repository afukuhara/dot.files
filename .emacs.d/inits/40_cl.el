;;; ================================================================ ;;;
;;;               Common Lisp Programming Settings                   ;;;
;;; ================================================================ ;;;

(setq inferior-lisp-program "clisp")
;; (add-to-list 'load-path "~/.emacs.d/elisp/slime")
(require 'slime)
(slime-setup)

;; Slime の自動終了
;; From: http://d.hatena.ne.jp/suu-g/20070911
(defun slime-kill-all-buffers ()
  "Kill all the slime related buffers. This is only used by the
  repl command sayoonara."
  (dolist (buf (buffer-list))
    (when (or (string= (buffer-name buf) slime-event-buffer-name)
              (string-match "^\\*inferior-lisp*" (buffer-name buf))
              (string-match
                  "^\\*slime-repl\\[[0-9]+\\]\\*$" (buffer-name buf))
              (string-match "^\\*sldb .*\\*$" (buffer-name buf)))
      (kill-buffer buf))))

(defun slime-smart-quit ()
  (interactive)
  (when (slime-connected-p)
    (if (equal (slime-machine-instance) "my.workstation")
      (slime-quit-lisp)
      (slime-disconnect)))
  (slime-kill-all-buffers))

(add-hook 'kill-emacs-hook 'slime-smart-quit)


