;;; ================================================================ ;;;
;;;               JavaScript Programming Settings                    ;;;
;;; ================================================================ ;;;

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq js2-basic-offset 4)
             (setq js2-cleanup-whitespace nil)
             (setq align-c++-modes (cons 'js2-mode align-c++-modes))
             (setq js2-mirror-mode nil)
             (setq js2-electric-keys '())))
