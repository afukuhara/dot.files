;;; ================================================================ ;;;
;;;                   Scala Programming Settings                     ;;;
;;; ================================================================ ;;;

;; ------------------------------------------------------------ ;;;
;;  From: https://github.com/hvesalai/scala-mode2
;; ------------------------------------------------------------ ;;;
;; (require 'scala-mode2)

(autoload 'scala-mode "scala-mode2" nil t)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))


(add-hook
 'scala-mode-hook
 (lambda ()
   (local-set-key "\C-cc" 'quickrun-with-arg)

   (setq quickrun-timeout-seconds 30)))
