;;; ================================================================ ;;;
;;;                   Scala Programming Settings                     ;;;
;;; ================================================================ ;;;

;; ================================================================ ;;;
;;  From: http://blog.iss.ms/2012/06/02/101357  ;;;
;; ================================================================ ;;;
(defun my-scala-newline(arg)
  (interactive "p")
  (cond ((scala-in-multi-line-comment-p)
         (scala-newline))
        ((char-equal ?\} (following-char))
         (let (killed)
           (newline-and-indent)
           (newline-and-indent)
           (forward-char)
           (setq killed (not (my-end-of-line-p)))
           (if killed (kill-line))
           (previous-line)
           (indent-for-tab-command)
           (if killed (yank))))
        (t
         (newline-and-indent))))

(require 'scala-mode-feature-electric)

(setq scala-mode-feature:electric-expand-delimiters-list '(?\{))


;; ================================================================ ;;;
;;  From: http://blog.iss.ms/2012/06/02/101357  ;;;
;; ================================================================ ;;;
(require 'scala-mode-auto)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(add-hook 'scala-mode-hook
  (function
    (lambda ()
      (setq scala-mode-indent:step 4)
      (scala-mode-lib:define-keys scala-mode-map
                                  ([(shift tab)]   'scala-undent-line)
                                  ([(control tab)] nil))
      (local-set-key [(return)] 'newline-and-indent)
      (scala-mode-feature-electric-mode)
      (define-key scala-mode-map "\r" 'my-scala-newline))))
(add-hook 'scala-mode-hook 'jaspace-mode)

