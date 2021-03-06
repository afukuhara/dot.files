;;; ================================================================ ;;;
;;;  Settings for Subversion
;;; ================================================================ ;;;

;; --- psvn.el ------------------------------------------------------ ;;
(require 'psvn)

(define-key svn-status-mode-map "q" 'egg-self-insert-command)
(define-key svn-status-mode-map "Q" 'svn-status-bury-buffer)
(define-key svn-status-mode-map "p" 'svn-status-previous-line)
(define-key svn-status-mode-map "n" 'svn-status-next-line)
(define-key svn-status-mode-map "<" 'svn-status-examine-parent)

(add-hook 'dired-mode-hook
          '(lambda ()
             (require 'dired-x)
             (define-key dired-mode-map "V" 'svn-status)
             (turn-on-font-lock) ))

(setq svn-status-hide-unmodified t)

(setq process-coding-system-alist
      (cons '("svn" . euc-jp) process-coding-system-alist))

;; VC-SVN
(add-to-list 'vc-handled-backends 'SVN)

