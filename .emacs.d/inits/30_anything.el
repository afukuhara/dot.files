;;; ================================================================ ;;;
;;;               Anything.el Settings                    ;;;
;;; ================================================================ ;;;

;; anything.el
(require 'anything-config)
(setq anything-sources
      (list anything-c-source-buffers
            anything-c-source-bookmarks
            anything-c-source-file-name-history
            anything-c-source-man-pages
            anything-c-source-info-pages
            anything-c-source-calculation-result
            anything-c-source-locate))
(define-key anything-map (kbd "C-v") 'anything-next-source)
(define-key anything-map (kbd "M-v") 'anything-previous-source)

