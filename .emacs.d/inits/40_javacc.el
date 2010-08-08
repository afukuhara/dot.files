;;----------------------------------------------------------------------
;; javacc-mode
;;  - http://sourceforge.net/projects/mmm-mode/
;;  - http://www.bookshelf.jp/soft/meadow_13.html#SEC100
;;----------------------------------------------------------------------
(require 'javacc-mode)
(autoload 'javacc-mode "javacc-mode")
(setq auto-mode-alist
      (append '(("\\.jj$" . javacc-mode)) auto-mode-alist))

;;---- javacc-mode [END] -----------------------------------------------

