
;; actionsctipt mode
(autoload 'actionscript-mode "actionscript-mode" "AScript" t)
(setq auto-mode-alist
      (cons
       '("\\.as$" . actionscript-mode) auto-mode-alist))
