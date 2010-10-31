;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNU GLOBAL(gtags)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-gt" 'gtags-find-tag)
         (local-set-key "\M-gr" 'gtags-find-rtag)
         (local-set-key "\M-gs" 'gtags-find-symbol)
         (local-set-key "\M-gp" 'gtags-pop-stack)))
