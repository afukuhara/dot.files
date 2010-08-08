
;; mode-compile
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)
(global-set-key "\C-cn" 'next-error) ;; エラー箇所に飛ぶ
;; 質問してこないように
(setq mode-compile-always-save-buffer-p t)
(setq mode-compile-never-edit-command-p t)
(setq mode-compile-expert-p t)
(setq mode-compile-reading-time 0)
;; コンパイルウィンドウサイズ
(setq compilation-window-height 30)

