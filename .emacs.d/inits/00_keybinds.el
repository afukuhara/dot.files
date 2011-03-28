;; use \C-h as backspace
(keyboard-translate ?\C-h ?\C-?)

;; use \C-? as help
(global-set-key [\C-?] 'help)

(global-set-key [?\C--] (lambda () (interactive) (insert "->"))) ; powered by hiboma
(global-set-key [?\M--] (lambda () (interactive) (insert "=>")))


(global-set-key "\C-_" 'undo)

;;
;; redo+
;;   ref:「Emacsテクニックバイブル」 p.123
;;
(require 'redo+)
(global-set-key (kbd "C-M-/") 'redo)
(setq undo-no-redo t) ; 過去のundoがredoされないようにする
;; 大量のundoに耐えられるようにする
(setq undo-limit 600000)
(setq undo-strong-limit 900000)

(global-set-key [?\C-\:] 'anything)

(global-set-key "\C-x\C-b" 'recentf-open-files)

(global-set-key "\C-cs" 'open-junk-file)

(global-set-key [f6] 'today)

;; align
(global-set-key [?\C-;] 'align)

;; \C-z で最小化しない
(global-unset-key "\C-z")



;; バッファの読み直し
(global-set-key "\C-x\C-r" 'revert-buffer)
