;; use \C-h as backspace
(keyboard-translate ?\C-h ?\C-?)

;; use \C-? as help
(global-set-key [\C-?] 'help)

(global-set-key [?\C--] (lambda () (interactive) (insert "->"))) ; powered by hiboma
(global-set-key [?\M--] (lambda () (interactive) (insert "=>")))


(global-set-key "\C-\\" 'redo)
(require 'redo)
(global-set-key "\C-_" 'undo)

(global-set-key [?\C-\:] 'anything)

(global-set-key "\C-x\C-b" 'recentf-open-files)

(global-set-key "\C-cs" 'open-junk-file)

(global-set-key [f6] 'today)

;; align
(global-set-key [?\C-;] 'align)
