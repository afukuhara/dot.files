;;; tt
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist
      (cons
       '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons
       '("\\.tt2?$" . html-helper-mode) auto-mode-alist))
(require 'html-tt)
(add-hook 'html-helper-mode-hook 'html-tt-load-hook)

;; Setting values:
;; change sequence face
(make-face 'my-sequence-face)
(set-face-foreground 'my-sequence-face "blue")
(set-face-background 'my-sequence-face "bisque")
(setq html-tt-sequence-face 'my-sequence-face)

;; change sequence for insert
(setq html-tt-sequence-start "[% ")
(setq html-tt-sequence-end " %]")
