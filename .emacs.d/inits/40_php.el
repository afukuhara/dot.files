(load-library "php-mode")
(require 'php-mode)

(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

(add-hook
 'php-mode-hook
 (defun php-mode-hook ()
   (c-set-offset 'case-label' 0)
   (c-set-offset 'arglist-intro' 4)
   (c-set-offset 'arglist-cont-nonempty' 0)
   (c-set-offset 'arglist-close' 0))

 (lambda ()
   (local-set-key "\C-c\C-l"  'php-newline)
   (defun php-newline ()
     (interactive)
     (insert "PHP_EOL"))
   ))

