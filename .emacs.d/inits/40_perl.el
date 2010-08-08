;;; ================================================================ ;;;
;;;                   Perl Programming Settings                      ;;;
;;;  From: http://www.hasta-pronto.org/archives/2007/05/30-2252.php  ;;;
;;; ================================================================ ;;;

(autoload 'cperl-mode "cperl-mode"
  "alternate mode for editing Perl programs" t)
(defalias 'perl-mode 'cperl-mode)
(setq cperl-indent-parens-as-block t)
(setq cperl-close-paren-offset -4)
(setq cperl-indent-level 4)
(setq cperl-label-offset -4)
(setq cperl-continued-statement-offset 4)
(setq cperl-highlight-variables-indiscriminately t)
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(setq auto-mode-alist (cons '("\\.\\(p[lm]\\|f?cgi\\|t\\)$" . cperl-mode) auto-mode-alist))
(add-hook
 'cperl-mode-hook
 (lambda ()
   (require 'perlplus)
   (local-set-key "\C-cp"     'cperl-check-syntax) ; Syntax Check
   (local-set-key "\C-ct"     'perltidy-region)    ; perltidy
   (local-set-key "\C-c\C-t"  'perltidy-defun)
   (local-set-key "\C-c\C-ct" 'perltidy-buffer)
   (local-set-key "\M-p"      'cperl-perldoc) ; perldoc
   (local-set-key "\C-c\C-m"  'cperl-view-module-src-at-point) ; perldoc -m
   (local-set-key "\C-c\C-cc" 'perl-eval)   ; リージョンを実行
   (local-set-key "\C-c\C-cp" 'cperl-prove) ; テストを実行
   (local-set-key "\C-c\C-cs" 'cperl-start)
   (local-set-key "\C-c\C-l"  'cperl-local-newline)
   (local-set-key "\C-c\C-cd" 'cperl-use-data-dumper)
   (local-set-key "\C-c\C-cy" 'cperl-use-yaml-dump)
   (local-set-key "\C-c\C-w"  'cperl-warn-dumper)
   (local-set-key "\C-c\C-y"  'cperl-warn-yaml)
   (local-set-key "\C-c\C-ch" 'cperl-pod4method)
   (local-set-key "\C-c\C-cf" 'cperl-use-feature)

;;    (require 'perl-completion)
;;    (add-to-list 'ac-sources 'ac-source-perl-completion)
;;    (perl-completion-mode t)

   (define-key cperl-mode-map "\M-," 'perlplus-complete-symbol) ; 関数&シンボル補完
   (perlplus-setup)
   (require 'perl-debug)
   (perl-debug-set-coding-system)
   (define-key cperl-mode-map "\C-cd"    'perl-debug-lint)
   (define-key cperl-mode-map "\C-c\C-d" 'perl-debug) ; デバッガの起動
   ;; syntax color
   (set-face-bold-p   'cperl-array-face nil)
   (set-face-bold-p   'cperl-hash-face nil)
   (set-face-italic-p 'cperl-hash-face nil)

   ;; snipets
   (defun cperl-start ()
     (interactive)
     (insert "use strict\;\nuse warnings\;\n"))
   (defun cperl-local-newline ()
     (interactive)
     (insert "local $\\ \= \"\\n\";\n"))
   (defun cperl-use-data-dumper ()
     (interactive)
     (insert "use Data::Dumper;\n"))
   (defun cperl-use-yaml-dump ()
     (interactive)
     (insert "use YAML::Syck;\n"))
   (defun cperl-warn-dumper ()
     (interactive)
     (insert "warn Dumper "))
   (defun cperl-warn-yaml ()
     (interactive)
     (insert "warn Dump "))
   (defun cperl-pod4method ()
     (interactive)
     (insert "  Description\: \n  Args       \:\n  Return     \:\n"))
   (defun cperl-use-feature ()
     (interactive)
     (insert "use feature \":5.10\"\;"))
   ))

;; ;;; Perl Debug の設定
(autoload 'perl-debug "perl-debug" nil t)
(autoload 'perl-debug-lint "perl-debug" nil t)

;;; perltidy-region powered by typester
(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)))
(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
                  (perltidy-region)))
(defun perltidy-buffer ()
  "Run perltidy on the current buffer."
  (interactive)
  (save-excursion (mark-whole-buffer)
                  (perltidy-region)))

;;; perldoc -m
(defun cperl-view-module-src-at-point (module)
  "Visit perl module's source file"
  (interactive
   (list (let* ((default-entry (or (cperl-word-at-point) ""))
                (input (read-string
                        (format "View perl module's source%s: "
                                (if (string= default-entry "")""
                                  (format " (default %s)" default-entry))))))
           (if (string= input "")
               (if (string= default-entry "")
                   (error "No Perl module given")default-entry)input))))
  (let ((file (substring (shell-command-to-string(concat "perldoc -m " module))0 -1)))
    (if (string-match "No module found for" file)
        (error file)
      (view-file-other-window file))))

;;; テストを実行 by PBP
(defun cperl-prove ()
  "Run the current test."
  (interactive)
  (save-excursion
    (shell-command (concat "prove -vl "
                           (shell-quote-argument (buffer-file-name))))))

;;; リージョン内のコードを実行する by PBP
(defun perl-eval (beg end)
  "Run selected region as Perl code"
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "perl")))


;;; ================================================================ ;;;
;;;                   flymake for Perl Programming                   ;;;
;;; from: http://d.hatena.ne.jp/antipop/20080701/1214838633          ;;;
;;; ================================================================ ;;;

;; flymake (Emacs22から標準添付されている)
(require 'flymake)

;; set-perl5lib
;; 開いたスクリプトのパスに応じて、@INCにlibを追加してくれる
;; 以下からダウンロードする必要あり
;; http://svn.coderepos.org/share/lang/elisp/set-perl5lib/set-perl5lib.el
(require 'set-perl5lib)

;; エラー、ウォーニング時のフェイス
(set-face-background 'flymake-errline "pink")
(set-face-foreground 'flymake-errline "black")
(set-face-background 'flymake-warnline "yellow")
(set-face-foreground 'flymake-warnline "black")

;; エラーをミニバッファに表示
;; http://d.hatena.ne.jp/xcezx/20080314/1205475020
(defun flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info
                                      flymake-err-info line-no)))
         (count               (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file
                            (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file
                            (nth (1- count) line-err-info-list)))
               (text       (flymake-ler-text
                            (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line
                            (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)))
      (setq count (1- count)))))

;; Perl用設定
;; http://unknownplace.org/memo/2007/12/21#e001
(defvar flymake-perl-err-line-patterns
  '(("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)))

(defconst flymake-allowed-perl-file-name-masks
  '(("\\.pl$" flymake-perl-init)
    ("\\.pm$" flymake-perl-init)
    ("\\.t$" flymake-perl-init)))

(defun flymake-perl-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "perl" (list "-wc" local-file))))

(defun flymake-perl-load ()
  (interactive)
  (defadvice flymake-post-syntax-check
    (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (setq flymake-allowed-file-name-masks
        (append flymake-allowed-file-name-masks
                flymake-allowed-perl-file-name-masks))
  (setq flymake-err-line-patterns flymake-perl-err-line-patterns)
  (set-perl5lib)
  (flymake-mode t))

(add-hook 'cperl-mode-hook 'flymake-perl-load)

;;;                 flymake for Perl Programming [END]                ;;;

;;;                 Perl Programming Settings [END]                   ;;;
