;;; ================================================================ ;;;
;;;     Org-mode Settings                                            ;;;
;;;  From                                                            ;;;
;;;     - http://d.hatena.ne.jp/rubikitch/20090121/1232468026        ;;;
;;;     - http://d.hatena.ne.jp/authorNari/20100416/1271410285       ;;;
;;; ================================================================ ;;;

(require 'org-install)
(setq org-startup-truncated nil)
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(org-remember-insinuate)
(setq org-directory "~/memo/")
(setq org-default-notes-file (concat org-directory "agenda.org"))
(setq org-remember-templates
      '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "Inbox")
        ("Book" ?b "** %?\n   %i\n   %a\n   %T" nil "Book Memo")
        ("Idea" ?i "** %?\n   %i\n   %a\n   %t" nil "New Ideas")
        ("WorkMemo" ?m "** %?\n   %i\n   %a\n   %T" nil "Work Memos")
        ))

(add-hook
 'org-mode-hook
 (lambda ()
   (local-set-key "\C-cl" 'org-store-link)
   (local-set-key "\C-ca" 'org-agenda)
   (local-set-key "\C-cb" 'org-iswitchb)
   ))

;;; 今日の日付の agenda を開く
(defun org-todays-agenda ()
  (interactive)
  (find-file (concat "~/memo/agendas/"
                     (format-time-string "%y%m%d.org"))))


;; User \M-r for executing org-remember
(global-unset-key "\M-r")
(global-set-key "\M-rr" 'org-remember)
(global-set-key "\M-rc" 'org-remember-code-reading)
(global-set-key "\M-ra" 'org-todays-agenda)

(defvar org-code-reading-software-name nil)
;; ~/memo/code-reading.org に記録する
(defvar org-code-reading-file "code-reading.org")
(defun org-code-reading-read-software-name ()
  (set (make-local-variable 'org-code-reading-software-name)
       (read-string "Code Reading Software: "
                    (or org-code-reading-software-name
                        (file-name-nondirectory
                         (buffer-file-name))))))

(defun org-code-reading-get-prefix (lang)
  (concat "[" lang "]"
          "[" (org-code-reading-read-software-name) "]"))
(defun org-remember-code-reading ()
  (interactive)
  (let* ((prefix (org-code-reading-get-prefix (substring (symbol-name major-mode) 0 -5)))
         (org-remember-templates
          `(("CodeReading" ?r "** %(identity prefix)%?\n   \n   %a\n   %t"
             ,org-code-reading-file "Memo"))))
    (org-remember)))
