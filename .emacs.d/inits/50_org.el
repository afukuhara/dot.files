;;; ================================================================ ;;;
;;;   Org-mode Settings                                              ;;;
;;;     From                                                         ;;;
;;;        - http://d.hatena.ne.jp/rubikitch/20090121/1232468026     ;;;
;;;        - http://d.hatena.ne.jp/authorNari/20100416/1271410285    ;;;
;;;        - http://hpcgi1.nifty.com/spen/index.cgi?OrgMode         ;;;
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
        ("Tips" ?p "** %?\n   %i\n   %a\n   %t" nil "Tips")
        ("WorkMemo" ?m "** %?\n   %i\n   %a\n   %T" nil "Work Memos")
        ))

(add-hook
 'org-mode-hook
 (lambda ()
   (local-set-key "\C-cl" 'org-store-link)
   (local-set-key "\C-ca" 'org-agenda)
   (local-set-key "\C-cb" 'org-iswitchb)
   ))

(global-unset-key "\M-r")
(global-set-key "\M-rr" 'org-remember)

;; org-mode のファイルを置くベースディレクトリ
(setq org-memo-base-dir "~/memo/")


;;; =======================================================================
;;; 今日のアジェンダを開く
;;; 初めてアジェンダファイルを開く場合は、
;;; 直近 (10日前まで) のアジェンダファイルの内容をデフォルト表示する
;;; =======================================================================
(defun open-todays-file (filetype ext)

  ;; from "doukaku"
  ;; http://ja.doukaku.org/comment/1291/
  (defun n-days-before (n)
    `(lambda (time) (- time (* ,n 3600 24))))

  (defun seconds-to-filename (sec)
    (format-time-string (concat org-memo-base-dir filetype "/%y%m%d." ext)
                        (seconds-to-time sec)))

  (let* ((now (float-time))
         (new-filename (seconds-to-filename now))
         (old-filename new-filename)
         (count 1))
    (find-file new-filename)
    (if (not (file-readable-p old-filename))
        (progn (while (and (not (file-readable-p old-filename))
                           (< count 10))
                 (setq now (funcall (n-days-before 1) now))
                 (setq old-filename (seconds-to-filename now))
                 (setq count (1+ count)))
               (if (and (file-readable-p old-filename)
                        (< count 10))
                   (insert-file-contents old-filename)
                 (insert "# -*- coding: utf-8; -*-\n#+STARTUP: showall\n"))))))

(defun todays-agenda ()
  (open-todays-file "agendas" "org"))

(defun command-todays-agenda ()
  (interactive)
  (todays-agenda))

(defun todays-report ()
  (open-todays-file "reports/daily" "txt"))

(defun command-todays-report ()
  (interactive)
  (todays-report))

(defun weekly-report ()
  (open-todays-file "reports/weekly" "txt"))

(defun command-weekly-report ()
  (interactive)
  (weekly-report))

(global-set-key "\M-ra" 'command-todays-agenda)
(global-set-key "\M-rd" 'command-todays-report)
(global-set-key "\M-rw" 'command-weekly-report)

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

