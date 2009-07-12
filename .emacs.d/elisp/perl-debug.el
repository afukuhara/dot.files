;;; perl-debug.el --- Run perl debugger and lint.

;; Copyright (C) 1998-2006 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: perl
;; Version: $Revision: 1.17 $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SDIC; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


;;; Commentary:

;; This file defines some commands as follows:
;;   (1) execute perl debugger, and
;;   (2) check syntax of scripts.
;;
;; The newest version of this program will be placed at
;; http://namazu.org/~tsuchiya/elisp/perl-debug.el.


;;; NEWS:

;; `perl-debug-coding-system' is abolished, and perl-debug.el does not
;; change coding systems of scripts.


;;; Install:

;; Put this program to appropriate directory, and put following
;; expressions to your ~/.emacs.
;;
;;    (autoload 'perl-debug "perl-debug" nil t)
;;    (autoload 'perl-debug-lint "perl-debug" nil t)
;;    (add-hook 'perl-mode-hook
;;              (lambda ()
;;                (define-key perl-mode-map "\C-cc" 'perl-debug-lint)
;;                (define-key perl-mode-map "\C-cd" 'perl-debug)))

;; And more, you should insert the following code to ~/.perldb, in
;; order to use an appropriate coding system to communicate Perl
;; debugger.

;; sub afterinit {
;;     if( $ENV{"PERL_DEBUG_ENCODING"} ){
;;         my $enc = sprintf(":encoding(%s)", $ENV{"PERL_DEBUG_ENCODING"});
;;         binmode $DB::OUT, $enc;
;;         binmode $DB::IN,  $enc;
;;     }
;; }


;;; Code:

(require 'gud)

(eval-when-compile
  (require 'cl)
  (require 'comint)
  (require 'shell))

(eval-and-compile
  (autoload 'compile-internal "compile")
  ;; Stuffs to keep compatibility between Emacsen.
  (if (locate-library "custom")
      (require 'custom)
    (or (fboundp 'defgroup)
	(defmacro defgroup (symbol members doc &rest args) nil))
    (or (fboundp 'defcustom)
	(defmacro defcustom (symbol value doc &rest args)
	  (` (defvar (, symbol) (, value) (, doc))))))
  (or (fboundp 'match-string)
      ;; Introduced in Emacs 19.29.
      (defun match-string (num &optional string)
	"Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
	(if (match-beginning num)
	    (if string
		(substring string (match-beginning num) (match-end num))
	      (buffer-substring (match-beginning num) (match-end num)))))))


(defgroup perl-debug nil
  "Perl debugger interface on Emacs."
  :prefix "perl-debug-"
  :group 'perl)

(defcustom perl-debug-window-height 10
  "*The number of lines in window to display results."
  :group 'perl-debug
  :type 'integer)

(defcustom perl-debug-command-name
  (or (if (boundp 'perldb-command-name)
	  (symbol-value 'perldb-command-name))
      "perl")
  "*Executable file name of perl."
  :group 'perl-debug
  :type 'string)

(defcustom perl-debug-lint-options '("-wc")
  "*Options to check syntax of script."
  :group 'perl-debug
  :type '(repeat string))

(defcustom perl-debug-complete-functions
  '(perl-debug-dynamic-complete-filename)
  "*List of functions to complete arguments for `perl-debug'."
  :group 'perl-debug
  :type 'hook)

(defcustom perl-debug-encoding-alist
  '(("iso-2022-jp" . "iso-2022-jp")
    ("euc-j" . "euc-jp")
    ("shift_jisx0213" . "cp932")
    ("shift_jis" . "shiftjis")
    ("utf-8" . "utf8"))
  "*Alist of regular expressions which match Emacs coding systems and
their Perl canonical encoding names."
  :group 'perl-debug
  :type '(repeat (cons
		  (string :tag "Regexp of Emacs Coding System")
		  (string :tag "Perl Encoding"))))

(defun perl-debug-dynamic-complete-filename ()
  "Dynamically complete the filename at point."
  (interactive)
  (require 'comint)
  (require 'shell)
  (let ((p (point)))
    (skip-chars-backward shell-file-name-chars)
    (if (eq ?~ (char-after (point)))
	(insert (prog1 (expand-file-name (buffer-substring (point) p))
		  (delete-region (point) p)))
      (goto-char p)))
  (comint-dynamic-complete-filename))

(defsubst perl-debug-get-command-name ()
  "Return executable file name of Perl."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (if (and (looking-at "#!\\s-*\\(\\(\\(/[^/]+\\)*/\\)?j?perl\\)\\b")
	       (file-executable-p (match-string 1)))
	  (match-string 1)
	perl-debug-command-name))))

(defmacro perl-debug-minibuffer-prompt-end ()
  "Return the buffer position of the end of the minibuffer prompt."
  (if (fboundp 'minibuffer-prompt-end)
      '(minibuffer-prompt-end)
    '(point-min)))

(defun perl-debug-read-minibuffer
  (prompt &optional initial-contents user-keymap read hist)
  "Read a command string in the minibuffer, with completion."
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap (or user-keymap minibuffer-local-map))
    (define-key keymap "\t"
      (lambda ()
	(interactive)
	(let ((orig-function (symbol-function 'message)))
	  (unwind-protect
	      (progn
		(defun message (string &rest arguments)
		  (let* ((s1 (concat prompt
				     (buffer-substring
				      (perl-debug-minibuffer-prompt-end)
				      (point-max))))
			 (s2 (apply (function format) string arguments))
			 (w (- (window-width)
			       (string-width s1)
			       (string-width s2)
			       1)))
		    (funcall orig-function
			     (if (>= w 0)
				 (concat s1 (make-string w ?\ ) s2)
			       s2))
		    (if (sit-for 0.3) (funcall orig-function s1))
		    s2))
		(run-hook-with-args-until-success
		 'perl-debug-complete-functions))
	    (fset 'message orig-function)))))
    (read-from-minibuffer prompt initial-contents keymap read hist)))

(eval-and-compile
  (if (boundp 'MULE)
      (progn
	(defvar file-coding-system)
	(defvar default-file-coding-system)
	(defun perl-debug-buffer-coding-system ()
	  "Return the coding system of this buffer."
	  (if (memq file-coding-system
		    '(*undecided*
		      *undecided-dos*
		      *undecided-mac*
		      *undecided-unix*))
	      default-file-coding-system
	    file-coding-system)))
    (defvar buffer-file-coding-system)
    (defun perl-debug-buffer-coding-system ()
      "Return the coding system of this buffer."
      (if (memq buffer-file-coding-system
		'(undecided
		  undecided-dos
		  undecided-mac
		  undecided-unix))
	  (default-value 'buffer-file-coding-system)
	buffer-file-coding-system))))

(defvar perl-debug-history nil "Hisotry list of `perl-debug' command.")
(make-variable-buffer-local 'perl-debug-history)

(defun perl-debug ()
  "Run debugger on the script in this buffer.
This command works as a wrapper to `perldb' and adds 2 extensions:
\(1\) enables completion for script arguments, and \(2\) sets the
coding system of this script to the debugger coding system."
  (interactive)
  (and (buffer-modified-p)
       (y-or-n-p (concat "Save file " buffer-file-name "? "))
       (basic-save-buffer))
  (let (str)
    ;; Workaround to refer buffer-local variables as history.
    (setq perl-debug-history
	  (let ((history perl-debug-history))
	    (setq str (perl-debug-read-minibuffer
		       (format "Input arguments [%s]: "
			       (directory-file-name default-directory))
		       (car history) nil nil (cons 'history 1)))
	    (if (equal str (car history))
		history
	      (cons str history))))
    (let ((cs (or (if (fboundp 'universal-coding-system-argument)
		      (symbol-value 'coding-system-for-read))
		  (perl-debug-buffer-coding-system))))
      (let ((coding-system-for-read cs)
	    (coding-system-for-write cs)
	    (default-process-coding-system (cons cs cs))
	    (process-environment (copy-sequence process-environment)))
	(setq cs (symbol-name cs))
	(setenv "PERL_DEBUG_ENCODING"
		(catch 'match
		  (dolist (pair perl-debug-encoding-alist)
		    (when (string-match (car pair) cs)
		      (throw 'match (cdr pair))))))
	(perldb (format "%s -d %s %s"
			(perl-debug-get-command-name)
			buffer-file-name str))
	(set-buffer-process-coding-system cs cs)))))

(defconst perl-debug-lint-regexp-alist
  '((".* at \\([^ ]+\\) line \\([0-9]+\\)[,\\.]" 1 2))
  "Regexp used to match syntax errors reported by Perl.
See `compilation-error-regexp-alist', for more detail.")

(defun perl-debug-lint ()
  "Check syntax on the script in this buffer."
  (interactive)
  (and (buffer-modified-p)
       (y-or-n-p (concat "Save file " buffer-file-name "? "))
       (basic-save-buffer))
  (let ((file buffer-file-name)
	(perl (perl-debug-get-command-name))
	(buf))
    (save-window-excursion
      (setq buf
	    (let* ((cs (perl-debug-buffer-coding-system))
		   (coding-system-for-read cs)
		   (coding-system-for-write cs)
		   (default-process-coding-system (cons cs cs)))
	      (compile-internal
	       (concat perl " "
		       (mapconcat 'identity perl-debug-lint-options " ")
		       " " file)
	       "No more errors" "perl-debug-lint" nil
	       perl-debug-lint-regexp-alist))))
    (let ((w (get-buffer-window buf)))
      (or w (set-window-buffer
	     (setq w (if (one-window-p)
			 (split-window (selected-window)
				       (- (window-height)
					  perl-debug-window-height))
		       (next-window)))
	     buf))
      (set-window-start w (point-min)))))

;;; This function is defined only to keep backward compatibility.
(defalias 'perl-debug-set-coding-system 'ignore)

(provide 'perl-debug)

;;; perl-debug.el ends here.
