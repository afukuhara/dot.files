;;; ob-php.el --- org-babel functions for php evaluation

;; Copyright (C) 2009-2011  Free Software Foundation

;; Author: Arinobu Fukuhara
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating php source code.

;;; Code:
(require 'ob)
(require 'ob-eval)
(eval-when-compile (require 'cl))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("php" . "php"))

(defvar org-babel-default-header-args:php '())

(defvar org-babel-php-command "php"
  "Name of command to use for executing php code.")

(defun org-babel-execute:php (body params)
  "Execute a block of PHP code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((session (cdr (assoc :session params)))
         (result-params (cdr (assoc :result-params params)))
         (result-type (cdr (assoc :result-type params)))
         (full-body (org-babel-expand-body:generic
             body params (org-babel-variable-assignments:php params)))
    (session (org-babel-php-initiate-session session)))
    (org-babel-reassemble-table
     (org-babel-php-evaluate session full-body result-type)
     (org-babel-pick-name
      (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
     (org-babel-pick-name
      (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))

(defun org-babel-prep-session:php (session params)
  "Prepare SESSION according to the header arguments in PARAMS."
  (error "Sessions are not supported for PHP."))

(defun org-babel-variable-assignments:php (params)
  "Return list of php statements assigning the block's variables"
  (mapcar
   (lambda (pair)
     (format "$%s=%s;"
         (car pair)
         (org-babel-php-var-to-php (cdr pair))))
   (mapcar #'cdr (org-babel-get-header params :var))))

;; helper functions

(defun org-babel-php-var-to-php (var)
  "Convert an elisp value to a php variable.
The elisp value, VAR, is converted to a string of php source code
specifying a var of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-php-var-to-php var ", ") "]")
    (format "%S" var)))

(defvar org-babel-php-buffers '(:default . nil))

(defun org-babel-php-initiate-session (&optional session params)
  "Return nil because sessions are not supported by php"
nil)

(defvar org-babel-php-wrapper-method
  "
<?php
function main() { %s }
$result = main();
$fh = fopen(%s, 'w');
fwrite($fh, (is_string($result) ? $result : var_export($result, true)) . PHP_EOL);
fclose($fh);
")

(defvar org-babel-php-pp-wrapper-method
  nil)

(defun org-babel-php-evaluate (session body &optional result-type)
  "Pass BODY to the Php process in SESSION.
If RESULT-TYPE equals 'output then return a list of the outputs
of the statements in BODY, if RESULT-TYPE equals 'value then
return the value of the last statement in BODY, as elisp."
  (when session (error "Sessions are not supported for PHP."))
  (case result-type
    (output (org-babel-eval org-babel-php-command body))
    (value (let ((tmp-file (org-babel-temp-file "php-")))
         (org-babel-eval
          org-babel-php-command
          (format org-babel-php-wrapper-method body
              (org-babel-process-file-name tmp-file 'noquote)))
         (org-babel-eval-read-file tmp-file)))))

(provide 'ob-php)



;;; ob-php.el ends here
