;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (el-get-checksum el-get-make-recipes el-get-cd
;;;;;;  el-get-self-update el-get-update-all el-get-version) "el-get/el-get"
;;;;;;  "el-get/el-get.el" (20776 8656))
;;; Generated autoloads from el-get/el-get.el

(autoload 'el-get-version "el-get/el-get" "\
Message the current el-get version

\(fn)" t nil)

(autoload 'el-get-update-all "el-get/el-get" "\
Performs update of all installed packages.

\(fn &optional NO-PROMPT)" t nil)

(autoload 'el-get-self-update "el-get/el-get" "\
Update el-get itself.  The standard recipe takes care of reloading the code.

\(fn)" t nil)

(autoload 'el-get-cd "el-get/el-get" "\
Open dired in the package directory.

\(fn PACKAGE)" t nil)

(autoload 'el-get-make-recipes "el-get/el-get" "\
Loop over `el-get-sources' and write a recipe file for each
entry which is not a symbol and is not already a known recipe.

\(fn &optional DIR)" t nil)

(autoload 'el-get-checksum "el-get/el-get" "\
Compute the checksum of the given package, and put it in the kill-ring

\(fn PACKAGE &optional PACKAGE-STATUS-ALIST)" t nil)

;;;***

;;;### (autoloads (el-get-list-packages) "el-get/el-get-list-packages"
;;;;;;  "el-get/el-get-list-packages.el" (20776 8656))
;;; Generated autoloads from el-get/el-get-list-packages.el

(autoload 'el-get-list-packages "el-get/el-get-list-packages" "\
Display a list of packages.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-quickrun anything-quickrun quickrun-shell
;;;;;;  quickrun-compile-only quickrun-replace-region quickrun-region
;;;;;;  quickrun-with-arg quickrun quickrun-add-command quickrun-set-default)
;;;;;;  "quickrun/quickrun" "quickrun/quickrun.el" (20776 9071))
;;; Generated autoloads from quickrun/quickrun.el

(autoload 'quickrun-set-default "quickrun/quickrun" "\
Set `key' as default key in programing language `lang'

\(fn LANG KEY)" nil nil)

(autoload 'quickrun-add-command "quickrun/quickrun" "\


\(fn KEY ALIST &key DEFAULT MODE OVERRIDE)" nil nil)

(autoload 'quickrun "quickrun/quickrun" "\
Run commands quickly for current buffer
   With universal prefix argument(C-u), select command-key,
   With double prefix argument(C-u C-u), run in compile-only-mode

\(fn &rest PLIST)" t nil)

(autoload 'quickrun-with-arg "quickrun/quickrun" "\
Run commands quickly for current buffer with arguments

\(fn ARG)" t nil)

(autoload 'quickrun-region "quickrun/quickrun" "\
Run commands with specified region

\(fn START END)" t nil)

(autoload 'quickrun-replace-region "quickrun/quickrun" "\
Run commands with specified region and replace

\(fn START END)" t nil)

(autoload 'quickrun-compile-only "quickrun/quickrun" "\
Exec only compilation

\(fn)" t nil)

(autoload 'quickrun-shell "quickrun/quickrun" "\
Run commands in shell for interactive programs

\(fn)" t nil)

(autoload 'anything-quickrun "quickrun/quickrun" "\


\(fn)" t nil)

(autoload 'helm-quickrun "quickrun/quickrun" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("el-get/el-get-autoloads.el" "el-get/el-get-build.el"
;;;;;;  "el-get/el-get-byte-compile.el" "el-get/el-get-core.el" "el-get/el-get-custom.el"
;;;;;;  "el-get/el-get-dependencies.el" "el-get/el-get-install.el"
;;;;;;  "el-get/el-get-methods.el" "el-get/el-get-notify.el" "el-get/el-get-recipes.el"
;;;;;;  "el-get/el-get-status.el" "quickrun/test-quickrun.el") (20776
;;;;;;  9077 803344))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
