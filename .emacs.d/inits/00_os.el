
;; ---------------------------------
;; ------ Setting for each OS ------
;; ---------------------------------
(cond ((string= system-type "gnu/linux") ; for Linux (GNU Emacs)
       (load "gnu-emacs"))
      ((string= system-type "darwin") ; for Mac
       (load "darwin")))

;; --- Setting for each OS [END] ---

