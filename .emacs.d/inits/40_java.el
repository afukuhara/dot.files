;;; ================================================================ ;;;
;;;                   For Java Programming                           ;;;
;;; From: http://www.02.246.ne.jp/~torutk/jdee/jdee.html#doc1_id40   ;;;
;;; ================================================================ ;;;

;; アノテーションを使った時にインデントがずれないようにする。
;; - http://www.ruimo.com/2006/04/13/1144911084440.html
(require 'java-mode-indent-annotations)
(defun my-java-mode-hook()
  (java-mode-indent-annotations-setup))
(add-hook 'java-mode-hook 'my-java-mode-hook)

