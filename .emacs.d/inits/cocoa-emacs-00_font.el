;;;; -*- mode: emacs-lisp; coding: utf-8 -*-

;; for cocoa emacs
(when (>= emacs-major-version 23)
 (setq fixed-width-use-QuickDraw-for-ascii t)
 (setq mac-allow-anti-aliasing t)
 (set-face-attribute 'default nil
;                    :family "monaco"
                     :family "Droid Sans Mono Slashed"
                     :height 105)
 (set-fontset-font
  (frame-parameter nil 'font)
  'japanese-jisx0208
  '("Hiragino Maru Gothic Pro" . "iso10646-1"))
 (set-fontset-font
  (frame-parameter nil 'font)
  'japanese-jisx0212
  '("Hiragino Maru Gothic Pro" . "iso10646-1"))
 ;;; Unicode フォント
 (set-fontset-font
  (frame-parameter nil 'font)
  'mule-unicode-0100-24ff
  '("Droid Sans Mono Slashed" . "iso10646-1"))
 ;;; キリル，ギリシア文字設定
;;; 注意： この設定だけでは古代ギリシア文字、コプト文字は表示できない
;;; http://socrates.berkeley.edu/~pinax/greekkeys/NAUdownload.html が必要
;;; キリル文字
 (set-fontset-font
  (frame-parameter nil 'font)
  'cyrillic-iso8859-5
  '("Droid Sans Mono Slashed" . "iso10646-1"))
;;; ギリシア文字
 (set-fontset-font
  (frame-parameter nil 'font)
  'greek-iso8859-7
  '("Droid Sans Mono Slashed" . "iso10646-1"))
 (setq face-font-rescale-alist
       '(("^-apple-hiragino.*" . 1.2)
         (".*osaka-bold.*" . 1.2)
         (".*osaka-medium.*" . 1.2)
         (".*courier-bold-.*-mac-roman" . 1.0)
         (".*Droid_Sans_Mono.*" . 1.0)
         (".*Droid_Sans_Mono-medium.*" . 1.0)
         (".*Droid_Sans_Fallback.*" . 1.2)
         (".*Droid_Sans_Fallback-medium.*" . 1.2)
         ("-cdac$" . 1.3))))

