;;;; -*- mode: emacs-lisp; coding: utf-8 -*-

;; for carbon emacs
(when (< emacs-major-version 23)
  (set-face-attribute 'default nil
                      :family "monaco"
                      :height 105)
  (set-fontset-font "fontset-default"
                    'japanese-jisx0208
                    '("ヒラギノ丸ゴ pro w4*" . "jisx0208.*"))
  (set-fontset-font "fontset-default"
                    'katakana-jisx0201
                    '("ヒラギノ丸ゴ pro w4*" . "jisx0201.*"))
  (add-to-list 'face-font-rescale-alist
               `(,(encode-coding-string ".*ヒラギノ丸ゴ pro w4.*" 'emacs-mule) . 1.2))

  (create-fontset-from-ascii-font "Menlo-14:weight=normal:slant=normal" nil "menlokakugo")
  (set-fontset-font "fontset-menlokakugo"
                    'unicode
                    (font-spec :family "Hiragino Kaku Gothic ProN" :size 16)
                    nil
                    'append)
  (add-to-list 'default-frame-alist '(font . "fontset-menlokakugo")))

