;;;; -*- mode: emacs-lisp; coding: utf-8 -*-

(let ((default-directory "~/.emacs.d/elisp/"))
  (add-to-list 'load-path "~/.emacs.d/elisp/")
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
  (normal-top-level-add-subdirs-to-load-path))

(require 'init-loader)
(init-loader-load "~/.emacs.d/inits") ; 設定ファイルがあるディレクトリを指定
