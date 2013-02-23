;;;; -*- mode: emacs-lisp; coding: utf-8 -*-

(let ((default-directory "~/.emacs.d/elisp/")
      (el-get-directory  "~/.emacs.d/el-get/el-get"))
  (add-to-list 'load-path
               '(default-directory el-get-directory))
  (normal-top-level-add-subdirs-to-load-path))

(require 'init-loader)
(init-loader-load "~/.emacs.d/inits") ; 設定ファイルがあるディレクトリを指定
