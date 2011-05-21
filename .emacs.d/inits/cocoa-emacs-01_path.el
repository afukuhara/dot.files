;; -*- coding: utf-8 -*-
;; 先頭に '/usr/local/bin' を追加
(setq exec-path
      (append '("/Users/arinobu/perl5/perlbrew/bin"
                "/opt/local/bin"
                "/usr/local/bin")
              exec-path))

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

