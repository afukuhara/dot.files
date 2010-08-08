;; RSense をインストールしたディレクトリ
(setq rsense-home (expand-file-name "~/opt/rsense"))

(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)

