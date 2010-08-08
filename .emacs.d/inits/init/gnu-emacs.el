;; font configuration
(set-face-attribute 'default nil
                    :family "lucidasans-bold"
                    :height 110)
(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  '(" pro w4*" . "jisx0208.*"))
(set-fontset-font "fontset-default"
                  'katakana-jisx0201
                  '(" pro w4*" . "jisx0201.*"))
(add-to-list 'default-frame-alist '(font . "fontset-default"))

;; 初期フレームの設定
(setq default-frame-alist
      (append (list '(foreground-color . "black")
                    '(background-color . "snow")
                    '(border-color . "black")
                    '(mouse-color . "white")
                    '(cursor-color . "black")
                    '(width . 90)
                    '(height . 72)
                    '(top . 15)
                    '(left . 5))
              default-frame-alist))
