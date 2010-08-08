;;----------------------------------------------------------------------
;; mmm-mode
;;  - http://sourceforge.net/projects/mmm-mode/
;;  - http://www.bookshelf.jp/soft/meadow_13.html#SEC100
;;----------------------------------------------------------------------

(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
;; 色設定．これは，好みで．色をつけたくないなら nil にします．
(set-face-background 'mmm-default-submode-face "alice blue")


(mmm-add-group
 'html-js
 '((js-tag
    :submode js2-mode
    :face mmm-code-submode-face
    :delimiter-mode nil
    :front "<script\[^>\]*\\(language=\"javascript\\([0-9.]*\\)\"\\|type=\"text/javascript\"\\)\[^>\]*>"
    :back"</script>"
    :insert ((?j js-tag nil @ "<script language=\"JavaScript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-inline
    :submode js2-mode
    :face mmm-code-submode-face
    :delimiter-mode nil
    :front "on\\w+=\""
    :back "\"")))
(mmm-add-mode-ext-class nil "\\.html?\\'" 'html-js)

(mmm-add-classes
 '((embedded-css
    :submode css
    :face mmm-declaration-submode-face
    :delimiter-mode nil
    :front "<style[^>]*>"
    :back "</style>")))
