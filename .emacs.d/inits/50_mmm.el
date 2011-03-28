;;----------------------------------------------------------------------
;; mmm-mode
;;  - http://sourceforge.net/projects/mmm-mode/
;;  - http://www.bookshelf.jp/soft/meadow_13.html#SEC100
;;----------------------------------------------------------------------

(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
;; 色設定．これは，好みで．色をつけたくないなら nil にします．
(set-face-background 'mmm-default-submode-face "SlateGray1")
(set-face-background 'mmm-default-submode-face "#ddf4ff")

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

(mmm-add-classes
 '((embedded-php
    :submode sh-mode
    :front "<\\?php"
    :back "\\?>")
   ))
(mmm-add-mode-ext-class nil "\\.txt?\\'" 'embedded-php)
(mmm-add-mode-ext-class nil "\\.org?\\'" 'embedded-php)

(mmm-add-classes
 '((embedded-sql
    :submode sql-mode
    :front "# *<sql>
"
    :back "# *</sql>")
   ))
(mmm-add-mode-ext-class nil "\\.sh?\\'" 'embedded-sql)
