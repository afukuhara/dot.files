
;;; IMEの設定
(mw32-ime-initialize)
(setq default-input-method "MW32-IME")
(setq-default mw32-ime-mode-line-state-indicator "[--]")
(setq mw32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
(add-hook 'mw32-ime-on-hook
          (function (lambda () (set-cursor-height 2))))
(add-hook 'mw32-ime-off-hook
          (function (lambda () (set-cursor-height 4))))
;;; マウスカーソルを消す設定
(setq w32-hide-mouse-on-key t)
(setq w32-hide-mouse-timeout 5000)

;;; フォントの設定
(let ((make-spec
       (function
        (lambda (size charset fontname &optional windows-charset)
          (setq size (- size))
          (if (not windows-charset)
              (setq windows-charset
                    (cadr (assq charset mw32-charset-windows-font-info-alist))))
          `(((:char-spec ,charset :height any)
             strict
             (w32-logfont ,fontname 0 ,size 300 0 nil nil nil ,windows-charset 1 3 0))
            ((:char-spec ,charset :height any :weight bold)
             strict
             (w32-logfont ,fontname 0 ,size 700 0 nil nil nil ,windows-charset 1 3 0)
             ((spacing . -1)))
            ((:char-spec ,charset :height any :slant italic)
             strict
             (w32-logfont ,fontname 0 ,size 300 0   t nil nil ,windows-charset 1 3 0))
            ((:char-spec ,charset :height any :weight bold :slant italic)
             strict
             (w32-logfont ,fontname 0 ,size 700 0   t nil nil ,windows-charset 1 3 0)
             ((spacing . -1)))))))
      (make-spec-list
       (function
        (lambda (size params-list)
          (list (cons 'spec
                      (apply 'append
                             (mapcar (lambda (params)
                                       (apply make-spec (cons size params)))
                                     params-list)))))))
      (define-fontset
        (function
         (lambda (fontname size fontset-list)
           (let ((spec (funcall make-spec-list size fontset-list)))
             (if (w32-list-fonts fontname)
                 (w32-change-font fontname spec)
               (w32-add-font fontname spec))))))
      (consolas-fontset-list '((ascii "Consolas")
                               (katakana-jisx0201 "ＭＳ ゴシック")
                               (japanese-jisx0208 "ＭＳ ゴシック")
                               (korean-ksc5601 "Dotum")
                               (chinese-gb2312 "SimHei")
                               (chinese-big5-1 "MingLiU")
                               (chinese-big5-2 "MingLiU"))))
  (mapcar (lambda (x) (funcall define-fontset
                               (concat "Consolas " (number-to-string x))
                               x
                               consolas-fontset-list))
          '(10 12 13 14 15 16 18 20 22 24 36 48)))

(add-to-list 'default-frame-alist '(font . "Consolas 13"))
(set-frame-font "Consolas 13")

;; AutohotKey
(require 'ahk-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahk-syntax-directory "c:/Program Files/AutoHotkey/Extras/Editors/Syntax/"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; 初期フレームの設定
(setq default-frame-alist
      (append (list '(foreground-color . "black")
                    '(background-color . "FloralWhite")
                    '(border-color . "black")
                    '(mouse-color . "white")
                    '(cursor-color . "black")
                    '(width . 90)
                    '(height . 63)
                    '(top . 0)
                    '(left . 10))
              default-frame-alist))

