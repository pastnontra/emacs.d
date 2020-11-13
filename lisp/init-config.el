;; -*- coding: utf-8; lexical-binding: t; -*-

(setq visible-bell t)

;; font
(set-face-attribute 'default nil :font "hack 12")
(setq face-font-rescale-alist '(("等距更纱黑体 T SC" . 1)))

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                        (font-spec :family "等距更纱黑体 T SC")))


(global-set-key (kbd "C-h") 'delete-backward-char)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)

(require 'init-evil)
(require 'evil-matchit)

;; theme
(load-theme 'doom-monokai-pro t)

;; In light environment, there are little different between above 3 color
;; (set-background-color "#000000") ; BLACK
;; (set-background-color "#080808") ; KURO
(set-background-color "#0C0C0C") ; RO
;; (set-background-color "#0B1013") ; KUROTSURUBAMI
;; (set-background-color "#1C1C1C") ; SUMI for comment?

(set-language-environment "UTF-8")

(provide 'init-config)