;; -*- coding: utf-8; lexical-binding: t; -*-

(setq visible-bell t)

;; set font
(add-to-list 'default-frame-alist
             '(font . "Hack-12"))
(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "微软雅黑")))

(global-set-key (kbd "C-h") 'delete-backward-char)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)

(require 'init-evil)
(require 'evil-matchit)
(setq my-use-m-for-matchit t)

(load-theme 'doom-monokai-pro t)

;; In light environment, there are little different between above 3 color
;; (set-background-color "#000000") ; BLACK
;; (set-background-color "#080808") ; KURO
(set-background-color "#0C0C0C") ; RO
;; (set-background-color "#0B1013") ; KUROTSURUBAMI
;; (set-background-color "#1C1C1C") ; SUMI for comment?

(provide 'init-config)