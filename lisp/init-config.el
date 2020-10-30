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

(load-theme 'doom-monokai-pro t)

(provide 'init-config)