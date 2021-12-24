;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package page-break-lines
  :custom
  (page-break-lines-modes '(emacs-lisp-mode lisp-mode scheme-mode compilation-mode outline-mode help-mode fundamental-mode))
  :diminish
  ;; :hook (after-init . global-page-break-lines-mode))
  :hook (emacs-startup . global-page-break-lines-mode))

;; theme
(load-theme 'doom-monokai-pro t)
(set-background-color "#121212")

;; font
(when window-system
(set-face-attribute 'default nil :font "hack 12")
(setq face-font-rescale-alist '(("Source Han Mono SC" . 1)))

(dolist (charset '(han kana symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                        (font-spec :family "Source Han Mono SC")))
)

(column-number-mode 1)

(provide 'init-ui)
