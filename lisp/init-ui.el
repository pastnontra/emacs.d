;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package page-break-lines
  :custom
  (page-break-lines-modes '(emacs-lisp-mode lisp-mode scheme-mode compilation-mode outline-mode help-mode fundamental-mode))
  :diminish
  ;; :hook (after-init . global-page-break-lines-mode))
  :hook (emacs-startup . global-page-break-lines-mode))

;; theme
(use-package doom-themes)
(load-theme 'doom-monokai-pro t)
(set-background-color "#212121")

;; font
(set-face-attribute 'default nil :font "hack 14")
(setq face-font-rescale-alist '(("Source Han Sans" . 1)))

(dolist (charset '(han kana symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                        (font-spec :family "Source Han Sans")))

(column-number-mode 1)

(use-package perfect-margin
  :config
  (perfect-margin-mode 1))

(use-package hl-todo
  :hook (after-init . global-hl-todo-mode)
  :custom
  (hl-todo-keyword-faces '(("TODO" . "#2dc937")
                           ("STRT" . "#ffdf01")
                           ("WAIT" . "#FF7E00")
                           ("DONE" . "#4c4a4d")
                           ("CANC" . "#cc3232"))))

(provide 'init-ui)
