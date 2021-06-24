;; -*- coding: utf-8; lexical-binding: t; -*-

;;; init-default
(set-language-environment "UTF-8")
(setq sentence-end-double-space nil)
(setq word-wrap-by-category t)

;; Files
(add-to-list 'recentf-exclude "/home/[a-z]+/Dropbox/notes/journal/")

;; System
(defun wsl-browse-url-xdg-open (url &optional ignored)
  (interactive (browse-url-interactive-arg "URL: "))
  (shell-command-to-string (concat "explorer.exe " url)))
(when *wsl* (advice-add #'browse-url-xdg-open :override #'wsl-browse-url-xdg-open))

(setq visible-bell nil
      ring-bell-function #'ignore)

(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; company
(setq company-idle-delay 0.3)

;; compile
(require 'compile-dwim)
(require 'smart-compile)

;; Keybindings
(global-set-key (kbd "C-h") (kbd "<backspace>"))
(require 'ivy)
(define-key ivy-minibuffer-map (kbd "C-w") 'ivy-backward-kill-word)


;;; init-ui
;; font
(set-face-attribute 'default nil :font "hack 12")
(setq face-font-rescale-alist '(("思源黑体" . 1)))

(dolist (charset '(han kana symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                        (font-spec :family "思源黑体")))

(when *win64* (setq eaf-python-command "python.exe"))

;; theme
(load-theme 'doom-monokai-pro t)

(column-number-mode 1)

;;; init-lang
;; rime
(require 'rime)

(when *is-a-mac*
  (setq rime-librime-root "/usr/local/share/librime/dist")
  (setq rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include"))

(setq rime-user-data-dir "~/.emacs.d/rime")
(global-set-key (kbd "C-\\") 'toggle-input-method)

(setq rime-translate-keybindings '("C-h"
                                   "C-p"
                                   "C-n"
                                   "C-`"
                                   "C-k"))


(setq default-input-method "rime"
      rime-show-candidate 'posframe)

(setq rime-posframe-properties
      (list :background-color "#333333"
            :foreground-color "#dcdccc"
            :font "Arial-14"
            :internal-border-width 10))

(setq rime-posframe-style 'vertical)

(setq rime-cursor "˰")


;;; init-tex
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;;; init-git
(require 'git-gutter)
(custom-set-variables '(git-gutter:modified-sign "*"))
(set-face-foreground 'git-gutter:modified "deep sky blue")
(set-face-foreground 'git-gutter:added "chartreuse")


;;; init-org
(add-hook 'org-mode-hook #'electric-pair-mode)

;; Appearance
(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(setq org-superstar-cycle-headline-bullets t)
(setq org-hide-leading-stars t)
(setq org-superstar-special-todo-items t)
(setq org-ellipsis " ▾")
(setq org-hide-emphasis-markers t)
(setq tooltip-use-echo-area (not tooltip-use-echo-area))

(setq org-superstar-item-bullet-alist
      '((?* . ?•)
        (?+ . ?➤)
        (?- . ?•)))

;; Keybindings
(evil-define-key 'normal org-mode-map (kbd "j") 'evil-next-visual-line
                                      (kbd "k") 'evil-previous-visual-line
                                      (kbd "C-i") 'evil-jump-forward)

;; Features
(setq org-todo-keywords (quote ((sequence "WAIT(w@/!)" "TODO(t)" "STRT(s)" "|" "DONE(d!/!)" "CANC(c@/!)"))))
(setq org-id-ts-format "%Y-%m-%d-%H%M%S")
(setq org-id-method 'ts)

;; org-journal
(setq org-agenda-files "~/Dropbox/notes/journal/")
(require 'org-journal)
(setq org-journal-dir "~/Dropbox/notes/journal/")
(setq org-journal-file-format "%Y%m%d.org")
(setq org-journal-date-format "%a, %Y-%m-%d")
(setq org-journal-carryover-items "")

;;; init-eaf
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)


(provide 'init-setting)
