;; -*- coding: utf-8; lexical-binding: t; -*-

;;; init-default
(set-language-environment "UTF-8")
(setq visible-bell t)

;; Keybindings
(require 'company)
(with-eval-after-load 'company (define-key company-active-map (kbd "C-n") 'company-select-next)
                               (define-key company-search-map (kbd "C-n") 'company-select-next)
                               (define-key company-active-map (kbd "C-p") 'company-select-previous)
                               (define-key company-search-map (kbd "C-p") 'company-select-previous)
                               (define-key company-active-map (kbd "C-h") nil)
                               (define-key company-active-map (kbd "C-w") nil)
                               (define-key company-active-map (kbd "C-u") nil))

(global-set-key (kbd "C-h") 'delete-backward-char)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)


;;; init-appearance
;; font
(set-face-attribute 'default nil :font "hack 12")
(setq face-font-rescale-alist '(("等距更纱黑体 T SC" . 1)))

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                        (font-spec :family "等距更纱黑体 T SC")))


;; theme
(load-theme 'doom-monokai-pro t)
(set-background-color "#121212")
;; In light environment, there are little different between above 3 color
;; (set-background-color "#000000") ; BLACK
;; (set-background-color "#080808") ; KURO
;; (set-background-color "#0C0C0C") ; RO
;; (set-background-color "#0B1013") ; KUROTSURUBAMI
;; (set-background-color "#1C1C1C") ; SUMI for comment?


;;; init-language

;; rime
(require 'rime)

(when *linux*
  (setq rime-user-data-dir "~/.config/ibus/rime"))
(when *is-a-mac*
  ;; It's better changer the location
  (setq rime-user-data-dir "~/Library/Rime")
  (setq rime-librime-root "/usr/local/share/librime/dist")
  (setq rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include"))
(when *win64*
  (setq rime-user-data-dir "~/AppData/Roaming/Rime"))

(global-set-key (kbd "C-\\") 'toggle-input-method)
(setq rime-cursor "˰")
(setq rime-translate-keybindings '("C-h"
                                   "C-p"
                                   "C-n"))
(setq rime-posframe-style 'vertical)


(setq rime-posframe-properties
      (list :background-color "#333333"
            :foreground-color "#dcdccc"
            :font "Arial-12"
            ;; :font "WenQuanYi Micro Hei Mono-14"
            :internal-border-width 10))

(setq default-input-method "rime"
      rime-show-candidate 'posframe)

;;; init-org
;;; Appearance
(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(setq org-superstar-cycle-headline-bullets t)
(setq org-hide-leading-stars t)
(setq org-superstar-special-todo-items t)

;;; Keybindings
(evil-define-key 'normal org-mode-map (kbd "j") 'evil-next-visual-line
                                      (kbd "k") 'evil-previous-visual-line)

(provide 'init-config)
