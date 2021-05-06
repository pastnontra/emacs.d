;; -*- coding: utf-8; lexical-binding: t; -*-

;;;; init-default
(set-language-environment "UTF-8")

; full screen in macOS
;; (setq ns-use-native-fullscreen nil)
;; (setq ns-use-fullscreen-animation nil)
;; (run-at-time "5sec" nil
;;              (lambda ()
;;                (let ((fullscreen (frame-parameter (selected-frame) 'fullscreen)))
;;                  ;; If emacs has in fullscreen status, maximized window first, drag from Mac's single space.
;;                  (when (memq fullscreen '(fullscreen fullboth))
;;                    (set-frame-parameter (selected-frame) 'fullscreen 'maximized))
;;                  ;; Manipulating a frame without waiting for the fullscreen
;;                  ;; animation to complete can cause a crash, or other unexpected
;;                  ;; behavior, on macOS (bug#28496).
;;                  (when (featurep 'cocoa) (sleep-for 0.5))
;;                  ;; Call `toggle-frame-fullscreen' to fullscreen emacs.
;;                  (toggle-frame-fullscreen))))

(setq visible-bell t)

;; (setq enable-recursive-minibuffers t)

;; compile
(require 'compile-dwim)
(require 'smart-compile)

;;; Keybindings
(require 'company)
(with-eval-after-load 'company (define-key company-active-map (kbd "C-n") 'company-select-next)
                               (define-key company-search-map (kbd "C-n") 'company-select-next)
                               (define-key company-active-map (kbd "C-p") 'company-select-previous)
                               (define-key company-search-map (kbd "C-p") 'company-select-previous)
                               (define-key company-active-map (kbd "C-h") nil)
                               (define-key company-active-map (kbd "C-w") nil)
                               (define-key company-active-map (kbd "C-u") nil))

(global-set-key (kbd "C-h") 'delete-backward-char)
(define-key ivy-minibuffer-map (kbd "C-h") 'ivy-backward-delete-char)
(define-key ivy-minibuffer-map (kbd "C-w") 'ivy-backward-kill-word)


;;;; init-appearance
;;; font
;; (set-face-attribute 'default nil :font "hack 12")
;; (setq face-font-rescale-alist '(("等距更纱黑体 T SC" . 1)))

;; (dolist (charset '(han kana symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     charset
;;                         (font-spec :family "等距更纱黑体 T SC")))

(set-face-attribute 'default nil :font "hack 12")
(setq face-font-rescale-alist '(("思源黑体" . 1)))

(dolist (charset '(han kana symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                        (font-spec :family "思源黑体")))
(when *win64* (setq eaf-python-command "python.exe"))

;;; theme
(load-theme 'doom-monokai-pro t)
;; In light environment, there are little different between above 3 color
;; (set-background-color "#000000") ; BLACK
;; (set-background-color "#080808") ; KURO
;; (set-background-color "#0C0C0C") ; RO
;; (set-background-color "#0B1013") ; KUROTSURUBAMI
;; (set-background-color "#1C1C1C") ; SUMI for comment?


;;; init-language

;; rime
(require 'rime)

(when *is-a-mac*
  (setq rime-user-data-dir "~/Library/Rime")
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


;;; init-org
(add-hook 'org-mode-hook (lambda () (setq electric-pair-mode t)))
;;; Appearance
(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(setq org-superstar-cycle-headline-bullets t)
(setq org-hide-leading-stars t)
(setq org-superstar-special-todo-items t)
(setq org-ellipsis " ▾")
(setq org-hide-emphasis-markers t)


;;; Keybindings
(evil-define-key 'normal org-mode-map (kbd "j") 'evil-next-visual-line
                                      (kbd "k") 'evil-previous-visual-line)


(provide 'init-config)
