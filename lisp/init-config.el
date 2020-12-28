;; -*- coding: utf-8; lexical-binding: t; -*-

(set-language-environment "UTF-8")

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

; theme
(load-theme 'doom-monokai-pro t)

;; In light environment, there are little different between above 3 color
;; (set-background-color "#000000") ; BLACK
;; (set-background-color "#080808") ; KURO
;; (set-background-color "#0C0C0C") ; RO
;; (set-background-color "#0B1013") ; KUROTSURUBAMI
;; (set-background-color "#1C1C1C") ; SUMI for comment?


; rime
(require 'rime)

(when *linux*
  (setq rime-user-data-dir "~/.config/ibus/rime"))
(when *is-a-mac*
  (setq rime-user-data-dir "~/Library/Rime"))
(when *win64*
  (setq rime-user-data-dir "~/AppData/Roaming/Rime"))

(global-set-key (kbd "C-\\") 'toggle-input-method)
(setq rime-cursor "˰")
(setq rime-translate-keybindings '("C-h"))
(setq rime-posframe-style 'vertical)


(setq rime-posframe-properties
      (list :background-color "#333333"
            :foreground-color "#dcdccc"
            :font "WenQuanYi Micro Hei Mono-14"
            :internal-border-width 10))

(setq default-input-method "rime"
      rime-show-candidate 'posframe)

(provide 'init-config)
