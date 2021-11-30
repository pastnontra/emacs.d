;; -*- coding: utf-8; lexical-binding: t; -*-

;;; init-default
(set-language-environment "UTF-8")
(setq sentence-end-double-space nil)
(setq word-wrap-by-category t)

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

(require 'magit)
(defun magit-submodule-remove-force ()
  (interactive)
  (magit-submodule-remove (list (magit-read-module-path "Remove module")) "--force" nil))

;; company
(setq company-idle-delay 0.5)
(with-eval-after-load 'company
(define-key company-active-map (kbd "C-h") (kbd "<backspace>"))
(define-key company-active-map (kbd "C-w") 'backward-kill-word))

;; Keybindings
(require 'ivy)
(define-key ivy-minibuffer-map (kbd "C-h") (kbd "<backspace>"))
(define-key ivy-minibuffer-map (kbd "C-w") 'ivy-backward-kill-word)

;;; init-ui
;; try ivy-rich
(require 'ivy-rich)
(ivy-rich-mode 1)

;; font
(set-face-attribute 'default nil :font "hack 12")
(setq face-font-rescale-alist '(("Source Han Mono SC" . 1)))

(dolist (charset '(han kana symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                        (font-spec :family "Source Han Mono SC")))

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
                                      ;; (kbd "C-i") 'evil-jump-forward
                                      (kbd "RET") 'org-ctrl-c-ctrl-c)

;; Features
(defun capture-new-file ()
  (let ((name (read-string "Name: ")))
    (expand-file-name (format "%s.org"
                              name) "~/inbox")))

(setq org-default-notes-file "~/notes/Warning.org")
(setq org-capture-templates
      '(("i" "inbox" entry
         (file capture-new-file)
         "* TODO %?")
        ("f" "foo" entry
         (file "")
         "* DONE %?\n  :PROPERTIES:\n  :ID:       %Y%-%m-%d\n  :END:")))

(setq org-todo-keywords (quote ((sequence "WAIT(w@/!)" "TODO(t)" "STRT(s)" "|" "DONE(d!/!)" "CANC(c@/!)"))))
(setq org-id-method 'ts)

;; org-roam
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-roam")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-roam/extensions/")
(load-library "org-roam")
(require 'org-roam-dailies)
(setq org-roam-directory "~/org-roam")
(setq org-roam-dailies-directory "daily")
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %? %i"
         :if-new (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d, %A>\n"))))
(org-roam-db-autosync-mode)

;;; init-term
(defun term-mode-hook-setup ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c") 'term-send-raw)
    (set-transient-map map (lambda () t))))

(add-hook 'term-mode-hook 'term-mode-hook-setup)

;; ;;; init-pair
;; (add-to-list 'load-path "~/.emacs.d/awesome-pair") ; add awesome-pair to your load-path
;; (require 'awesome-pair)
;; (dolist (hook (list
;;                'c-mode-common-hook
;;                'c-mode-hook
;;                'c++-mode-hook
;;                'java-mode-hook
;;                'haskell-mode-hook
;;                'emacs-lisp-mode-hook
;;                'lisp-interaction-mode-hook
;;                'lisp-mode-hook
;;                'maxima-mode-hook
;;                'ielm-mode-hook
;;                'sh-mode-hook
;;                'makefile-gmake-mode-hook
;;                'php-mode-hook
;;                'python-mode-hook
;;                'js-mode-hook
;;                'go-mode-hook
;;                'qml-mode-hook
;;                'jade-mode-hook
;;                'css-mode-hook
;;                'ruby-mode-hook
;;                'coffee-mode-hook
;;                'rust-mode-hook
;;                'qmake-mode-hook
;;                'lua-mode-hook
;;                'swift-mode-hook
;;                'minibuffer-inactive-mode-hook
;;                ))
;;   (add-hook hook '(lambda () (awesome-pair-mode 1))))
;; (define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
;; (define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
;; (define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
;; (define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
;; (define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
;; (define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
;; (define-key awesome-pair-mode-map (kbd "=") 'awesome-pair-equal)

;; (define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
;; (define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)

;; (define-key awesome-pair-mode-map (kbd "SPC") 'awesome-pair-space)
;; (define-key awesome-pair-mode-map (kbd "RET") 'awesome-pair-newline)

;; (define-key awesome-pair-mode-map (kbd "M-o") 'awesome-pair-backward-delete)
;; (define-key awesome-pair-mode-map (kbd "C-d") 'awesome-pair-forward-delete)
;; (define-key awesome-pair-mode-map (kbd "C-k") 'awesome-pair-kill)

;; (define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote)
;; (define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
;; (define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
;; (define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
;; (define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)

;; (define-key awesome-pair-mode-map (kbd "M-p") 'awesome-pair-jump-right)
;; (define-key awesome-pair-mode-map (kbd "M-n") 'awesome-pair-jump-left)
;; (define-key awesome-pair-mode-map (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline)
(provide 'init-setting)
