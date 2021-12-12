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
(define-key company-active-map (kbd "C-w") 'evil-delete-backward-word))

;; Keybindings
(require 'ivy)
(define-key ivy-minibuffer-map (kbd "C-h") (kbd "<backspace>"))
(define-key ivy-minibuffer-map (kbd "C-w") 'ivy-backward-kill-word)
;; (defun isearch-backward-delete-word ()
;;   "Delete last char of the search string."
;;   (interactive)
;;   (unless
;;       (equal (substring isearch-string -1)
;;              (or)))
;;     (isearch-backward-delete-char)))

(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
;; (define-key isearch-mode-map (kbd "C-w") 'isearch-backward-delete-word)

;;; init-ui
;; init-ivy
(ivy-rich-mode 1)
(with-eval-after-load 'counsel
  (setq ivy-initial-inputs-alist nil))


;;; init-lang
;; rime

(use-package rime
  :ensure t
  :config
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

  (setq rime-cursor "˰"))

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
(use-package org-superstar
  :ensure t
  :after org
  ;; :hook org-mode will show: failed to define function org-superstar
  :hook (org-mode . (lambda () (org-superstar-mode 1)))
  :config
  (setq org-superstar-cycle-headline-bullets t)
  (setq org-hide-leading-stars t)
  (setq org-superstar-special-todo-items t)
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)
  (setq tooltip-use-echo-area (not tooltip-use-echo-area)))


;; Keybindings
(evil-define-key 'normal org-mode-map (kbd "j") 'evil-next-visual-line
                                      (kbd "k") 'evil-previous-visual-line
                                      ;; (kbd "C-i") 'evil-jump-forward
                                      (kbd "RET") 'org-ctrl-c-ctrl-c)

;; Features
(defun +org--insert-item (direction)
  (let ((context (org-element-lineage
                  (org-element-context)
                  '(table table-row headline inlinetask item plain-list)
                  t)))
    (pcase (org-element-type context)
      ;; Add a new list item (carrying over checkboxes if necessary)
      ((or `item `plain-list)
       ;; Position determines where org-insert-todo-heading and org-insert-item
       ;; insert the new list item.
       (if (eq direction 'above)
           (org-beginning-of-item)
         (org-end-of-item)
         (backward-char))
       (org-insert-item (org-element-property :checkbox context))
       ;; Handle edge case where current item is empty and bottom of list is
       ;; flush against a new heading.
       (when (and (eq direction 'below)
                  (eq (org-element-property :contents-begin context)
                      (org-element-property :contents-end context)))
         (org-end-of-item)
         (org-end-of-line)))

      ;; Add a new table row
      ((or `table `table-row)
       (pcase direction
         ('below (save-excursion (org-table-insert-row t))
                 (org-table-next-row))
         ('above (save-excursion (org-shiftmetadown))
                 (+org/table-previous-row))))

      ;; Otherwise, add a new heading, carrying over any todo state, if
      ;; necessary.
      (_
       (let ((level (or (org-current-level) 1)))
         ;; I intentionally avoid `org-insert-heading' and the like because they
         ;; impose unpredictable whitespace rules depending on the cursor
         ;; position. It's simpler to express this command's responsibility at a
         ;; lower level than work around all the quirks in org's API.
         (pcase direction
           (`below
            (let (org-insert-heading-respect-content)
              (goto-char (line-end-position))
              (org-end-of-subtree)
              (insert "\n" (make-string level ?*) " ")))
           (`above
            (org-back-to-heading)
            (insert (make-string level ?*) " ")
            (save-excursion (insert "\n"))))
         (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                     (todo-type    (org-element-property :todo-type context)))
           (org-todo
            (cond ((eq todo-type 'done)
                   ;; Doesn't make sense to create more "DONE" headings
                   (car (+org-get-todo-keywords-for todo-keyword)))
                  (todo-keyword)
                  ('todo)))))))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p)))
      (evil-insert 1))))


(defun +org/insert-item-below (count)
  "Inserts a new heading, table cell or item below the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'below)))

(defun +org/insert-item-above (count)
  "Inserts a new heading, table cell or item above the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'above)))



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

;; (setq org-todo-keywords (quote ((sequence "WAIT(w@/!)" "TODO(t)" "STRT(s)" "|" "DONE(d!/!)" "CANC(c@/!)"))))
(setq org-todo-keywords (quote ((sequence "WAIT(w@/W!)" "TODO(t)" "STRT(s)" "|" "DONE(d!/!)" "CANC(c@/!)"))))
(setq org-todo-keyword-faces
      '(("STRT" . "yellow")
        ("WAIT" . "orange")))

(setq org-id-method 'ts)

;; org-roam
(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org-roam")
  (org-roam-dailies-directory "~/dailies")
  :config
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %? %i"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d, %A>\n"))))
  (org-roam-db-autosync-mode)
)
;;; init-term
(defun term-mode-hook-setup ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c") 'term-send-raw)
    (set-transient-map map (lambda () t))))

(add-hook 'term-mode-hook 'term-mode-hook-setup)

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-calendar-want-org-bindings t
        evil-collection-want-unimpaired-p nil))

 (use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-define-key 'motion org-agenda-mode-map
    "q" 'org-agenda-quit)
  (evil-org-agenda-set-keys))

(use-package calfw)
(use-package calfw-org
  :after calfw)

(provide 'init-setting)
