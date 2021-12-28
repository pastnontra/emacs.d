;; -*- coding: utf-8; lexical-binding: t; -*-


;;; init-default

(set-language-environment "UTF-8")
(setq sentence-end-double-space nil)
(setq word-wrap-by-category t)

(use-package recentf
  :ensure nil
  :init
  (add-to-list 'recentf-exclude "~/dailies")
  :config
  ;; move to custom?
  (setq recentf-filename-handlers
      (append '(abbreviate-file-name) recentf-filename-handlers))
  (recentf-mode 1))

(use-package helpful
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-at-point)
  :custom
  (helpful-max-buffers 1)
  (helpful-switch-buffer-function
        (lambda (buf) (if-let ((window (display-buffer-reuse-mode-window buf '((mode . helpful-mode)))))
                          ;; ensure the helpful window is selected for `helpful-update'.
                          (select-window window)
                        ;; line above returns nil if no available window is found
                        (pop-to-buffer buf))))
  :config
  ;; don't pop new window
  (defvar moon-helpful-history () "History of helpful, a list of buffers.")
  (advice-add #'helpful-update :around #'moon-helpful@helpful-update)
  (advice-add #'helpful--buffer :around (lambda (oldfunc &rest _)
                                          (let ((buf (apply oldfunc _)))
                                            (push buf moon-helpful-history)
                                            buf))))

(defun moon-helpful@helpful-update (oldfunc)
  "Insert back/forward buttons."
  (funcall oldfunc)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (insert-text-button "[back]"
                        'action (lambda (&rest _)
                                  (interactive)
                                  (moon-helpful-switch-to-buffer (current-buffer) 1)))
    (insert "  ")
    (insert-text-button "[forward]"
                        'action (lambda (&rest _)
                                  (interactive)
                                  (moon-helpful-switch-to-buffer (current-buffer)  -1)))
    (insert "\n\n")))

(defun moon-helpful-switch-to-buffer (buffer &optional offset)
  "Jump to last SYMBOL in helpful history, offset by OFFSET."
  (interactive)
  (require 'seq)
  (require 'cl-lib)
  (setq moon-helpful-history (seq-remove (lambda (buf) (not (buffer-live-p buf))) moon-helpful-history))
  (cl-labels ((find-index (elt lst)
                          (let ((idx 0)
                                (len (length lst)))
                            (while (and (not (eq elt (nth idx lst)))
                                        (not (eq idx len)))
                              (setq idx (1+ idx)))
                            (if (eq idx len)
                                nil
                              idx))))
    (let ((idx (+ (or offset 0) (find-index buffer moon-helpful-history))))
      (if (or (>= idx (length moon-helpful-history))
              (< idx 0))
          (message "No further history.")
        (switch-to-buffer (nth idx moon-helpful-history))))))

(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

(setq visible-bell nil
      ring-bell-function #'ignore)

(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(defun clear-messages-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (with-current-buffer "*Messages*"
      (erase-buffer))))


;;; init-os

(defun wsl-browse-url-xdg-open (url &optional ignored)
  (interactive (browse-url-interactive-arg "URL: "))
  (shell-command-to-string (concat "explorer.exe " url)))
(when *wsl* (advice-add #'browse-url-xdg-open :override #'wsl-browse-url-xdg-open))

;;; init-git

(require 'magit)
(defun magit-submodule-remove-force ()
  (interactive)
  (magit-submodule-remove (list (magit-read-module-path "Remove module")) "--force" nil))


;;; init-company

(use-package company
  :custom
  (company-idle-delay 0.5)
  :bind (:map company-active-map
              ("C-h" . "<backspace>")
              ("C-M-i" . company-complete)
              ;; seems override evil kbd
              ;; ("C-n" . company-select-next)
              ;; ("C-p" . company-select-previous)
              ("C-w" . evil-delete-backward-word)))


;;; init-ivy
;; TODO Clean code

(ivy-rich-mode 1)
(define-key ivy-minibuffer-map (kbd "C-h") (kbd "<backspace>"))
(define-key ivy-minibuffer-map (kbd "C-w") 'ivy-backward-kill-word)
;; Relative kbd
(define-key minibuffer-local-map (kbd "C-h") (kbd "<backspace>"))
(define-key minibuffer-local-map (kbd "C-w") 'ivy-backward-kill-word)

(with-eval-after-load 'counsel
  (setq ivy-initial-inputs-alist nil))


;;; init-lang

;; rime
(use-package rime
  :custom
  (when *is-a-mac*
    (rime-librime-root "/usr/local/share/librime/dist")
    (rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include"))
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-user-data-dir "~/.emacs.d/rime")
  (rime-posframe-properties
        (list :background-color "#333333"
              :foreground-color "#dcdccc"
              :font "Arial-14"
              :internal-border-width 10))
  (rime-posframe-style 'vertical)
  (rime-cursor "˰")
  :bind
  ("C-\\" . toggle-input-method)
  :config
  (setq rime-translate-keybindings '("C-h"
                                     "C-p"
                                     "C-n"
                                     "C-`"
                                     "C-k")))


;;; init-tex

(setq TeX-auto-save t)
(setq TeX-parse-self t)


;;; init-git

(require 'git-gutter)
(custom-set-variables '(git-gutter:modified-sign "*"))
(set-face-foreground 'git-gutter:modified "deep sky blue")
(set-face-foreground 'git-gutter:added "chartreuse")


;;; init-org

(use-package org
  :ensure nil
  :hook electric-pair-mode
  :custom
  (org-startup-indented t)
  (org-ellipsis " ▾")
  ;; (org-hide-emphasis-markers t)
  (org-todo-keywords (quote ((sequence "WAIT(w@/W!)" "TODO(t)" "STRT(s)" "|" "DONE(d!/!)" "CANC(c@/!)"))))
  (org-todo-keyword-faces
   '(("TODO" . "#2dc937")
     ("STRT" . "yellow")
     ("WAIT" . "#e7b416")
     ("CANC" . "#cc3232")))
  (org-default-notes-file "~/notes/Warning.org")
  (org-capture-templates
   '(("i" "inbox" entry
      (file capture-new-file)
      "* TODO %?")
     ("f" "foo" entry
      (file "")
      "* DONE %?\n  :PROPERTIES:\n  :ID:       %Y%-%m-%d\n  :END:")))
  (org-id-method 'ts)
  (org-modules nil)
  (org-fontify-quote-and-verse-blocks t)
  (org-n-level-faces 1)
  ;; (tooltip-use-echo-area (not tooltip-use-echo-area)) ;; WAIT obsolete?
  :custom-face
  ;; TODO Unable to load color "base"
  (org-level-1
   ((t (:foreground "base"))))
  (org-block-begin-line
   ((t (:background "#424242" :extend t))))
  (org-block
   ((t (:background "#424242" :extend t))))
  (org-block-end-line
   ((t (:background "#424242" :extend t))))
  (org-list-dt
   ((t (:bold t))))
  :config
  ;; https://stackoverflow.com/questions/1218238/how-to-make-part-of-a-word-bold-in-org-mode
  (setcar org-emphasis-regexp-components " \t('\"{[:alpha:]")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}\\")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  ;; copy from doom, see org-ctrl-c-ret after.
  ;; depended, doom-emacs/modules/lang/org/autoload/org-tables.el
  (defun +org/table-previous-row ()
    "Go to the previous row (same column) in the current table. Before doing so,
re-align the table if necessary. (Necessary because org-mode has a
`org-table-next-row', but not `org-table-previous-row')"
    (interactive)
    (org-table-maybe-eval-formula)
    (org-table-maybe-recalculate-line)
    (if (and org-table-automatic-realign
             org-table-may-need-update)
        (org-table-align))
    (let ((col (org-table-current-column)))
      (beginning-of-line 0)
      (when (or (not (org-at-table-p)) (org-at-table-hline-p))
        (beginning-of-line))
      (org-table-goto-column col)
      (skip-chars-backward "^|\n\r")
      (when (org-looking-at-p " ")
        (forward-char))))

  (defun +org-get-todo-keywords-for (&optional keyword)
    "Returns the list of todo keywords that KEYWORD belongs to."
    (when keyword
      (cl-loop for (type . keyword-spec)
               in (cl-remove-if-not #'listp org-todo-keywords)
               for keywords =
               (mapcar (lambda (x) (if (string-match "^\\([^(]+\\)(" x)
                                       (match-string 1 x)
                                     x))
                       keyword-spec)
               if (eq type 'sequence)
               if (member keyword keywords)
               return keywords)))

  (defun org-insert-object (direction)
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


  (defun org-insert-object-below (count)
    "Inserts a new heading, table cell or item below the current one."
    (interactive "p")
    (dotimes (_ count) (org-insert-object 'below)))

  (defun org-insert-object-above (count)
    "Inserts a new heading, table cell or item above the current one."
    (interactive "p")
    (dotimes (_ count) (org-insert-object 'above)))

  (defun capture-new-file ()
    (let ((name (read-string "Name: ")))
      (expand-file-name (format "%s.org"
                                name) "~/inbox"))))

;; Keybindings
(evil-define-key 'normal org-mode-map (kbd "j") 'evil-next-visual-line
                                      (kbd "k") 'evil-previous-visual-line
                                      ;; (kbd "C-i") 'evil-jump-forward
                                      (kbd "RET") 'org-ctrl-c-ctrl-c
                                      (kbd "C-<return>") 'org-insert-object-below ;; not in visual
                                      (kbd "C-S-<return>") 'org-insert-object-above) ;; shift opposite
(evil-define-key 'insert org-mode-map (kbd "C-<return>") 'org-insert-object-below
                                      (kbd "C-S-<return>") 'org-insert-object-above)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-todo-bullet-alist
   '(("TODO" . ?☐)
     ("[ ]"  . ?☐)
     ("DONE" . ?☑)
     ("[X]"  . ?☑)))
  (org-superstar-headline-bullets-list '(?•))
  (org-superstar-special-todo-items t)
  ;; TODO Bold item bullets.
  (org-superstar-item-bullet-alist
   '((?* . (?✤))
     (?+ . (?+))
     (?- . (?⁃))))
  (org-superstar-leading-bullet ?\s)
  (org-superstar-leading-fallback ?\s))

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
  (org-roam-db-autosync-mode))

(use-package calfw)
(use-package calfw-org
  :after calfw)


;;; init-term

(defun term-mode-hook-setup ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c") 'term-send-raw)
    (set-transient-map map (lambda () t))))

(add-hook 'term-mode-hook 'term-mode-hook-setup)


;; init-prog

(use-package quickrun
  :defer t
  :config
  (quickrun-set-default "c" "c/clang"))

(provide 'init-setting)
