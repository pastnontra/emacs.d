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

;; https://emacs-china.org/t/wsl-emacs-windows/18605/3?u=pastnontra
(defun my/browse-url-generic (url &optional _new-window)
  ;; new-window ignored
  "Ask the WWW browser defined by `browse-url-generic-program' to load URL.
Default to the URL around or before point.  A fresh copy of the
browser is started up in a new process with possible additional arguments
`browse-url-generic-args'.  This is appropriate for browsers which
don't offer a form of remote control."
  (interactive (browse-url-interactive-arg "URL: "))
  (if (not browse-url-generic-program)
      (error "No browser defined (`browse-url-generic-program')"))
  (apply 'call-process browse-url-generic-program nil
	 0 nil
	 (append browse-url-generic-args
                 (list (format "start %s"
                               (replace-regexp-in-string "&" "^&" url))))))

(when (and (eq system-type 'gnu/linux)
           (string-match
            "Linux.*Microsoft.*Linux"
            (shell-command-to-string "uname -a")))

  (setq browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args     '("/c")
        browse-url-browser-function #'my/browse-url-generic))

;;; init-git

;; (require 'magit)
(defun magit-submodule-remove-force ()
  (interactive)
  (magit-submodule-remove (list (magit-read-module-path "Remove module")) "--force" nil))


;;; init-company

(use-package company
  :custom
  ;; Need to change
  (company-idle-delay 0.5)
  :bind (:map company-active-map
              ("C-h" . [backspace])
              ("C-M-i" . company-complete)
              ;; seems override evil kbd
              ;; ("C-n" . company-select-next)
              ;; ("C-p" . company-select-previous)
              ("C-w" . evil-delete-backward-word)))

(use-package company-box
  :hook (company-mode . company-box-mode))


;;; init-ivy
;; TODO Clean code
(use-package ivy
  :bind (:map ivy-minibuffer-map
              ("C-h" . [backspace])
              ("C-w" . ivy-backward-kill-word)))

(use-package ivy-rich
  :after ivy
  :config
(ivy-rich-mode 1))
;; Relative kbd
(define-key minibuffer-local-map (kbd "C-h") (kbd "<backspace>"))
(define-key minibuffer-local-map (kbd "C-w") 'ivy-backward-kill-word)

(with-eval-after-load 'counsel
  (setq ivy-initial-inputs-alist nil))


;;; init-lang
(use-package sis
  :init
  ;; Default lists will slow down the speed of =C-h=.
  ;; Also can solve by set =sis-respect-prefix-and-buffer= to =nil=.
  (defvar sis-prefix-override-keys
    (list "C-c" "C-x"))
  :config
  (when *win64*
    (sis-ism-lazyman-config "1033" "2052" 'im-select))
  (when *wsl*
    (setq sis-english-source "1033")
    (setq sis-other-source "2052")
    (setq sis-do-get (lambda ()
                       (sis--ensure-dir
                        (string-trim (shell-command-to-string "im-select.exe")))))
    (setq sis-do-set (lambda(source)
                       (sis--ensure-dir
                        (call-process "/bin/bash" nil t nil "-c" (concat "im-select.exe " source)))))

    (setq sis-external-ism "im-select.exe"))
  (sis-global-respect-mode t)
  ;; Cause problem when insert a item after a empty item.
  ;; - | <-Cursor here
  ;; (sis-global-context-mode t)
  (sis-global-inline-mode t))

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
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))
(use-package flyspell-correct-ivy
  :after flyspell-correct)

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
  :hook
  (org-mode . electric-pair-mode)
  (org-mode . (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  (org-tab-first . +org-cycle-only-current-subtree-h)
  ;; (add-hook 'org-tab-first-hook
  ;;            #'+org-cycle-only-current-subtree-h)
  :custom
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-src-tab-acts-natively t)
  (org-src-fontify-natively t)
  (org-hide-emphasis-markers t)
  (org-ellipsis " ▾")
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
  (help-at-pt-display-when-idle t)
  (org-n-level-faces 1)
  ;; (tooltip-use-echo-area (not tooltip-use-echo-area)) ;; WAIT obsolete?
  :custom-face
  ;; TODO Unable to load color "base"
  (org-level-1
   ((t (:foreground "base"))))
  (org-block-begin-line
   ((t (:background "#333" :extend t))))
  (org-block
   ((t (:background "#333" :extend t))))
  (org-block-end-line
   ((t (:background "#333" :extend t))))
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
           ;; TODO This line Seems not work in my config, and after comment this, everything likely work fine.
           ;; (backward-char)
           )
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
                                name) "~/inbox")))

  (defun +org-cycle-only-current-subtree-h (&optional arg)
    "Toggle the local fold at the point, and no deeper.
`org-cycle's standard behavior is to cycle between three levels: collapsed,
subtree and whole document. This is slow, especially in larger org buffer. Most
of the time I just want to peek into the current subtree -- at most, expand
*only* the current subtree.
All my (performant) foldings needs are met between this and `org-show-subtree'
(on zO for evil users), and `org-cycle' on shift-TAB if I need it."
    (interactive "P")
    (unless (eq this-command 'org-shifttab)
      (save-excursion
        (org-beginning-of-line)
        (let (invisible-p)
          (when (and (org-at-heading-p)
                     (or org-cycle-open-archived-trees
                         (not (member org-archive-tag (org-get-tags))))
                     (or (not arg)
                         (setq invisible-p (outline-invisible-p (line-end-position)))))
            (unless invisible-p
              (setq org-cycle-subtree-status 'subtree))
            (org-cycle-internal-local)
            t))))))

;; Keybindings
(evil-define-key 'normal org-mode-map (kbd "j") 'evil-next-visual-line
                                      (kbd "k") 'evil-previous-visual-line
                                      ;; (kbd "C-i") 'evil-jump-forward
                                      (kbd "RET") 'org-ctrl-c-ctrl-c
                                      (kbd "C-<return>") 'org-insert-object-below ;; not in visual
                                      (kbd "C-S-<return>") 'org-insert-object-above) ;; shift opposite
(evil-define-key 'insert org-mode-map (kbd "C-<return>") 'org-insert-object-below
                                      (kbd "C-S-<return>") 'org-insert-object-above)

(use-package org-appear
  :hook (org-mode . org-appear-mode))

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
    '(;(?- . (?⁃))
      (?* . (?✤))
      (?+ . (?+))))
  (org-superstar-leading-bullet ?\s)
  (org-superstar-leading-fallback ?\s))


;; org-roam
(use-package org-roam
  :after org
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
  :after (org calfw))


;;; init-term

(use-package vterm
  :hook (vterm-mode . vterm-mode-hook-setup)
  :config
  ;; TODO Workaround, set C-c instead of prefix.
  (defun vterm-mode-hook-setup ()
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c") 'vterm-send-C-c)
      (set-transient-map map (lambda () t)))))


;;; init-prog

(use-package quickrun
  :defer t
  :config
  (quickrun-set-default "c" "c/clang"))

;; LSP
(use-package lsp-mode
  :custom
  ;; enable log only for debug
  (setq lsp-log-io nil)
  ;; use `evil-matchit' instead
  (setq lsp-enable-folding nil)
  ;; handle yasnippet by myself
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-symbol-highlighting t)
  ;; use find-fine-in-project instead
  (setq lsp-enable-links t)
  ;; auto restart lsp
  (setq lsp-restart 'auto-restart)
  :config
  (evil-define-key 'normal lsp-mode-map (kbd "gd") 'lsp-find-definition)
  (evil-define-key 'normal lsp-mode-map (kbd "gr") 'lsp-find-references)
  ;; no real time syntax check
  (setq lsp-diagnostic-package :none)
  ;; don't watch 3rd party javascript libraries
  (push "[/\\\\][^/\\\\]*\\.\\(json\\|html\\|jade\\)$" lsp-file-watch-ignored)
  ;; don't ping LSP language server too frequently
  (defvar lsp-on-touch-time 0)
  (defun my-lsp-on-change-hack (orig-fun &rest args)
    ;; do NOT run `lsp-on-change' too frequently
    (when (> (- (float-time (current-time))
                lsp-on-touch-time) 120) ;; 2 mins
      (setq lsp-on-touch-time (float-time (current-time)))
      (apply orig-fun args)))
  (advice-add 'lsp-on-change :around #'my-lsp-on-change-hack))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))


(provide 'init-setting)
