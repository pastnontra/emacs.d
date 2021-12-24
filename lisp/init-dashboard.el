;; -*- coding: utf-8; lexical-binding: t; -*-
(defun icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
  (and (display-graphic-p)
       (require 'all-the-icons nil t)))

(use-package dashboard
  :diminish dashboard-mode
  :functions (all-the-icons-faicon
              all-the-icons-material
              winner-undo
              widget-forward)
  :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  :hook (dashboard-mode . (lambda () (setq-local frame-title-format nil)))
  :init
  (setq ;; dashboard-startup-banner nil
        dashboard-page-separator "\n\f\n"
        dashboard-center-content t
        dashboard-show-shortcuts nil
        dashboard-items '((agenda . 10)
                          (recents  . 10)
                          (bookmarks . 5)
                          ;; Not support ffip
                          ;; (projects . 5)
                          )
        dashboard-agenda-release-buffers t
        dashboard-set-init-info nil
        dashboard-set-file-icons t
        dashboard-set-heading-icons t
        dashboard-heading-icons '((recents   . "file-text")
                                  (bookmarks . "bookmark")
                                  (agenda    . "calendar")
                                  (projects  . "briefcase")
                                  (registers . "database"))
        dashboard-item-shortcuts '((recents . "r")
                                   (bookmarks . "m")
                                   (projects . "p")
                                   (agenda . "a")
                                   (registers . "e"))
        dashboard-set-footer t
        dashboard-footer-icon "")

  (dashboard-setup-startup-hook)
  :config
(defun dashboard-insert-footer ()
  "Insert footer of dashboard."
  (let ((footer (and dashboard-set-footer (dashboard-random-footer))))
    (when footer
      (insert "\n")
      (dashboard-center-line footer)
      (insert dashboard-footer-icon)
      (insert " ")
    (let ((init-info (if (functionp dashboard-init-info)
                         (funcall dashboard-init-info)
                       dashboard-init-info)))
      (insert (propertize init-info 'face 'font-lock-comment-face)))
      (insert "\n"))))
  ;; Remove buffer file name from agenda section https://github.com/emacs-dashboard/emacs-dashboard/issues/323
  ;; Use advice?
  (defun dashboard-agenda-entry-format ()
    "Format agenda entry to show it on dashboard."
    (let* ((schedule-time (org-get-scheduled-time (point)))
           (deadline-time (org-get-deadline-time (point)))
           (item (org-agenda-format-item
                  (dashboard-agenda-entry-time (or schedule-time deadline-time))
                  (org-get-heading)
                  (org-outline-level)
                  ""
                  (org-get-tags)
                  t))
           (loc (point))
           (file (buffer-file-name)))
      (dashboard-agenda--set-agenda-headline-face item)
      (list (string-trim item) loc file)))

  (defvar dashboard-recover-layout-p nil
    "Whether recovers the layout.")

  (defun restore-previous-session ()
    "Restore the previous session."
    (interactive)
    (when (bound-and-true-p persp-mode)
      (restore-session persp-auto-save-fname)))

  (defun restore-session (fname)
    "Restore the specified session."
    (interactive (list (read-file-name "Load perspectives from a file: "
                                       persp-save-dir)))
    (when (bound-and-true-p persp-mode)
      (message "Restoring session...")
      (quit-window t)
      (condition-case-unless-debug err
          (persp-load-state-from-file fname)
        (error "Error: Unable to restore session -- %s" err))
      (message "Restoring session...done")))

  (defun dashboard-goto-recent-files ()
    "Go to recent files."
    (interactive)
    (let ((func (local-key-binding "r")))
      (and func (funcall func))))

  (defun dashboard-goto-projects ()
    "Go to projects."
    (interactive)
    (let ((func (local-key-binding "p")))
      (and func (funcall func))))

  (defun dashboard-goto-bookmarks ()
    "Go to bookmarks."
    (interactive)
    (let ((func (local-key-binding "m")))
      (and func (funcall func))))

  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    ;; Check if need to recover layout
    (if (> (length (window-list-1))
           ;; exclude `treemacs' window
           (if (and (fboundp 'treemacs-current-visibility)
                    (eq (treemacs-current-visibility) 'visible))
               2
             1))
        (setq dashboard-recover-layout-p t))

    (delete-other-windows)

    ;; Refresh dashboard buffer
    (when (get-buffer dashboard-buffer-name)
      (kill-buffer dashboard-buffer-name))
    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name)

    ;; Jump to the first section
    (dashboard-goto-recent-files))

  (defun quit-dashboard ()
    "Quit dashboard window."
    (interactive)
    (quit-window t)
    (when (and dashboard-recover-layout-p
               (bound-and-true-p winner-mode))
      (winner-undo)
      (setq dashboard-recover-layout-p nil))))

(provide 'init-dashboard)
