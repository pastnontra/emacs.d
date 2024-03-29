;; -*- coding: utf-8; lexical-binding: t; -*-

;; My frequently used commands are listed here

;; enable evil-mode
(evil-mode 1)

;; {{ replace undo-tree with undo-fu
;; @see https://github.com/emacs-evil/evil/issues/1074
;; copied from doom-emacs
(define-minor-mode undo-fu-mode
  "Enables `undo-fu' for the current session."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap undo] #'undo-fu-only-undo)
            (define-key map [remap redo] #'undo-fu-only-redo)
            (define-key map (kbd "C-_")     #'undo-fu-only-undo)
            (define-key map (kbd "M-_")     #'undo-fu-only-redo)
            (define-key map (kbd "C-M-_")   #'undo-fu-only-redo-all)
            (define-key map (kbd "C-x r u") #'undo-fu-session-save)
            (define-key map (kbd "C-x r U") #'undo-fu-session-recover)
            map)
  :init-value nil
  :global t)
(undo-fu-mode 1)
(define-key evil-normal-state-map "u" 'undo-fu-only-undo)
(define-key evil-normal-state-map "U" 'undo-fu-only-redo)
(define-key evil-normal-state-map (kbd "C-r") 'undo-fu-only-redo)
;; }}

;; Store more undo history to prevent loss of data
(setq undo-limit 8000000
      undo-strong-limit 8000000
      undo-outer-limit 8000000)

;; {{ @see https://github.com/timcharper/evil-surround for tutorial
(my-run-with-idle-timer 2 #'global-evil-surround-mode)
(with-eval-after-load 'evil-surround
  (defun evil-surround-prog-mode-hook-setup ()
    "Set up surround shortcuts."
    (cond
     ((memq major-mode '(sh-mode))
      (push '(?$ . ("$(" . ")")) evil-surround-pairs-alist))
     (t
      (push '(?$ . ("${" . "}")) evil-surround-pairs-alist)))

    (when (memq major-mode '(org-mode))
      (push '(?\[ . ("[[" . "]]")) evil-surround-pairs-alist) ; [
      (push '(?= . ("=" . "=")) evil-surround-pairs-alist))

    (when (memq major-mode '(emacs-lisp-mode))
      (push '(?\( . ("( " . ")")) evil-surround-pairs-alist)
      (push '(?` . ("`" . "'")) evil-surround-pairs-alist))

    (when (derived-mode-p 'js-mode)
      (push '(?> . ("(e) => " . "(e)")) evil-surround-pairs-alist))

    ;; generic
    (push '(?/ . ("/" . "/")) evil-surround-pairs-alist))
  (add-hook 'prog-mode-hook 'evil-surround-prog-mode-hook-setup))
;; }}

;; {{ For example, press `viW*`
(setq evil-visualstar/persistent t)
(my-run-with-idle-timer 2 #'global-evil-visualstar-mode)
;; }}

;; ffip-diff-mode (read only) evil setup
(defun ffip-diff-mode-hook-setup ()
  (evil-local-set-key 'normal "q" (lambda () (interactive) (quit-window t)))
  (evil-local-set-key 'normal (kbd "RET") 'ffip-diff-find-file)
  ;; "C-c C-a" is binding to `diff-apply-hunk' in `diff-mode'
  (evil-local-set-key 'normal "u" 'diff-undo)
  (evil-local-set-key 'normal "a" 'ffip-diff-apply-hunk)
  (evil-local-set-key 'normal "o" 'ffip-diff-find-file))
(add-hook 'ffip-diff-mode-hook 'ffip-diff-mode-hook-setup)

;; {{ define my own text objects, works on evil v1.0.9 using older method
;; @see http://stackoverflow.com/questions/18102004/emacs-evil-mode-how-to-create-a-new-text-object-to-select-words-with-any-non-sp
(defmacro my-evil-define-and-bind-text-object (key start-regex end-regex)
  (let* ((inner-name (make-symbol "inner-name"))
         (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

;; between equal signs
(my-evil-define-and-bind-text-object "=" "=" "=")
;; between pipe characters:
(my-evil-define-and-bind-text-object "|" "|" "|")
;; regular expression
(my-evil-define-and-bind-text-object "/" "/" "/")
;; trimmed line
(my-evil-define-and-bind-text-object "l" "^ *" " *$")
;; angular template
(my-evil-define-and-bind-text-object "r" "\{\{" "\}\}")
;; }}


;; {{ nearby file path as text object,
;;      - "vif" to select base name
;;      - "vaf" to select full path
;;
;;  example:
;;    "/hello/world"
;;    "/test/back.exe"
;;    "C:hello\\hello\\world\\test.exe"
;;    "D:blah\\hello\\world\\base.exe"
(defun my-evil-path-is-separator-char (ch)
  "Check ascii table that CH is slash characters.
If the character before and after CH is space or tab, CH is NOT slash"
  (let* (rlt prefix-ch postfix-ch)
    (when (and (> (point) (point-min)) (< (point) (point-max)))
      (save-excursion
        (backward-char)
        (setq prefix-ch (following-char)))
      (save-excursion
        (forward-char)
        (setq postfix-ch (following-char))))
    (if (and (not (or (= prefix-ch 32) (= postfix-ch 32)))
             (or (= ch 47) (= ch 92)) )
        (setq rlt t))
    rlt))

(defun my-evil-path-not-path-char (ch)
  "Check ascii table for character CH."
  (or (and (<= 0 ch) (<= ch 32))
      (memq ch
            '(34 ; double quotes
              ?'
              40 ; (
              41 ; )
              ?<
              ?>
              91 ; [
              93 ; ]
              ?`
              ?{
              ?}
              127))))

(defun my-evil-path-calculate-path (b e)
  (let* (rlt f)
    (when (and b e)
      (setq b (+ 1 b))
      (when (save-excursion
              (goto-char e)
              (setq f (my-evil-path-search-forward-char 'my-evil-path-is-separator-char t))
              (and f (>= f b)))
        (setq rlt (list b (+ 1 f) (- e 1)))))
    rlt))

(defun my-evil-path-get-path-already-inside ()
  (let* (b e)
    (save-excursion
      (setq b (my-evil-path-search-forward-char 'my-evil-path-not-path-char t)))
    (save-excursion
      (when (setq e (my-evil-path-search-forward-char 'my-evil-path-not-path-char))
        (goto-char (- e 1))
        ;; example: hello/world,
        (if (memq (following-char) '(?, ?.))
            (setq e (- e 1)))))
    (my-evil-path-calculate-path b e)))

(defun my-evil-path-search-forward-char (fn &optional backward)
  (let* (found
         rlt
         (limit (if backward (point-min) (point-max)))
         out-of-loop)
    (save-excursion
      (while (not out-of-loop)
        ;; for the char, exit
        (if (setq found (apply fn (list (following-char))))
            (setq out-of-loop t)
          ;; reach the limit, exit
          (if (= (point) limit)
              (setq out-of-loop t)
            ;; keep moving
            (if backward (backward-char) (forward-char)))))
      (if found (setq rlt (point))))
    rlt))

(defun my-evil-path-extract-region ()
  "Find the closest file path."
  (let* (rlt b f1 f2)
    (if (and (not (my-evil-path-not-path-char (following-char)))
             (setq rlt (my-evil-path-get-path-already-inside)))
        ;; maybe (point) is in the middle of the path
        t
      ;; need search forward AND backward to find the right path
      (save-excursion
        ;; path in backward direction
        (when (setq b (my-evil-path-search-forward-char #'my-evil-path-is-separator-char t))
          (goto-char b)
          (setq f1 (my-evil-path-get-path-already-inside))))
      (save-excursion
        ;; path in forward direction
        (when (setq b (my-evil-path-search-forward-char #'my-evil-path-is-separator-char))
          (goto-char b)
          (setq f2 (my-evil-path-get-path-already-inside))))
      ;; pick one path as the final result
      (cond
       ((and f1 f2)
        (if (> (- (point) (nth 2 f1)) (- (nth 0 f2) (point)))
            (setq rlt f2)
          (setq rlt f1)))
       (f1
        (setq rlt f1))
       (f2
        (setq rlt f2))))

    rlt))

(evil-define-text-object my-evil-path-inner-text-object (&optional count begin end type)
  "File name of nearby path"
  (let* ((selected-region (my-evil-path-extract-region)))
    (if selected-region
        (evil-range (nth 1 selected-region) (nth 2 selected-region) :expanded t))))

(evil-define-text-object my-evil-path-outer-text-object (&optional count begin end type)
  "Nearby path."
  (let* ((selected-region (my-evil-path-extract-region)))
    (when selected-region
      (evil-range (car selected-region) (+ 1 (nth 2 selected-region)) type :expanded t))))

(define-key evil-inner-text-objects-map "f" 'my-evil-path-inner-text-object)
(define-key evil-outer-text-objects-map "f" 'my-evil-path-outer-text-object)
;; }}

;; WAIT evil-escape seems useless if you don't want to use key sequence to escape.
;; - return normal state when emacs state
;; (define-key evil-emacs-state-map (kbd "<escape>") 'evil-force-normal-state)
;; ;; {{ https://github.com/syl20bnr/evil-escape
;; (global-set-key (kbd "<escape>") 'evil-escape)
;; (setq-default evil-escape-key-sequence nil)
;; (setq evil-escape-excluded-states '(motion-state))
;; (evil-escape-mode 1)
;; ;; }}


;; Move back the cursor one position when exiting insert mode
(setq evil-move-cursor-back t)

(define-key evil-normal-state-map "gh" 'beginning-of-defun)

;; As a general rule, mode specific evil leader keys started
;; with upper cased character or 'g' or special character except "=" and "-"
(evil-declare-key 'normal org-mode-map
  "gh" 'outline-up-heading
  "$" 'org-end-of-line ; smarter behavior on headlines etc.
  "^" 'org-beginning-of-line ; ditto
  "<" (lambda () (interactive) (org-demote-or-promote 1)) ; out-dent
  ">" 'org-demote-or-promote ; indent
  (kbd "TAB") 'org-cycle)

(evil-declare-key 'normal markdown-mode-map
  "gh" 'outline-up-heading
  (kbd "TAB") 'markdown-cycle)

;; ;; {{ specify major mode uses Evil (vim) NORMAL state or EMACS original state.

;; ;; You may delete this setup to use Evil NORMAL state always.
;; (dolist (p '((minibuffer-inactive-mode . emacs)
;;              (calendar-mode . emacs)
;;              (special-mode . emacs)
;;              (grep-mode . emacs)
;;              (Info-mode . emacs)
;;              (term-mode . insert)
;;              (sdcv-mode . emacs)
;;              (anaconda-nav-mode . emacs)
;;              (log-edit-mode . emacs)
;;              (vc-log-edit-mode . emacs)
;;              (magit-log-edit-mode . emacs)
;;              (erc-mode . emacs)
;;              (neotree-mode . emacs)
;;              (w3m-mode . emacs)
;;              (gud-mode . emacs)
;;              (help-mode . emacs)
;;              (eshell-mode . emacs)
;;              (shell-mode . emacs)
;;              (xref--xref-buffer-mode . emacs)
;;              ;;(message-mode . emacs)
;;              (epa-key-list-mode . emacs)
;;              (fundamental-mode . emacs)
;;              (weibo-timeline-mode . emacs)
;;              (weibo-post-mode . emacs)
;;              (woman-mode . emacs)
;;              (sr-mode . emacs)
;;              (profiler-report-mode . emacs)
;;              (dired-mode . emacs)
;;              (compilation-mode . emacs)
;;              (speedbar-mode . emacs)
;;              (ivy-occur-mode . emacs)
;;              (ffip-file-mode . emacs)
;;              (ivy-occur-grep-mode . normal)
;;              (messages-buffer-mode . normal)
;;              (js2-error-buffer-mode . emacs)))
;;   (evil-set-initial-state (car p) (cdr p)))
;; ;; }}

;; I prefer Emacs way after pressing ":" in evil-mode
(define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-ex-completion-map (kbd "C-b") 'backward-char)

(define-key evil-normal-state-map "Y" (kbd "y$"))
;; (define-key evil-normal-state-map (kbd "RET") 'ivy-switch-buffer-by-pinyin) ; RET key is preserved for occur buffer
(define-key evil-normal-state-map "go" 'goto-char)
(define-key evil-normal-state-map (kbd "C-]") 'counsel-etags-find-tag-at-point)
(define-key evil-visual-state-map (kbd "C-]") 'counsel-etags-find-tag-at-point)
(define-key evil-insert-state-map (kbd "C-x C-n") 'evil-complete-next-line)
(define-key evil-insert-state-map (kbd "C-x C-p") 'evil-complete-previous-line)
(define-key evil-insert-state-map (kbd "C-]") 'aya-expand)
(define-key evil-motion-state-map (kbd "<escape>") 'nil)

(defun my-search-defun-from-pos (search pos)
  (evil-search search t t pos)
  ;; ignore this.f1 = this.fn.bind(this) code
  (when (and (memq major-mode '(js-mode js2-mode rjsx-mode))
             (string-match-p "^[ \t]*this\.[a-zA-Z0-9]+[ \t]*=[ \t]*this\.[a-zA-Z0-9]*\.bind(this);"
                             (my-line-str)))

    (forward-line 1)
    (evil-search search t t (point))))

;; "gd" or `evil-goto-definition' now use `imenu', `xref' first,
;; BEFORE searching string from `point-min'.
;; xref part is annoying because I already use `counsel-etags' to search tag.
(evil-define-motion my-evil-goto-definition ()
  "Go to local definition or first occurrence of symbol under point in current buffer."
  :jump t
  :type exclusive
  (let* ((string (evil-find-symbol t))
         (search (format "\\_<%s\\_>" (regexp-quote string)))
         ientry ipos)
    ;; load imenu if available
    (my-ensure 'imenu)

    (if (null string)
        (user-error "No symbol under cursor")
      (setq isearch-forward t)

      (my-ensure 'counsel-etags)
      (counsel-etags-push-marker-stack)

      ;; if imenu is available, try it
      (cond
       ((and (derived-mode-p 'js2-mode)
             (or (null (get-text-property (point) 'face))
                 (font-belongs-to (point) '(rjsx-tag))))
        (js2-jump-to-definition))
       ((fboundp 'imenu--make-index-alist)
        (condition-case nil
            (setq ientry (imenu--make-index-alist))
          (error nil))
        (setq ientry (assoc string ientry))
        (setq ipos (cdr ientry))
        (when (and (markerp ipos)
                   (eq (marker-buffer ipos) (current-buffer)))
          (setq ipos (marker-position ipos)))
        ;; imenu found a position, so go there and
        ;; highlight the occurrence
        (my-search-defun-from-pos search (if (numberp ipos) ipos (point-min))))
       ;; otherwise just go to first occurrence in buffer
       (t
        (my-search-defun-from-pos search (point-min)))))))

;; Use below line to restore old vim "gd"
;; (define-key evil-normal-state-map "gd" 'my-evil-goto-definition)

;; I learn this trick from ReneFroger, need latest expand-region
;; @see https://github.com/redguardtoo/evil-matchit/issues/38
(define-key evil-visual-state-map (kbd "v") 'er/expand-region)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up) ; evil-want-u-scorll
(define-key evil-insert-state-map (kbd "C-u") 'evil-delete-back-to-indentation)
;; It's not the final version for indent
(define-key evil-insert-state-map (kbd "<backtab>") 'evil-shift-left-line)
(define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-d") 'delete-char)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C-h") (kbd "<backspace>"))
(setq evil-want-Y-yank-to-eol t)
;; https://emacs.stackexchange.com/questions/9631/what-is-the-difference-between-tab-and-tab
;; (define-key evil-insert-state-map (kbd "TAB") 'yas-expand)
;; (define-key evil-insert-state-map (kbd "<tab>") 'yas-expand)
;; (define-key evil-emacs-state-map (kbd "TAB") 'yas-expand)
;; (define-key evil-emacs-state-map (kbd "<tab>") 'yas-expand)

;; {{
(defvar evil-global-markers-history nil)
(defun my-evil-set-marker-hack (char &optional pos advance)
  "Place evil marker's position into history."
  (unless pos (setq pos (point)))
  ;; only remember global markers
  (when (and (>= char ?A) (<= char ?Z) buffer-file-name)
    (setq evil-global-markers-history
          (delq nil
                (mapcar `(lambda (e)
                           (unless (string-match (format "^%s@" (char-to-string ,char)) e)
                             e))
                        evil-global-markers-history)))
    (setq evil-global-markers-history
          (add-to-list 'evil-global-markers-history
                       (format "%s@%s:%d:%s"
                               (char-to-string char)
                               (file-truename buffer-file-name)
                               (line-number-at-pos pos)
                               (string-trim (my-line-str)))))))
(advice-add 'evil-set-marker :before #'my-evil-set-marker-hack)

(defun my-evil-goto-mark-line-hack (orig-func &rest args)
  "Place line marker into history."
  (let* ((char (nth 0 args))
         (orig-pos (point)))
    (condition-case nil
        (apply orig-func args)
      (error (progn
               (when (and (eq orig-pos (point)) evil-global-markers-history)
                 (let* ((markers evil-global-markers-history)
                        (i 0)
                        m
                        file
                        found)
                   (while (and (not found) (< i (length markers)))
                     (setq m (nth i markers))
                     (when (string-match (format "\\`%s@\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'"
                                                 (char-to-string char))
                                         m)
                       (setq file (match-string-no-properties 1 m))
                       (setq found (match-string-no-properties 2 m)))
                     (setq i (1+ i)))
                   (when file
                     (find-file file)
                     (counsel-etags-forward-line found)))))))))
(advice-add 'evil-goto-mark-line :around #'my-evil-goto-mark-line-hack)

(defun counsel-evil-goto-global-marker ()
  "Goto global evil marker."
  (interactive)
  (my-ensure 'counsel-etags)
  (ivy-read "Goto global evil marker"
            evil-global-markers-history
            :action (lambda (m)
                      (when (string-match "\\`[A-Z]@\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" m)
                        (let* ((file (match-string-no-properties 1 m))
                               (linenum (match-string-no-properties 2 m)))
                          ;; item's format is like '~/proj1/ab.el:39: (defun hello() )'
                          (counsel-etags-push-marker-stack (point-marker))
                          ;; open file, go to certain line
                          (find-file file)
                          (counsel-etags-forward-line linenum))
                        ;; flash, Emacs v25 only API
                        (xref-pulse-momentarily)))))
;; }}

(local-require 'general)
(general-evil-setup t)

;; {{
(evil-define-text-object my-evil-a-statement (count &optional beg end type)
  "Select a statement."
  (list (my-skip-white-space (line-beginning-position) 1)
        (line-end-position)))

(evil-define-text-object my-evil-inner-statement (count &optional beg end type)
  "Select inner statement."
  (let* ((b (my-skip-white-space (line-beginning-position) 1))
         (e (line-end-position)))
    (list (save-excursion
            (goto-char b)
            (while (and (< (point) e) (not (eq (following-char) 61)))
              (forward-char))
            (cond
             ((eq (point) e)
              b)
             (t
              ;; skip '=' at point
              (goto-char (my-skip-white-space (1+ (point)) 1))
              (point))))
          (cond
           ((eq (char-before e) 59) ; ";"
            (my-skip-white-space (1- e) -1))
           (t
            e)))))

(define-key evil-outer-text-objects-map "v" #'my-evil-a-statement)
(define-key evil-inner-text-objects-map "v" #'my-evil-inner-statement)
;; }}

;; {{ I select string inside single quote frequently
(defun my-single-or-double-quote-range (count beg end type inclusive)
  "Get maximum range of single or double quote text object.
If INCLUSIVE is t, the text object is inclusive."
  (let* ((s-range (evil-select-quote ?' beg end type count inclusive))
         (d-range (evil-select-quote ?\" beg end type count inclusive))
         (beg (min (nth 0 s-range) (nth 0 d-range)))
         (end (max (nth 1 s-range) (nth 1 d-range))))
    (setf (nth 0 s-range) beg)
    (setf (nth 1 s-range) end)
    s-range))

(evil-define-text-object my-evil-a-single-or-double-quote (count &optional beg end type)
  "Select a single-quoted expression."
  :extend-selection t
  (my-single-or-double-quote-range count beg end type t))

(evil-define-text-object my-evil-inner-single-or-double-quote (count &optional beg end type)
  "Select 'inner' single-quoted expression."
  :extend-selection nil
  (my-single-or-double-quote-range count beg end type nil))

(define-key evil-outer-text-objects-map "i" #'my-evil-a-single-or-double-quote)
(define-key evil-inner-text-objects-map "i" #'my-evil-inner-single-or-double-quote)
;; }}

;; {{ use `,` as leader key
(general-create-definer my-comma-leader-def
  :prefix ","
  :states '(normal visual))

(defvar my-web-mode-element-rename-previous-tag nil
  "Used by my-rename-thing-at-point.")

(defun my-detect-new-html-tag (flag)
  (cond
   ((eq flag 'pre)
    (message "str=%s" (buffer-string (line-beginning-position) (line-end-position))))
   ((eq flag 'post)
    (message "str=%s" (buffer-string (line-beginning-position) (line-end-position))))))
(push '(rename-html-tag my-detect-new-html-tag) evil-repeat-types)
(evil-set-command-property #'web-mode-element-rename :repeat 'rename-html-tag)

(defun my-rename-thing-at-point (&optional n)
  "Rename thing at point.
If N > 0 and working on HTML, repeating previous tag name operation.
If N > 0 and working on javascript, only occurrences in current N lines are renamed."
  (interactive "P")
  (cond
   ((eq major-mode 'web-mode)
    (unless (and n my-web-mode-element-rename-previous-tag)
      (setq my-web-mode-element-rename-previous-tag (read-string "New tag name? ")))
    (web-mode-element-rename my-web-mode-element-rename-previous-tag))

   ((derived-mode-p 'js2-mode)
    ;; use `js2-mode' parser, much smarter and works in any scope
    (js2hl-rename-thing-at-point n))

   (t
    ;; simple string search/replace in function scope
    (evilmr-replace-in-defun))))

(defun my-beautify-code (&optional indent-offset)
  "Beautify code using INDENT-OFFSET."
  (interactive "P")
  (cond
   ((derived-mode-p 'js-mode)
    (my-js-beautify indent-offset))

   ((derived-mode-p 'python-mode)
    (when (and (boundp 'elpy-enabled-p) elpy-enabled-p))
    (elpy-format-code))

   (t
    (message "Can only beautify code written in python/javascript"))))

(my-comma-leader-def
  ;; "," 'evilnc-comment-operator
  ;; "bf" 'beginning-of-defun
  ;; "bu" 'backward-up-list
  ;; ;; "bb" (lambda () (interactive) (switch-to-buffer nil)) ; to previous buffer
  ;; "ef" 'end-of-defun
  "m" 'evil-set-marker
  "em" 'shellcop-erase-buffer
  ;; "eb" 'eval-buffer
  "sc" 'scratch
  ;; "ee" 'eval-'copy-to-x-clipboard ; used frequently
  "aw" 'ace-swap-window
  "af" 'ace-maximize-window
  "ac" 'aya-create
  ;; "pp" 'paste-from-x-clipboard ; used frequently
  "bs" '(lambda () (interactive) (goto-edge-by-comparing-font-face -1))
  "es" 'goto-edge-by-comparing-font-face
  "vj" 'my-validate-json-or-js-expression
  "kc" 'kill-ring-to-clipboard
  "fn" 'cp-filename-of-current-buffer
  "fp" 'cp-fullpath-of-current-buffer
  ;; "dj" 'dired-jump ;; open the dired from current file
  ;; "xo" 'ace-window
  ;; "ff" 'my-toggle-full-window ;; I use WIN+F in i3
  "ip" 'find-file-in-project
  ;; "tt" 'find-file-in-current-directory
  ;; "jj" 'find-file-in-project-at-point
  ;; "kk" 'find-file-in-project-by-selected
  "kn" 'find-file-with-similar-name ; ffip v5.3.1
  "fd" 'find-directory-in-project-by-selected
  "trm" 'get-term
  ;; "tff" 'toggle-frame-fullscreen
  ;; "tfm" 'toggle-frame-maximized
  "ti" 'fastdef-insert
  "th" 'fastdef-insert-from-history
  ;; "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'my-evilnc-comment-or-uncomment-paragraphs
  "ct" 'evilnc-comment-or-uncomment-html-tag ; evil-nerd-commenter v3.3.0 required
  "ic" 'my-imenu-comments
  "rv" 'my-rename-thing-at-point
  "nm" 'js2hl-add-namespace-to-thing-at-point
  ;; "rb" 'evilmr-replace-in-buffer
  "ts" 'evilmr-tag-selected-region ;; recommended
  "rt" 'counsel-etags-recent-tag
  "ft" 'counsel-etags-find-tag
  "yy" 'counsel-browse-kill-ring
  "cf" 'counsel-grep ; grep current buffer
  ;; "gf" 'counsel-git ; find file
  ;; "gg" 'my-counsel-git-grep ; quickest grep should be easy to press
  "gd" 'ffip-show-diff-by-description ;find-file-in-project 5.3.0+
  "vv" 'my-evil-goto-definition ; frequently used
  "sh" 'my-select-from-search-text-history
  "rjs" 'run-js
  "jsr" 'js-comint-send-region
  "jsb" 'my-js-clear-send-buffer
  ;; "kb" 'kill-buffer-and-window ;; "k" is preserved to replace "C-g"
  "ls" 'highlight-symbol
  "lq" 'highlight-symbol-query-replace
  "ln" 'highlight-symbol-nav-mode ; use M-n/M-p to navigation between symbols
  "ii" 'my-imenu-or-list-tag-in-current-file
  ;; @see https://github.com/pidu/git-timemachine
  ;; p: previous; n: next; w:hash; W:complete hash; g:nth version; q:quit
  ;; "tm" 'my-git-timemachine
  ;; toggle overview,  @see http://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
  ;; "op" 'compile
  "c$" 'org-archive-subtree ; `C-c $'
  ;; org-do-demote/org-do-premote support selected region
  "c<" 'org-do-promote ; `C-c C-<'
  "c>" 'org-do-demote ; `C-c C->'
  "cam" 'org-tags-view ; `C-c a m': search items in org-file-apps by tag
  "cxi" 'org-clock-in ; `C-c C-x C-i'
  "cxo" 'org-clock-out ; `C-c C-x C-o'
  "cxr" 'org-clock-report ; `C-c C-x C-r'
  "qq" 'my-multi-purpose-grep
  "dd" 'counsel-etags-grep-current-directory ; it seems can't work
  "dc" 'my-grep-pinyin-in-current-directory
  ;; "rr" 'my-counsel-recentf
  "da" 'diff-lisp-mark-selected-text-as-a
  "db" 'diff-lisp-diff-a-and-b
  "di" 'evilmi-delete-items
  "si" 'evilmi-select-items
  "jb" 'my-beautify-code
  "jp" 'my-print-json-path
  ;; {{ @see http://ergoemacs.org/emacs/emacs_pinky_2020.html
  ;; `keyfreq-show' proved sub-window operations happen most.
  ;; "x0" 'delete-window
  ;; "x1" 'delete-other-windows
  ;; "x2" 'split-window-vertically
  ;; "x3" 'split-window-horizontally
  ;; "xq" 'delete-window
  ;; "xa" 'split-window-vertically
  ;; "xd" 'split-window-horizontally
  ;; "s0" 'delete-window
  ;; "s1" 'delete-other-windows
  ;; "s2" 'split-window-vertically
  ;; "s3" 'split-window-horizontally
  ;; "sq" 'delete-window
  ;; "sa" 'split-window-vertically
  ;; "sd" 'split-window-horizontally
  ;; "oo" 'delete-other-windows
  ;; }}
  "cr" 'my-window-setup
  ;; "uu" 'my-transient-winner-undo
  "fs" 'ffip-save-ivy-last
  "fr" 'ivy-resume
  "ss" 'my-swiper
  "sf" 'shellcop-search-in-shell-buffer-of-other-window
  "fb" '(lambda ()
          (interactive)
          (my-ensure 'wucuo)
          (let* ((wucuo-flyspell-start-mode "normal"))
            (wucuo-spell-check-internal)))
  ;; "fe" 'flyspell-goto-next-error
  ;; "fa" 'flyspell-auto-correct-word
  "lb" 'langtool-check-buffer
  "ll" 'langtool-goto-next-error
  "pe" 'flymake-goto-prev-error
  "ne" 'flymake-goto-next-error
  "og" 'org-agenda
  "otl" 'org-toggle-link-display
  "oa" '(lambda ()
          (interactive)
          (my-ensure 'org)
          (counsel-org-agenda-headlines))
  "ar" 'align-regexp
  "wrn" 'httpd-restart-now
  "wrd" 'httpd-restart-at-default-directory
  "bk" 'buf-move-up
  "bj" 'buf-move-down
  "bh" 'buf-move-left
  "bl" 'buf-move-right
  ;; "0" 'winum-select-window-0-or-10
  ;; "1" 'winum-select-window-1
  ;; "2" 'winum-select-window-2
  ;; "3" 'winum-select-window-3
  ;; "4" 'winum-select-window-4
  ;; "5" 'winum-select-window-5
  ;; "6" 'winum-select-window-6
  ;; "7" 'winum-select-window-7
  ;; "8" 'winum-select-window-8
  ;; "9" 'winum-select-window-9
  ;; "xm" 'counsel-M-x
  ;; "xx" 'er/expand-region
  ;; "xf" 'counsel-find-file
  ;; "xb" 'ivy-switch-buffer-by-pinyin
  "xh" 'mark-whole-buffer
  "xk" 'kill-buffer
  "xs" 'save-buffer
  ;; "xc" 'my-switch-to-shell
  ;; "xz" 'my-switch-to-shell
  "vf" 'vc-rename-file-and-buffer
  "vc" 'vc-copy-file-and-rename-buffer
  "xv" 'vc-next-action ; 'C-x v v' in original
  "va" 'git-add-current-file
  "vk" 'git-checkout-current-file
  "vg" 'vc-annotate ; 'C-x v g' in original
  "vm" 'vc-msg-show
  "v=" 'git-gutter:popup-hunk
  "hh" 'cliphist-paste-item
  "yu" 'cliphist-select-item
  "ih" 'my-goto-git-gutter ; use ivy-mode
  "ir" 'ivy-resume
  ;; "ww" 'narrow-or-widen-dwim
  "ycr" 'my-yas-reload-all
  "wf" 'popup-which-function)
;; }}

;; {{ Use `SPC` as leader key
;; all keywords arguments are still supported
(general-create-definer my-space-leader-def
  :prefix "SPC"
  :states '(normal visual))

(general-create-definer my-local-leader-def
  ;; :prefix my-local-leader
  :prefix "SPC m")

(my-space-leader-def
  "SPC" 'counsel-M-x
  "TAB" (lambda () (interactive) (switch-to-buffer nil)) ; last buffer

  "b" '(:ignore t :which-key "buffers")
  "bb" 'ivy-switch-buffer-by-pinyin
  "bk" 'kill-buffer-and-window ;; "k" is preserved to replace "C-g"

  "f" '(:ignore t :which-key "files")
  "fd" 'find-file-in-current-directory
  "ff" 'counsel-find-file
  "fg" 'counsel-git ; find file
  "fj" 'dired-jump
  "fp" 'find-file-in-project-at-point
  "fr" 'counsel-recentf
  "fs" 'find-file-in-project-by-selected

  "g" '(:ignore t :which-key "git")
  "gg" 'my-counsel-git-grep
  ;; @see https://github.com/pidu/git-timemachine
  ;; p: previous; n: next; w:hash; W:complete hash; g:nth version; q:quit
  "gm" 'my-git-timemachine

  "h" '(:ignore t :which-key "help")
  "hb" 'describe-bindings
  "hc" 'helpful-command
  "hF" 'helpful-function
  "hf" 'helpful-callable
  "hh" 'helpful-at-point
  "hk" 'helpful-key
  "hl" 'view-lossage
  "hm" 'describe-mode
  "hv" 'helpful-variable

  "j" '(:ignore t :which-key "jump")
  "jb" 'beginning-of-defun
  "je" 'end-of-defun
  "ju" 'backward-up-list

  "n" '(:ignore t :which-key "note")
  "nc" 'org-roam-capture
  "nf" 'org-roam-node-find
  "ni" 'org-roam-node-insert

  "o" '(:ignore t :which-key "open")
  "oa" 'org-agenda
  "od" 'org-roam-dailies-capture-today
  "og" 'org-roam-dailies-goto-today
  "ot" 'vterm

  "q" '(:ignore t :which-key "quit")
  "qq" 'save-buffers-kill-emacs

  "t" '(:ignore t :which-key "toggle")
  "tt" 'random-healthy-color-theme
  "tn" 'narrow-or-widen-dwim

  "w" '(:ignore t :which-key "windows")
  ;: TODO winum-select-window-1..9
  "1" '(winum-select-window-1 :which-key t)
  "2" '(winum-select-window-2 :which-key t)
  "3" '(winum-select-window-3 :which-key t)
  "4" '(winum-select-window-4 :which-key t)
  "5" '(winum-select-window-5 :which-key t)
  "6" '(winum-select-window-6 :which-key t)
  "7" '(winum-select-window-7 :which-key t)
  "8" '(winum-select-window-8 :which-key t)
  "9" '(winum-select-window-9 :which-key t)
  "wf" 'my-toggle-full-window
  "wg" 'split-window-horizontally
  "wv" 'split-window-vertically
  "wh" 'evil-window-left
  "wl" 'evil-window-right
  "wk" 'evil-window-up
  "wj" 'evil-window-down
  "wo" 'delete-other-windows
  "wq" 'delete-window ; I dislike this keybinding
  "wu" 'my-transient-winner-undo
  ;; "wr" resize use a hydra menu
  ;; "w=" recover the original status layout
  "ww" 'evil-window-next
  "w TAB" 'alternate-window

  "r" 'evilmr-replace-in-buffer

  "s" '(:ignore t :which-key "system")
  "sc" 'copy-to-x-clipboard
  "sv" 'paste-from-x-clipboard

  "y" 'hydra-launcher/body)

;; TODO Display org in mode
(my-local-leader-def
  :states '(normal visual)
  :keymaps '(prog-mode-map)
  "c" 'my-compile
  "r" '(:ignore t :which-key "quickrun")
  "rr" 'quickrun)

(my-local-leader-def
  :states '(normal visual)
  :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
  "" '(:ignore t :which-key "elisp")
  "a" 'eval-buffer-list
  "e" '(:ignore nil :which-key "eval")
  "eb" 'eval-buffer
  "ed" 'eval-defun
  "ee" 'eval-last-sexp
  "eE" 'eval-expression)

(my-local-leader-def
  :states '(normal visual)
  :keymaps '(org-mode-map)
  "" '(:ignore t :which-key "org")
  "b" '(:ignore nil :which-key "org-babel")
  "bt" 'org-babel-tangle
  "rb" 'org-roam-buffer-toggle
  "d" 'org-deadline
  "s" 'org-schedule
  "t" 'org-todo)

;; Please check "init-ediff.el" which contains `my-space-leader-def' code too
(my-comma-leader-def
  "SPC n" (lambda ()
            (interactive)
            (if (derived-mode-p 'diff-mode) (my-search-next-diff-hunk)
              (my-search-next-merge-conflict)))
  "SPC p" (lambda ()
            (interactive)
            (if (derived-mode-p 'diff-mode) (my-search-prev-diff-hunk)
              (my-search-prev-merge-conflict)))
  "SPC ch" 'my-dired-redo-from-commands-history
  "SPC dd" 'pwd
  "SPC mm" 'counsel-evil-goto-global-marker
  "SPC mf" 'mark-defun
  "SPC xc" 'save-buffers-kill-terminal ; not used frequently
  "SPC cc" 'my-dired-redo-last-command
  "SPC ss" 'wg-create-workgroup ; save windows layout
  "SPC sc" 'shell-command
  "SPC ll" 'wg-open-workgroup ; load windows layout

  "SPC jj" 'scroll-other-window
  "SPC kk" 'scroll-other-window-up
  ;; "SPC hh" 'random-healthy-color-theme
  ;; "SPC yy" 'hydra-launcher/body
  "SPC yy" 'my-hydra-zoom/body
  "SPC ii" 'my-toggle-indentation
  "SPC g" 'my-hydra-git/body
  "SPC ur" 'gud-remove
  "SPC ub" 'gud-break
  "SPC uu" 'gud-run
  "SPC up" 'gud-print
  "SPC un" 'gud-next
  "SPC us" 'gud-step
  "SPC ui" 'gud-stepi
  "SPC uc" 'gud-cont
  "SPC uf" 'gud-finish)

;; {{ Use `;` as leader key, for searching something
(general-create-definer my-semicolon-leader-def
  :prefix ";"
  :states '(normal visual))

(my-semicolon-leader-def
  ;; Search character(s) at the beginning of word
  ;; See https://github.com/abo-abo/avy/issues/70
  ;; You can change the avy font-face in ~/.custom.el:
  ;;  (with-eval-after-load 'avy
  ;;    (set-face-attribute 'avy-lead-face-0 nil :foreground "black")
  ;;    (set-face-attribute 'avy-lead-face-0 nil :background "#f86bf3"))
  ";" 'ace-pinyin-jump-char-2
  "f" 'my-open-pdf-from-history
  "w" 'avy-goto-word-or-subword-1
  ;; "a" 'avy-goto-char-timer
  "db" 'sdcv-search-input ; details
  "dt" 'sdcv-search-input+ ; summary
  "dd" 'my-lookup-dict-org
  "mm" 'lookup-doc-in-man
  ;; "gg" 'w3m-google-search
  ;; "gd" 'w3m-search-financial-dictionary
  ;; "ga" 'w3m-java-search
  ;; "gh" 'w3mext-hacker-search ; code search in all engines with firefox
  ;; "gq" 'w3m-stackoverflow-search
  )
;; }}

;; {{ remember what we searched
;; http://emacs.stackexchange.com/questions/24099/how-to-yank-text-to-search-command-after-in-evil-mode/
(defvar my-search-text-history nil "List of text I searched.")
(defun my-select-from-search-text-history ()
  "My select the history of text searching."
  (interactive)
  (ivy-read "Search text history:" my-search-text-history
            :action (lambda (item)
                      (copy-yank-str item)
                      (message "%s => clipboard & yank ring" item))))

(defun my-cc-isearch-string (&rest args)
  "Add `isearch-string' inot history."
  (and isearch-string
       (> (length isearch-string) 0)
       (push isearch-string my-search-text-history)))
(advice-add 'evil-search-incrementally :after #'my-cc-isearch-string)
(advice-add 'evil-search-word :after #'my-cc-isearch-string)
(advice-add 'evil-visualstar/begin-search :after #'my-cc-isearch-string)
;; }}

;; ;; {{ change mode-line color by evil state
;; (defconst my-default-color (cons (face-background 'mode-line)
;;                                  (face-foreground 'mode-line)))
;; (defun my-show-evil-state ()
;;   "Change mode line color to notify user evil current state."
;;   (let* ((color (cond ((minibufferp) my-default-color)
;;                       ((evil-insert-state-p) '("#e80000" . "#ffffff"))
;;                       ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
;;                       ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
;;                       (t my-default-color))))
;;     (set-face-background 'mode-line (car color))
;;     (set-face-foreground 'mode-line (cdr color))))
;; (add-hook 'post-command-hook #'my-show-evil-state)
;; ;; }}

;; {{ evil-nerd-commenter
(my-run-with-idle-timer 2 #'evilnc-default-hotkeys)
(define-key evil-motion-state-map "gc" 'evilnc-comment-operator) ; same as doom-emacs
(define-key evil-motion-state-map "gb" 'evilnc-copy-and-comment-operator)
(define-key evil-motion-state-map "gc" 'evilnc-yank-and-comment-operator)

(defun my-current-line-html-p (paragraph-region)
  "Is current line html?"
  (let* ((line (buffer-substring-no-properties (line-beginning-position)
                                               (line-end-position)))
         (re (format "^[ \t]*\\(%s\\)?[ \t]*</?[a-zA-Z]+"
                     (regexp-quote evilnc-html-comment-start))))
    ;; current paragraph does contain html tag
    (if (and (>= (point) (car paragraph-region))
             (string-match-p re line))
        t)))

(defun my-evilnc-comment-or-uncomment-paragraphs (&optional num)
  "Comment or uncomment NUM paragraphs which might contain html tags."
  (interactive "p")
  (my-ensure 'evil-nerd-commenter)
  (let* ((paragraph-region (evilnc--get-one-paragraph-region))
         (html-p (ignore-errors
                   (or (save-excursion
                         (sgml-skip-tag-backward 1)
                         (my-current-line-html-p paragraph-region))
                       (save-excursion
                         (sgml-skip-tag-forward 1)
                         (my-current-line-html-p paragraph-region))))))
    (if html-p (evilnc-comment-or-uncomment-html-paragraphs num)
      (evilnc-comment-or-uncomment-paragraphs num))))

(defun my-imenu-comments ()
  "Imenu display comments."
  (interactive)
  (my-ensure 'counsel)
  (when (fboundp 'evilnc-imenu-create-index-function)
    (let* ((imenu-create-index-function 'evilnc-imenu-create-index-function))
      (counsel-imenu))))
;; }}

;; {{ `evil-matchit'
(my-run-with-idle-timer 2 #'global-evil-matchit-mode)
;; }}

;; {{ evil-exchange
;; press gx twice to exchange, gX to cancel
;; change default key bindings (if you want) HERE
;; (setq evil-exchange-key (kbd "zx"))
;; (my-run-with-idle-timer 4 #'evil-exchange-install)
;; }}

;; {{ @see https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org#replacing-text-with-iedit
;; same keybindings as spacemacs:
;;  - Start `iedit-mode' by `evil-iedit-state/iedit-mode'
;;  - "TAB" to toggle current occurrence
;;  - "n" next, "N" previous (obviously we use "p" for yank)
;;  - "gg" the first occurrence, "G" the last occurrence
;;  - Please note ";;" or `avy-goto-char-timer' is also useful
;; }}

;; {{ Evil’s f/F/t/T command can search PinYin ,
(my-run-with-idle-timer 4 #'evil-find-char-pinyin-mode)
;; }}

;; ;; In insert mode, press "fg" in 0.3 second to trigger my-counsel-company
;; ;; Run "grep fg english-words.txt", got "afghan".
;; ;; "afgan" is rarely used when programming
;; ;; @see https://github.com/redguardtoo/emacs.d/issues/870 first
;; (general-imap "f"
;;   (general-key-dispatch 'self-insert-command
;;     :timeout 0.3
;;     "g" 'my-counsel-company))

(defun my-switch-to-shell ()
  "Switch to built in or 3rd party shell."
  (interactive)
  (cond
   ((display-graphic-p)
    (switch-to-builtin-shell))
   (t
    (suspend-frame))))

;; press ",xx" to expand region
;; then press "char" to contract, "x" to expand
(with-eval-after-load 'evil
  ;; evil re-assign "M-." to `evil-repeat-pop-next' which I don't use actually.
  ;; Restore "M-." to original binding command
  (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
  (setq expand-region-contract-fast-key "V")
  ;; @see https://bitbucket.org/lyro/evil/issue/360/possible-evil-search-symbol-forward
  ;; evil 1.0.8 search word instead of symbol
  (setq evil-symbol-word-search t)

  ;; don't add replaced text to `kill-ring'
  (setq evil-kill-on-visual-paste nil)

  ;; @see https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  ;; uncomment below line to make "dw" has exact same behaviour in evil as as in vim
  ;; (defalias #'forward-evil-word #'forward-evil-symbol)

  ;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
  (defmacro adjust-major-mode-keymap-with-evil (m &optional r)
    `(with-eval-after-load (quote ,(if r r m))
       (evil-make-overriding-map ,(intern (concat m "-mode-map")) 'normal)
       ;; force update evil keymaps after git-timemachine-mode loaded
       (add-hook (quote ,(intern (concat m "-mode-hook"))) #'evil-normalize-keymaps)))

  (adjust-major-mode-keymap-with-evil "git-timemachine")

  ;; @see https://bitbucket.org/lyro/evil/issue/342/evil-default-cursor-setting-should-default
  ;; Cursor is always black because of evil.
  ;; Here is the workaround
  (setq evil-default-cursor t))

;; {{ per-major-mode setup
(general-create-definer my-javascript-leader-def
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :states '(normal motion insert emacs)
  :keymaps 'js2-mode-map)

(my-javascript-leader-def
  "de" 'js2-display-error-list
  "nn" 'js2-next-error
  "te" 'js2-mode-toggle-element
  "tf" 'js2-mode-toggle-hide-functions)

(general-create-definer my-org-leader-def
  :prefix ";"
  :non-normal-prefix "M-;"
  :states '(normal motion visual)
  :keymaps 'org-mode-map)

(my-org-leader-def
  "f" 'my-navigate-in-pdf
  "g" 'my-open-pdf-goto-page)
;; }}

;; @see https://github.com/redguardtoo/emacs.d/issues/955
;; `evil-paste-after' => `current-kill' => `interprogram-paste-function'=> `gui-selection-value'
;; `gui-selection-value' returns clipboard text from CLIPBOARD or "PRIMARY" clipboard which are
;; also controlled by `select-enable-clipboard' and `select-enable-primary'.
;; Please note `evil-visual-update-x-selection' automatically updates PRIMARY clipboard with
;; visual selection.
;; I set `my-evil-enable-visual-update-x-selection' to nil to avoid all those extra "features".
(defvar my-evil-enable-visual-update-x-selection nil
  "Automatically copy the selected text into evil register.
I'm not sure this is good idea.")
(defun my-evil-visual-update-x-selection-hack (orig-func &rest args)
  (when my-evil-enable-visual-update-x-selection
    (apply orig-func args)))
(advice-add 'evil-visual-update-x-selection :around #'my-evil-visual-update-x-selection-hack)

(defun alternate-window ()
  (interactive)
  (let ((prev-window (get-mru-window nil t t)))
  (unless prev-window (user-error "Last window not found."))
  (select-window prev-window)))

;; @see https://github.com/emacs-evil/evil-surround/pull/48
(evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)

(define-key evil-normal-state-map "t" 'avy-goto-char-timer)

;; {{ per-major-mode setup
(general-create-definer my-javascript-leader-def
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :states '(normal motion insert emacs)
  :keymaps 'js2-mode-map)

(my-javascript-leader-def
  "de" 'js2-display-error-list
  "nn" 'js2-next-error
  "te" 'js2-mode-toggle-element
  "tf" 'js2-mode-toggle-hide-functions)

(general-create-definer my-org-leader-def
  :prefix ";"
  :non-normal-prefix "M-;"
  :states '(normal motion visual)
  :keymaps 'org-mode-map)

(my-org-leader-def
  "f" 'my-open-pdf-from-history
  "n" 'my-open-pdf-next-page
  "g" 'my-open-pdf-goto-page
  "p" 'my-open-pdf-previous-page)
;; }}


(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-cx-install))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-calendar-want-org-bindings t
        evil-collection-want-unimpaired-p nil))

 (use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  ;;hook (org-mode . evil-org-mode) won't load after org-agenda
  :config
  (require 'evil-org-agenda)
  (evil-define-key 'motion org-agenda-mode-map
    "q" 'org-agenda-quit)
  (evil-org-agenda-set-keys))

(use-package evil-anzu
  :after evil
  :config
  (global-anzu-mode 1))

(provide 'init-evil)