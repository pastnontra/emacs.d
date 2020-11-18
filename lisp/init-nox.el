;; -*- coding: utf-8; lexical-binding: t; -*-

  (add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/nox"))
  (require 'posframe)
  (require 'xref)
  (require 'nox)
  (dolist (hook (list
                 'js-mode-hook
                 'rust-mode-hook
                 'python-mode-hook
                 'ruby-mode-hook
                 'java-mode-hook
                 'sh-mode-hook
                 'php-mode-hook
                 'c-mode-common-hook
                 'c-mode-hook
                 'c++-mode-hook
                 'haskell-mode-hook
                 ))
    (add-hook hook '(lambda () (nox-ensure))))
  (set 'nox-python-server-dir "~/python-language-server/output/bin/Debug/")

(provide 'init-nox)