;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package eaf
  :load-path "site-lisp/emacs-application-framework"
  :custom
  (when *win64* (setq eaf-python-command "python.exe"))
  (eaf-proxy-type "http")
  (when *wsl* (eaf-proxy-host (shell-command-to-string "grep nameserver /etc/resolv.conf | awk '{printf $2}'")))
  (eaf-proxy-port "8889")
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  (eaf-kill-process-after-last-buffer-closed t)
  (eaf-start-python-process-when-require nil)
  :config
  (defalias 'browse-web #'eaf-open-browser)
  (require 'eaf)
  (require 'eaf-terminal)
  (require 'eaf-markdown-previewer)
  (require 'eaf-music-player)
  (require 'eaf-pdf-viewer)
  (require 'eaf-file-browser)
  (require 'eaf-netease-cloud-music)
  (require 'eaf-jupyter)
  (require 'eaf-browser)
  (require 'eaf-video-player)
  (require 'eaf-image-viewer)
  (require 'eaf-file-manager)
  (require 'eaf-video-player)
  (require 'eaf-file-sender)
  (require 'eaf-org-previewer))

(provide 'init-eaf)
